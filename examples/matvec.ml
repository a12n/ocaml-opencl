(* OCaml version of matrix/vector multiplication example from
 * "OpenCL in Action". *)

open Bigarray

let read_file fname =
  let chan = open_in fname in
  let size = in_channel_length chan in
  let buf = Bytes.create size in
  really_input chan buf 0 size;
  close_in chan;
  buf

let () =
  let platform = match Cl.Platform.get () with
    | platform :: _ -> platform
    | [] -> failwith "No platforms" in
  let device = match Cl.Device.get platform [`All] with
    | device :: _ -> device
    | [] -> failwith "No devices" in
  let context = Cl.Context.create [`Platform platform] [device] in
  let source = read_file "matvec.cl" |> Bytes.to_string in
  let options = "" in
  let program = Cl.Program.create_with_source context [source] in
  Cl.Program.build program [device] options;
  let kernel = Cl.Kernel.create program "matvec_mul" in
  let queue = Cl.Command_queue.create context device [`Profiling true] in
  (* Create arrays *)
  let mat = Array2.create float32 c_layout 4 4 in
  for i = 0 to 4 - 1 do
    for j = 0 to 4 - 1 do
      mat.{i, j} <- float_of_int (i + j)
    done
  done;
  let vec = Array1.create float32 c_layout 4 in
  for i = 0 to 4 - 1 do
    vec.{i} <- float_of_int (i * 3)
  done;
  let ans = Array1.create float32 c_layout 4 in
  (* Create buffers *)
  let mat_buf = Cl.Mem.create_buffer context [`Read_only; `Copy_host_ptr]
      (`Use (genarray_of_array2 mat)) in
  let vec_buf = Cl.Mem.create_buffer context [`Read_only; `Copy_host_ptr]
      (`Use (genarray_of_array1 vec)) in
  let ans_buf = Cl.Mem.create_buffer context [`Write_only]
      (`Alloc (float32, [|4|])) in
  (* Set kernel args *)
  Cl.Kernel.(set_arg kernel 0 (`Mem mat_buf);
             set_arg kernel 1 (`Mem vec_buf);
             set_arg kernel 2 (`Mem ans_buf));
  (* Enqueue kernel *)
  let global_work_size = [|4|] in
  let event = Cl.Command_queue.nd_range_kernel queue kernel ~global_work_size in
  Cl.Event.wait [event];
  let command_start, command_end =
    Cl.Event.(command_start event, command_end event) in
  Printf.eprintf "Time: %Ld\n" (Int64.sub command_end command_start);
  (* Print ans *)
  let event = Cl.Command_queue.read_buffer queue
      ans_buf (genarray_of_array1 ans) in
  Cl.Event.wait [event];
  for i = 0 to 4 - 1 do
    Printf.printf "ans.{%d} = %f\n" i ans.{i}
  done
