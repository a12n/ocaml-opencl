let headers =
  "#ifdef __APPLE__\n" ^
  "#   include <OpenCL/cl.h>\n" ^
  "#else\n" ^
  "#   include <CL/cl.h>\n" ^
  "#endif\n"

let () =
  let fmt = Format.std_formatter in
  let prefix = "caml_" in
  match Sys.argv.(1) with
  | "c" -> (Format.fprintf fmt "%s@." headers;
            Cstubs.write_c fmt ~prefix (module Cl_bindings.Make))
  | "ml" -> Cstubs.write_ml fmt ~prefix (module Cl_bindings.Make)
  | _other -> failwith "Invalid binding generation mode"
