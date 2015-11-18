open Ctypes

module C = Cl_bindings.Make (Cl_generated)
module T = Cl_types.Make (Cl_types_detected)

type buffer
type image

type platform = T.cl_platform_id
type device = T.cl_device_id
type context = T.cl_context
type command_queue = T.cl_command_queue
type 'k mem = T.cl_mem
type program = T.cl_program
type kernel = T.cl_kernel
type event = T.cl_event
type sampler = T.cl_sampler

type error =
  | Device_not_found
  | Device_not_available
  | Compiler_not_available
  | Mem_object_allocation_failure
  | Out_of_resources
  | Out_of_host_memory
  | Profiling_info_not_available
  | Mem_copy_overlap
  | Image_format_mismatch
  | Image_format_not_supported
  | Build_program_failure
  | Map_failure
  | Invalid_value
  | Invalid_device_type
  | Invalid_platform
  | Invalid_device
  | Invalid_context
  | Invalid_queue_properties
  | Invalid_command_queue
  | Invalid_host_ptr
  | Invalid_mem_object
  | Invalid_image_format_descriptor
  | Invalid_image_size
  | Invalid_sampler
  | Invalid_binary
  | Invalid_build_options
  | Invalid_program
  | Invalid_program_executable
  | Invalid_kernel_name
  | Invalid_kernel_definition
  | Invalid_kernel
  | Invalid_arg_index
  | Invalid_arg_value
  | Invalid_arg_size
  | Invalid_kernel_args
  | Invalid_work_dimension
  | Invalid_work_group_size
  | Invalid_work_item_size
  | Invalid_global_offset
  | Invalid_event_wait_list
  | Invalid_event
  | Invalid_operation
  | Invalid_gl_object
  | Invalid_buffer_size
  | Invalid_mip_level
  | Invalid_global_work_size

(* TODO *)
let to_error = function _any -> Device_not_found

exception Exn of error

let check_error err =
  if err <> T._CL_SUCCESS then
    raise (Exn (to_error err))

let of_bool = function false -> T._CL_FALSE
                     | true -> T._CL_TRUE

module Platform = struct
  let get () =
    let num_platforms = allocate T.cl_uint Unsigned.UInt32.zero in
    C.clGetPlatformIDs Unsigned.UInt32.zero (from_voidp T.cl_platform_id null)
      num_platforms |> check_error;
    let platforms = CArray.make T.cl_platform_id
        (Unsigned.UInt32.to_int (!@ num_platforms)) in
    C.clGetPlatformIDs (!@ num_platforms) (CArray.start platforms)
      (from_voidp T.cl_uint null) |> check_error;
    CArray.to_list platforms

  (* TODO *)
  let extensions _platform = ""
  let name _platform = ""
  let profile _platform = ""
  let vendor _platform = ""
  let version _platform = ""
end

module Command_queue = struct
  type property =
    [ `Out_of_order_exec_mode of bool |
      `Profiling of bool ]

  (* TODO *)
  let create _context _device _properties = from_voidp T._cl_command_queue null

  (* TODO *)
  let context _queue = from_voidp T._cl_context null
  let device _queue = from_voidp T._cl_device_id null
  let properties _queue = []

  (* TODO *)
  let set_properties _queue _properties = []

  (* TODO *)
  let read_buffer ?(wait_list=[]) ?(blocking=true) ?(offset=0) ?size
      _queue _mem _ba = from_voidp T._cl_event null

  (* TODO *)
  let write_buffer ?(wait_list=[]) ?(blocking=true) ?(offset=0) ?size
      _queue _mem _ba = from_voidp T._cl_event null

  (* TODO *)
  let copy_buffer ?(wait_list=[]) _queue ~src_buffer ~dst_buffer
      ~src_offset ~dst_offset ~size = from_voidp T._cl_event null

  (* TODO *)
  let read_image ?(wait_list=[]) ?(blocking=true) ?(row_pitch=0)
      ?(slice_pitch=0) _queue _mem ~origin ~region _ba =
    from_voidp T._cl_event null

  (* TODO *)
  let write_image ?(wait_list=[]) ?(blocking=true) ?(row_pitch=0)
      ?(slice_pitch=0) _queue _mem ~origin ~region _ba =
    from_voidp T._cl_event null

  (* TODO *)
  let copy_image ?(wait_list=[]) _queue ~src_image ~dst_image
      ~src_origin ~dst_origin ~region = from_voidp T._cl_event null

  (* TODO *)
  let copy_image_to_buffer ?(wait_list=[]) _queue ~src_image
      ~dst_buffer ~src_origin ~region ~dst_offset = from_voidp T._cl_event null

  (* TODO *)
  let copy_buffer_to_image ?(wait_list=[]) _queue ~src_buffer
      ~dst_image ~src_offset ~dst_origin ~region = from_voidp T._cl_event null

  (* TODO *)
  let nd_range_kernel ?(wait_list=[]) ?global_work_offset
      ?local_work_size _queue _kernel ~global_work_size =
    from_voidp T._cl_event null

  (* TODO *)
  let task ?(wait_list=[]) _queue _kernel = from_voidp T._cl_event null

  (* TODO *)
  let marker _queue = from_voidp T._cl_event null
  let wait_for_events _queue _wait_list = ()
  let barrier _queue = ()

  (* TODO *)
  let finish _queue = ()
  let flush _queue = ()
end

module Device = struct
  type device_type =
    [ `Default | `Cpu | `Gpu | `Accelerator ]

  type exec_capabilities = {
    kernel : bool;
    native_kernel : bool;
  }

  type fp_config = {
    denorm : bool;
    inf_nan : bool;
    round_to_nearest : bool;
    round_to_zero : bool;
    round_to_inf : bool;
    fma : bool;
  }

  let of_device_type_list list =
    List.fold_left Unsigned.UInt64.add Unsigned.UInt64.zero
      (List.map (function `Default -> T._CL_DEVICE_TYPE_DEFAULT
                        | `Cpu -> T._CL_DEVICE_TYPE_CPU
                        | `Gpu -> T._CL_DEVICE_TYPE_GPU
                        | `Accelerator -> T._CL_DEVICE_TYPE_ACCELERATOR
                        | `All -> T._CL_DEVICE_TYPE_ALL) list)

  let get platform device_types =
    let device_type = of_device_type_list device_types in
    let num_devices = allocate T.cl_uint Unsigned.UInt32.zero in
    C.clGetDeviceIDs platform device_type Unsigned.UInt32.zero
      (from_voidp T.cl_device_id null) num_devices |> check_error;
    let devices = CArray.make T.cl_device_id
        (Unsigned.UInt32.to_int (!@ num_devices)) in
    C.clGetDeviceIDs platform device_type (!@ num_devices)
      (CArray.start devices) (from_voidp T.cl_uint null) |> check_error;
    CArray.to_list devices

  (* TODO *)
  let driver_version _device = ""
  let extensions _device = ""
  let name _device = ""
  let profile _device = ""
  let vendor _device = ""
  let version _device = ""

  (* TODO *)
  let address_bits _device = 0
  let global_mem_cache_size _device = 0L
  let global_mem_cacheline_size _device = 0
  let global_mem_size _device = 0L
  let image2d_max_height _device = 0
  let image2d_max_width _device = 0
  let image3d_max_depth _device = 0
  let image3d_max_height _device = 0
  let image3d_max_width _device = 0
  let local_mem_size _device = 0L
  let max_clock_frequency _device = 0
  let max_compute_units _device = 0
  let max_constant_args _device = 0
  let max_constant_buffer_size _device = 0L
  let max_mem_alloc_size _device = 0L
  let max_parameter_size _device = 0
  let max_read_image_args _device = 0
  let max_samplers _device = 0
  let max_work_group_size _device = 0
  let max_work_item_dimensions _device = 0
  let max_write_image_args _device = 0
  let mem_base_addr_align _device = 0
  let min_data_type_align_size _device = 0
  let preferred_vector_width_char _device = 0
  let preferred_vector_width_double _device = 0
  let preferred_vector_width_float _device = 0
  let preferred_vector_width_int _device = 0
  let preferred_vector_width_long _device = 0
  let preferred_vector_width_short _device = 0
  let profiling_timer_resolution _device = 0
  let vendor_id _device = 0

  (* TODO *)
  let available _device = false
  let compiler_available _device = false
  let endian_little _device = false
  let error_correction_support _device = false
  let image_support _device = false

  (* TODO *)
  let single_fp_config _device =
    { denorm = false;
      inf_nan = false;
      round_to_nearest = false;
      round_to_zero = false;
      round_to_inf = false;
      fma = false }

  (* TODO *)
  let device_type _device = []
  let execution_capabilities _device =
    { kernel = false;
      native_kernel = false }
  let global_mem_cache_type _device = None
  let local_mem_type _device = None
  let max_work_item_sizes _device = [||]
  let platform _device = (from_voidp T._cl_platform_id null)
  let queue_properties _device = []
end

module Context = struct
  type property =
    [ `Platform of platform ]

  let of_property = function `Platform platform ->
    [ Unsigned.UInt32.to_int T._CL_CONTEXT_PLATFORM |> Nativeint.of_int;
      raw_address_of_ptr (to_voidp platform) ]

  let of_property_list properties =
    List.fold_right (@)
      (List.map of_property properties) [Nativeint.zero]

  (* TODO: notify closure *)
  let create ?notify properties devices =
    let properties = CArray.of_list T.cl_context_properties
        (of_property_list properties) in
    let devices = CArray.of_list T.cl_device_id devices in
    let err = allocate T.cl_int T._CL_SUCCESS in
    let context = C.clCreateContext (CArray.start properties)
        (Unsigned.UInt32.of_int (CArray.length devices))
        (CArray.start devices) None null err in
    check_error (!@ err);
    context

  (* TODO: notify closure *)
  let create_from_type ?notify properties device_types =
    let properties = CArray.of_list T.cl_context_properties
        (of_property_list properties) in
    let device_type = Device.of_device_type_list device_types in
    let err = allocate T.cl_int T._CL_SUCCESS in
    let context = C.clCreateContextFromType (CArray.start properties)
        device_type None null err in
    check_error (!@ err);
    context

  (* TODO *)
  let devices _context = []
  let properties _context = []
end

module Mem = struct
  type flag =
    [ `Read_write | `Write_only | `Read_only |
      `Use_host_ptr | `Alloc_host_ptr | `Copy_host_ptr ]

  let of_flag_list flags =
    List.fold_left Unsigned.UInt64.add Unsigned.UInt64.zero
      (List.map (function `Read_write -> T._CL_MEM_READ_WRITE
                        | `Write_only -> T._CL_MEM_WRITE_ONLY
                        | `Read_only -> T._CL_MEM_READ_ONLY
                        | `Use_host_ptr -> T._CL_MEM_USE_HOST_PTR
                        | `Alloc_host_ptr -> T._CL_MEM_ALLOC_HOST_PTR
                        | `Copy_host_ptr -> T._CL_MEM_COPY_HOST_PTR) flags)

  type mem_type =
    [ `Buffer | `Image2d | `Image3d ]

  let of_mem_type = function
    | `Buffer -> T._CL_MEM_OBJECT_BUFFER
    | `Image2d -> T._CL_MEM_OBJECT_IMAGE2D
    | `Image3d -> T._CL_MEM_OBJECT_IMAGE3D

  (* TODO *)
  let create_buffer _context _flags _host_data = from_voidp T._cl_mem null

  (* TODO *)
  let context _mem = from_voidp T._cl_context null
  let flags _mem = []
  let map_count _mem = 0
  let mem_type _mem = `Buffer
  let size _mem = 0

  type intensity_channel_type =
    [ `Unorm_int8 | `Unorm_int16 |
      `Snorm_int8 | `Snorm_int16 |
      `Half_float | `Float ]

  let to_intensity_channel_type = function
    | c when c = T._CL_UNORM_INT8 -> `Unorm_int8
    | c when c = T._CL_UNORM_INT16 -> `Unorm_int16
    | c when c = T._CL_SNORM_INT8 -> `Snorm_int8
    | c when c = T._CL_SNORM_INT16 -> `Snorm_int16
    | c when c = T._CL_HALF_FLOAT -> `Half_float
    | c when c = T._CL_FLOAT -> `Float
    | _other -> failwith "Cl.Mem.to_intensity_channel_type"

  type rgb_channel_type =
    [ `Unorm_short_565 | `Unorm_short_555 | `Unorm_int_101010 ]

  let to_rgb_channel_type = function
    | c when c = T._CL_UNORM_SHORT_565 -> `Unorm_short_565
    | c when c = T._CL_UNORM_SHORT_555 -> `Unorm_short_555
    | c when c = T._CL_UNORM_INT_101010 -> `Unorm_int_101010
    | _other -> failwith "Cl.Mem.to_rgb_channel_type"

  type argb_channel_type =
    [ `Unorm_int8 | `Snorm_int8 |
      `Signed_int8 | `Unsigned_int8 ]

  let to_argb_channel_type = function
    | c when c = T._CL_UNORM_INT8 -> `Unorm_int8
    | c when c = T._CL_SNORM_INT8 -> `Snorm_int8
    | c when c = T._CL_SIGNED_INT8 -> `Signed_int8
    | c when c = T._CL_UNSIGNED_INT8 -> `Unsigned_int8
    | _other -> failwith "Cl.Mem.to_argb_channel_type"

  type channel_type =
    [ intensity_channel_type | rgb_channel_type | argb_channel_type |
      `Signed_int16 | `Signed_int32 |
      `Unsigned_int16 | `Unsigned_int32 ]

  let to_channel_type = function
    | c when c = T._CL_SIGNED_INT16 -> `Signed_int16
    | c when c = T._CL_SIGNED_INT32 -> `Signed_int32
    | c when c = T._CL_UNSIGNED_INT16 -> `Unsigned_int16
    | c when c = T._CL_UNSIGNED_INT32 -> `Unsigned_int32
    | data_type ->
      (try to_intensity_channel_type data_type
       with Failure _ ->
         (try to_rgb_channel_type data_type
          with Failure _ ->
            (try to_argb_channel_type data_type
             with Failure _ -> failwith "Cl.Mem.to_channel_type")))

  type image_format =
    [ `R of channel_type |
      `A of channel_type |
      `Intensity of intensity_channel_type |
      `Luminance of intensity_channel_type |
      `Rg of channel_type |
      `Ra of channel_type |
      `Rgb of rgb_channel_type |
      `Rgba of channel_type |
      `Argb of argb_channel_type |
      `Bgra of argb_channel_type ]

  let to_image_format format =
    let order = getf format T.image_channel_order in
    let data_type = getf format T.image_channel_data_type in
    match order with
    | c when c = T._CL_R -> `R (to_channel_type data_type)
    | c when c = T._CL_A -> `A (to_channel_type data_type)
    | c when c = T._CL_INTENSITY ->
      `Intensity (to_intensity_channel_type data_type)
    | c when c = T._CL_LUMINANCE ->
      `Luminance (to_intensity_channel_type data_type)
    | c when c = T._CL_RG -> `Rg (to_channel_type data_type)
    | c when c = T._CL_RA -> `Ra (to_channel_type data_type)
    | c when c = T._CL_RGB -> `Rgb (to_rgb_channel_type data_type)
    | c when c = T._CL_RGBA -> `Rgba (to_channel_type data_type)
    | c when c = T._CL_ARGB -> `Argb (to_argb_channel_type data_type)
    | c when c = T._CL_BGRA -> `Bgra (to_argb_channel_type data_type)
    | _other -> failwith "Cl.Mem.to_image_format"

  (* TODO *)
  let create_image2d ?(row_pitch=0) _context _flags _format
      ~width ~height _ba_opt = from_voidp T._cl_mem null

  (* TODO *)
  let create_image3d ?(row_pitch=0) ?(slice_pitch=0) _context _flags _format
      ~width ~height ~depth _ba_opt = from_voidp T._cl_mem null

  let supported_image_formats context flags mem_type =
    let flags = of_flag_list flags in
    let image_type = of_mem_type mem_type in
    let num_image_formats = allocate T.cl_uint Unsigned.UInt32.zero in
    C.clGetSupportedImageFormats context flags image_type Unsigned.UInt32.zero
      (from_voidp T.cl_image_format null) num_image_formats |> check_error;
    let image_formats = CArray.make T.cl_image_format
        (Unsigned.UInt32.to_int (!@ num_image_formats)) in
    C.clGetSupportedImageFormats context flags image_type (!@ num_image_formats)
      (CArray.start image_formats) (from_voidp T.cl_uint null) |> check_error;
    CArray.to_list image_formats |> List.map to_image_format

  (* TODO *)
  let image_format _mem = `Intensity `Float
  let image_element_size _mem = 0
  let image_row_pitch _mem = 0
  let image_slice_pitch _mem = 0
  let image_width _mem = 0
  let image_height _mem = 0
  let image_depth _mem = 0
end

module Sampler = struct
  type addressing_mode =
    [ `Clamp_to_edge | `Clamp | `Repeat ]

  type filter_mode =
    [ `Nearest | `Linear ]

  let create context norm_coords addressing filter =
    let normalized_coords = of_bool norm_coords in
    let addressing_mode =
      match addressing with
      | None -> T._CL_ADDRESS_NONE
      | Some `Clamp_to_edge -> T._CL_ADDRESS_CLAMP_TO_EDGE
      | Some `Clamp -> T._CL_ADDRESS_CLAMP
      | Some `Repeat -> T._CL_ADDRESS_REPEAT in
    let filter_mode =
      match filter with
      | `Nearest -> T._CL_FILTER_NEAREST
      | `Linear -> T._CL_FILTER_LINEAR in
    let err = allocate T.cl_int T._CL_SUCCESS in
    let sampler = C.clCreateSampler context normalized_coords
        addressing_mode filter_mode err in
    check_error (!@ err);
    sampler

  (* TODO *)
  let context _sampler = from_voidp T._cl_context null
  let addressing_mode _sampler = None
  let filter_mode _sampler = `Nearest
  let normalized_coords _sampler = false
end

module Program = struct
  let create_with_source context strings =
    let lengths = CArray.of_list size_t
        (List.map Unsigned.Size_t.of_int (List.map String.length strings)) in
    let strings = CArray.of_list string strings in
    let err = allocate T.cl_int T._CL_SUCCESS in
    let program = C.clCreateProgramWithSource context
        (Unsigned.UInt32.of_int (CArray.length strings)) (CArray.start strings)
        (CArray.start lengths) err in
    check_error (!@ err);
    program

  let to_binary_status = function
    | c when c = T._CL_SUCCESS -> None
    | c when c = T._CL_INVALID_VALUE -> Some `Invalid_value
    | c when c = T._CL_INVALID_BINARY -> Some `Invalid_binary
    | _other -> failwith "Cl.Program.to_binary_status"

  let create_with_binary context device_binaries =
    let devices, binaries = List.split device_binaries in
    let devices = CArray.of_list T.cl_device_id devices in
    let lengths = CArray.of_list size_t
        (List.map Unsigned.Size_t.of_int (List.map Bytes.length binaries)) in
    let binaries = CArray.of_list ocaml_bytes
        (List.map ocaml_bytes_start binaries) in
    let binary_status = CArray.make T.cl_int (CArray.length devices) in
    let err = allocate T.cl_int T._CL_SUCCESS in
    let program = C.clCreateProgramWithBinary context
        (Unsigned.UInt32.of_int (CArray.length devices)) (CArray.start devices)
        (CArray.start lengths) (CArray.start binaries)
        (CArray.start binary_status) err in
    check_error (!@ err);
    program, List.map to_binary_status (CArray.to_list binary_status)

  let build ?notify program devices options =
    let devices = CArray.of_list T.cl_device_id devices in
    let notify = None in        (* TODO *)
    C.clBuildProgram program (Unsigned.UInt32.of_int (CArray.length devices))
      (CArray.start devices) options notify null |> check_error

  (* TODO *)
  let context _program = from_voidp T._cl_context null
  let devices _program = []
  let source _program = ""
  let binaries _program = []

  (* TODO *)
  let build_status _program _device = None
  let build_options _program _device = ""
  let build_log _program _device = ""
end

module Kernel = struct
  let create program kernel_name =
    let err = allocate T.cl_int T._CL_SUCCESS in
    let kernel = C.clCreateKernel program kernel_name err in
    check_error (!@ err);
    kernel

  let create_all program =
    let num_kernels = allocate T.cl_uint Unsigned.UInt32.zero in
    C.clCreateKernelsInProgram program Unsigned.UInt32.zero
      (from_voidp T.cl_kernel null) num_kernels |> check_error;
    let kernels = CArray.make T.cl_kernel
        (Unsigned.UInt32.to_int (!@ num_kernels)) in
    C.clCreateKernelsInProgram program (!@ num_kernels) (CArray.start kernels)
      (from_voidp T.cl_uint null) |> check_error;
    CArray.to_list kernels

  type 'k arg =
    [ `Char of char | `Uchar of int |
      `Short of int | `Ushort of int |
      `Int of int | `Uint of int |
      `Long of int64 | `Ulong of int64 |
      `Float of float | `Double of float | `Half of int |
      `Mem of 'k mem | `Null_mem |
      `Sampler of sampler ]

  type local_arg =
    [ `Local_char |
      `Local_short |
      `Local_int |
      `Local_long |
      `Local_float |
      `Local_double |
      `Local_half ]

  let set_arg kernel index arg_opt =
    let arg_size, arg_value =
      match arg_opt with
      | `Char c -> (sizeof T.cl_char,
                    to_voidp (allocate T.cl_char (Char.code c)))
      | `Uchar uc -> (sizeof T.cl_uchar,
                      to_voidp (allocate T.cl_uchar (Unsigned.UInt8.of_int uc)))
      | `Short s -> (sizeof T.cl_short, to_voidp (allocate T.cl_short s))
      | `Ushort us ->
        (sizeof T.cl_ushort,
         to_voidp (allocate T.cl_ushort (Unsigned.UInt16.of_int us)))
      | `Int i -> (sizeof T.cl_int,
                   to_voidp (allocate T.cl_int (Signed.Int32.of_int i)))
      | `Uint ui -> (sizeof T.cl_uint,
                     to_voidp (allocate T.cl_uint (Unsigned.UInt32.of_int ui)))
      | `Long l -> (sizeof T.cl_long,
                    to_voidp (allocate T.cl_long (Signed.Int64.of_int64 l)))
      | `Ulong ul ->
        (sizeof T.cl_ulong,
         to_voidp (allocate T.cl_ulong (Unsigned.UInt64.of_int64 ul)))
      | `Float f -> (sizeof T.cl_float, to_voidp (allocate T.cl_float f))
      | `Double d -> (sizeof T.cl_double, to_voidp (allocate T.cl_double d))
      | `Half h -> (sizeof T.cl_half,
                    to_voidp (allocate T.cl_half (Unsigned.UInt16.of_int h)))
      | `Mem m -> (sizeof T.cl_mem, to_voidp (allocate T.cl_mem m))
      | `Null_mem -> (sizeof T.cl_mem, null)
      | `Sampler s -> (sizeof T.cl_sampler, to_voidp (allocate T.cl_sampler s))
      | `Local_char -> (sizeof T.cl_char, null)
      | `Local_short -> (sizeof T.cl_short, null)
      | `Local_int -> (sizeof T.cl_int, null)
      | `Local_long -> (sizeof T.cl_long, null)
      | `Local_float -> (sizeof T.cl_float, null)
      | `Local_double -> (sizeof T.cl_double, null)
      | `Local_half -> (sizeof T.cl_half, null) in
    C.clSetKernelArg kernel (Unsigned.UInt32.of_int index)
      (Unsigned.Size_t.of_int arg_size) arg_value |> check_error

  (* TODO *)
  let function_name _kernel = ""
  let num_args _kernel = 0
  let context _kernel = from_voidp T._cl_context null
  let program _kernel = from_voidp T._cl_program null

  let work_group_size _kernel _device = 0
  let compile_work_group_size _kernel _device = 0, 0, 0
  let local_mem_size _kernel _device = 0L
end

module Event = struct
  type command_type =
    [ `Ndrange_kernel | `Task | `Native_kernel |
      `Read_buffer | `Write_buffer | `Copy_buffer |
      `Read_image | `Write_image |
      `Copy_image | `Copy_image_to_buffer | `Copy_buffer_to_image |
      `Map_buffer | `Map_image | `Unmap_mem_object |
      `Marker ]

  type command_execution_status =
    [ `Queued | `Submitted | `Running | `Complete | `Error of int ]

  let wait events =
    let events = CArray.of_list T.cl_event events in
    C.clWaitForEvents (Unsigned.UInt32.of_int (CArray.length events))
      (CArray.start events) |> check_error

  (* TODO *)
  let command_queue _event = from_voidp T._cl_command_queue null
  let command_type _event = `Marker
  let command_execution_status _event = `Complete

  (* TODO *)
  let command_queued _event = 0L
  let command_submit _event = 0L
  let command_start _event = 0L
  let command_end _event = 0L
end

let unload_compiler () = C.clUnloadCompiler () |> check_error
