open Ctypes

module C = Cl_bindings.Make (Cl_generated)
module T = Cl_types.Make (Cl_types_detected)

module CArray_ext = struct
  let of_array typ array =
    let ans = CArray.make typ (Array.length array) in
    Array.iteri (fun i v -> CArray.set ans i v) array;
    ans

  let to_array carray =
    Array.init (CArray.length carray) (CArray.get carray)
end

(* GC and object lifetime related functions. *)
module Gc_ext = struct
  (* Make heap allocated [b] value valid for the lifetime of heap
     allocated [a]. *)
  let link a b = Gc.finalise (fun _a -> ignore b) a

  (* Links [b] to [a], but only if there is some [b]. *)
  let link_opt a = function Some b -> link a b | None -> ()
end

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

let to_error = function
  | n when n = T._CL_DEVICE_NOT_FOUND -> Device_not_found
  | n when n = T._CL_DEVICE_NOT_AVAILABLE -> Device_not_available
  | n when n = T._CL_COMPILER_NOT_AVAILABLE -> Compiler_not_available
  | n when n = T._CL_MEM_OBJECT_ALLOCATION_FAILURE ->
    Mem_object_allocation_failure
  | n when n = T._CL_OUT_OF_RESOURCES -> Out_of_resources
  | n when n = T._CL_OUT_OF_HOST_MEMORY -> Out_of_host_memory
  | n when n = T._CL_PROFILING_INFO_NOT_AVAILABLE ->
    Profiling_info_not_available
  | n when n = T._CL_MEM_COPY_OVERLAP -> Mem_copy_overlap
  | n when n = T._CL_IMAGE_FORMAT_MISMATCH -> Image_format_mismatch
  | n when n = T._CL_IMAGE_FORMAT_NOT_SUPPORTED -> Image_format_not_supported
  | n when n = T._CL_BUILD_PROGRAM_FAILURE -> Build_program_failure
  | n when n = T._CL_MAP_FAILURE -> Map_failure
  | n when n = T._CL_INVALID_VALUE -> Invalid_value
  | n when n = T._CL_INVALID_DEVICE_TYPE -> Invalid_device_type
  | n when n = T._CL_INVALID_PLATFORM -> Invalid_platform
  | n when n = T._CL_INVALID_DEVICE -> Invalid_device
  | n when n = T._CL_INVALID_CONTEXT -> Invalid_context
  | n when n = T._CL_INVALID_QUEUE_PROPERTIES -> Invalid_queue_properties
  | n when n = T._CL_INVALID_COMMAND_QUEUE -> Invalid_command_queue
  | n when n = T._CL_INVALID_HOST_PTR -> Invalid_host_ptr
  | n when n = T._CL_INVALID_MEM_OBJECT -> Invalid_mem_object
  | n when n = T._CL_INVALID_IMAGE_FORMAT_DESCRIPTOR ->
    Invalid_image_format_descriptor
  | n when n = T._CL_INVALID_IMAGE_SIZE -> Invalid_image_size
  | n when n = T._CL_INVALID_SAMPLER -> Invalid_sampler
  | n when n = T._CL_INVALID_BINARY -> Invalid_binary
  | n when n = T._CL_INVALID_BUILD_OPTIONS -> Invalid_build_options
  | n when n = T._CL_INVALID_PROGRAM -> Invalid_program
  | n when n = T._CL_INVALID_PROGRAM_EXECUTABLE -> Invalid_program_executable
  | n when n = T._CL_INVALID_KERNEL_NAME -> Invalid_kernel_name
  | n when n = T._CL_INVALID_KERNEL_DEFINITION -> Invalid_kernel_definition
  | n when n = T._CL_INVALID_KERNEL -> Invalid_kernel
  | n when n = T._CL_INVALID_ARG_INDEX -> Invalid_arg_index
  | n when n = T._CL_INVALID_ARG_VALUE -> Invalid_arg_value
  | n when n = T._CL_INVALID_ARG_SIZE -> Invalid_arg_size
  | n when n = T._CL_INVALID_KERNEL_ARGS -> Invalid_kernel_args
  | n when n = T._CL_INVALID_WORK_DIMENSION -> Invalid_work_dimension
  | n when n = T._CL_INVALID_WORK_GROUP_SIZE -> Invalid_work_group_size
  | n when n = T._CL_INVALID_WORK_ITEM_SIZE -> Invalid_work_item_size
  | n when n = T._CL_INVALID_GLOBAL_OFFSET -> Invalid_global_offset
  | n when n = T._CL_INVALID_EVENT_WAIT_LIST -> Invalid_event_wait_list
  | n when n = T._CL_INVALID_EVENT -> Invalid_event
  | n when n = T._CL_INVALID_OPERATION -> Invalid_operation
  | n when n = T._CL_INVALID_GL_OBJECT -> Invalid_gl_object
  | n when n = T._CL_INVALID_BUFFER_SIZE -> Invalid_buffer_size
  | n when n = T._CL_INVALID_MIP_LEVEL -> Invalid_mip_level
  | n when n = T._CL_INVALID_GLOBAL_WORK_SIZE -> Invalid_global_work_size
  | _other -> failwith "Cl.to_error"

exception Exn of error

let check_error err =
  if err <> T._CL_SUCCESS then
    raise (Exn (to_error err))

let of_bool = function false -> T._CL_FALSE
                     | true -> T._CL_TRUE

let to_bool = ((<>) T._CL_FALSE)

(* Get various info attributes of OpenCL objects. *)
module Info = struct
  let carray info_function typ =
    let param_size = allocate size_t Unsigned.Size_t.zero in
    info_function Unsigned.Size_t.zero null param_size |> check_error;
    let param_value = CArray.make typ
        ((Unsigned.Size_t.to_int (!@ param_size)) / (sizeof typ)) in
    info_function (!@ param_size) (to_voidp (CArray.start param_value))
      (from_voidp size_t null) |> check_error;
    param_value

  let array info_function typ = carray info_function typ |> CArray_ext.to_array

  let string info_function =
    let chars = carray info_function char in
    match CArray.length chars - 1 with
    | length when length > 0 -> string_from_ptr ~length (CArray.start chars)
    | _other -> ""

  let value info_function typ =
    let param_value = CArray.make typ 1 in
    info_function (Unsigned.Size_t.of_int (sizeof typ))
      (to_voidp (CArray.start param_value))
      (from_voidp size_t null) |> check_error;
    CArray.get param_value 0
end

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

  let extensions platform =
    Info.string (C.clGetPlatformInfo platform T._CL_PLATFORM_EXTENSIONS)

  let name platform =
    Info.string (C.clGetPlatformInfo platform T._CL_PLATFORM_NAME)

  let profile platform =
    Info.string (C.clGetPlatformInfo platform T._CL_PLATFORM_PROFILE)

  let vendor platform =
    Info.string (C.clGetPlatformInfo platform T._CL_PLATFORM_VENDOR)

  let version platform =
    Info.string (C.clGetPlatformInfo platform T._CL_PLATFORM_VERSION)
end

module Command_queue = struct
  type property =
    [ `Out_of_order_exec_mode of bool |
      `Profiling of bool ]

  let of_property = function
    | `Out_of_order_exec_mode true -> T._CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE
    | `Profiling true -> T._CL_QUEUE_PROFILING_ENABLE
    | _other -> Unsigned.UInt64.zero

  let of_property_list properties =
    List.fold_left Unsigned.UInt64.add Unsigned.UInt64.zero
      (List.map of_property properties)

  let create context device properties =
    let properties = of_property_list properties in
    let err = allocate T.cl_int T._CL_SUCCESS in
    let queue = C.clCreateCommandQueue context device properties err in
    check_error (!@ err);
    queue

  let context queue =
    Info.value (C.clGetCommandQueueInfo queue T._CL_QUEUE_CONTEXT) T.cl_context

  let device queue =
    Info.value (C.clGetCommandQueueInfo queue T._CL_QUEUE_DEVICE) T.cl_device_id

  (* TODO *)
  let properties _queue = []

  (* TODO *)
  let set_properties _queue _properties = []

  (* XXX: Read/write operations for bigarrays are always blocking.
     There seems to be no way to guarantee that bigarray will be
     reachable for the duration of the operation. *)

  let tuple3_to_carray typ conv (x, y, z) =
    CArray_ext.of_array typ [|conv x; conv y; conv z|]

  let rw_buffer ?size c_function wait_list blocking offset queue mem ba =
    let blocking = T._CL_TRUE in
    let array = array_of_bigarray genarray ba in
    let size = match size with
      | Some n -> n
      | None -> CArray.length array * (sizeof (CArray.element_type array)) in
    let wait_list = CArray.of_list T.cl_event wait_list in
    let event = allocate T.cl_event (from_voidp T._cl_event null) in
    c_function queue mem blocking (Unsigned.Size_t.of_int offset)
      (Unsigned.Size_t.of_int size) (to_voidp (CArray.start array))
      (Unsigned.UInt32.of_int (CArray.length wait_list))
      (CArray.start wait_list) event |> check_error;
    !@ event

  let read_buffer ?(wait_list=[]) ?(blocking=true) ?(offset=0) ?size =
    rw_buffer ?size C.clEnqueueReadBuffer wait_list blocking offset

  let write_buffer ?(wait_list=[]) ?(blocking=true) ?(offset=0) ?size =
    rw_buffer ?size C.clEnqueueWriteBuffer wait_list blocking offset

  let copy_buffer ?(wait_list=[]) queue ~src_buffer ~dst_buffer
      ~src_offset ~dst_offset ~size =
    let wait_list = CArray.of_list T.cl_event wait_list in
    let event = allocate T.cl_event (from_voidp T._cl_event null) in
    C.clEnqueueCopyBuffer queue src_buffer dst_buffer
      (Unsigned.Size_t.of_int src_offset) (Unsigned.Size_t.of_int dst_offset)
      (Unsigned.Size_t.of_int size)
      (Unsigned.UInt32.of_int (CArray.length wait_list))
      (CArray.start wait_list) event |> check_error;
    !@ event

  let rw_image c_function wait_list blocking row_pitch slice_pitch
      queue mem origin region ba =
    let blocking = T._CL_TRUE in
    let origin = tuple3_to_carray size_t Unsigned.Size_t.of_int origin in
    let region = tuple3_to_carray size_t Unsigned.Size_t.of_int region in
    let wait_list = CArray.of_list T.cl_event wait_list in
    let event = allocate T.cl_event (from_voidp T._cl_event null) in
    c_function queue mem blocking (CArray.start origin) (CArray.start region)
      (Unsigned.Size_t.of_int row_pitch) (Unsigned.Size_t.of_int slice_pitch)
      (to_voidp (bigarray_start genarray ba))
      (Unsigned.UInt32.of_int (CArray.length wait_list))
      (CArray.start wait_list) event |> check_error;
    !@ event

  let read_image ?(wait_list=[]) ?(blocking=true) ?(row_pitch=0)
      ?(slice_pitch=0) queue mem ~origin ~region ba =
    rw_image C.clEnqueueReadImage wait_list blocking row_pitch slice_pitch
      queue mem origin region ba

  let write_image ?(wait_list=[]) ?(blocking=true) ?(row_pitch=0)
      ?(slice_pitch=0) queue mem ~origin ~region ba =
    rw_image C.clEnqueueWriteImage wait_list blocking row_pitch slice_pitch
      queue mem origin region ba

  let copy_image ?(wait_list=[]) queue ~src_image ~dst_image
      ~src_origin ~dst_origin ~region =
    let src_origin =
      tuple3_to_carray size_t Unsigned.Size_t.of_int src_origin in
    let dst_origin =
      tuple3_to_carray size_t Unsigned.Size_t.of_int dst_origin in
    let region = tuple3_to_carray size_t Unsigned.Size_t.of_int region in
    let wait_list = CArray.of_list T.cl_event wait_list in
    let event = allocate T.cl_event (from_voidp T._cl_event null) in
    C.clEnqueueCopyImage queue src_image dst_image (CArray.start src_origin)
      (CArray.start dst_origin) (CArray.start region)
      (Unsigned.UInt32.of_int (CArray.length wait_list))
      (CArray.start wait_list) event |> check_error;
    !@ event

  let copy_image_to_buffer ?(wait_list=[]) queue ~src_image
      ~dst_buffer ~src_origin ~region ~dst_offset =
    let src_origin =
      tuple3_to_carray size_t Unsigned.Size_t.of_int src_origin in
    let region = tuple3_to_carray size_t Unsigned.Size_t.of_int region in
    let wait_list = CArray.of_list T.cl_event wait_list in
    let event = allocate T.cl_event (from_voidp T._cl_event null) in
    C.clEnqueueCopyImageToBuffer queue src_image dst_buffer
      (CArray.start src_origin) (CArray.start region)
      (Unsigned.Size_t.of_int dst_offset)
      (Unsigned.UInt32.of_int (CArray.length wait_list))
      (CArray.start wait_list) event |> check_error;
    !@ event

  let copy_buffer_to_image ?(wait_list=[]) queue ~src_buffer
      ~dst_image ~src_offset ~dst_origin ~region =
    let dst_origin =
      tuple3_to_carray size_t Unsigned.Size_t.of_int dst_origin in
    let region = tuple3_to_carray size_t Unsigned.Size_t.of_int region in
    let wait_list = CArray.of_list T.cl_event wait_list in
    let event = allocate T.cl_event (from_voidp T._cl_event null) in
    C.clEnqueueCopyBufferToImage queue src_buffer dst_image
      (Unsigned.Size_t.of_int src_offset) (CArray.start dst_origin)
      (CArray.start region) (Unsigned.UInt32.of_int (CArray.length wait_list))
      (CArray.start wait_list) event |> check_error;
    !@ event

  let nd_range_kernel ?(wait_list=[]) ?global_work_offset
      ?local_work_size queue kernel ~global_work_size =
    let work_dim = Array.length global_work_size in
    let global_work_offset = from_voidp size_t null in
    let global_work_size = CArray_ext.of_array size_t
        (Array.map Unsigned.Size_t.of_int global_work_size) |> CArray.start in
    let local_work_size = match local_work_size with
      | Some a ->
        ( assert (Array.length a = work_dim);
          CArray_ext.of_array size_t
            (Array.map Unsigned.Size_t.of_int a) |> CArray.start )
      | None -> from_voidp size_t null in
    let wait_list = CArray.of_list T.cl_event wait_list in
    let event = allocate T.cl_event (from_voidp T._cl_event null) in
    C.clEnqueueNDRangeKernel queue kernel (Unsigned.UInt32.of_int work_dim)
      global_work_offset global_work_size local_work_size
      (Unsigned.UInt32.of_int (CArray.length wait_list))
      (CArray.start wait_list) event |> check_error;
    !@ event

  let task ?(wait_list=[]) queue kernel =
    let wait_list = CArray.of_list T.cl_event wait_list in
    let event = allocate T.cl_event (from_voidp T._cl_event null) in
    C.clEnqueueTask queue kernel
      (Unsigned.UInt32.of_int (CArray.length wait_list))
      (CArray.start wait_list) event |> check_error;
    !@ event

  let marker queue =
    let event = allocate T.cl_event (from_voidp T._cl_event null) in
    C.clEnqueueMarker queue event |> check_error;
    !@ event

  let wait_for_events queue wait_list =
    let wait_list = CArray.of_list T.cl_event wait_list in
    C.clEnqueueWaitForEvents queue
      (Unsigned.UInt32.of_int (CArray.length wait_list))
      (CArray.start wait_list) |> check_error

  let barrier queue = C.clEnqueueBarrier queue |> check_error

  let finish queue = C.clFinish queue |> check_error
  let flush queue = C.clFlush queue |> check_error
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

  let driver_version device =
    Info.string (C.clGetDeviceInfo device T._CL_DRIVER_VERSION)

  let extensions device =
    Info.string (C.clGetDeviceInfo device T._CL_DEVICE_EXTENSIONS)

  let name device =
    Info.string (C.clGetDeviceInfo device T._CL_DEVICE_NAME)

  let profile device =
    Info.string (C.clGetDeviceInfo device T._CL_DEVICE_PROFILE)

  let vendor device =
    Info.string (C.clGetDeviceInfo device T._CL_DEVICE_VENDOR)

  let version device =
    Info.string (C.clGetDeviceInfo device T._CL_DEVICE_VERSION)

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

  let notify_proxy callback err_info _priv_data _size _user_data =
    (* TODO: Make bytes of priv data and pass it to callback *)
    callback err_info Bytes.empty

  let create ?notify properties devices =
    let properties = CArray.of_list T.cl_context_properties
        (of_property_list properties) in
    let devices = CArray.of_list T.cl_device_id devices in
    let notify' = match notify with
      | Some callback -> Some (notify_proxy callback)
      | None -> None in
    let err = allocate T.cl_int T._CL_SUCCESS in
    let context = C.clCreateContext (CArray.start properties)
        (Unsigned.UInt32.of_int (CArray.length devices))
        (CArray.start devices) notify' null err in
    check_error (!@ err);
    Gc_ext.link_opt context notify;
    context

  let create_from_type ?notify properties device_types =
    let properties = CArray.of_list T.cl_context_properties
        (of_property_list properties) in
    let device_type = Device.of_device_type_list device_types in
    let notify' = match notify with
      | Some callback -> Some (notify_proxy callback)
      | None -> None in
    let err = allocate T.cl_int T._CL_SUCCESS in
    let context = C.clCreateContextFromType (CArray.start properties)
        device_type notify' null err in
    check_error (!@ err);
    Gc_ext.link_opt context notify;
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

  let create_buffer context flags host_data =
    let flags = of_flag_list flags in
    let ba_opt, host_ptr, size =
      match host_data with
      | `Use ba ->
        ( let array = array_of_bigarray genarray ba in
          let size =
            CArray.length array * (sizeof (CArray.element_type array)) in
          let host_ptr = CArray.start array in
          Some ba, to_voidp host_ptr, size )
      | `Alloc (kind, dims) ->
        ( let num_elts = Array.fold_left ( * ) 1 dims in
          let elt_typ = typ_of_bigarray_kind kind in
          let size = num_elts * (sizeof elt_typ) in
          None, null, size ) in
    let err = allocate T.cl_int T._CL_SUCCESS in
    let mem = C.clCreateBuffer context flags
        (Unsigned.Size_t.of_int size) host_ptr err in
    check_error (!@ err);
    if Unsigned.UInt64.(logand flags T._CL_MEM_USE_HOST_PTR <> zero) then
      Gc_ext.link_opt mem ba_opt;
    mem

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

  let of_intensity_channel_type = function
    | `Unorm_int8 -> T._CL_UNORM_INT8
    | `Unorm_int16 -> T._CL_UNORM_INT16
    | `Snorm_int8 -> T._CL_SNORM_INT8
    | `Snorm_int16 -> T._CL_SNORM_INT16
    | `Half_float -> T._CL_HALF_FLOAT
    | `Float -> T._CL_FLOAT

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

  let of_rgb_channel_type = function
    | `Unorm_short_565 -> T._CL_UNORM_SHORT_565
    | `Unorm_short_555 -> T._CL_UNORM_SHORT_555
    | `Unorm_int_101010 -> T._CL_UNORM_INT_101010

  let to_rgb_channel_type = function
    | c when c = T._CL_UNORM_SHORT_565 -> `Unorm_short_565
    | c when c = T._CL_UNORM_SHORT_555 -> `Unorm_short_555
    | c when c = T._CL_UNORM_INT_101010 -> `Unorm_int_101010
    | _other -> failwith "Cl.Mem.to_rgb_channel_type"

  type argb_channel_type =
    [ `Unorm_int8 | `Snorm_int8 |
      `Signed_int8 | `Unsigned_int8 ]

  let of_argb_channel_type = function
    | `Unorm_int8 -> T._CL_UNORM_INT8
    | `Snorm_int8 -> T._CL_SNORM_INT8
    | `Signed_int8 -> T._CL_SIGNED_INT8
    | `Unsigned_int8 -> T._CL_UNSIGNED_INT8

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

  (* XXX: Duplicates of_*_channel type functions. *)
  let of_channel_type = function
    (* channel_type *)
    | `Signed_int16 -> T._CL_SIGNED_INT16
    | `Signed_int32 -> T._CL_SIGNED_INT32
    | `Unsigned_int16 -> T._CL_UNSIGNED_INT16
    | `Unsigned_int32 -> T._CL_UNSIGNED_INT32
    (* intensity_channel_type *)
    | `Unorm_int8 -> T._CL_UNORM_INT8
    | `Unorm_int16 -> T._CL_UNORM_INT16
    | `Snorm_int8 -> T._CL_SNORM_INT8
    | `Snorm_int16 -> T._CL_SNORM_INT16
    | `Half_float -> T._CL_HALF_FLOAT
    | `Float -> T._CL_FLOAT
    (* rgb_channel_type *)
    | `Unorm_short_565 -> T._CL_UNORM_SHORT_565
    | `Unorm_short_555 -> T._CL_UNORM_SHORT_555
    | `Unorm_int_101010 -> T._CL_UNORM_INT_101010
    (* argb_channel_type - intensity_channel_type *)
    | `Signed_int8 -> T._CL_SIGNED_INT8
    | `Unsigned_int8 -> T._CL_UNSIGNED_INT8

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

  let of_image_format (format : image_format) =
    let order, data_type =
      match format with
      | `R data_type -> T._CL_R, of_channel_type data_type
      | `A data_type -> T._CL_A, of_channel_type data_type
      | `Intensity data_type ->
        T._CL_INTENSITY, of_intensity_channel_type data_type
      | `Luminance data_type ->
        T._CL_LUMINANCE, of_intensity_channel_type data_type
      | `Rg data_type -> T._CL_RG, of_channel_type data_type
      | `Ra data_type -> T._CL_RA, of_channel_type data_type
      | `Rgb data_type -> T._CL_RGB, of_rgb_channel_type data_type
      | `Rgba data_type -> T._CL_RGBA, of_channel_type data_type
      | `Argb data_type -> T._CL_ARGB, of_argb_channel_type data_type
      | `Bgra data_type -> T._CL_BGRA, of_argb_channel_type data_type in
    let ans = make T.cl_image_format in
    setf ans T.image_channel_order order;
    setf ans T.image_channel_data_type data_type;
    ans

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

  let create_image2d ?(row_pitch=0) context flags format ~width ~height ba_opt =
    let flags = of_flag_list flags in
    let image_format = of_image_format format in
    let host_ptr, row_pitch =
      match ba_opt with
      | Some ba -> to_voidp (bigarray_start genarray ba), row_pitch
      | None -> null, 0 in
    let err = allocate T.cl_int T._CL_SUCCESS in
    let mem = C.clCreateImage2D context flags (addr image_format)
        (Unsigned.Size_t.of_int width) (Unsigned.Size_t.of_int height)
        (Unsigned.Size_t.of_int row_pitch) host_ptr err in
    check_error (!@ err);
    if Unsigned.UInt64.(logand flags T._CL_MEM_USE_HOST_PTR <> zero) then
      Gc_ext.link_opt mem ba_opt;
    mem

  let create_image3d ?(row_pitch=0) ?(slice_pitch=0) context flags format ~width
      ~height ~depth ba_opt =
    let flags = of_flag_list flags in
    let image_format = of_image_format format in
    let host_ptr, row_pitch, slice_pitch =
      match ba_opt with
      | Some ba -> to_voidp (bigarray_start genarray ba), row_pitch, slice_pitch
      | None -> null, 0, 0 in
    let err = allocate T.cl_int T._CL_SUCCESS in
    let mem = C.clCreateImage3D context flags (addr image_format)
        (Unsigned.Size_t.of_int width) (Unsigned.Size_t.of_int height)
        (Unsigned.Size_t.of_int depth) (Unsigned.Size_t.of_int row_pitch)
        (Unsigned.Size_t.of_int slice_pitch) host_ptr err in
    check_error (!@ err);
    if Unsigned.UInt64.(logand flags T._CL_MEM_USE_HOST_PTR <> zero) then
      Gc_ext.link_opt mem ba_opt;
    mem

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

  let image_format mem =
    Info.value (C.clGetImageInfo mem T._CL_IMAGE_FORMAT)
      T.cl_image_format |> to_image_format

  let image_element_size mem =
    Info.value (C.clGetImageInfo mem T._CL_IMAGE_ELEMENT_SIZE)
      size_t |> Unsigned.Size_t.to_int

  let image_row_pitch mem =
    Info.value (C.clGetImageInfo mem T._CL_IMAGE_ROW_PITCH)
      size_t |> Unsigned.Size_t.to_int

  let image_slice_pitch mem =
    Info.value (C.clGetImageInfo mem T._CL_IMAGE_SLICE_PITCH)
      size_t |> Unsigned.Size_t.to_int

  let image_width mem =
    Info.value (C.clGetImageInfo mem T._CL_IMAGE_WIDTH)
      size_t |> Unsigned.Size_t.to_int

  let image_height mem =
    Info.value (C.clGetImageInfo mem T._CL_IMAGE_HEIGHT)
      size_t |> Unsigned.Size_t.to_int

  let image_depth mem =
    Info.value (C.clGetImageInfo mem T._CL_IMAGE_DEPTH)
      size_t |> Unsigned.Size_t.to_int
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

  let context sampler =
    Info.value (C.clGetSamplerInfo sampler T._CL_SAMPLER_CONTEXT) T.cl_context

  let addressing_mode sampler =
    Info.value (C.clGetSamplerInfo sampler T._CL_SAMPLER_ADDRESSING_MODE)
      T.cl_addressing_mode |> function
    | c when c = T._CL_ADDRESS_NONE -> None
    | c when c = T._CL_ADDRESS_CLAMP_TO_EDGE -> Some `Clamp_to_edge
    | c when c = T._CL_ADDRESS_CLAMP -> Some `Clamp
    | c when c = T._CL_ADDRESS_REPEAT -> Some `Repeat
    | _other -> failwith "Cl.Sampler.addressing_mode"

  let filter_mode sampler =
    Info.value (C.clGetSamplerInfo sampler T._CL_SAMPLER_FILTER_MODE)
      T.cl_filter_mode |> function
    | c when c = T._CL_FILTER_NEAREST -> `Nearest
    | c when c = T._CL_FILTER_LINEAR -> `Linear
    | _other -> failwith "Cl.Sampler.filter_mode"

  let normalized_coords sampler =
    Info.value (C.clGetSamplerInfo sampler T._CL_SAMPLER_NORMALIZED_COORDS)
      T.cl_bool |> to_bool
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
    let notify = match notify with
      | Some callback ->
        ( Gc_ext.link program callback;
          (* TODO: Notify is called only once. Unlink after it's being
             called, instead of keeping it for the lifetime of the
             program. *)
          Some (fun program _user_data -> callback program) )
      | None -> None in
    C.clBuildProgram program (Unsigned.UInt32.of_int (CArray.length devices))
      (CArray.start devices) options notify null |> check_error

  (* TODO *)
  let context _program = from_voidp T._cl_context null
  let devices _program = []

  let source program =
    Info.string (C.clGetProgramInfo program T._CL_PROGRAM_SOURCE)

  (* TODO *)
  let binaries _program = []

  (* TODO *)
  let build_status _program _device = None

  let build_options program device =
    Info.string (C.clGetProgramBuildInfo program device
                   T._CL_PROGRAM_BUILD_OPTIONS)

  let build_log program device =
    Info.string (C.clGetProgramBuildInfo program device T._CL_PROGRAM_BUILD_LOG)
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

  let function_name kernel =
    Info.string (C.clGetKernelInfo kernel T._CL_KERNEL_FUNCTION_NAME)

  (* TODO *)
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
