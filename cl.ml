type buffer
type image

type platform = unit
type device = unit
type context = unit
type command_queue = unit
type 'k mem = unit
type program = unit
type kernel = unit
type event = unit
type sampler = unit

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

exception Exn of error

module Platform = struct
  (* TODO *)
  let get () = []

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
  let create _context _device _properties = ()

  (* TODO *)
  let context _queue = ()
  let device _queue = ()
  let properties _queue = []

  (* TODO *)
  let set_properties _queue _properties = []

  (* TODO *)
  let read_buffer ?(wait_list=[]) ?(blocking=true) ?(offset=0) ?size
      _queue _mem _ba = ()

  (* TODO *)
  let write_buffer ?(wait_list=[]) ?(blocking=true) ?(offset=0) ?size
      _queue _mem _ba = ()

  (* TODO *)
  let copy_buffer ?(wait_list=[]) _queue ~src_buffer ~dst_buffer
      ~src_offset ~dst_offset ~size = ()

  (* TODO *)
  let read_image ?(wait_list=[]) ?(blocking=true) ?(row_pitch=0)
      ?(slice_pitch=0) _queue _mem ~origin ~region _ba = ()

  (* TODO *)
  let write_image ?(wait_list=[]) ?(blocking=true) ?(row_pitch=0)
      ?(slice_pitch=0) _queue _mem ~origin ~region _ba = ()

  (* TODO *)
  let copy_image ?(wait_list=[]) _queue ~src_image ~dst_image
      ~src_origin ~dst_origin ~region = ()

  (* TODO *)
  let copy_image_to_buffer ?(wait_list=[]) _queue ~src_image
      ~dst_buffer ~src_origin ~region ~dst_offset = ()

  (* TODO *)
  let copy_buffer_to_image ?(wait_list=[]) _queue ~src_buffer
      ~dst_image ~src_offset ~dst_origin ~region = ()

  (* TODO *)
  let nd_range_kernel ?(wait_list=[]) ?global_work_offset
      ?local_work_size _queue _kernel ~global_work_size = ()

  (* TODO *)
  let task ?(wait_list=[]) _queue _kernel = ()

  (* TODO *)
  let marker _queue = ()
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

  (* TODO *)
  let get _platform _device_types = []

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
  let platform _device = ()
  let queue_properties _device = []
end

module Context = struct
  type property =
    [ `Platform of platform ]

  (* TODO *)
  let create ?notify _properties _devices = ()

  (* TODO *)
  let create_from_type ?notify _properties _device_types = ()

  (* TODO *)
  let devices _context = []
  let properties _context = []
end

module Mem = struct
  type channel_order =
    [ `R | `A |
      `Intensity | `Luminance | (* Float, Half_float, Snorm_int16,
                                   Snorm_int8, Unorm_int16, Unorm_int8 *)
      `Rg | `Ra |
      `Rgb |  (* Unorm_int_101010, Unorm_short_555, Unorm_short_565 *)
      `Rgba |
      `Argb | `Bgra ]          (* Signed_int8, Snorm_int8, Unorm_int8,
                                  Unsigned_int8 *)

  type channel_type =
    [ `Snorm_int8 | `Snorm_int16 |
      `Unorm_int8 | `Unorm_int16 |
      `Unorm_short_565 | `Unorm_short_555 | `Unorm_int_101010 |
      `Signed_int8 | `Signed_int16 | `Signed_int32 |
      `Unsigned_int8 | `Unsigned_int16 | `Unsigned_int32 |
      `Half_float | `Float ]

  (* TODO: Compile time image format validation. *)
  type image_format = channel_order * channel_type

  type flag =
    [ `Read_write | `Write_only | `Read_only |
      `Use_host_ptr | `Alloc_host_ptr | `Copy_host_ptr ]

  type mem_type =
    [ `Buffer | `Image2d | `Image3d ]

  (* TODO *)
  let create_buffer _context _flags _host_data = ()

  (* TODO *)
  let context _mem = ()
  let flags _mem = []
  let map_count _mem = 0
  let mem_type _mem = `Buffer
  let size _mem = 0

  (* TODO *)
  let create_image2d ?(row_pitch=0) _context _flags _format
      ~width ~height _ba_opt = ()

  (* TODO *)
  let create_image3d ?(row_pitch=0) ?(slice_pitch=0) _context _flags _format
      ~width ~height ~depth _ba_opt = ()

  (* TODO *)
  let supported_image_formats _context _flags _mem_type = []

  (* TODO *)
  let image_format _mem = (`Intensity, `Float)
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

  (* TODO *)
  let create _context _norm_coords _addressing _filter = ()

  (* TODO *)
  let context _sampler = ()
  let addressing_mode _sampler = None
  let filter_mode _sampler = `Nearest
  let normalized_coords _sampler = false
end

module Program = struct
  (* TODO *)
  let create_with_source _context _strings = ()

  (* TODO *)
  let create_with_binary _context _device_binaries = (), []

  (* TODO *)
  let build ?notify _program _devices _options = ()

  (* TODO *)
  let context _program = ()
  let devices _program = []
  let source _program = ""
  let binaries _program = []

  (* TODO *)
  let build_status _program _device = None
  let build_options _program _device = ""
  let build_log _program _device = ""
end

module Kernel = struct
  (* TODO *)
  let create _program _kernel_name = ()
  let create_in_program _program = []

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

  (* TODO *)
  let set_arg _kernel _index _arg_opt = ()

  (* TODO *)
  let function_name _kernel = ""
  let num_args _kernel = 0
  let context _kernel = ()
  let program _kernel = ()

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

  (* TODO *)
  let wait _wait_list = ()

  (* TODO *)
  let command_queue _event = ()
  let command_type _event = `Marker
  let command_execution_status _event = `Complete

  (* TODO *)
  let command_queued _event = 0L
  let command_submit _event = 0L
  let command_start _event = 0L
  let command_end _event = 0L
end

(* TODO *)
let unload_compiler () = ()
