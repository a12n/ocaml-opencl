(** Binding for OpenCL 1.0. *)

type buffer
type image
(** Mem object kinds. *)

type platform
type device
type context
type command_queue
type 'k mem
type program
type kernel
type event
type sampler

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

module Platform : sig
  val get : unit -> platform list

  (* Platform info attributes. *)

  val extensions : platform -> string
  val name : platform -> string
  val profile : platform -> string
  val vendor : platform -> string
  val version : platform -> string
end

module Command_queue : sig
  type property =
    [ `Out_of_order_exec_mode of bool |
      `Profiling of bool ]

  val create : context -> device -> property list -> command_queue

  (* Command queue info attributes. *)

  val context : command_queue -> context
  val device : command_queue -> device
  val properties : command_queue -> property list

  (* Modify properties. *)

  val set_properties : command_queue -> property list -> property list

  (* Reading, writing, and copying buffer objects. *)

  val read_buffer : ?wait_list:(event list) -> ?blocking:bool ->
    ?offset:int -> ?size:int -> command_queue -> buffer mem ->
    ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> event

  val write_buffer : ?wait_list:(event list) -> ?blocking:bool ->
    ?offset:int -> ?size:int -> command_queue -> buffer mem ->
    ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> event

  val copy_buffer : ?wait_list:(event list) -> command_queue ->
    src_buffer:(buffer mem) -> dst_buffer:(buffer mem) ->
    src_offset:int -> dst_offset:int -> size:int -> event

  (* Reading, writing, and copying image objects. *)

  val read_image : ?wait_list:(event list) -> ?blocking:bool ->
    ?row_pitch:int -> ?slice_pitch:int -> command_queue -> image mem ->
    origin:(int * int * int) -> region:(int * int * int) ->
    ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> event

  val write_image : ?wait_list:(event list) -> ?blocking:bool ->
    ?row_pitch:int -> ?slice_pitch:int -> command_queue -> image mem ->
    origin:(int * int * int) -> region:(int * int * int) ->
    ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t -> event

  val copy_image : ?wait_list:(event list) -> command_queue ->
    src_image:(image mem) -> dst_image:(image mem) ->
    src_origin:(int * int * int) -> dst_origin:(int * int * int) ->
    region:(int * int * int) -> event

  (* Copying between image and buffer objects. *)

  val copy_image_to_buffer : ?wait_list:(event list) -> command_queue ->
    src_image:(image mem) -> dst_buffer:(buffer mem) ->
    src_origin:(int * int * int) -> region:(int * int * int) ->
    dst_offset:int -> event

  val copy_buffer_to_image : ?wait_list:(event list) -> command_queue ->
    src_buffer:(buffer mem) -> dst_image:(image mem) -> src_offset:int ->
    dst_origin:(int * int * int) -> region:(int * int * int) -> event

  (* Mapping and unmapping memory objects *)
  (* TODO *)

  (* Executing kernels. *)

  val nd_range_kernel : ?wait_list:(event list) ->
    ?global_work_offset:int array -> ?local_work_size:int array ->
    command_queue -> kernel -> global_work_size:int array -> event

  val task : ?wait_list:(event list) -> command_queue -> kernel -> event

  (* TODO: native_kernel? *)

  (* Out of order execution. *)

  val marker : command_queue -> event
  val wait_for_events : command_queue -> event list -> unit
  val barrier : command_queue -> unit

  (* Flush and finish. *)

  val finish : command_queue -> unit
  val flush : command_queue -> unit
end

module Device : sig
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

  val get : platform -> [ device_type | `All ] list -> device list

  (* String device info attributes. *)

  val driver_version : device -> string
  val extensions : device -> string
  val name : device -> string
  val profile : device -> string
  val vendor : device -> string
  val version : device -> string

  (* Integer device info attributes. *)

  val address_bits : device -> int
  val global_mem_cache_size : device -> int64
  val global_mem_cacheline_size : device -> int
  val global_mem_size : device -> int64
  val image2d_max_height : device -> int
  val image2d_max_width : device -> int
  val image3d_max_depth : device -> int
  val image3d_max_height : device -> int
  val image3d_max_width : device -> int
  val local_mem_size : device -> int64
  val max_clock_frequency : device -> int
  val max_compute_units : device -> int
  val max_constant_args : device -> int
  val max_constant_buffer_size : device -> int64
  val max_mem_alloc_size : device -> int64
  val max_parameter_size : device -> int
  val max_read_image_args : device -> int
  val max_samplers : device -> int
  val max_work_group_size : device -> int
  val max_work_item_dimensions : device -> int
  val max_write_image_args : device -> int
  val mem_base_addr_align : device -> int
  val min_data_type_align_size : device -> int
  val preferred_vector_width_char : device -> int
  val preferred_vector_width_double : device -> int
  val preferred_vector_width_float : device -> int
  val preferred_vector_width_int : device -> int
  val preferred_vector_width_long : device -> int
  val preferred_vector_width_short : device -> int
  val profiling_timer_resolution : device -> int
  val vendor_id : device -> int

  (* Boolean device info attributes. *)

  val available : device -> bool
  val compiler_available : device -> bool
  val endian_little : device -> bool
  val error_correction_support : device -> bool
  val image_support : device -> bool

  (* [fp_config] device info attributes. *)

  val single_fp_config : device -> fp_config

  (* Other device info. *)

  val device_type : device -> device_type list
  val execution_capabilities : device -> exec_capabilities
  val global_mem_cache_type : device ->
    [ `Read_only_cache | `Read_write_cache ] option
  val local_mem_type : device -> [ `Local | `Global ] option
  val max_work_item_sizes : device -> int array
  val platform : device -> platform
  val queue_properties : device -> Command_queue.property list
end

module Context : sig
  type property =
    [ `Platform of platform ]

  val create : ?notify:(string -> bytes -> unit) ->
    property list -> device list -> context

  val create_from_type : ?notify:(string -> bytes -> unit) ->
    property list -> [ Device.device_type | `All ] list -> context

  (* Context info attributes. *)

  val devices : context -> device list
  val properties : context -> property list
end

module Mem : sig
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

  (* Create a buffer object. *)

  val create_buffer : context -> flag list ->
    [< `Use of ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t |
       `Alloc of ('a, 'b) Bigarray.kind * int array ] -> buffer mem

  (* Mem object info. *)

  val context : 'k mem -> context
  val flags : 'k mem -> flag list
  val map_count : 'k mem -> int
  val mem_type : 'k mem -> mem_type
  val size : 'k mem -> int
  (* TODO: host_ptr? *)

  (* Creating image objects. *)

  val create_image2d : ?row_pitch:int -> context ->
    flag list -> image_format -> width:int -> height:int ->
    ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t option -> image mem

  val create_image3d : ?row_pitch:int -> ?slice_pitch:int -> context ->
    flag list -> image_format -> width:int -> height:int -> depth:int ->
    ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t option -> image mem

  val supported_image_formats : context -> flag list ->
    [ `Image2d | `Image3d ] -> image_format list

  (* Image info. *)

  val image_format : image mem -> image_format
  val image_element_size : image mem -> int
  val image_row_pitch : image mem -> int
  val image_slice_pitch : image mem -> int
  val image_width : image mem -> int
  val image_height : image mem -> int
  val image_depth : image mem -> int
end

module Sampler : sig
  type addressing_mode =
    [ `Clamp_to_edge | `Clamp | `Repeat ]

  type filter_mode =
    [ `Nearest | `Linear ]

  (* Creating sampler objects. *)

  val create : context -> bool -> addressing_mode option -> filter_mode ->
    sampler

  (* Sampler info attributes. *)

  val context : sampler -> context
  val addressing_mode : sampler -> addressing_mode option
  val filter_mode : sampler -> filter_mode
  val normalized_coords : sampler -> bool
end

module Program : sig
  (* Creating program objects. *)

  val create_with_source : context -> string list -> program

  val create_with_binary : context -> (device * bytes) list ->
    program * ([ `Invalid_value | `Invalid_binary ] option list)

  (* Building program executables. *)

  val build : ?notify:(program -> unit) -> program ->
    device list -> string -> unit

  (* Program info. *)

  val context : program -> context
  val devices : program -> device list
  val source : program -> string
  val binaries : program -> bytes list

  val build_status : program -> device ->
    [ `Error | `Success | `In_progress ] option
  val build_options : program -> device -> string
  val build_log : program -> device -> string
end

module Kernel : sig
  (* Creating kernel objects *)

  val create : program -> string -> kernel

  val create_in_program : program -> kernel list

  (* Setting kernel arguments *)

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

  val set_arg : kernel -> int -> [ 'k arg | local_arg ] -> unit

  (* Kernel info. *)

  val function_name : kernel -> string
  val num_args : kernel -> int
  val context : kernel -> context
  val program : kernel -> program

  val work_group_size : kernel -> device -> int
  val compile_work_group_size : kernel -> device -> int * int * int
  val local_mem_size : kernel -> device -> int64
end

module Event : sig
  type command_type =
    [ `Ndrange_kernel | `Task | `Native_kernel |
      `Read_buffer | `Write_buffer | `Copy_buffer |
      `Read_image | `Write_image |
      `Copy_image | `Copy_image_to_buffer | `Copy_buffer_to_image |
      `Map_buffer | `Map_image | `Unmap_mem_object |
      `Marker ]
  (* TODO: Acquire/relase GL objects. *)

  type command_execution_status =
    [ `Queued | `Submitted | `Running | `Complete | `Error of int ]

  (* Wait for events. *)

  val wait : event list -> unit

  (* Event info. *)

  val command_queue : event -> command_queue
  val command_type : event -> command_type
  val command_execution_status : event -> command_execution_status

  (* Event profiling info. *)

  val command_queued : event -> int64
  val command_submit : event -> int64
  val command_start : event -> int64
  val command_end : event -> int64
end

(* Unloading the OpenCL compiler. *)

val unload_compiler : unit -> unit
