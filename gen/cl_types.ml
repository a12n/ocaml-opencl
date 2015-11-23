module Make (T : Cstubs.Types.TYPE) = struct
  (**************)
  (* OpenCL 1.0 *)
  (**************)

  (* Basic typedefs *)
  let intptr_t = T.typedef T.nativeint "intptr_t" (* XXX *)

  let cl_char = T.typedef T.int8_t "cl_char"
  let cl_uchar = T.typedef T.uint8_t "cl_uchar"
  let cl_short = T.typedef T.int16_t "cl_short"
  let cl_ushort = T.typedef T.uint16_t "cl_ushort"
  let cl_int = T.typedef T.int32_t "cl_int"
  let cl_uint = T.typedef T.uint32_t "cl_uint"
  let cl_long = T.typedef T.int64_t "cl_long"
  let cl_ulong = T.typedef T.uint64_t "cl_ulong"
  let cl_half = T.typedef T.uint16_t "cl_half"
  let cl_float = T.typedef T.float "cl_float"
  let cl_double = T.typedef T.double "cl_double"

  let cl_GLuint = T.typedef T.uint "cl_GLuint"
  let cl_GLint = T.typedef T.int "cl_GLint"
  let cl_GLenum = T.typedef T.uint "cl_GLenum"

  (* Abstract types *)
  type _cl_platform_id
  type _cl_device_id
  type _cl_context
  type _cl_command_queue
  type _cl_mem
  type _cl_program
  type _cl_kernel
  type _cl_event
  type _cl_sampler

  let _cl_platform_id : _cl_platform_id Ctypes_static.structure T.typ =
    T.structure "_cl_platform_id"
  let _cl_device_id : _cl_device_id Ctypes_static.structure T.typ =
    T.structure "_cl_device_id"
  let _cl_context : _cl_context Ctypes_static.structure T.typ =
    T.structure "_cl_context"
  let _cl_command_queue : _cl_command_queue Ctypes_static.structure T.typ =
    T.structure "_cl_command_queue"
  let _cl_mem : _cl_mem Ctypes_static.structure T.typ = T.structure "_cl_mem"
  let _cl_program : _cl_program Ctypes_static.structure T.typ =
    T.structure "_cl_program"
  let _cl_kernel : _cl_kernel Ctypes_static.structure T.typ =
    T.structure "_cl_kernel"
  let _cl_event : _cl_event Ctypes_static.structure T.typ =
    T.structure "_cl_event"
  let _cl_sampler : _cl_sampler Ctypes_static.structure T.typ =
    T.structure "_cl_sampler"

  type cl_platform_id =
    _cl_platform_id Ctypes_static.structure Ctypes_static.ptr
  type cl_device_id = _cl_device_id Ctypes_static.structure Ctypes_static.ptr
  type cl_context = _cl_context Ctypes_static.structure Ctypes_static.ptr
  type cl_command_queue =
    _cl_command_queue Ctypes_static.structure Ctypes_static.ptr
  type cl_mem = _cl_mem Ctypes_static.structure Ctypes_static.ptr
  type cl_program = _cl_program Ctypes_static.structure Ctypes_static.ptr
  type cl_kernel = _cl_kernel Ctypes_static.structure Ctypes_static.ptr
  type cl_event = _cl_event Ctypes_static.structure Ctypes_static.ptr
  type cl_sampler = _cl_sampler Ctypes_static.structure Ctypes_static.ptr

  let cl_platform_id : cl_platform_id T.typ = T.ptr _cl_platform_id
  let cl_device_id : cl_device_id T.typ = T.ptr _cl_device_id
  let cl_context : cl_context T.typ = T.ptr _cl_context
  let cl_command_queue : cl_command_queue T.typ = T.ptr _cl_command_queue
  let cl_mem : cl_mem T.typ = T.ptr _cl_mem
  let cl_program : cl_program T.typ = T.ptr _cl_program
  let cl_kernel : cl_kernel T.typ = T.ptr _cl_kernel
  let cl_event : cl_event T.typ = T.ptr _cl_event
  let cl_sampler : cl_sampler T.typ = T.ptr _cl_sampler

  (* Typedefs *)
  let cl_bool = T.typedef cl_uint "cl_bool"
  let cl_bitfield = T.typedef cl_ulong "cl_bitfield"
  let cl_device_type = T.typedef cl_bitfield "cl_device_type"
  let cl_platform_info = T.typedef cl_uint "cl_platform_info"
  let cl_device_info = T.typedef cl_uint "cl_device_info"
  let cl_device_fp_config = T.typedef cl_bitfield "cl_device_fp_config"
  let cl_device_mem_cache_type = T.typedef cl_uint "cl_device_mem_cache_type"
  let cl_device_local_mem_type = T.typedef cl_uint "cl_device_local_mem_type"
  let cl_device_exec_capabilities =
    T.typedef cl_bitfield "cl_device_exec_capabilities"
  let cl_command_queue_properties =
    T.typedef cl_bitfield "cl_command_queue_properties"

  let cl_context_properties = T.typedef intptr_t "cl_context_properties"
  let cl_context_info = T.typedef cl_uint "cl_context_info"
  let cl_command_queue_info = T.typedef cl_uint "cl_command_queue_info"
  let cl_channel_order = T.typedef cl_uint "cl_channel_order"
  let cl_channel_type = T.typedef cl_uint "cl_channel_type"
  let cl_mem_flags = T.typedef cl_bitfield "cl_mem_flags"
  let cl_mem_object_type = T.typedef cl_uint "cl_mem_object_type"
  let cl_mem_info = T.typedef cl_uint "cl_mem_info"
  let cl_image_info = T.typedef cl_uint "cl_image_info"
  let cl_addressing_mode = T.typedef cl_uint "cl_addressing_mode"
  let cl_filter_mode = T.typedef cl_uint "cl_filter_mode"
  let cl_sampler_info = T.typedef cl_uint "cl_sampler_info"
  let cl_map_flags = T.typedef cl_bitfield "cl_map_flags"
  let cl_program_info = T.typedef cl_uint "cl_program_info"
  let cl_program_build_info = T.typedef cl_uint "cl_program_build_info"
  let cl_build_status = T.typedef cl_int "cl_build_status"
  let cl_kernel_info = T.typedef cl_uint "cl_kernel_info"
  let cl_kernel_work_group_info = T.typedef cl_uint "cl_kernel_work_group_info"
  let cl_event_info = T.typedef cl_uint "cl_event_info"
  let cl_command_type = T.typedef cl_uint "cl_command_type"
  let cl_profiling_info = T.typedef cl_uint "cl_profiling_info"

  (* Struct types *)
  type _cl_image_format
  let _cl_image_format : _cl_image_format Ctypes_static.structure T.typ =
    T.structure "_cl_image_format"
  let image_channel_order =
    T.field _cl_image_format "image_channel_order" cl_channel_order
  let image_channel_data_type =
    T.field _cl_image_format "image_channel_data_type" cl_channel_type
  let () = T.seal _cl_image_format
  let cl_image_format = T.typedef _cl_image_format "cl_image_format"

  (* Error Codes *)
  let _CL_SUCCESS = T.constant "CL_SUCCESS" cl_int
  let _CL_DEVICE_NOT_FOUND = T.constant "CL_DEVICE_NOT_FOUND" cl_int
  let _CL_DEVICE_NOT_AVAILABLE = T.constant "CL_DEVICE_NOT_AVAILABLE" cl_int
  let _CL_COMPILER_NOT_AVAILABLE = T.constant "CL_COMPILER_NOT_AVAILABLE" cl_int
  let _CL_MEM_OBJECT_ALLOCATION_FAILURE =
    T.constant "CL_MEM_OBJECT_ALLOCATION_FAILURE" cl_int
  let _CL_OUT_OF_RESOURCES = T.constant "CL_OUT_OF_RESOURCES" cl_int
  let _CL_OUT_OF_HOST_MEMORY = T.constant "CL_OUT_OF_HOST_MEMORY" cl_int
  let _CL_PROFILING_INFO_NOT_AVAILABLE =
    T.constant "CL_PROFILING_INFO_NOT_AVAILABLE" cl_int
  let _CL_MEM_COPY_OVERLAP = T.constant "CL_MEM_COPY_OVERLAP" cl_int
  let _CL_IMAGE_FORMAT_MISMATCH = T.constant "CL_IMAGE_FORMAT_MISMATCH" cl_int
  let _CL_IMAGE_FORMAT_NOT_SUPPORTED =
    T.constant "CL_IMAGE_FORMAT_NOT_SUPPORTED" cl_int
  let _CL_BUILD_PROGRAM_FAILURE = T.constant "CL_BUILD_PROGRAM_FAILURE" cl_int
  let _CL_MAP_FAILURE = T.constant "CL_MAP_FAILURE" cl_int

  let _CL_INVALID_VALUE = T.constant "CL_INVALID_VALUE" cl_int
  let _CL_INVALID_DEVICE_TYPE = T.constant "CL_INVALID_DEVICE_TYPE" cl_int
  let _CL_INVALID_PLATFORM = T.constant "CL_INVALID_PLATFORM" cl_int
  let _CL_INVALID_DEVICE = T.constant "CL_INVALID_DEVICE" cl_int
  let _CL_INVALID_CONTEXT = T.constant "CL_INVALID_CONTEXT" cl_int
  let _CL_INVALID_QUEUE_PROPERTIES =
    T.constant "CL_INVALID_QUEUE_PROPERTIES" cl_int
  let _CL_INVALID_COMMAND_QUEUE = T.constant "CL_INVALID_COMMAND_QUEUE" cl_int
  let _CL_INVALID_HOST_PTR = T.constant "CL_INVALID_HOST_PTR" cl_int
  let _CL_INVALID_MEM_OBJECT = T.constant "CL_INVALID_MEM_OBJECT" cl_int
  let _CL_INVALID_IMAGE_FORMAT_DESCRIPTOR =
    T.constant "CL_INVALID_IMAGE_FORMAT_DESCRIPTOR" cl_int
  let _CL_INVALID_IMAGE_SIZE = T.constant "CL_INVALID_IMAGE_SIZE" cl_int
  let _CL_INVALID_SAMPLER = T.constant "CL_INVALID_SAMPLER" cl_int
  let _CL_INVALID_BINARY = T.constant "CL_INVALID_BINARY" cl_int
  let _CL_INVALID_BUILD_OPTIONS = T.constant "CL_INVALID_BUILD_OPTIONS" cl_int
  let _CL_INVALID_PROGRAM = T.constant "CL_INVALID_PROGRAM" cl_int
  let _CL_INVALID_PROGRAM_EXECUTABLE =
    T.constant "CL_INVALID_PROGRAM_EXECUTABLE" cl_int
  let _CL_INVALID_KERNEL_NAME = T.constant "CL_INVALID_KERNEL_NAME" cl_int
  let _CL_INVALID_KERNEL_DEFINITION =
    T.constant "CL_INVALID_KERNEL_DEFINITION" cl_int
  let _CL_INVALID_KERNEL = T.constant "CL_INVALID_KERNEL" cl_int
  let _CL_INVALID_ARG_INDEX = T.constant "CL_INVALID_ARG_INDEX" cl_int
  let _CL_INVALID_ARG_VALUE = T.constant "CL_INVALID_ARG_VALUE" cl_int
  let _CL_INVALID_ARG_SIZE = T.constant "CL_INVALID_ARG_SIZE" cl_int
  let _CL_INVALID_KERNEL_ARGS = T.constant "CL_INVALID_KERNEL_ARGS" cl_int
  let _CL_INVALID_WORK_DIMENSION = T.constant "CL_INVALID_WORK_DIMENSION" cl_int
  let _CL_INVALID_WORK_GROUP_SIZE =
    T.constant "CL_INVALID_WORK_GROUP_SIZE" cl_int
  let _CL_INVALID_WORK_ITEM_SIZE = T.constant "CL_INVALID_WORK_ITEM_SIZE" cl_int
  let _CL_INVALID_GLOBAL_OFFSET = T.constant "CL_INVALID_GLOBAL_OFFSET" cl_int
  let _CL_INVALID_EVENT_WAIT_LIST =
    T.constant "CL_INVALID_EVENT_WAIT_LIST" cl_int
  let _CL_INVALID_EVENT = T.constant "CL_INVALID_EVENT" cl_int
  let _CL_INVALID_OPERATION = T.constant "CL_INVALID_OPERATION" cl_int
  let _CL_INVALID_GL_OBJECT = T.constant "CL_INVALID_GL_OBJECT" cl_int
  let _CL_INVALID_BUFFER_SIZE = T.constant "CL_INVALID_BUFFER_SIZE" cl_int
  let _CL_INVALID_MIP_LEVEL = T.constant "CL_INVALID_MIP_LEVEL" cl_int
  let _CL_INVALID_GLOBAL_WORK_SIZE =
    T.constant "CL_INVALID_GLOBAL_WORK_SIZE" cl_int

  (* cl_bool *)
  let _CL_FALSE = T.constant "CL_FALSE" cl_bool
  let _CL_TRUE = T.constant "CL_TRUE" cl_bool

  (* cl_platform_info *)
  let _CL_PLATFORM_PROFILE = T.constant "CL_PLATFORM_PROFILE" cl_platform_info
  let _CL_PLATFORM_VERSION = T.constant "CL_PLATFORM_VERSION" cl_platform_info
  let _CL_PLATFORM_NAME = T.constant "CL_PLATFORM_NAME" cl_platform_info
  let _CL_PLATFORM_VENDOR = T.constant "CL_PLATFORM_VENDOR" cl_platform_info
  let _CL_PLATFORM_EXTENSIONS =
    T.constant "CL_PLATFORM_EXTENSIONS" cl_platform_info

  (* cl_device_type - bitfield *)
  let _CL_DEVICE_TYPE_DEFAULT =
    T.constant "CL_DEVICE_TYPE_DEFAULT" cl_device_type
  let _CL_DEVICE_TYPE_CPU = T.constant "CL_DEVICE_TYPE_CPU" cl_device_type
  let _CL_DEVICE_TYPE_GPU = T.constant "CL_DEVICE_TYPE_GPU" cl_device_type
  let _CL_DEVICE_TYPE_ACCELERATOR =
    T.constant "CL_DEVICE_TYPE_ACCELERATOR" cl_device_type
  let _CL_DEVICE_TYPE_ALL = T.constant "CL_DEVICE_TYPE_ALL" cl_device_type

  (* cl_device_info *)
  let _CL_DEVICE_TYPE = T.constant "CL_DEVICE_TYPE" cl_device_info
  let _CL_DEVICE_VENDOR_ID = T.constant "CL_DEVICE_VENDOR_ID" cl_device_info
  let _CL_DEVICE_MAX_COMPUTE_UNITS =
    T.constant "CL_DEVICE_MAX_COMPUTE_UNITS" cl_device_info
  let _CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS =
    T.constant "CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS" cl_device_info
  let _CL_DEVICE_MAX_WORK_GROUP_SIZE =
    T.constant "CL_DEVICE_MAX_WORK_GROUP_SIZE" cl_device_info
  let _CL_DEVICE_MAX_WORK_ITEM_SIZES =
    T.constant "CL_DEVICE_MAX_WORK_ITEM_SIZES" cl_device_info
  let _CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR =
    T.constant "CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR" cl_device_info
  let _CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT =
    T.constant "CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT" cl_device_info
  let _CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT =
    T.constant "CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT" cl_device_info
  let _CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG =
    T.constant "CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG" cl_device_info
  let _CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT =
    T.constant "CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT" cl_device_info
  let _CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE =
    T.constant "CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE" cl_device_info
  let _CL_DEVICE_MAX_CLOCK_FREQUENCY =
    T.constant "CL_DEVICE_MAX_CLOCK_FREQUENCY" cl_device_info
  let _CL_DEVICE_ADDRESS_BITS =
    T.constant "CL_DEVICE_ADDRESS_BITS" cl_device_info
  let _CL_DEVICE_MAX_READ_IMAGE_ARGS =
    T.constant "CL_DEVICE_MAX_READ_IMAGE_ARGS" cl_device_info
  let _CL_DEVICE_MAX_WRITE_IMAGE_ARGS =
    T.constant "CL_DEVICE_MAX_WRITE_IMAGE_ARGS" cl_device_info
  let _CL_DEVICE_MAX_MEM_ALLOC_SIZE =
    T.constant "CL_DEVICE_MAX_MEM_ALLOC_SIZE" cl_device_info
  let _CL_DEVICE_IMAGE2D_MAX_WIDTH =
    T.constant "CL_DEVICE_IMAGE2D_MAX_WIDTH" cl_device_info
  let _CL_DEVICE_IMAGE2D_MAX_HEIGHT =
    T.constant "CL_DEVICE_IMAGE2D_MAX_HEIGHT" cl_device_info
  let _CL_DEVICE_IMAGE3D_MAX_WIDTH =
    T.constant "CL_DEVICE_IMAGE3D_MAX_WIDTH" cl_device_info
  let _CL_DEVICE_IMAGE3D_MAX_HEIGHT =
    T.constant "CL_DEVICE_IMAGE3D_MAX_HEIGHT" cl_device_info
  let _CL_DEVICE_IMAGE3D_MAX_DEPTH =
    T.constant "CL_DEVICE_IMAGE3D_MAX_DEPTH" cl_device_info
  let _CL_DEVICE_IMAGE_SUPPORT =
    T.constant "CL_DEVICE_IMAGE_SUPPORT" cl_device_info
  let _CL_DEVICE_MAX_PARAMETER_SIZE =
    T.constant "CL_DEVICE_MAX_PARAMETER_SIZE" cl_device_info
  let _CL_DEVICE_MAX_SAMPLERS =
    T.constant "CL_DEVICE_MAX_SAMPLERS" cl_device_info
  let _CL_DEVICE_MEM_BASE_ADDR_ALIGN =
    T.constant "CL_DEVICE_MEM_BASE_ADDR_ALIGN" cl_device_info
  let _CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE =
    T.constant "CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE" cl_device_info
  let _CL_DEVICE_SINGLE_FP_CONFIG =
    T.constant "CL_DEVICE_SINGLE_FP_CONFIG" cl_device_info
  let _CL_DEVICE_GLOBAL_MEM_CACHE_TYPE =
    T.constant "CL_DEVICE_GLOBAL_MEM_CACHE_TYPE" cl_device_info
  let _CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE =
    T.constant "CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE" cl_device_info
  let _CL_DEVICE_GLOBAL_MEM_CACHE_SIZE =
    T.constant "CL_DEVICE_GLOBAL_MEM_CACHE_SIZE" cl_device_info
  let _CL_DEVICE_GLOBAL_MEM_SIZE =
    T.constant "CL_DEVICE_GLOBAL_MEM_SIZE" cl_device_info
  let _CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE =
    T.constant "CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE" cl_device_info
  let _CL_DEVICE_MAX_CONSTANT_ARGS =
    T.constant "CL_DEVICE_MAX_CONSTANT_ARGS" cl_device_info
  let _CL_DEVICE_LOCAL_MEM_TYPE =
    T.constant "CL_DEVICE_LOCAL_MEM_TYPE" cl_device_info
  let _CL_DEVICE_LOCAL_MEM_SIZE =
    T.constant "CL_DEVICE_LOCAL_MEM_SIZE" cl_device_info
  let _CL_DEVICE_ERROR_CORRECTION_SUPPORT =
    T.constant "CL_DEVICE_ERROR_CORRECTION_SUPPORT" cl_device_info
  let _CL_DEVICE_PROFILING_TIMER_RESOLUTION =
    T.constant "CL_DEVICE_PROFILING_TIMER_RESOLUTION" cl_device_info
  let _CL_DEVICE_ENDIAN_LITTLE =
    T.constant "CL_DEVICE_ENDIAN_LITTLE" cl_device_info
  let _CL_DEVICE_AVAILABLE = T.constant "CL_DEVICE_AVAILABLE" cl_device_info
  let _CL_DEVICE_COMPILER_AVAILABLE =
    T.constant "CL_DEVICE_COMPILER_AVAILABLE" cl_device_info
  let _CL_DEVICE_EXECUTION_CAPABILITIES =
    T.constant "CL_DEVICE_EXECUTION_CAPABILITIES" cl_device_info
  let _CL_DEVICE_QUEUE_PROPERTIES =
    T.constant "CL_DEVICE_QUEUE_PROPERTIES" cl_device_info
  let _CL_DEVICE_NAME = T.constant "CL_DEVICE_NAME" cl_device_info
  let _CL_DEVICE_VENDOR = T.constant "CL_DEVICE_VENDOR" cl_device_info
  let _CL_DRIVER_VERSION = T.constant "CL_DRIVER_VERSION" cl_device_info
  let _CL_DEVICE_PROFILE = T.constant "CL_DEVICE_PROFILE" cl_device_info
  let _CL_DEVICE_VERSION = T.constant "CL_DEVICE_VERSION" cl_device_info
  let _CL_DEVICE_EXTENSIONS = T.constant "CL_DEVICE_EXTENSIONS" cl_device_info
  let _CL_DEVICE_PLATFORM = T.constant "CL_DEVICE_PLATFORM" cl_device_info

  (* cl_device_fp_config - bitfield *)
  let _CL_FP_DENORM = T.constant "CL_FP_DENORM" cl_device_fp_config
  let _CL_FP_INF_NAN = T.constant "CL_FP_INF_NAN" cl_device_fp_config
  let _CL_FP_ROUND_TO_NEAREST =
    T.constant "CL_FP_ROUND_TO_NEAREST" cl_device_fp_config
  let _CL_FP_ROUND_TO_ZERO =
    T.constant "CL_FP_ROUND_TO_ZERO" cl_device_fp_config
  let _CL_FP_ROUND_TO_INF = T.constant "CL_FP_ROUND_TO_INF" cl_device_fp_config
  let _CL_FP_FMA = T.constant "CL_FP_FMA" cl_device_fp_config

  (* cl_device_mem_cache_type *)
  let _CL_NONE = T.constant "CL_NONE" cl_device_mem_cache_type
  let _CL_READ_ONLY_CACHE =
    T.constant "CL_READ_ONLY_CACHE" cl_device_mem_cache_type
  let _CL_READ_WRITE_CACHE =
    T.constant "CL_READ_WRITE_CACHE" cl_device_mem_cache_type

  (* cl_device_local_mem_type *)
  let _CL_LOCAL = T.constant "CL_LOCAL" cl_device_local_mem_type
  let _CL_GLOBAL = T.constant "CL_GLOBAL" cl_device_local_mem_type

  (* cl_device_exec_capabilities - bitfield *)
  let _CL_EXEC_KERNEL = T.constant "CL_EXEC_KERNEL" cl_device_exec_capabilities
  let _CL_EXEC_NATIVE_KERNEL =
    T.constant "CL_EXEC_NATIVE_KERNEL" cl_device_exec_capabilities

  (* cl_command_queue_properties - bitfield *)
  let _CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE = T.constant
      "CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE" cl_command_queue_properties
  let _CL_QUEUE_PROFILING_ENABLE =
    T.constant "CL_QUEUE_PROFILING_ENABLE" cl_command_queue_properties

  (* cl_context_info *)
  let _CL_CONTEXT_REFERENCE_COUNT =
    T.constant "CL_CONTEXT_REFERENCE_COUNT" cl_context_info
  let _CL_CONTEXT_DEVICES = T.constant "CL_CONTEXT_DEVICES" cl_context_info
  let _CL_CONTEXT_PROPERTIES =
    T.constant "CL_CONTEXT_PROPERTIES" cl_context_info

  (* cl_context_info + cl_context_properties *)
  let _CL_CONTEXT_PLATFORM = T.constant "CL_CONTEXT_PLATFORM" cl_context_info

  (* cl_command_queue_info *)
  let _CL_QUEUE_CONTEXT = T.constant "CL_QUEUE_CONTEXT" cl_command_queue_info
  let _CL_QUEUE_DEVICE = T.constant "CL_QUEUE_DEVICE" cl_command_queue_info
  let _CL_QUEUE_REFERENCE_COUNT =
    T.constant "CL_QUEUE_REFERENCE_COUNT" cl_command_queue_info
  let _CL_QUEUE_PROPERTIES =
    T.constant "CL_QUEUE_PROPERTIES" cl_command_queue_info

  (* cl_mem_flags - bitfield *)
  let _CL_MEM_READ_WRITE = T.constant "CL_MEM_READ_WRITE" cl_mem_flags
  let _CL_MEM_WRITE_ONLY = T.constant "CL_MEM_WRITE_ONLY" cl_mem_flags
  let _CL_MEM_READ_ONLY = T.constant "CL_MEM_READ_ONLY" cl_mem_flags
  let _CL_MEM_USE_HOST_PTR = T.constant "CL_MEM_USE_HOST_PTR" cl_mem_flags
  let _CL_MEM_ALLOC_HOST_PTR = T.constant "CL_MEM_ALLOC_HOST_PTR" cl_mem_flags
  let _CL_MEM_COPY_HOST_PTR = T.constant "CL_MEM_COPY_HOST_PTR" cl_mem_flags

  (* cl_channel_order *)
  let _CL_R = T.constant "CL_R" cl_channel_order
  let _CL_A = T.constant "CL_A" cl_channel_order
  let _CL_RG = T.constant "CL_RG" cl_channel_order
  let _CL_RA = T.constant "CL_RA" cl_channel_order
  let _CL_RGB = T.constant "CL_RGB" cl_channel_order
  let _CL_RGBA = T.constant "CL_RGBA" cl_channel_order
  let _CL_BGRA = T.constant "CL_BGRA" cl_channel_order
  let _CL_ARGB = T.constant "CL_ARGB" cl_channel_order
  let _CL_INTENSITY = T.constant "CL_INTENSITY" cl_channel_order
  let _CL_LUMINANCE = T.constant "CL_LUMINANCE" cl_channel_order

  (* cl_channel_type *)
  let _CL_SNORM_INT8 = T.constant "CL_SNORM_INT8" cl_channel_type
  let _CL_SNORM_INT16 = T.constant "CL_SNORM_INT16" cl_channel_type
  let _CL_UNORM_INT8 = T.constant "CL_UNORM_INT8" cl_channel_type
  let _CL_UNORM_INT16 = T.constant "CL_UNORM_INT16" cl_channel_type
  let _CL_UNORM_SHORT_565 = T.constant "CL_UNORM_SHORT_565" cl_channel_type
  let _CL_UNORM_SHORT_555 = T.constant "CL_UNORM_SHORT_555" cl_channel_type
  let _CL_UNORM_INT_101010 = T.constant "CL_UNORM_INT_101010" cl_channel_type
  let _CL_SIGNED_INT8 = T.constant "CL_SIGNED_INT8" cl_channel_type
  let _CL_SIGNED_INT16 = T.constant "CL_SIGNED_INT16" cl_channel_type
  let _CL_SIGNED_INT32 = T.constant "CL_SIGNED_INT32" cl_channel_type
  let _CL_UNSIGNED_INT8 = T.constant "CL_UNSIGNED_INT8" cl_channel_type
  let _CL_UNSIGNED_INT16 = T.constant "CL_UNSIGNED_INT16" cl_channel_type
  let _CL_UNSIGNED_INT32 = T.constant "CL_UNSIGNED_INT32" cl_channel_type
  let _CL_HALF_FLOAT = T.constant "CL_HALF_FLOAT" cl_channel_type
  let _CL_FLOAT = T.constant "CL_FLOAT" cl_channel_type

  (* cl_mem_object_type *)
  let _CL_MEM_OBJECT_BUFFER =
    T.constant "CL_MEM_OBJECT_BUFFER" cl_mem_object_type
  let _CL_MEM_OBJECT_IMAGE2D =
    T.constant "CL_MEM_OBJECT_IMAGE2D" cl_mem_object_type
  let _CL_MEM_OBJECT_IMAGE3D =
    T.constant "CL_MEM_OBJECT_IMAGE3D" cl_mem_object_type

  (* cl_mem_info *)
  let _CL_MEM_TYPE = T.constant "CL_MEM_TYPE" cl_mem_info
  let _CL_MEM_FLAGS = T.constant "CL_MEM_FLAGS" cl_mem_info
  let _CL_MEM_SIZE = T.constant "CL_MEM_SIZE" cl_mem_info
  let _CL_MEM_HOST_PTR = T.constant "CL_MEM_HOST_PTR" cl_mem_info
  let _CL_MEM_MAP_COUNT = T.constant "CL_MEM_MAP_COUNT" cl_mem_info
  let _CL_MEM_REFERENCE_COUNT = T.constant "CL_MEM_REFERENCE_COUNT" cl_mem_info
  let _CL_MEM_CONTEXT = T.constant "CL_MEM_CONTEXT" cl_mem_info

  (* cl_image_info *)
  let _CL_IMAGE_FORMAT = T.constant "CL_IMAGE_FORMAT" cl_image_info
  let _CL_IMAGE_ELEMENT_SIZE = T.constant "CL_IMAGE_ELEMENT_SIZE" cl_image_info
  let _CL_IMAGE_ROW_PITCH = T.constant "CL_IMAGE_ROW_PITCH" cl_image_info
  let _CL_IMAGE_SLICE_PITCH = T.constant "CL_IMAGE_SLICE_PITCH" cl_image_info
  let _CL_IMAGE_WIDTH = T.constant "CL_IMAGE_WIDTH" cl_image_info
  let _CL_IMAGE_HEIGHT = T.constant "CL_IMAGE_HEIGHT" cl_image_info
  let _CL_IMAGE_DEPTH = T.constant "CL_IMAGE_DEPTH" cl_image_info

  (* cl_addressing_mode *)
  let _CL_ADDRESS_NONE = T.constant "CL_ADDRESS_NONE" cl_addressing_mode
  let _CL_ADDRESS_CLAMP_TO_EDGE =
    T.constant "CL_ADDRESS_CLAMP_TO_EDGE" cl_addressing_mode
  let _CL_ADDRESS_CLAMP = T.constant "CL_ADDRESS_CLAMP" cl_addressing_mode
  let _CL_ADDRESS_REPEAT = T.constant "CL_ADDRESS_REPEAT" cl_addressing_mode

  (* cl_filter_mode *)
  let _CL_FILTER_NEAREST = T.constant "CL_FILTER_NEAREST" cl_filter_mode
  let _CL_FILTER_LINEAR = T.constant "CL_FILTER_LINEAR" cl_filter_mode

  (* cl_sampler_info *)
  let _CL_SAMPLER_REFERENCE_COUNT =
    T.constant "CL_SAMPLER_REFERENCE_COUNT" cl_sampler_info
  let _CL_SAMPLER_CONTEXT = T.constant "CL_SAMPLER_CONTEXT" cl_sampler_info
  let _CL_SAMPLER_NORMALIZED_COORDS =
    T.constant "CL_SAMPLER_NORMALIZED_COORDS" cl_sampler_info
  let _CL_SAMPLER_ADDRESSING_MODE =
    T.constant "CL_SAMPLER_ADDRESSING_MODE" cl_sampler_info
  let _CL_SAMPLER_FILTER_MODE =
    T.constant "CL_SAMPLER_FILTER_MODE" cl_sampler_info

  (* cl_map_flags - bitfield *)
  let _CL_MAP_READ = T.constant "CL_MAP_READ" cl_map_flags
  let _CL_MAP_WRITE = T.constant "CL_MAP_WRITE" cl_map_flags

  (* cl_program_info *)
  let _CL_PROGRAM_REFERENCE_COUNT =
    T.constant "CL_PROGRAM_REFERENCE_COUNT" cl_program_info
  let _CL_PROGRAM_CONTEXT = T.constant "CL_PROGRAM_CONTEXT" cl_program_info
  let _CL_PROGRAM_NUM_DEVICES =
    T.constant "CL_PROGRAM_NUM_DEVICES" cl_program_info
  let _CL_PROGRAM_DEVICES = T.constant "CL_PROGRAM_DEVICES" cl_program_info
  let _CL_PROGRAM_SOURCE = T.constant "CL_PROGRAM_SOURCE" cl_program_info
  let _CL_PROGRAM_BINARY_SIZES =
    T.constant "CL_PROGRAM_BINARY_SIZES" cl_program_info
  let _CL_PROGRAM_BINARIES = T.constant "CL_PROGRAM_BINARIES" cl_program_info

  (* cl_program_build_info *)
  let _CL_PROGRAM_BUILD_STATUS =
    T.constant "CL_PROGRAM_BUILD_STATUS" cl_program_build_info
  let _CL_PROGRAM_BUILD_OPTIONS =
    T.constant "CL_PROGRAM_BUILD_OPTIONS" cl_program_build_info
  let _CL_PROGRAM_BUILD_LOG =
    T.constant "CL_PROGRAM_BUILD_LOG" cl_program_build_info

  (* cl_build_status *)
  let _CL_BUILD_SUCCESS = T.constant "CL_BUILD_SUCCESS" cl_build_status
  let _CL_BUILD_NONE = T.constant "CL_BUILD_NONE" cl_build_status
  let _CL_BUILD_ERROR = T.constant "CL_BUILD_ERROR" cl_build_status
  let _CL_BUILD_IN_PROGRESS = T.constant "CL_BUILD_IN_PROGRESS" cl_build_status

  (* cl_kernel_info *)
  let _CL_KERNEL_FUNCTION_NAME =
    T.constant "CL_KERNEL_FUNCTION_NAME" cl_kernel_info
  let _CL_KERNEL_NUM_ARGS = T.constant "CL_KERNEL_NUM_ARGS" cl_kernel_info
  let _CL_KERNEL_REFERENCE_COUNT =
    T.constant "CL_KERNEL_REFERENCE_COUNT" cl_kernel_info
  let _CL_KERNEL_CONTEXT = T.constant "CL_KERNEL_CONTEXT" cl_kernel_info
  let _CL_KERNEL_PROGRAM = T.constant "CL_KERNEL_PROGRAM" cl_kernel_info

  (* cl_kernel_work_group_info *)
  let _CL_KERNEL_WORK_GROUP_SIZE =
    T.constant "CL_KERNEL_WORK_GROUP_SIZE" cl_kernel_work_group_info
  let _CL_KERNEL_COMPILE_WORK_GROUP_SIZE =
    T.constant "CL_KERNEL_COMPILE_WORK_GROUP_SIZE" cl_kernel_work_group_info
  let _CL_KERNEL_LOCAL_MEM_SIZE =
    T.constant "CL_KERNEL_LOCAL_MEM_SIZE" cl_kernel_work_group_info

  (* cl_event_info *)
  let _CL_EVENT_COMMAND_QUEUE =
    T.constant "CL_EVENT_COMMAND_QUEUE" cl_event_info
  let _CL_EVENT_COMMAND_TYPE = T.constant "CL_EVENT_COMMAND_TYPE" cl_event_info
  let _CL_EVENT_REFERENCE_COUNT =
    T.constant "CL_EVENT_REFERENCE_COUNT" cl_event_info
  let _CL_EVENT_COMMAND_EXECUTION_STATUS =
    T.constant "CL_EVENT_COMMAND_EXECUTION_STATUS" cl_event_info

  (* cl_command_type *)
  let _CL_COMMAND_NDRANGE_KERNEL =
    T.constant "CL_COMMAND_NDRANGE_KERNEL" cl_command_type
  let _CL_COMMAND_TASK = T.constant "CL_COMMAND_TASK" cl_command_type
  let _CL_COMMAND_NATIVE_KERNEL =
    T.constant "CL_COMMAND_NATIVE_KERNEL" cl_command_type
  let _CL_COMMAND_READ_BUFFER =
    T.constant "CL_COMMAND_READ_BUFFER" cl_command_type
  let _CL_COMMAND_WRITE_BUFFER =
    T.constant "CL_COMMAND_WRITE_BUFFER" cl_command_type
  let _CL_COMMAND_COPY_BUFFER =
    T.constant "CL_COMMAND_COPY_BUFFER" cl_command_type
  let _CL_COMMAND_READ_IMAGE =
    T.constant "CL_COMMAND_READ_IMAGE" cl_command_type
  let _CL_COMMAND_WRITE_IMAGE =
    T.constant "CL_COMMAND_WRITE_IMAGE" cl_command_type
  let _CL_COMMAND_COPY_IMAGE =
    T.constant "CL_COMMAND_COPY_IMAGE" cl_command_type
  let _CL_COMMAND_COPY_IMAGE_TO_BUFFER =
    T.constant "CL_COMMAND_COPY_IMAGE_TO_BUFFER" cl_command_type
  let _CL_COMMAND_COPY_BUFFER_TO_IMAGE =
    T.constant "CL_COMMAND_COPY_BUFFER_TO_IMAGE" cl_command_type
  let _CL_COMMAND_MAP_BUFFER =
    T.constant "CL_COMMAND_MAP_BUFFER" cl_command_type
  let _CL_COMMAND_MAP_IMAGE = T.constant "CL_COMMAND_MAP_IMAGE" cl_command_type
  let _CL_COMMAND_UNMAP_MEM_OBJECT =
    T.constant "CL_COMMAND_UNMAP_MEM_OBJECT" cl_command_type
  let _CL_COMMAND_MARKER = T.constant "CL_COMMAND_MARKER" cl_command_type
  let _CL_COMMAND_ACQUIRE_GL_OBJECTS =
    T.constant "CL_COMMAND_ACQUIRE_GL_OBJECTS" cl_command_type
  let _CL_COMMAND_RELEASE_GL_OBJECTS =
    T.constant "CL_COMMAND_RELEASE_GL_OBJECTS" cl_command_type

  (* command execution status *)
  let _CL_COMPLETE = T.constant "CL_COMPLETE" cl_int
  let _CL_RUNNING = T.constant "CL_RUNNING" cl_int
  let _CL_SUBMITTED = T.constant "CL_SUBMITTED" cl_int
  let _CL_QUEUED = T.constant "CL_QUEUED" cl_int

  (* cl_profiling_info *)
  let _CL_PROFILING_COMMAND_QUEUED =
    T.constant "CL_PROFILING_COMMAND_QUEUED" cl_profiling_info
  let _CL_PROFILING_COMMAND_SUBMIT =
    T.constant "CL_PROFILING_COMMAND_SUBMIT" cl_profiling_info
  let _CL_PROFILING_COMMAND_START =
    T.constant "CL_PROFILING_COMMAND_START" cl_profiling_info
  let _CL_PROFILING_COMMAND_END =
    T.constant "CL_PROFILING_COMMAND_END" cl_profiling_info

  (**************)
  (* OpenCL 1.1 *)
  (**************)

  (* Typedefs *)
  let cl_buffer_create_type = T.typedef cl_uint "cl_buffer_create_type"

  (* Struct types *)
  type _cl_buffer_region
  let _cl_buffer_region : _cl_buffer_region Ctypes_static.structure T.typ =
    T.structure "_cl_buffer_region"
  let origin = T.field _cl_buffer_region "origin" T.size_t
  let size = T.field _cl_buffer_region "size" T.size_t
  let () = T.seal _cl_buffer_region
  let cl_buffer_region = T.typedef _cl_buffer_region "cl_buffer_region"

  (* Error Codes *)
  let _CL_MISALIGNED_SUB_BUFFER_OFFSET =
    T.constant "CL_MISALIGNED_SUB_BUFFER_OFFSET" cl_int
  let _CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST =
    T.constant "CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST" cl_int
  let _CL_INVALID_PROPERTY = T.constant "CL_INVALID_PROPERTY" cl_int

  (* cl_device_info *)
  let _CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF =
    T.constant "CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF" cl_device_info
  let _CL_DEVICE_HOST_UNIFIED_MEMORY =
    T.constant "CL_DEVICE_HOST_UNIFIED_MEMORY" cl_device_info
  let _CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR =
    T.constant "CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR" cl_device_info
  let _CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT =
    T.constant "CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT" cl_device_info
  let _CL_DEVICE_NATIVE_VECTOR_WIDTH_INT =
    T.constant "CL_DEVICE_NATIVE_VECTOR_WIDTH_INT" cl_device_info
  let _CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG =
    T.constant "CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG" cl_device_info
  let _CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT =
    T.constant "CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT" cl_device_info
  let _CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE =
    T.constant "CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE" cl_device_info
  let _CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF =
    T.constant "CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF" cl_device_info
  let _CL_DEVICE_OPENCL_C_VERSION =
    T.constant "CL_DEVICE_OPENCL_C_VERSION" cl_device_info

  (* cl_device_fp_config - bitfield *)
  let _CL_FP_SOFT_FLOAT = T.constant "CL_FP_SOFT_FLOAT" cl_device_fp_config

  (* cl_context_info *)
  let _CL_CONTEXT_NUM_DEVICES =
    T.constant "CL_CONTEXT_NUM_DEVICES" cl_context_info

  (* cl_context_info + cl_context_properties *)
  let _CL_CONTEXT_PLATFORM = T.constant "CL_CONTEXT_PLATFORM" cl_context_info

  (* cl_channel_order *)
  let _CL_Rx = T.constant "CL_Rx" cl_channel_order
  let _CL_RGx = T.constant "CL_RGx" cl_channel_order
  let _CL_RGBx = T.constant "CL_RGBx" cl_channel_order

  (* cl_mem_info *)
  let _CL_MEM_ASSOCIATED_MEMOBJECT =
    T.constant "CL_MEM_ASSOCIATED_MEMOBJECT" cl_mem_info
  let _CL_MEM_OFFSET = T.constant "CL_MEM_OFFSET" cl_mem_info

  (* cl_addressing_mode *)
  let _CL_ADDRESS_MIRRORED_REPEAT =
    T.constant "CL_ADDRESS_MIRRORED_REPEAT" cl_addressing_mode

  (* cl_kernel_work_group_info *)
  let _CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE = T.constant
      "CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE" cl_kernel_work_group_info
  let _CL_KERNEL_PRIVATE_MEM_SIZE =
    T.constant "CL_KERNEL_PRIVATE_MEM_SIZE" cl_kernel_work_group_info

  (* cl_event_info *)
  let _CL_EVENT_CONTEXT = T.constant "CL_EVENT_CONTEXT" cl_event_info

  (* cl_command_type *)
  let _CL_COMMAND_READ_BUFFER_RECT =
    T.constant "CL_COMMAND_READ_BUFFER_RECT" cl_command_type
  let _CL_COMMAND_WRITE_BUFFER_RECT =
    T.constant "CL_COMMAND_WRITE_BUFFER_RECT" cl_command_type
  let _CL_COMMAND_COPY_BUFFER_RECT =
    T.constant "CL_COMMAND_COPY_BUFFER_RECT" cl_command_type
  let _CL_COMMAND_USER = T.constant "CL_COMMAND_USER" cl_command_type

  (* cl_buffer_create_type *)
  let _CL_BUFFER_CREATE_TYPE_REGION =
    T.constant "CL_BUFFER_CREATE_TYPE_REGION" cl_buffer_create_type
end
