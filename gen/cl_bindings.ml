open Ctypes
open Foreign

module T = Cl_types.Make (Cl_types_detected)

module Make (F : Cstubs.FOREIGN) = struct
  (**************)
  (* OpenCL 1.0 *)
  (**************)

  (** {2 Platform API} *)

  let clGetPlatformIDs = F.foreign "clGetPlatformIDs"
      (T.cl_uint @-> ptr T.cl_platform_id @-> ptr T.cl_uint @->
       returning T.cl_int)

  let clGetPlatformInfo = F.foreign "clGetPlatformInfo"
      (T.cl_platform_id @-> T.cl_platform_info @-> size_t @-> ptr void @->
       ptr size_t @-> returning T.cl_int)

  (** {2 Device APIs} *)

  let clGetDeviceIDs = F.foreign "clGetDeviceIDs"
      (T.cl_platform_id @-> T.cl_device_type @-> T.cl_uint @->
       ptr T.cl_device_id @-> ptr T.cl_uint @-> returning T.cl_int)

  let clGetDeviceInfo = F.foreign "clGetDeviceInfo"
      (T.cl_device_id @-> T.cl_device_info @-> size_t @-> ptr void @->
       ptr size_t @-> returning T.cl_int)

  (** {2 Context APIs} *)

  let clCreateContext = F.foreign "clCreateContext"
      (ptr T.cl_context_properties @-> T.cl_uint @-> ptr T.cl_device_id @->
       funptr_opt (string @-> ptr void @-> size_t @-> ptr void @->
                   returning void) @-> ptr void @-> ptr T.cl_int @->
       returning T.cl_context)

  let clCreateContextFromType = F.foreign "clCreateContextFromType"
      (ptr T.cl_context_properties @->
       T.cl_device_type @->
       funptr_opt (string @-> ptr void @-> size_t @-> ptr void @->
                   returning void) @-> ptr void @-> ptr T.cl_int @->
       returning T.cl_context)

  let clRetainContext = F.foreign "clRetainContext"
      (T.cl_context @-> returning T.cl_int)

  let clReleaseContext = F.foreign "clReleaseContext"
      (T.cl_context @-> returning T.cl_int)

  let clGetContextInfo = F.foreign "clGetContextInfo"
      (T.cl_context @-> T.cl_context_info @-> size_t @-> ptr void @->
       ptr size_t @-> returning T.cl_int)

  (** {2 Command Queue APIs} *)

  let clCreateCommandQueue = F.foreign "clCreateCommandQueue"
      (T.cl_context @-> T.cl_device_id @-> T.cl_command_queue_properties @->
       ptr T.cl_int @-> returning T.cl_command_queue)

  let clRetainCommandQueue = F.foreign "clRetainCommandQueue"
      (T.cl_command_queue @-> returning T.cl_int)

  let clReleaseCommandQueue = F.foreign "clReleaseCommandQueue"
      (T.cl_command_queue @-> returning T.cl_int)

  let clGetCommandQueueInfo = F.foreign "clGetCommandQueueInfo"
      (T.cl_command_queue @-> T.cl_command_queue_info @-> size_t @->
       ptr void @-> ptr size_t @-> returning T.cl_int)

  (** {2 Memory Object APIs} *)

  let clCreateBuffer = F.foreign "clCreateBuffer"
      (T.cl_context @-> T.cl_mem_flags @-> size_t @-> ptr void @->
       ptr T.cl_int @-> returning T.cl_mem)

  let clCreateImage2D = F.foreign "clCreateImage2D"
      (T.cl_context @-> T.cl_mem_flags @-> ptr T.cl_image_format @->
       size_t @-> size_t @-> size_t @-> ptr void @-> ptr T.cl_int @->
       returning T.cl_mem)

  let clCreateImage3D = F.foreign "clCreateImage3D"
      (T.cl_context @-> T.cl_mem_flags @-> ptr T.cl_image_format @->
       size_t @-> size_t @-> size_t @-> size_t @-> size_t @->
       ptr void @-> ptr T.cl_int @-> returning T.cl_mem)

  let clRetainMemObject = F.foreign "clRetainMemObject"
      (T.cl_mem @-> returning T.cl_int)

  let clReleaseMemObject = F.foreign "clReleaseMemObject"
      (T.cl_mem @-> returning T.cl_int)

  let clGetSupportedImageFormats = F.foreign "clGetSupportedImageFormats"
      (T.cl_context @-> T.cl_mem_flags @-> T.cl_mem_object_type @->
       T.cl_uint @-> ptr T.cl_image_format @-> ptr T.cl_uint @->
       returning T.cl_int)

  let clGetMemObjectInfo = F.foreign "clGetMemObjectInfo"
      (T.cl_mem @-> T.cl_mem_info @-> size_t @-> ptr void @->
       ptr size_t @-> returning T.cl_int)

  let clGetImageInfo = F.foreign "clGetImageInfo"
      (T.cl_mem @-> T.cl_image_info @-> size_t @-> ptr void @->
       ptr size_t @-> returning T.cl_int)

  (** {2 Sampler APIs} *)

  let clCreateSampler = F.foreign "clCreateSampler"
      (T.cl_context @-> T.cl_bool @-> T.cl_addressing_mode @->
       T.cl_filter_mode @-> ptr T.cl_int @-> returning T.cl_sampler)

  let clRetainSampler = F.foreign "clRetainSampler"
      (T.cl_sampler @-> returning T.cl_int)

  let clReleaseSampler = F.foreign "clReleaseSampler"
      (T.cl_sampler @-> returning T.cl_int)

  let clGetSamplerInfo = F.foreign "clGetSamplerInfo"
      (T.cl_sampler @-> T.cl_sampler_info @-> size_t @-> ptr void @->
       ptr size_t @-> returning T.cl_int)

  (** {2 Program Object APIs} *)

  let clCreateProgramWithSource = F.foreign "clCreateProgramWithSource"
      (T.cl_context @-> T.cl_uint @-> ptr string @-> ptr size_t @->
       ptr T.cl_int @-> returning T.cl_program)

  let clCreateProgramWithBinary = F.foreign "clCreateProgramWithBinary"
      (T.cl_context @-> T.cl_uint @-> ptr T.cl_device_id @-> ptr size_t @->
       ptr ocaml_bytes @-> ptr T.cl_int @-> ptr T.cl_int @->
       returning T.cl_program)

  let clRetainProgram = F.foreign "clRetainProgram"
      (T.cl_program @-> returning T.cl_int)

  let clReleaseProgram = F.foreign "clReleaseProgram"
      (T.cl_program @-> returning T.cl_int)

  let clBuildProgram = F.foreign "clBuildProgram"
      (T.cl_program @-> T.cl_uint @-> ptr T.cl_device_id @-> string @->
       funptr_opt (T.cl_program @-> ptr void @-> returning void) @->
       ptr void @-> returning T.cl_int)

  let clUnloadCompiler = F.foreign "clUnloadCompiler"
      (void @-> returning T.cl_int)

  let clGetProgramInfo = F.foreign "clGetProgramInfo"
      (T.cl_program @-> T.cl_program_info @-> size_t @-> ptr void @->
       ptr size_t @-> returning T.cl_int)

  let clGetProgramBuildInfo = F.foreign "clGetProgramBuildInfo"
      (T.cl_program @-> T.cl_device_id @-> T.cl_program_build_info @->
       size_t @-> ptr void @-> ptr size_t @-> returning T.cl_int)

  (** {2 Kernel Object APIs} *)

  let clCreateKernel = F.foreign "clCreateKernel"
      (T.cl_program @-> string @-> ptr T.cl_int @-> returning T.cl_kernel)

  let clCreateKernelsInProgram = F.foreign "clCreateKernelsInProgram"
      (T.cl_program @-> T.cl_uint @-> ptr T.cl_kernel @-> ptr T.cl_uint @->
       returning T.cl_int)

  let clRetainKernel = F.foreign "clRetainKernel"
      (T.cl_kernel @-> returning T.cl_int)

  let clReleaseKernel = F.foreign "clReleaseKernel"
      (T.cl_kernel @-> returning T.cl_int)

  let clSetKernelArg = F.foreign "clSetKernelArg"
      (T.cl_kernel @-> T.cl_uint @-> size_t @-> ptr void @->
       returning T.cl_int)

  let clGetKernelInfo = F.foreign "clGetKernelInfo"
      (T.cl_kernel @-> T.cl_kernel_info @-> size_t @-> ptr void @->
       ptr size_t @-> returning T.cl_int)

  let clGetKernelWorkGroupInfo = F.foreign "clGetKernelWorkGroupInfo"
      (T.cl_kernel @-> T.cl_device_id @-> T.cl_kernel_work_group_info @->
       size_t @-> ptr void @-> ptr size_t @-> returning T.cl_int)

  (** {2 Event Object APIs} *)

  let clWaitForEvents = F.foreign "clWaitForEvents"
      (T.cl_uint @-> ptr T.cl_event @-> returning T.cl_int)

  let clGetEventInfo = F.foreign "clGetEventInfo"
      (T.cl_event @-> T.cl_event_info @-> size_t @-> ptr void @->
       ptr size_t @-> returning T.cl_int)

  let clRetainEvent = F.foreign "clRetainEvent"
      (T.cl_event @-> returning T.cl_int)

  let clReleaseEvent = F.foreign "clReleaseEvent"
      (T.cl_event @-> returning T.cl_int)

  (** {2 Profiling APIs} *)

  let clGetEventProfilingInfo = F.foreign "clGetEventProfilingInfo"
      (T.cl_event @-> T.cl_profiling_info @-> size_t @-> ptr void @->
       ptr size_t @-> returning T.cl_int)

  (** {2 Flush and Finish APIs} *)

  let clFlush = F.foreign "clFlush"
      (T.cl_command_queue @-> returning T.cl_int)

  let clFinish = F.foreign "clFinish"
      (T.cl_command_queue @-> returning T.cl_int)

  (** {2 Enqueued Commands APIs} *)

  let clEnqueueReadBuffer = F.foreign "clEnqueueReadBuffer"
      (T.cl_command_queue @-> T.cl_mem @-> T.cl_bool @-> size_t @->
       size_t @-> ptr void @-> T.cl_uint @-> ptr T.cl_event @->
       ptr T.cl_event @-> returning T.cl_int)

  let clEnqueueWriteBuffer = F.foreign "clEnqueueWriteBuffer"
      (T.cl_command_queue @-> T.cl_mem @-> T.cl_bool @-> size_t @->
       size_t @-> ptr void @-> T.cl_uint @-> ptr T.cl_event @->
       ptr T.cl_event @-> returning T.cl_int)

  let clEnqueueCopyBuffer = F.foreign "clEnqueueCopyBuffer"
      (T.cl_command_queue @-> T.cl_mem @-> T.cl_mem @-> size_t @->
       size_t @-> size_t @-> T.cl_uint @-> ptr T.cl_event @->
       ptr T.cl_event @-> returning T.cl_int)

  let clEnqueueReadImage = F.foreign "clEnqueueReadImage"
      (T.cl_command_queue @-> T.cl_mem @-> T.cl_bool @-> ptr size_t @->
       ptr size_t @-> size_t @-> size_t @-> ptr void @-> T.cl_uint @->
       ptr T.cl_event @-> ptr T.cl_event @-> returning T.cl_int)

  let clEnqueueWriteImage = F.foreign "clEnqueueWriteImage"
      (T.cl_command_queue @-> T.cl_mem @-> T.cl_bool @-> ptr size_t @->
       ptr size_t @-> size_t @-> size_t @-> ptr void @-> T.cl_uint @->
       ptr T.cl_event @-> ptr T.cl_event @-> returning T.cl_int)

  let clEnqueueCopyImage = F.foreign "clEnqueueCopyImage"
      (T.cl_command_queue @-> T.cl_mem @-> T.cl_mem @-> ptr size_t @->
       ptr size_t @-> ptr size_t @-> T.cl_uint @-> ptr T.cl_event @->
       ptr T.cl_event @-> returning T.cl_int)

  let clEnqueueCopyImageToBuffer = F.foreign "clEnqueueCopyImageToBuffer"
      (T.cl_command_queue @-> T.cl_mem @-> T.cl_mem @-> ptr size_t @->
       ptr size_t @-> size_t @-> T.cl_uint @-> ptr T.cl_event @->
       ptr T.cl_event @-> returning T.cl_int)

  let clEnqueueCopyBufferToImage = F.foreign "clEnqueueCopyBufferToImage"
      (T.cl_command_queue @-> T.cl_mem @-> T.cl_mem @-> size_t @->
       ptr size_t @-> ptr size_t @-> T.cl_uint @-> ptr T.cl_event @->
       ptr T.cl_event @-> returning T.cl_int)

  let clEnqueueMapBuffer = F.foreign "clEnqueueMapBuffer"
      (T.cl_command_queue @-> T.cl_mem @-> T.cl_bool @-> T.cl_map_flags @->
       size_t @-> size_t @-> T.cl_uint @-> ptr T.cl_event @->
       ptr T.cl_event @-> ptr T.cl_int @-> returning (ptr void))

  let clEnqueueMapImage = F.foreign "clEnqueueMapImage"
      (T.cl_command_queue @-> T.cl_mem @-> T.cl_bool @-> T.cl_map_flags @->
       ptr size_t @-> ptr size_t @-> ptr size_t @-> ptr size_t @->
       T.cl_uint @-> ptr T.cl_event @-> ptr T.cl_event @-> ptr T.cl_int @->
       returning (ptr void))

  let clEnqueueUnmapMemObject = F.foreign "clEnqueueUnmapMemObject"
      (T.cl_command_queue @-> T.cl_mem @-> ptr void @-> T.cl_uint @->
       ptr T.cl_event @-> ptr T.cl_event @-> returning T.cl_int)

  let clEnqueueNDRangeKernel = F.foreign "clEnqueueNDRangeKernel"
      (T.cl_command_queue @-> T.cl_kernel @-> T.cl_uint @-> ptr size_t @->
       ptr size_t @-> ptr size_t @-> T.cl_uint @-> ptr T.cl_event @->
       ptr T.cl_event @-> returning T.cl_int)

  let clEnqueueTask = F.foreign "clEnqueueTask"
      (T.cl_command_queue @-> T.cl_kernel @-> T.cl_uint @-> ptr T.cl_event @->
       ptr T.cl_event @-> returning T.cl_int)

  let clEnqueueNativeKernel = F.foreign "clEnqueueNativeKernel"
      (T.cl_command_queue @-> funptr (ptr void @-> returning void) @->
       ptr void @-> size_t @-> T.cl_uint @-> ptr T.cl_mem @->
       ptr (ptr void) @-> T.cl_uint @-> ptr T.cl_event @-> ptr T.cl_event @->
       returning T.cl_int)

  let clEnqueueMarker = F.foreign "clEnqueueMarker"
      (T.cl_command_queue @-> ptr T.cl_event @-> returning T.cl_int)

  let clEnqueueWaitForEvents = F.foreign "clEnqueueWaitForEvents"
      (T.cl_command_queue @-> T.cl_uint @-> ptr T.cl_event @->
       returning T.cl_int)

  let clEnqueueBarrier = F.foreign "clEnqueueBarrier"
      (T.cl_command_queue @-> returning T.cl_int)

  let clGetExtensionFunctionAddress = F.foreign "clGetExtensionFunctionAddress"
      (ptr char @-> returning (ptr void))

  (**************)
  (* OpenCL 1.1 *)
  (**************)

  (** {2 Memory Object APIs} *)

  let clCreateSubBuffer = F.foreign "clCreateSubBuffer"
      (T.cl_mem @-> T.cl_mem_flags @-> T.cl_buffer_create_type @->
       ptr void @-> ptr T.cl_int @-> returning T.cl_mem)

  let clSetMemObjectDestructorCallback =
    F.foreign "clSetMemObjectDestructorCallback"
      (T.cl_mem @-> funptr (T.cl_mem @-> ptr void @-> returning void) @->
       ptr void @-> returning T.cl_int)

  (** {2 Event Object APIs} *)

  let clCreateUserEvent = F.foreign "clCreateUserEvent"
      (T.cl_context @-> ptr T.cl_int @-> returning T.cl_event)

  let clSetUserEventStatus = F.foreign "clSetUserEventStatus"
      (T.cl_event @-> T.cl_int @-> returning T.cl_int)

  let clSetEventCallback = F.foreign "clSetEventCallback"
      (T.cl_event @-> T.cl_int @->
       funptr (T.cl_event @-> T.cl_int @-> ptr void @-> returning void) @->
       ptr void @-> returning T.cl_int)

  (** {2 Enqueued Commands APIs} *)

  let clEnqueueReadBufferRect = F.foreign "clEnqueueReadBufferRect"
      (T.cl_command_queue @-> T.cl_mem @-> T.cl_bool @-> ptr size_t @->
       ptr size_t @-> ptr size_t @-> size_t @-> size_t @-> size_t @->
       size_t @-> ptr void @-> T.cl_uint @-> ptr T.cl_event @->
       ptr T.cl_event @-> returning T.cl_int)

  let clEnqueueWriteBufferRect = F.foreign "clEnqueueWriteBufferRect"
      (T.cl_command_queue @-> T.cl_mem @-> T.cl_bool @-> ptr size_t @->
       ptr size_t @-> ptr size_t @-> size_t @-> size_t @-> size_t @->
       size_t @-> ptr void @-> T.cl_uint @-> ptr T.cl_event @->
       ptr T.cl_event @-> returning T.cl_int)

  let clEnqueueCopyBufferRect = F.foreign "clEnqueueCopyBufferRect"
      (T.cl_command_queue @-> T.cl_mem @-> T.cl_mem @-> ptr size_t @->
       ptr size_t @-> ptr size_t @-> size_t @-> size_t @-> size_t @->
       size_t @-> T.cl_uint @-> ptr T.cl_event @-> ptr T.cl_event @->
       returning T.cl_int)
  (**************)
  (* OpenCL 1.2 *)
  (**************)

  (* clGetExtensionFunctionAddressForPlatform *)

  (** {2 Device APIs} *)

  let clCreateSubDevices = F.foreign "clCreateSubDevices"
      (T.cl_device_id @-> ptr T.cl_device_partition_property @->
       T.cl_uint @-> ptr T.cl_device_id @-> ptr T.cl_uint @->
       returning T.cl_int)

  let clRetainDevice = F.foreign "clRetainDevice"
      (T.cl_device_id @-> returning T.cl_int)

  let clReleaseDevice = F.foreign "clReleaseDevice"
      (T.cl_device_id @-> returning T.cl_int)

  (** {2 Memory Object APIs} *)

  let clCreateImage = F.foreign "clCreateImage"
      (T.cl_context @-> T.cl_mem_flags @-> ptr T.cl_image_format @->
       ptr T.cl_image_desc @-> ptr void @-> ptr T.cl_int @->
       returning T.cl_mem)

  (** {2 Program Object APIs} *)

  let clCreateProgramWithBuiltInKernels =
    F.foreign "clCreateProgramWithBuiltInKernels"
      (T.cl_context @-> T.cl_uint @-> ptr T.cl_device_id @-> ptr char @->
       ptr T.cl_int @-> returning T.cl_program)

  let clCompileProgram = F.foreign "clCompileProgram"
      (T.cl_program @-> T.cl_uint @-> ptr T.cl_device_id @-> ptr char @->
       T.cl_uint @-> ptr T.cl_program @-> ptr (ptr char) @->
       funptr (T.cl_program @-> ptr void @-> returning void) @->
       ptr void @-> returning T.cl_int)

  let clLinkProgram = F.foreign "clLinkProgram"
      (T.cl_context @-> T.cl_uint @-> ptr T.cl_device_id @-> ptr char @->
       T.cl_uint @-> ptr T.cl_program @->
       funptr (T.cl_program @-> ptr void @-> returning void) @->
       ptr void @-> ptr T.cl_int @-> returning T.cl_program)

  let clUnloadPlatformCompiler = F.foreign "clUnloadPlatformCompiler"
      (T.cl_platform_id @-> returning T.cl_int)

  (** {2 Kernel Object APIs} *)

  let clGetKernelArgInfo = F.foreign "clGetKernelArgInfo"
      (T.cl_kernel @-> T.cl_uint @-> T.cl_kernel_arg_info @-> size_t @->
       ptr void @-> ptr size_t @-> returning T.cl_int)

  (** {2 Enqueued Commands APIs} *)

  let clEnqueueFillBuffer = F.foreign "clEnqueueFillBuffer"
      (T.cl_command_queue @-> T.cl_mem @-> ptr void @-> size_t @->
       size_t @-> size_t @-> T.cl_uint @-> ptr T.cl_event @->
       ptr T.cl_event @-> returning T.cl_int)

  let clEnqueueFillImage = F.foreign "clEnqueueFillImage"
      (T.cl_command_queue @-> T.cl_mem @-> ptr void @-> ptr size_t @->
       ptr size_t @-> T.cl_uint @-> ptr T.cl_event @-> ptr T.cl_event @->
       returning T.cl_int)

  let clEnqueueMigrateMemObjects = F.foreign "clEnqueueMigrateMemObjects"
      (T.cl_command_queue @-> T.cl_uint @-> ptr T.cl_mem @->
       T.cl_mem_migration_flags @-> T.cl_uint @-> ptr T.cl_event @->
       ptr T.cl_event @-> returning T.cl_int)

  let clEnqueueMarkerWithWaitList = F.foreign "clEnqueueMarkerWithWaitList"
      (T.cl_command_queue @-> T.cl_uint @-> ptr T.cl_event @->
       ptr T.cl_event @-> returning T.cl_int)

  let clEnqueueBarrierWithWaitList = F.foreign "clEnqueueBarrierWithWaitList"
      (T.cl_command_queue @-> T.cl_uint @-> ptr T.cl_event @->
       ptr T.cl_event @-> returning T.cl_int)
end
