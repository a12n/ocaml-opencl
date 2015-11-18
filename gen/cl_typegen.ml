let headers =
  "#ifdef __APPLE__\n" ^
  "#   include <OpenCL/cl.h>\n" ^
  "#else\n" ^
  "#   include <CL/cl.h>\n" ^
  "#endif\n"

let () =
  let fmt = Format.std_formatter in
  Format.fprintf fmt "%s@." headers;
  Cstubs.Types.write_c fmt (module Cl_types.Make)
