open Ocamlbuild_pack
open Ocamlbuild_plugin

let ctypes_lib_dir = Sys.getenv "CTYPES_LIB_DIR"
let ocaml_lib_dir = Sys.getenv "OCAML_LIB_DIR"

let () = dispatch (function
    | After_rules ->

      (* Generate C program to generate OCaml type detect program *)
      rule "gen/x_types.ml -> gen/x_types_detect.c"
        ~deps:["gen/%_typegen.byte"]
        ~prods:["gen/%_types_detect.c"]
        (fun env build ->
           Cmd (S [A (env "gen/%_typegen.byte"); Sh ">";
                   A (env "gen/%_types_detect.c")]));

      (* Build type detect generation C program *)
      rule "gen/x_types_detect.c -> gen/x_types_detect"
        ~deps:["gen/%_types_detect.c"]
        ~prods:["gen/%_types_detect"]
        (fun env build ->
           Cmd (S [A "cc";
                   A "-I"; A "/usr/local/include";
                   A "-I"; A ctypes_lib_dir;
                   A "-I"; A ocaml_lib_dir;
                   A "-o"; A (env "gen/%_types_detect");
                   A (env "gen/%_types_detect.c")]));

      (* Detect types and constants *)
      rule "gen/x_types_detect -> x_types_detected.ml"
        ~deps:["gen/%_types_detect"]
        ~prods:["%_types_detected.ml"]
        (fun env build ->
           Cmd (S [A (env "gen/%_types_detect"); Sh ">";
                   A (env "%_types_detected.ml")]));

      (* Generate C stubs *)
      rule "gen/x_bindings.ml -> x_stubs.c"
        ~deps:["gen/%_bindgen.byte"]
        ~prods:["%_stubs.c"]
        (fun env build ->
           Cmd (S [A (env "gen/%_bindgen.byte"); A "c"; Sh ">";
                   A (env "%_stubs.c")]));

      (* Generate OCaml bindings *)
      rule "gen/x_bindings.ml -> x_generated.ml"
        ~deps:["gen/%_bindgen.byte"]
        ~prods:["%_generated.ml"]
        (fun env build ->
           Cmd (S [A (env "gen/%_bindgen.byte"); A "ml"; Sh ">";
                   A (env "%_generated.ml")]));

      (* Add /usr/local/ include and lib paths for C *)
      flag ["c"; "compile"] & S [A "-ccopt"; A "-I/usr/local/include"];
      flag ["c"; "ocamlmklib"] & A "-L/usr/local/lib";

      (* Linking cstubs *)
      flag ["c"; "compile"; "use_ctypes"] & S [A "-I"; A ctypes_lib_dir];

      (* Linking libOpenCL *)
      flag ["c"; "ocamlmklib"; "use_opencl"] & A "-lOpenCL";

      (* Linking generated stubs (bytecode) *)
      dep ["ocaml"; "link"; "byte"; "library";
           "use_cl_stubs"] ["dllcl_stubs" -.- (! Options.ext_dll)];
      flag ["ocaml"; "link"; "byte"; "library";
            "use_cl_stubs"] & S [A "-dllib"; A "-lcl_stubs"];

      (* Linking generated stubs (native) *)
      dep ["ocaml"; "link"; "native"; "library";
           "use_cl_stubs"] ["libcl_stubs" -.- (! Options.ext_lib)];
      flag ["ocaml"; "link"; "native"; "library";
            "use_cl_stubs"] & S [A "-cclib"; A "-lcl_stubs";
                                 A "-cclib"; A "-lOpenCL"];

    | _ -> ())
