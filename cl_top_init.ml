let pp_device f d =
  Format.fprintf f "<Cl.Device \"%s\">" (Cl.Device.name d);;

let pp_platform f p =
  Format.fprintf f "<Cl.Platform \"%s\">" (Cl.Platform.name p);;

#install_printer pp_device;;
#install_printer pp_platform;;
