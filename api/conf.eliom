(**
**   API Configuration file
*)

(******************************************************************************
********************************* Conf getter *********************************
*******************************************************************************)

module Configuration =
struct

  module Read =
  struct

    let config = Eliom_config.get_config ()

    let block parent name =
      let exists = function
        | Simplexmlparser.Element (n, _, _) when String.compare n name == 0 -> true
        | _ -> false
      in
      let extract = function
        | Simplexmlparser.Element (_, _, b) -> b
        | _ -> raise Not_found
      in
      try extract (List.find exists parent)
      with _ ->
        raise (Ocsigen_extensions.Error_in_config_file
                 (name ^" must be configured"))

    let string parent name =
      let extract = function
        | Simplexmlparser.PCData v -> v
        | _ -> raise Not_found
      in
      try extract (List.hd (block parent name))
      with _ ->
        raise (Ocsigen_extensions.Error_in_config_file
                 (name ^" must be configured"))

    let opt_string parent name =
      try Some (string parent name)
      with _ -> None

    let int parent name =
      try int_of_string (string parent name)
      with _ ->
        raise (Ocsigen_extensions.Error_in_config_file
                 (name ^" must be configured"))

    let float parent name =
      try float_of_string (string parent name)
      with _ ->
        raise (Ocsigen_extensions.Error_in_config_file
                 (name ^" must be configured"))

  end

  module Bot =
  struct

    let block = Read.block Read.config "bot"

    let directory = Read.string block "directory"
    let logdir = Read.string block "logdir"
    let max_simultaneous_process = Read.int block "maxsimultaneousprocess"

  end


end
