open Yojson.Basic.Util

(** [jsonfile] is the json file that contains [names] *)
let jsonfile = "Names.json"

(** [name_pkg_of_json j] is the string that represents a [name] parsed from the
    json bundle [j] *)
let name_pkg_of_json j =
  j |> member "name" |> to_string

(** [name_pkg_lst j] is the list of strings that represent [names] *)
let name_pkg_lst j =
  j |> member "names" |> to_list |> List.map name_pkg_of_json

(** [names] is the list of string of names that is parsed from the [jsonfile] *)
let names = name_pkg_lst (Yojson.Basic.from_file jsonfile)

(** [generate_indices i acc] is the list of [i] randomly generated integers *)
let rec generate_indices i acc =
  if (i = 0) then acc else (
    let temp_index = Random.int (List.length names) in
    if (List.mem temp_index acc) then (generate_indices i acc) 
    else (generate_indices (i - 1) (temp_index :: acc))
  )

(** [generate_names i] is the list of [i] randomly selected names from the list
    of names [names] *)
let generate_names i =
  let lst_of_indices = generate_indices i [] in
  List.map (fun x -> (List.nth names x)) lst_of_indices