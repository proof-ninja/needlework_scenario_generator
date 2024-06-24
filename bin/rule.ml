open Needlework_scenario_generator
open Common

type t = {
    name: string;
    source: string list;
    destination: string list;
}

let dump t =
  !%"Rule{%s: src=[%s]; dst=[%s]}"
    t.name
    (String.concat ", " t.source)
    (String.concat ", " t.destination)

let addresses book names =
  List.concat_map (AddressBook.find book) names

let sources book t =
  addresses book t.source

let destinations book t =
  addresses book t.destination
