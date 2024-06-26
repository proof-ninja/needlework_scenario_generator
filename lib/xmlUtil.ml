open Common

type xml = Xml.xml
type t = xml

let parse_file filename = Xml.parse_file filename

let to_string = Xml.to_string
let children = Xml.children
(*let find_child tag xml =
  Xml.children xml
  |> List.find_opt (fun xml -> Xml.tag xml = tag)

let find_child_data tag xml =
  Xml.children xml
  |> List.find_opt (fun xml -> Xml.tag xml = tag)
  |> Option.map Xml.children
  |> Option.map List.hd
  |> Option.map Xml.pcdata

let get_child tag xml =
  Xml.children xml
  |> List.find_opt (fun xml -> Xml.tag xml = tag)
  |> function
    | None ->
       failwith (!%"get_child: tag '%s' not found." tag)
    | Some xml -> xml
*)
let text xml = try Xml.children xml |> List.hd |> Xml.pcdata with
               | e -> prerr_endline (!%"Reader.text: %s: %s" (Printexc.to_string e) (Xml.to_string xml));
                      raise e
(*
let get_child_data tag xml =
  find_child_data tag xml
  |> function
    | None -> failwith (!%"get_child_data: tag '%s' not found." tag)
    | Some xml -> xml

let get_child_data_list tag xml =
  Xml.children xml
  |> List.filter (fun xml -> Xml.tag xml = tag)
  |> List.map Xml.pcdata

let has tag xml =
  Xml.children xml
  |> List.exists (fun xml -> Xml.tag xml = tag)
*)

let (/) xml tag = Xml.children xml |> List.filter (fun xml -> Xml.tag xml = tag)

let (//) xml tag : xml =
  Xml.children xml
  |> List.find (fun xml -> Xml.tag xml = tag)

let (//?) xml tag : xml option =
  Xml.children xml
  |> List.find_opt (fun xml -> Xml.tag xml = tag)

let (/@) xml attr = Xml.attrib xml attr
