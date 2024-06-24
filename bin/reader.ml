open Needlework_scenario_generator
open Common
module Ip = Ipaddr.V4

let find_child tag xml =
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

let text xml = try Xml.children xml |> List.hd |> Xml.pcdata with
               | e -> prerr_endline (!%"Reader.text: %s: %s" (Printexc.to_string e) (Xml.to_string xml));
                      raise e

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

let (/) xml tag = Xml.children xml |> List.filter (fun xml -> Xml.tag xml = tag)
let (//) xml tag : Xml.xml = get_child tag xml
let (/@) xml attr = Xml.attrib xml attr

open AddressBook

let address_book_of_xmls address_entries address_grps =
  let address_of_xml entry =
    let name = Xml.attrib entry "name" in
    let addr = match find_child_data "ip-netmask" entry, find_child_data "fqdn" entry with
    | Some ip, _ when String.ends_with ~suffix:"32" ip ->
       let ip = String.split_on_char '/' ip |> List.hd in
       SingleHost (Ip.of_string_exn ip)
    | Some ip, _ ->
       AddressBook.of_string ip
    | None, Some fqdn ->
       FQDN fqdn
    | None, None -> failwith "address_of_xml"
    in
    (name, addr)
  in
  let group_of_xml entry =
    let name = entry /@ "name" in
    let members = entry // "static"
                  |> Xml.children
                  |> List.map text
    in
    (name, AddressBook.Group members)
  in
  List.map address_of_xml address_entries
  @ List.map group_of_xml address_grps

let read_rules xml =
  let rule_of_entry entry =
    let name = entry /@ "name" in
    let source =
      entry // "source" / "member" |> List.map text
    in
    let destination =
      entry // "destination" / "member" |> List.map text
    in
    Rule.{name; source; destination}
  in
  xml // "rulebase" // "security" // "rules"
  |> Xml.children
  |> List.map rule_of_entry

let read_vsys_entry xml =
  let address_entries = xml // "address" |> Xml.children in
  let address_grps = xml // "address-group" |> Xml.children in
  let book = address_book_of_xmls address_entries address_grps in
  let rules = read_rules xml in
  (book, rules)

let read : Xml.xml -> (AddressBook.t * Rule.t list) = fun xml ->
  try
  xml // "devices" // "entry" // "vsys" // "entry"
  |> read_vsys_entry
  with
  | Xml.Not_pcdata xml as e ->
     Log.error "===ERROR: Not pcdata %s" (Xml.to_string xml);
     raise e
  | e ->
     Log.error "===ERROR: %s" (Printexc.to_string e);
     raise e
