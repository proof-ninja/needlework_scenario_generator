open Needlework_scenario_generator
open Common
module Ip = Ipaddr.V4
module XmlLight = Xml
open XmlUtil
module Xml = XmlUtil

open AddressBook

let address_book_of_xmls address_entries address_grps =
  let address_of_xml entry =
    let name = entry /@ "name" in
    let addr = match entry //? "ip-netmask" |> Option.map text, entry //? "fqdn" |> Option.map text with
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
  |> AddressBook.mk_book

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
  let address_grps =
    match xml / "address-group" with
    | [] -> []
    | [xml] -> Xml.children xml
    | _ :: _ -> failwith (!%"Reader.read_vsys_entry: unexpected multiple address-group: \n%s" (Xml.to_string xml))
  in
  let book = address_book_of_xmls address_entries address_grps in
  let rules = read_rules xml in
  (book, rules)

let read : xml -> (AddressBook.t * Rule.t list) = fun xml ->
  try
  xml // "devices" // "entry" // "vsys" // "entry"
  |> read_vsys_entry
  with
  | XmlLight.Not_pcdata xml as e ->
     Log.error "===ERROR: Not pcdata %s" (XmlLight.to_string xml);
     raise e
  | e ->
     Log.error "===ERROR: %s" (Printexc.to_string e);
     raise e
