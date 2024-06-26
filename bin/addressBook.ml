open Needlework_scenario_generator
open Common
module Ip = Ipaddr.V4
module Pre = Ip.Prefix

type address =
| SingleHost of Ip.t
| INet of Pre.t
| Group of string list (* ここではいったんnameのまま,たどるときはもう一度辞書を引く必要がある *)
| FQDN of string

type t = (string * address) list

let of_cidr s =
  let prefix = Ip.Prefix.of_string_exn s in
  INet prefix

let of_string s =
  match Ip.of_string s with
  | Ok ip -> SingleHost ip
  | Error _ -> of_cidr s

let mk_book assoc =
  ("any", of_string "8.8.8.8") :: assoc

let dump t =
  let item = function
    | SingleHost addr -> !%"Single '%s'" (Ip.to_string addr)
    | INet prefix ->
       let f, l = Pre.first prefix, Pre.last prefix in
       !%"INet(%s: %s, %s)" (Pre.to_string prefix)
         (Ip.to_string f) (Ip.to_string l)
    | Group _ -> "Group"
    | FQDN url -> !%"FQDN: %s" url
  in
  List.map (fun (name, addr) -> !%"[%s -> %s]" name (item addr)) t
  |> String.concat "\n  "

let unit x = List.to_seq [x]

let rec find book name =
  match List.assoc_opt name book with
  | None -> Log.warn "AddressBook.find: name '%s' does not found" name; []
  | Some (Group names) ->
     List.concat_map (find book) names
  | Some a -> [a]

let gen_of_prefix pre =
  let (f, l) = Pre.first pre, Pre.last pre in
  let a, b = Ip.to_int32 f, Ip.to_int32 l in
  let n = Int32.(sub b a) in
  let open Gen in
  Gen.int32 n >>= fun i ->
  Int32.add a i
  |> Ip.of_int32
  |> return
