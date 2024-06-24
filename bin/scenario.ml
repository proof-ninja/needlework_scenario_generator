module Ip = Ipaddr.V4
open Needlework_scenario_generator
open Common

type row = {
(* protocol: いったん空欄 *)
    src_ip: string; (*[抽出した`source`のIPアドレス] *)
(* src-port(option): いったん空欄 *)
(* src-nat-ip(option): いったん空欄 *)
(* is-receiver-physical(option): いったん空欄 *)
(* dst-nat-ip(option): いったん空欄 *)
(* dst-nat-port (option): いったん空欄 *)
    dst_ip: string; (*[抽出した`destination`のIPアドレス] # ただしFQDNだった場合は空欄 *)
(* dst-port: いったん空欄 *)
    url_domain : string; (* (option): [Aで抽出したFQDN] *)
(* anti-virus(option): いったん空欄 *)
(* timeout(option): いったん空欄 *)
(* try(option): いったん空欄 *)
(* expect: いったん空欄 *)
(* ping-traceroute: いったん空欄 *)
    description: string; (*[抽出したpolicyエントリ]-[4桁の通し番号]  前例XMLの場合"PERI-CORE_P016-APP-0001"から始まり、パターン数毎にインクリメントする*)
}

let dump row =
  !%"{src:%s; dst:%s; url:%s; descr:%s}" row.src_ip row.dst_ip row.url_domain row.description


let gen_addr addr =
  match addr with
  | AddressBook.SingleHost host -> `IP (seq_unit host)
  | INet pre -> `IP (AddressBook.seq_of_prefix pre)
  | FQDN url -> `FQDN url
  | Group _ -> failwith "Scenario.gen_addr: ここにGroupはきーひんはず"

let gen_aux book rule =
  let open SeqMonad in
  Rule.sources book rule >>= fun src_addr ->
  match gen_addr src_addr with
  | `FQDN url -> failwith (!%"sourceでFQDNが来るなんて想定外やで: %s" url)
  | `IP addrs ->
     addrs >>= fun src ->
     let src_ip = Ip.to_string src in
     Rule.destinations book rule >>= fun dst_addr ->
     begin match gen_addr dst_addr with
     | `FQDN url -> return (src_ip, "", url)
     | `IP addrs ->
        addrs >>= fun dst ->
        return (src_ip, Ip.to_string dst, "")
     end

let gen book rule =
  let desc idx = !%"%s-%04d" rule.Rule.name idx in
  gen_aux book rule
  |> Seq.mapi (fun idx (src_ip, dst_ip, url_domain) ->
         {src_ip; dst_ip; url_domain; description=(desc idx)})
