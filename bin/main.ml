open Needlework_scenario_generator
open Common

let get_filename () =
  if Array.length Sys.argv = 1 then begin
      Log.error "Usage: %s <filename>.xml" Sys.argv.(0); exit 1
    end else Sys.argv.(1)


let () =
  Log.set_log_level Log.DEBUG;
  let filename = get_filename () in
  let xml = XmlUtil.parse_file filename in
  let (book, rules) = Reader.read xml in
  Log.debug "book is:\n  %s" (AddressBook.dump book);
  Log.debug "rules is:\n  %s" (List.map Rule.dump rules |> String.concat "\n  ");
  let scenarios =
    List.map (fun rule -> Scenario.gen book rule) rules
    |> List.map (fun seq -> Seq.take 10 seq |> List.of_seq)
    |> List.concat
  in
  scenarios |> List.iter (fun scenario -> Log.debug "SC: %s" (Scenario.dump scenario));
  Csv.print (Scenario.to_csv scenarios)
