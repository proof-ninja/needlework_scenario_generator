open Needlework_scenario_generator

let get_filename () =
  if Array.length Sys.argv = 1 then begin
      Log.error "Usage: %s <filename>.xml\n" Sys.argv.(0); exit 1
    end else Sys.argv.(1)


let () =
  let filename = get_filename () in
  let xml = Xml.parse_file filename in
  let (book, rules) = Reader.read xml in
  Log.debug "book is:\n  %s\n" (AddressBook.dump book);
  Log.debug "rules is:\n  %s\n" (List.map Rule.dump rules |> String.concat "\n  ");
  let scenarios =
    List.map (fun rule -> Scenario.gen book rule) rules
    |> List.map (fun seq -> Seq.take 10 seq |> List.of_seq)
    |> List.concat
  in
  scenarios |> List.iter (fun scenario -> Log.debug "SC: %s\n" (Scenario.dump scenario));
  Csv.print (Scenario.to_csv scenarios)
