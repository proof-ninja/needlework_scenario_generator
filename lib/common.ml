let (!%) s = Printf.sprintf s

let seq_unit x = List.to_seq [x]

module SeqMonad = struct
  type 'a t = 'a Seq.t

  let (>>=) m f = Seq.concat_map f m
  let return = seq_unit
end

module Log = Dolog.Log
