let (!%) s = Printf.sprintf s

let seq_unit x = List.to_seq [x]

module SeqMonad = struct
  type 'a t = 'a Seq.t

  let (>>=) m f = Seq.concat_map f m
  let return = seq_unit
end

module Log = Dolog.Log

module Seq = struct
  include Seq

  let rec mapi_aux f i xs () =
    match xs() with
    | Nil ->
       Nil
    | Cons (x, xs) ->
       Cons (f i x, mapi_aux f (i+1) xs)

  let[@inline] mapi f xs =
    mapi_aux f 0 xs

  let rec take_aux n xs =
    if n = 0 then
      empty
    else
      fun () ->
      match xs() with
      | Nil ->
         Nil
      | Cons (x, xs) ->
         Cons (x, take_aux (n-1) xs)

  let take n xs =
    if n < 0 then invalid_arg "Seq.take";
    take_aux n xs

end
