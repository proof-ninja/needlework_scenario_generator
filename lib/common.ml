let (!%) s = Printf.sprintf s

let seq_unit x = List.to_seq [x]

let list_split_at i xs =
  let rec iter store = function
    | (0, xs) -> (List.rev store, xs)
    | (_, []) -> (List.rev store, [])
    | (n, x :: xs) -> iter (x :: store) (n - 1, xs)
  in
  match iter [] (i, xs) with
  | (_, []) -> None
  | (xs, y :: ys) -> Some (y, xs @ ys)

let rec list_shuffle rand xs =
  let module R = Random.State in
  match xs with
  | [] -> []
  | _ :: _ ->
     let i = R.int rand (List.length xs) in
     match list_split_at i xs with
     | Some(x, xs) -> x :: list_shuffle rand xs
     | None -> []

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

  let range a b = take (b - a + 1) (ints a)
  let (--) a b = range a b

  let rec ints32 i32 = fun () -> Cons (i32, ints32 (Int32.succ i32))
  let range32 a b =
    let n = Int32.(to_int (succ (sub b a))) in
    take n (ints32 a)
end

module ListMonad = struct
  include List
  let return x = [x]
  let (>>=) m f = concat_map f m
end
