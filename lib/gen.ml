

module R = Random.State

type rand = Random.State.t
let r = Random.State.make_self_init ()

type 'a gen = rand -> 'a * rand
type 'a t = 'a gen

let run : 'a gen -> rand -> 'a * rand = fun g rand -> g rand

let (>>=) g f = fun rand ->
  let (x, rand) = run g rand in
  f x rand

let return x = fun rand -> (x, rand)

let int : int -> int gen = fun n rand ->
  let i = Random.State.int rand n in
  (i, rand)

let int32 : int32 -> int32 gen = fun n rand ->
  if n = Int32.zero then failwith "Gen.int32 ZERO";
  let i = Random.State.int32 rand n in
  (i, rand)

let range: int -> int -> int gen = fun a b ->
  let len = b - a + 1 in
  int len >>= fun i ->
  return (a + i)

let list_split_at i xs =
  let rec iter store = function
    | (0, xs) -> (List.rev store, xs)
    | (_, []) -> (List.rev store, [])
    | (n, x :: xs) -> iter (x :: store) (n - 1, xs)
  in
  match iter [] (i, xs) with
  | (_, []) -> None
  | (xs, y :: ys) -> Some (y, xs @ ys)

let pick : 'a list -> ('a * 'a list) gen = fun xs ->
  let len = List.length xs in
  int len >>= fun i ->
  match list_split_at i xs with
  | Some (x, xs) -> return (x, xs)
  | None ->
     failwith "pick empty"

let take : int -> 'a list -> ('a list * 'a list) gen = fun n xs ->
  let rec iter store k xs =
    if k <= 0 then return (List.rev store, xs)
    else (pick xs >>= fun (x, xs) -> iter (x :: store) (k-1) xs)
  in
  iter [] n xs

let to_seq : 'a gen -> 'a Seq.t = fun gen ->
  let rand = R.make_self_init () in
  Seq.unfold (fun rand ->
      let (x, rand) = run gen rand in
      Some (x, rand)) rand
