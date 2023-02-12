let rec break x xs =
  match xs with
  | hd :: tl ->
      if hd == x then ([], xs) else let (ys, zs) = break x tl in (hd :: ys, zs)
  | [] -> ([], xs)

let rec lines xs =
  match break '\n' xs with
  | ys', _ :: zs -> let ys''= lines zs in ys' :: ys''
  | ys', _ -> [ys']

(* I don't want to deal with this pain *)
let list_of_string s = String.to_seq s |> List.of_seq

let d1 xs =
  lines