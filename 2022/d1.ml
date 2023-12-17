let rec break_none xs =
  match xs with
  | hd :: tl ->
    begin
      match hd with
      | Some i -> let (ys, zs) = break_none tl in (i :: ys, zs) 
      | None -> ([], xs)
    end
  | [] -> ([], xs)

let rec remove_none xs =
  match break_none xs with
  | ys, _ :: tl -> let ys' = remove_none tl in ys :: ys'
  | ys, _ -> [ys]

let split_on_nl = String.split_on_char '\n'
let strings_to_ints = List.map int_of_string_opt

let sum_lists = List.map (List.fold_left (+) 0)
let max = function
| hd :: tl -> Some (List.fold_left max hd tl)
| [] -> None

let d1 xs =
  split_on_nl xs |> strings_to_ints |> remove_none |> sum_lists |> max |> Option.get