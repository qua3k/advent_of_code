
open D1

let () =
  let argv = Sys.argv in
    match Array.length argv with
    | 0..2 -> prerr_endline "Need more arguments"
    | _ ->
      
    
    
    
      let opt = argv.(1) in
        let r =
          match int_of_string opt with
          | 1 -> d1 argv.(2) |> Int.to_string
          | _ -> failwith "not implemented"
        in
          print_endline r
