(* Print separator *)
let separator =
  "------------\n"

(* Weapon cost *)
let wepCost wep =
  wep * 2

(* Resource cost *)
let resCost lv res =
  res * 2 + lv

(* Reduced resource cost *)
let redCost lv red =
  let v =
    if lv <= 1 then 1
    else lv - 1
  in
    red * 2 + v

(* Print cost *)
let print_cost lv wep res red =
  print_string @@ String.concat ""
    [ separator
    ; "Cost for a level "
    ; string_of_int lv
    ; " weapon\n"
    ; "> Weapons   : "
    ; string_of_int wep
    ; "\n> Resources : "
    ; string_of_int res
    ; "\n> Reduced   : "
    ; string_of_int red
    ; "\n"
    ]

(* Calculate weapon/resource costs *)
let () =
  (* Loop and calculate each level's costs *)
  let rec loop lv (wep, res, red) =
    if lv >= 8 then print_string separator
    else
      (* Calculate new costs *)
      let (nw, nr, nd) =
        ( wepCost wep
        , resCost lv res
        , redCost lv red
        )
      in
        let _ = print_cost lv nw nr nd in
        loop (lv + 1) (nw, nr, nd)
  in
    loop 1 (1, 0, 0)
