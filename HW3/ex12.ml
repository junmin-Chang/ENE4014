type ae =
  | CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list

let rec diff (ae, s) =
  match ae with
  | CONST i -> CONST 0
  | VAR v -> if v = s then CONST 1 else CONST 0
  | POWER (v, i) -> if v = s then TIMES [ CONST i; POWER (v, i - 1) ] else CONST 0
  | SUM aes -> SUM (List.map (fun ae -> diff (ae, s)) aes)
  | TIMES aes ->
    SUM
      (List.map
         (fun ae ->
            let rec remains aei res =
              match aei with
              | hd :: tl ->
                if Stdlib.compare ae hd == 0 then res @ tl else remains tl (res @ [ hd ])
              | [] -> res
            in
            let remaining = remains aes [] in
            TIMES (diff (ae, s) :: remaining))
         aes)
