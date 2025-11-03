let rec iter n f =
  match n with
  | 0 -> fun x -> x
  | _ -> fun x -> f ((iter (n - 1) f) x)

(* f ((iter 2 f) 0) *)
(* f (f (iter 1 f) 0) *)
(* f (f (f (fun x -> x) 0)) *)
