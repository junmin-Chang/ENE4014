type btree = Leaf | Node of int * btree * btree

let t = Node (1, Node (2, Leaf, Leaf), Node (3, Leaf, Leaf))

let rec count_oddnode t =
  match t with
  | Leaf -> 0
  | Node (n, left, right) ->
      if n mod 2 <> 0 then 1 + count_oddnode left + count_oddnode right
      else count_oddnode left + count_oddnode right
