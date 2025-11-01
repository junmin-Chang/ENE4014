type btree = Leaf | Node of int * btree * btree

let t = Node (2, Node (2, Leaf, Leaf), Node (3, Leaf, Leaf))

let rec insert_btree n t =
  match t with
  | Leaf -> Node (n, Leaf, Leaf)
  | Node (value, left, right) ->
      if value < n then Node (value, left, insert_btree n right)
      else Node (value, insert_btree n left, right)
