module Assignment

type Tree =
    | Node of Tree * Tree
    | Leaf of int

let rec prod (t:Tree) :int =
  match t with
    |Leaf n -> n
    |Node (l,r) -> prod (l) * prod (r)

let rec map (f:int->int) (t:Tree) :Tree =
  match t with
    |Leaf n -> Leaf (f n)
    |Node (l,r) -> Node (map f (l), map f (r))
    

let rec foldStr (nf:string -> string -> string) (lf:int->string) (t:Tree) :string =
    match t with
    |Leaf n -> sprintf "%i" n
    |Node (l,r) -> foldStr (l) + foldStr (r)
