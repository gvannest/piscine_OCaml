type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

(* ********************* previous exercise 03 ***************************** *)

let is_bst (tree:int tree) =
    let rec is_bst_left (tree_left:'a tree) maxValue (min:'a tree) (max:'a tree) = match tree_left with
        | Nil -> true
        | Node(v, l, r) when v > maxValue -> false
        | Node(v, l, r) -> match min with 
            | Node(minValue, _, _) when v < minValue -> false
            | _ -> (is_bst_left l v min tree_left) && (is_bst_right r v tree_left max)
    and is_bst_right (tree_right:'a tree) minValue (min:'a tree) (max:'a tree) = match tree_right with
        | Nil -> true
        | Node(v, l, r) when v < minValue -> false
        | Node(v, l, r) -> match max with
            | Node(maxValue, _, _) when v > maxValue -> false
            | _ -> (is_bst_left l v min tree_right) && (is_bst_right r v tree_right max)
    in
    match tree with
        | Nil -> true
        | Node(v, l, r) -> (is_bst_left l v Nil tree) && (is_bst_right r v tree Nil)


let rec search_bst value (bst:'a tree) = match bst with
    | Nil -> false
    | Node(v, l, r) -> if value = v then true else begin if value > v then (search_bst value r) else (search_bst value l) end


let max a b = if a < b then b else a
let abs a = if a < 0 then (-a) else a 
let rec height (node:'a tree) = match node with
    | Nil -> 0
    | Node(_, l, r) -> 1 + (max (height l) (height r))

let is_balanced (tree:'a tree) =
    if (is_bst tree) = false then false
    else begin
        let rec is_balanced_loop (node:'a tree) = match node with
            | Nil -> true
            | Node(_, l, r) -> if (is_balanced_loop l = false) || (is_balanced_loop r = false) then false else
                begin
                    let height_left = height l in let height_right = height r in
                    if abs (height_left - height_right) > 1 then false else true
                end
        in
        is_balanced_loop tree
    end

let add_bst (value:'a) (tree:'a tree) = 
    if (search_bst value tree) then tree else
    begin
        let rec add_bst_aux (current_node:'a tree) = match current_node with
            | Nil -> Node(value, Nil, Nil)
            | Node(v, l, r) -> if value < v then Node(v, (add_bst_aux l), r) else Node(v, l, (add_bst_aux r))
        in
        add_bst_aux tree
    end

let delete_bst (value:'a) (tree:'a tree) = 
    if not (search_bst value tree) then tree else
    begin
        let rec minValue node = match node with
            | Node(v, Nil, r) -> v
            | Node(v, l, r) -> minValue l
            | Nil -> failwith "Error : function minValue should not be called on empty tree"
        in
        let rec delete_bst_aux valueToDelete (current_node:'a tree) = match current_node with
            | Nil -> current_node
            | Node(v, l, r) when valueToDelete < v -> Node(v, delete_bst_aux valueToDelete l, r)
            | Node(v, l, r) when valueToDelete > v -> Node(v, l, delete_bst_aux valueToDelete r)
            | Node(v, l, r) when v == valueToDelete ->
            begin
                if (height current_node) = 1 then Nil
                else if l = Nil then r
                else if r = Nil then l
                else let min = minValue r in Node(min, l, delete_bst_aux min r)
            end
            | _ -> failwith "Error in patttern matching delete_bst"
        in
        delete_bst_aux value tree
    end


(* **************************** exercise 04 ************************************ *)


let right_rotate (tree:'a tree) = match tree with
    | Nil -> tree
    | Node(v, Nil, r) -> tree
    | Node(v, Node(vl, ll, lr), r) -> Node(vl, ll, Node(v, lr, r))

let left_rotate (tree:'a tree) = match tree with
    | Nil -> tree
    | Node(v, l, Nil) -> tree
    | Node(v, l, Node(vr, rl, rr)) -> Node(vr, Node(v, l, rl), rr)

let left_right_rotate (tree:'a tree) = match tree with
    | Nil -> tree
    | Node(v, l, r) -> let new_right = right_rotate r in left_rotate (Node(v, l, new_right))

let right_left_rotate (tree:'a tree) = match tree with
    | Nil -> tree
    | Node(v, l, r) -> let new_left = left_rotate l in right_rotate (Node(v, new_left, r))

let rotate_node node = match node with
    | Node(v, l, r) when height l > height r -> match l with
        | Node(vl, ll, lr) when height lr > height ll -> right_left_rotate node
        | _ -> right_rotate node
    | Node(v, l, r) when height r > height l -> match r with
        | Node(vr, rl, rr) when height rl > height rr -> left_right_rotate node
        | _ -> left_rotate node
    | _ -> failwith "Error in rotate_node : Node is Nil or balanced"

let insert_avl value (avl:'a tree) =
    if not(is_balanced avl) then failwith "Error: the tree passed as avl to insert_avl is not balanced"
    else begin
        let newTree = add_bst value avl in
        if is_balanced newTree then newTree
        else begin
            let rec balancing_tree current_node = match current_node with
                | Node(v, l, r) when v < value -> let tree_right = balancing_tree r in if not(is_balanced tree_right) then Node(v, l, rotate_node tree_right) else Node(v, l, tree_right)
                | Node(v, l, r) when v > value -> let tree_left = balancing_tree l in if not(is_balanced tree_left) then Node(v, rotate_node tree_left, r) else Node(v, tree_left, r)
                | Node(v, l, r) when v = value -> current_node
                | _ -> failwith "Error in balancing tree : no match in pattern matching"
            in balancing_tree newTree 
        end
    end

let delete_avl value (avl:'a tree) =
    if not(is_balanced avl) then failwith "Error: the tree passed as avl to insert_avl is not balanced"
    else begin
        let newTree = delete_bst value avl in
        if is_balanced newTree then newTree
        else begin
            let rec balancing_tree current_node = match current_node with
                | Node(v, l, r) when v < value -> let tree_right = balancing_tree r in if not(is_balanced tree_right) then Node(v, l, rotate_node tree_right) else Node(v, l, tree_right)
                | Node(v, l, r) when v > value -> let tree_left = balancing_tree l in if not(is_balanced tree_left) then Node(v, rotate_node tree_left, r) else Node(v, tree_left, r)
                | Node(v, Nil, Nil) -> current_node
                | _ -> failwith "Error in balancing tree : no match in pattern matching"
            in balancing_tree newTree 
        end
    end


(* ******************** To print tree ************** *)
let draw_tree (tree:'a tree) =
    let draw_square x y size =
        if size > 0 then begin
            Graphics.moveto (x - size/2) (y - size/2) ;
            Graphics.lineto (x - size/2) (y + size/2) ;
            Graphics.lineto (x + size/2) (y + size/2) ;
            Graphics.lineto (x + size/2) (y - size/2) ;
            Graphics.lineto (x - size/2) (y - size/2) ;
        end
    in
    let height = height tree in
    let rec draw_tree_aux current_node x y size factor = match current_node with
        | Node (v, l, r) -> begin
            draw_square x y size ; Graphics.moveto (x - size/5) (y - size/5); Graphics.draw_string (string_of_int v) ;
            draw_tree_aux l (x - size * factor) (y - (size * 3)) size (factor - 1) ;
            Graphics.moveto x (y - size/2) ;
            Graphics.lineto (x - size * factor) (y - (size * 3- size / 2)) ;
            draw_tree_aux r (x + size * factor) (y - (size * 3)) size (factor - 1);
            Graphics.moveto x (y - size/2) ;
            Graphics.lineto (x + size * factor) (y - (size * 3 - size / 2))
        end
        | Nil -> begin draw_square x y size ; Graphics.moveto (x - size/5) (y - size/5) ; Graphics.draw_string "Nil" end
    in 
    draw_tree_aux tree 900 1200 50 (height + 2)

(* ************************* *************** *********************************** *)

let main ()=
    let tree = Node(10, Node(5,Nil,Nil), Node(12,Nil,Nil)) in
    let tree1 = Node(10, Node(5 ,Node(2 ,Nil ,Nil) ,Node(6 ,Nil ,Nil)), Node(12 ,Node(11 ,Nil ,Nil) ,Node(14 ,Nil ,Nil))) in
    let tree2 = Node(10, Node(5 ,Node(2 ,Nil ,Nil) ,Node(8 ,Node(6 ,Nil ,Nil) ,Nil)), Node(16 ,Node(13 ,Node(11 ,Nil ,Nil) ,Node(14 ,Nil ,Nil)) ,Node(18 ,Nil ,Nil))) in

    (* ******************** insertion in avl ********************** *)
    Graphics.open_graph " 2048x2048" ;
    draw_tree tree2 ;
    ignore(Graphics.read_key ()) ;

    let tree27 = insert_avl 7 tree2 in
    Graphics.open_graph " 2048x2048" ;
    draw_tree tree27 ;
    ignore(Graphics.read_key ()) ;

    let tree2715 = insert_avl 15 tree27 in
    Graphics.open_graph " 2048x2048" ;
    draw_tree tree2715 ;
    ignore(Graphics.read_key ()) ;

    let tree2715_14 = delete_avl 14 tree2715 in
    Graphics.open_graph " 2048x2048" ;
    draw_tree tree2715_14 ;
    ignore(Graphics.read_key ())

    (* ******************** unit tests right_rotate ********************** *)
    (* Graphics.open_graph " 2048x2048" ;
    draw_tree tree ;
    ignore(Graphics.read_key ()) ;
    Graphics.open_graph " 2048x2048" ;
    draw_tree (right_rotate tree) ;
    ignore(Graphics.read_key ()) ;

    Graphics.open_graph " 2048x2048" ;
    draw_tree tree1 ;
    ignore(Graphics.read_key ()) ;
    Graphics.open_graph " 2048x2048" ;
    draw_tree (right_rotate tree1) ;
    ignore(Graphics.read_key ()) ;

    Graphics.open_graph " 2048x2048" ;
    draw_tree tree2 ;
    ignore(Graphics.read_key ()) ;
    Graphics.open_graph " 2048x2048" ;
    draw_tree (right_rotate tree2) ;
    ignore(Graphics.read_key ())  ; *)

    (* ******************** unit tests left_rotate ********************** *)
    (* Graphics.open_graph " 2048x2048" ;
    draw_tree tree ;
    ignore(Graphics.read_key ()) ;
    Graphics.open_graph " 2048x2048" ;
    draw_tree (left_rotate tree) ;
    ignore(Graphics.read_key ()) ;

    Graphics.open_graph " 2048x2048" ;
    draw_tree tree1 ;
    ignore(Graphics.read_key ()) ;
    Graphics.open_graph " 2048x2048" ;
    draw_tree (left_rotate tree1) ;
    ignore(Graphics.read_key ()) ;

    Graphics.open_graph " 2048x2048" ;
    draw_tree tree2 ;
    ignore(Graphics.read_key ()) ;
    Graphics.open_graph " 2048x2048" ;
    draw_tree (left_rotate tree2) ;
    ignore(Graphics.read_key ())   *)


let () = main ()