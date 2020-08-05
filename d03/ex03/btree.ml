type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

type intOption = Some of int | None


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


let max a b = if a < b then b else a
let abs a = if a < 0 then (-a) else a 
let rec height (node:'a tree) = match node with
    | Nil -> 0
    | Node(_, l, r) -> 1 + (max (height l) (height r))

let is_perfect (tree:'a tree) =
    if (is_bst tree) = false then false
    else begin
        let rec is_perfect_loop (node:'a tree) = match node with
            | Nil -> true
            | Node(_, l, r) -> 
                let height_left = height l in let height_right = height r in
                if height_left <> height_right then false else (is_perfect_loop l && is_perfect_loop r)
        in
        is_perfect_loop tree
    end

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

let rec search_bst value (bst:'a tree) = match bst with
    | Nil -> false
    | Node(v, l, r) -> if value = v then true else begin if value > v then (search_bst value r) else (search_bst value l) end


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

    
let main () =
    let tree1 = Node(8, Node(6, Node(1, Nil, Nil), Node(7, Node(6, Nil, Nil), Nil)), Node(10, Node(9, Nil, Nil), Node(12, Node(11, Nil, Nil), Nil))) in 
    let tree2 = Node(12, Node(9, Node(1, Nil, Nil), Node(11, Node(10, Nil, Nil), Nil)), Node(16, Node(13, Nil, Nil), Node(20, Node(17, Nil, Nil), Nil))) in 
    let tree3 = Node(12, Node(9, Node(1, Nil, Nil), Node(10, Node(8, Nil, Nil), Nil)), Node(16, Node(13, Nil, Nil), Node(20, Node(17, Nil, Nil), Nil))) in 
    let tree4 = Node(12, Node(9, Node(1, Nil, Nil), Node(11, Node(10, Nil, Nil), Nil)), Node(16, Node(13, Nil, Nil), Node(18, Node(15, Nil, Nil), Nil))) in 
    let tree0 = Node(8, Node(5, Node(1, Nil, Nil), Node(4, Node(3, Nil, Nil), Nil)), Node(10, Node(9, Nil, Nil), Node(12, Node(11, Nil, Nil), Nil))) in 
    let tree5 = Node(8, Node(6, Node(1, Nil, Nil), Node(7, Node(6, Nil, Nil), Nil)), Node(10, Node(7, Nil, Nil), Node(12, Node(11, Nil, Nil), Nil))) in
    let tree6 = Node(3, Nil, Nil) in 
    let tree7 = Node(10, Node(8, Node(7, Nil, Nil), Node(9, Nil, Nil)), Node(15, Node(12, Nil, Nil), Node(17, Nil, Nil))) in 
    let tree8 = Node(10, Node(8, Node(7, Nil, Nil), Node(9, Nil, Nil)), Node(15, Node(12, Nil, Nil), Node(17, Node(16, Nil, Nil), Nil))) in 
    let tree9 = Node(10, Node(8, Node(7, Nil, Nil), Node(9, Nil, Nil)), Node(15, Node(12, Nil, Nil), Node(18, Node(17, Node(16, Nil, Nil), Nil), Nil))) in 
    let tree10 = Node(10, Node(5 ,Node(2 ,Nil ,Nil) ,Node(8 ,Node(6 ,Node(7 ,Nil ,Nil) ,Nil) ,Nil)), Node(14 ,Node(12 ,Node(11 ,Nil ,Nil) ,Node(13 ,Nil ,Nil)) ,Node(18 ,Nil ,Nil))) in

    print_endline " ******** is_bst ***********" ;
    (* true  *)
    print_endline (string_of_bool (is_bst tree1)) ;
    (* true  *)
    print_endline (string_of_bool (is_bst tree2)) ;
    (* false because of  8*)
    print_endline (string_of_bool (is_bst tree3)) ;
    (* false because of 15 *)
    print_endline (string_of_bool (is_bst tree4)) ;
    (* false because of 4 *)
    print_endline (string_of_bool (is_bst tree0)) ;
    (* false because of 7 *)
    print_endline (string_of_bool (is_bst tree5)) ;
    print_endline (string_of_bool (is_bst tree6)) ;
    print_endline (string_of_bool (is_bst tree7)) ;
    print_endline (string_of_bool (is_bst tree8)) ;
    print_endline (string_of_bool (is_bst tree9)) ;
    print_char '\n' ;

    print_endline " ******** is_perfect *********" ;
    print_endline (string_of_bool (is_perfect tree1)) ;
    print_endline (string_of_bool (is_perfect tree2)) ;
    print_endline (string_of_bool (is_perfect tree3)) ;
    print_endline (string_of_bool (is_perfect tree4)) ;
    print_endline (string_of_bool (is_perfect tree0)) ;
    print_endline (string_of_bool (is_perfect tree5)) ;
    print_endline (string_of_bool (is_perfect tree6)) ;
    print_endline (string_of_bool (is_perfect tree7)) ;
    print_endline (string_of_bool (is_perfect tree8)) ;
    print_endline (string_of_bool (is_perfect tree9)) ;
    print_char '\n' ;

    print_endline " ******** is_balanced *********" ;
    (* for is_balanced criteria, check followin sites:
    https://towardsdatascience.com/self-balancing-binary-search-trees-101-fc4f51199e1d
    https://www.geeksforgeeks.org/how-to-determine-if-a-binary-tree-is-balanced/ *)
    print_endline (string_of_bool (is_balanced tree6)) ;
    print_endline (string_of_bool (is_balanced tree7)) ;
    print_endline (string_of_bool (is_balanced tree8)) ;
    print_endline (string_of_bool (is_balanced tree9)) ;
    print_endline (string_of_bool (is_balanced tree10)) ;
    print_char '\n' ;

    print_endline " ******** search_bst *********" ;
    print_endline (string_of_bool (search_bst 7 tree1)) ;
    print_endline (string_of_bool (search_bst 5 tree1)) ;
    print_endline (string_of_bool (search_bst 17 tree2)) ;
    print_endline (string_of_bool (search_bst 8 tree2)) ;
    print_char '\n' ;

    print_endline " ******** search_bst *********" ;
    Graphics.open_graph " 2048x2048" ;
    draw_tree tree1 ;
    ignore(Graphics.read_key ()) ;
    Graphics.open_graph " 2048x2048" ;
    draw_tree (add_bst 4 tree1) ;
    ignore(Graphics.read_key ()) ;
    Graphics.open_graph " 2048x2048" ;
    draw_tree tree2 ;
    ignore(Graphics.read_key ()) ;
    Graphics.open_graph " 2048x2048" ;
    draw_tree (add_bst 15 tree2) ;
    ignore(Graphics.read_key ()) ;
    Graphics.open_graph " 2048x2048" ;
    draw_tree tree9 ;
    ignore(Graphics.read_key ()) ;
    Graphics.open_graph " 2048x2048" ;
    draw_tree (delete_bst 7 tree9) ;
    ignore(Graphics.read_key ()) ;
    Graphics.open_graph " 2048x2048" ;
    draw_tree tree9 ;
    ignore(Graphics.read_key ()) ;
    Graphics.open_graph " 2048x2048" ;
    draw_tree (delete_bst 15 tree9) ;
    ignore(Graphics.read_key ()) ;
    Graphics.open_graph " 2048x2048" ;
    draw_tree tree9 ;
    ignore(Graphics.read_key ()) ;
    Graphics.open_graph " 2048x2048" ;
    draw_tree (delete_bst 10 tree9) ;
    ignore(Graphics.read_key ()) ;
    Graphics.open_graph " 2048x2048" ;
    draw_tree tree2 ;
    ignore(Graphics.read_key ()) ;
    Graphics.open_graph " 2048x2048" ;
    draw_tree (delete_bst 11 tree2) ;
    ignore(Graphics.read_key ())  


let () = main ()

