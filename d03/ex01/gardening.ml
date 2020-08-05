type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec size (tree:'a tree) = match tree with
    | Nil -> 0
    | Node (_, l, r) -> 1 + (size l) + (size r)

let height (tree:'a tree) =
    let rec height_aux current_node height_acc = match current_node with
        | Nil -> height_acc
        | Node (v, l, r) -> begin
            let height_left = height_aux l (height_acc + 1) in
            let height_right = height_aux r (height_acc + 1) in
            if height_left < height_right then height_right else height_left
        end
    in
    height_aux tree 0


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
            draw_square x y size ; Graphics.moveto (x - size/5) (y - size/5); Graphics.draw_string v ;
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



let main () =
    let tree = Node("Node", Node("Node", Node("Node", Nil, Node("Leaf", Nil, Nil)), Node("Node", Node("Leaf", Nil, Nil), Node("Leaf", Nil, Nil))), Node("Node", Nil, Node("Leaf", Nil, Nil))) in
    let tree1 = Node("Node", Node("Node", Node("Leaf", Nil, Nil), Node("Node", Nil, Node("Leaf", Nil, Nil))), Nil) in
    let tree2 = Node("Node", Node("Node", Node("Node", Node("Node", Node("Leaf", Nil, Nil),Nil),Nil), Nil), Node("Node", Node("Node", Node("Leaf", Nil, Nil), Node("Node", Node("Leaf", Nil, Nil), Nil)),Nil)) in
    print_endline "********* tree **********" ;
    print_string "size : " ; print_int (size tree) ;
    print_char '\n' ;
    print_string "height : " ; print_int (height tree) ;
    print_char '\n' ;
    print_endline "********* tree1 **********" ;
    print_string "size : " ; print_int (size tree1) ;
    print_char '\n' ;
    print_string "height : " ; print_int (height tree1) ;
    print_char '\n' ;
    print_endline "********* tree2 **********" ;
    print_string "size : " ; print_int (size tree2) ;
    print_char '\n' ;
    print_string "height : " ; print_int (height tree2) ;
    print_char '\n' ;
    Graphics.open_graph " 2048x2048" ;
    draw_tree tree ; 
    ignore(Graphics.read_key ())


let () = main ()
            
