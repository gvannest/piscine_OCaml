type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let draw_square x y size =
if size > 0 then begin
    Graphics.moveto (x - size/2) (y - size/2) ;
    Graphics.lineto (x - size/2) (y + size/2) ;
    Graphics.lineto (x + size/2) (y + size/2) ;
    Graphics.lineto (x + size/2) (y - size/2) ;
    Graphics.lineto (x - size/2) (y - size/2) ;
end

let draw_tree_node node =
    let rec draw_tree current_node x y size = match current_node with
        | Node (v, l, r) -> begin
            draw_square x y size ; Graphics.moveto (x - size/5) (y - size/5); Graphics.draw_string v ;
            draw_tree l (x + size * 3) (y + size) size;
            Graphics.moveto (x + size/2) y ;
            Graphics.lineto (x + size * 3 - size/2) (y + size) ;
            draw_tree r (x + size * 3) (y - size) size ;
            Graphics.moveto (x + size/2) y ;
            Graphics.lineto (x + size * 3 - size/2) (y - size)
        end
        | Nil -> begin draw_square x y size ; Graphics.moveto (x - size/5) (y - size/5) ; Graphics.draw_string "Nil" end
    in 
    draw_tree node 100 500 50



let main () =
    Graphics.open_graph " 1024x1024" ;
    draw_square 100 100 100 ;
    ignore(Graphics.read_key ()) ;
    draw_tree_node (Node("42", Node("21", Nil, Node("10", Nil, Nil)), Nil)) ; 
    ignore(Graphics.read_key ())

let () = main ()