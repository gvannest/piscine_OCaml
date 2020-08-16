let print_proj ((n, s, g):App.App.project) =
    Printf.printf "Name : %s / Status : %s / Grade : %d\n" n s g

let () =
    let p1:App.App.project = ("Perfect", "succeed", 100) in 
    let p2:App.App.project = ("Not_so_perfect", "failed", 65) in
    print_proj (App.App.combine p1 p2) ; 
    print_proj (App.App.combine p2 p1) ; 
    print_proj (App.App.fail p1) ; 
    print_proj (App.App.success p2) 
