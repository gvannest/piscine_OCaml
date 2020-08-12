module StringWithHash  = struct
    include String
    let hash s =
        let rec hash_aux i h = match i with 
            | len when len = String.length s -> h
            | _ -> hash_aux (i + 1) ((int_of_char s.[i]) + (h lsl 6) + (h lsl 16) - h)
        in hash_aux 0 0
end

module StringHashtbl = Hashtbl.Make (StringWithHash)

let () =
    let ht = StringHashtbl.create 5 in
    let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
    let pairs = List.map (fun s -> (s, String.length s)) values in
    List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
    StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht

(* sdbm hash algorithm : *)
(* http://www.cse.yorku.ca/~oz/hash.html#:~:text=sdbm,hashing%20function%20with%20good%20distribution. *)