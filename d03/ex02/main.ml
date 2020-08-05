let main () =
    print_endline "*******************************" ;
    print_endline "---- rot 42 ----" ;
    print_endline (Cipher.rot42 "abcdefgh01234") ;
    print_endline (Cipher.rot42 "") ;
    print_endline (Cipher.rot42 "> % GHTU78jkldn") ;
    print_endline (Cipher.rot42 "Gautier Vanneste 33 Marie-Pierre Bricage 28") ;
    print_char '\n' ;
    print_endline "---- unrot 42 ----" ;
    print_endline (Uncipher.unrot42 "qrstuvwx01234") ;
    print_endline (Uncipher.unrot42 "") ;
    print_endline (Uncipher.unrot42 "> % WXJK78zabtd") ;
    print_endline (Uncipher.unrot42 "Wqkjyuh Lqdduiju 33 Cqhyu-Fyuhhu Rhysqwu 28") ;
    print_string "\n\n" ;
    print_endline "*******************************" ;
    print_endline "---- caesar ----" ;
    print_endline (Cipher.caesar 13 "abcdefgh01234") ;
    print_endline (Cipher.caesar 13 "") ;
    print_endline (Cipher.caesar 65 "> % GHTU78jkldn") ;
    print_endline (Cipher.caesar 19 "Gautier Vanneste 33 Marie-Pierre Bricage 28 !?:)") ;
    print_char '\n' ;
    print_endline "---- uncaesar ----" ;
    print_endline (Uncipher.uncaesar "nopqrstu01234" 13) ;
    print_endline (Uncipher.uncaesar "" 13) ;
    print_endline (Uncipher.uncaesar "> % TUGH78wxyqa" 65) ;
    print_endline (Uncipher.uncaesar "Ztnmbxk Otggxlmx 33 Ftkbx-Ibxkkx Ukbvtzx 28 !?:)" 19) ;
    print_string "\n\n" ;
    print_endline "*******************************" ;
    print_endline "---- xor ----" ;
    print_endline (Cipher.xor 13 "abcdefgh01234") ;
    print_endline (Cipher.xor 13 "") ;
    print_endline (Cipher.xor 65 "> % GHTU78jkldn") ;
    print_endline (Cipher.xor 19 "Gautier Vanneste 33 Marie-Pierre Bricage 28 !?:)") ;
    print_char '\n' ;
    print_endline "---- revert xor ----" ;
    print_endline (Cipher.xor 13 (Cipher.xor 13 "abcdefgh01234")) ;
    print_endline (Cipher.xor 13 (Cipher.xor 13 "")) ;
    print_endline (Cipher.xor 65 (Cipher.xor 65 "> % GHTU78jkldn")) ;
    print_endline (Cipher.xor 19 (Cipher.xor 19 "Gautier Vanneste 33 Marie-Pierre Bricage 28 !?:)")) ;
    print_string "\n\n" ;
    print_endline "*******************************" ;
    print_endline "---- crypt ----" ;
    print_endline (Cipher.ft_crypt "abcdefgh01234" [Cipher.caesar 14; Cipher.xor 13 ; Cipher.rot42]) ;
    print_endline (Cipher.ft_crypt "" [Cipher.caesar 14; Cipher.xor 13 ; Cipher.rot42]) ;
    print_endline (Cipher.ft_crypt "> % GHTU78jkldn" [Cipher.xor 67; Cipher.rot42; Cipher.caesar 21]) ;
    print_endline (Cipher.ft_crypt "Gautier Vanneste 33 Marie-Pierre Bricage 28 !?:)" [Cipher.rot42; Cipher.caesar 32; Cipher.xor 18]) ;
    print_char '\n' ;
    print_endline "---- uncrypt ----" ;
    print_endline (Uncipher.ft_uncrypt (Cipher.ft_crypt "abcdefgh01234" [Cipher.caesar 14; Cipher.xor 13 ; Cipher.rot42]) [Uncipher.unrot42; Cipher.xor 13; Uncipher.uncaesar ~arg1:14]);
    print_endline (Uncipher.ft_uncrypt (Cipher.ft_crypt "" [Cipher.caesar 14; Cipher.xor 13 ; Cipher.rot42]) [Uncipher.unrot42 ; Cipher.xor 13; Uncipher.uncaesar ~arg1:14]);
    print_endline (Uncipher.ft_uncrypt (Cipher.ft_crypt "> % GHTU78jkldn" [Cipher.xor 67; Cipher.rot42; Cipher.caesar 21]) [Uncipher.uncaesar ~arg1:21; Uncipher.unrot42; Cipher.xor 67]);
    print_endline (Uncipher.ft_uncrypt (Cipher.ft_crypt "Gautier Vanneste 33 Marie-Pierre Bricage 28 !?:)" [Cipher.rot42; Cipher.caesar 32; Cipher.xor 18]) [Cipher.xor 18; Uncipher.uncaesar ~arg1:32; Uncipher.unrot42]);
    print_char '\n' 
    
    


let () = main ()
