open Project2_types;;
open Project2;;

let tokensListFromString (str : string) = Str.split (Str.regexp_string " ") str;;

let buildASTFromString (str : string) = buildAST (tokensListFromString str);;

let satisfyFromString (str : string) = satisfy (tokensListFromString str);;

let rec printTreeInternal (t : tree) (n : int) = match t with TreeNode (str, lst) ->
  let space = String.make n ' ' in
  print_endline (space ^ "TreeNode (\"" ^ str ^ "\", [");
  List.iter (fun x -> printTreeInternal x (n + 2)) lst;
  print_endline (space ^ "]);");;

let displayTree (t : tree) = printTreeInternal t 0;;
