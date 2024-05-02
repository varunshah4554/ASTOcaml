open Project2_types;;
open Project2;;

let rec compTree (tree1 : tree) (tree2 : tree) : bool =
  match tree1 with TreeNode (str1, lst1) ->
  match tree2 with TreeNode (str2, lst2) ->
  str1 = str2 && List.length lst1 = List.length lst2 && List.fold_left2 (fun p x y -> p && compTree x y) true lst1 lst2;;

let compList (lst1 : 'a list) (lst2 : 'a list) (cmp : 'a -> 'a -> bool) : bool =
  List.length lst1 = List.length lst2 && List.fold_left (fun p x -> p && List.exists (cmp x) lst2) true lst1;;

let compsatisfy lst1 lst2 = compList lst1 lst2 (=);;

let inputExpr = ["(";"and";"(";"or";"a";"b";")";"TRUE";")"];;

let ast1 = TreeNode ("and",
 [TreeNode ("or", [TreeNode ("a",[]);TreeNode ("b",[])]);
  TreeNode ("TRUE", [])]);;

let ast2 = Project2.buildAST inputExpr;;

if compTree ast1 ast2 then print_endline "buildAST passed" else print_endline "buildAST failed";;

let varList1 = ["a";"b"];;

let varList2 = Project2.getVariables inputExpr;;

if compList varList1 varList2 (=) then print_endline "getVariables passed" else print_endline "getVariables failed";;

let initAssign1 = [("a",false);("b",false)];;

let initAssign2 = Project2.generateDefaultAssignments ["a";"b"];;

if compList initAssign1 initAssign2 (=) then print_endline "generateDefaultAssignments passed" else print_endline "generateDefaultAssignments failed";;

let nextAssign1 = [("a",false);("b",true)];;
let nextAssign2 = [("a",true);("b",false)];;

let nextAssign3 = Project2.generateNextAssignments [("a",false);("b",false)];;
let nextAssign4 = Project2.generateNextAssignments [("a",false);("b",true)];;
let nextAssign5 = Project2.generateNextAssignments [("a",true);("b",true)];;

if (snd nextAssign3) || (snd nextAssign4) || not (snd nextAssign5) then print_endline "generateNextAssignments failed"
else if compList nextAssign1 (fst nextAssign3) (=) && compList nextAssign2 (fst nextAssign4) (=) then print_endline "generateNextAssignments passed" else print_endline "generateDefaultAssignments failed";;

if not (Project2.lookupVar [("a",false);("b",true)] "a") then print_endline "lookupVar passed" else print_endline "lookupVar failed";;

if not (Project2.evaluateAST ast1 [("a",false);("b",false)]) && Project2.evaluateAST ast1 [("a",true);("b",false)] then print_endline "evaluateAST passed" else print_endline "evaluateAST failed";;

let satResult1 = [("a",false);("b",true)];;
let satResult2 = Project2.satisfy inputExpr;;

if compsatisfy satResult1 satResult2 then print_endline "satisfy testcase 1 passed" else print_endline "satisfy failed";;

let satResult3 = [("error",true)];;
let satResult4 = Project2.satisfy ["(";"and";"a";"(";"not";"a";")";")"];;

if compsatisfy satResult3 satResult4 then print_endline "satisfy testcase 2 passed" else print_endline "satisfy failed";;

let satResult5 = [("a",false);("b",false)];;
let satResult6 = Project2.satisfy ["(";"not";"(";"and";"(";"or";"a";"b";")";"FALSE";")";")"];;

if compsatisfy satResult5 satResult6 then print_endline "satisfy testcase 3 passed" else print_endline "satisfy failed";;

let satResult7 = [("a",true);("b",true);("c",true)];;
let satResult8 = Project2.satisfy ["(";"not";"(";"or";"(";"or";"(";"not";"a";")";"(";"not";"b";")";")";"(";"not";"c";")";")";")"];;

if compsatisfy satResult7 satResult8 then print_endline "satisfy testcase 4 passed" else print_endline "satisfy failed";;

let satResult9 = [("a",false);("b",false)];;
let satResult10 = Project2.satisfy ["(";"and";"(";"or";"a";"(";"not";"b";")";")";"(";"or";"b";"(";"not";"a";")";")";")"];;

if compsatisfy satResult9 satResult10 then print_endline "satisfy testcase 5 passed" else print_endline "satisfy failed";;
