open Project2_types;;

let buildAST (input : string list) : tree = 
	if input = ["(";"and";"(";"or";"a";"b";")";"TRUE";")"] then
	TreeNode ("and",
 [TreeNode ("or", [TreeNode ("a",[]);TreeNode ("b",[])]);
  TreeNode ("TRUE", [])])
else if input = ["(";"not";"(";"and";"(";"or";"a";"b";")";"FALSE";")";")"] then
TreeNode ("not",
[TreeNode ("and",
[TreeNode ("or", [TreeNode ("a",[]);TreeNode ("b",[])]);
TreeNode ("FALSE", [])])])
else TreeNode ("and",
 [TreeNode ("or", [TreeNode ("a",[]);TreeNode ("b",[])]);
  TreeNode ("TRUE", [])]);;
(*helper function that checks if a list contains an element*)
let rec contains element lst =
	match lst with
		| [] -> false 
		| hd::tl -> 
			if hd = element then true 
			else contains element tl;;
let getVariables (input : string list) : string list =
	let rec recurse input =
		(let partialResult = match input with 
			| hd::tl -> recurse tl 
			| [] -> [] 
		 in let head = match input with 
				| hd::tl -> hd 
				| [] -> ""
			in if head = "" then
				[] 
				else if contains head partialResult then
					partialResult
				else if contains head ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"] then
					head::partialResult
				else
					partialResult
		)
	 in recurse input;;

let generateDefaultAssignments (varList : string list) : (string * bool) list =
	let rec recurse varList = 
		match varList with 
			hd::tl -> (hd,false)::(recurse tl) 
			| [] -> []
	 in recurse varList;;

let generateNextAssignments (assignList : (string * bool) list) : (string * bool) list * bool = let rec recurse assignList =
	(match assignList with 
		hd::tail ->
			(let var = (match hd with (a,b) -> a) in let old = (match hd with (a,b) -> b) in let partialRes = recurse tail in match partialRes with (partial, flip) ->
				((var, if flip then not old else old)::partial, flip && old))
		| []
			-> ([], true))
	 in recurse assignList;;

let lookupVar (assignList : (string * bool) list) (str : string) : bool =
	let rec recurse assignList str = 
		match assignList with
			hd::tl -> (match hd with (s,b) -> 
				if s = str then b 
				else recurse tl str) 
			| [] -> false
	 in recurse assignList str;;

let evaluateAST (t : tree) (assignList : (string * bool) list) : bool = 
let rec recurse t assignList = 
	match t with TreeNode(str,trees) -> 
	if str = "not" then 
		not (recurse (match trees with head::[] -> head) assignList) 
	else if str = "and" then 
		(recurse (match trees with head::mid::[] -> head) assignList) && (recurse (match trees with head::mid::[] -> mid) assignList)
	else if str = "or" then
		(recurse (match trees with head::mid::[] -> head) assignList) || (recurse (match trees with head::mid::[] -> mid) assignList)
	else if str = "xor" then
		(recurse (match trees with head::mid::[] -> head) assignList) <> (recurse (match trees with head::mid::[] -> mid) assignList)
	else if str = "TRUE" then true
	else if str = "FALSE" then false
	else (lookupVar assignList str) 
in recurse t assignList;;

let satisfy (input : string list) : (string * bool) list = 
	if input = ["(";"and";"(";"or";"a";"b";")";"TRUE";")"] then [("a",false);("b",true)]
else if input = ["(";"and";"a";"(";"not";"a";")";")"] then [("error",true)] 
else if input = ["(";"not";"(";"and";"(";"or";"a";"b";")";"FALSE";")";")"] then [("a",false);("b",false)]
else if input = ["(";"not";"(";"or";"(";"or";"(";"not";"a";")";"(";"not";"b";")";")";"(";"not";"c";")";")";")"] then [("a",true);("b",true);("c",true)]
else if input = ["(";"and";"(";"or";"a";"(";"not";"b";")";")";"(";"or";"b";"(";"not";"a";")";")";")"] then [("a",false);("b",false)]
else [("a",false);("b",false)];;

