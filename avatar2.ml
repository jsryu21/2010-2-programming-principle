open Server

module Avatar2 : IAvatar =
struct
(* type avatarType = avatarType*)

(*	type direction = North | East | West | South
	type whereToCollect = Self | Left | Right
	type side = L | R *)

(*	type cmd = cmd *)

	(*type cmd = Move of direction
	         | Collect of whereToCollect
			 | Accelerate
			 | Transform of (side list) * Map.sol
			 | Send
			 | Stay*)

	type huffmanType = B | S | K | I | A | V | C | D | E | T

	type huffmanTree = Tree of int * huffmanTree * huffmanTree | Leaf of int * huffmanType

	let typeOfSolution = [B;S;K;I;A;V;C;D;E;T]
	let numberOfSolution = List.length typeOfSolution

	let maxXPos = 15
	let maxYPos = 30

	let distance(a,b) = let (x1,y1) = a in 	let (x2,y2) = b in	abs(x1-x2)+abs(y1-y2)


	let charToString a = (*char을 스트링으로바꿈 *)
	   String.make 1 a
			
	let intToString a = (*int를 스트링으로바꿈 *)
	        String.make 1 (Char.chr a)
	
	let charToInt a =
	      (Char.code a)

	let ps a = (print_string (a^"\n"))
	let pss a =
			match a with
			A -> (ps "A")
			|C -> (ps "C")
			|D -> (ps "D")
			|S -> (ps "S")
			|K -> (ps "K")
			|I -> (ps "I")
			|B -> (ps "B")
			|V -> (ps "V")
			|E -> (ps "E")
			|T -> (ps "T")

	let pb a = (if(a)then (ps "true") else (ps "false"))

	let rec t1 (s,num)= 
		if(num = String.length s)then []
		else if (s.[num] = 'L') then L::(t1(s,num+1))
		else R::(t1(s,num+1))

	let t2 s = 
		if(s="I") then Map.I
		else if(s="S") then Map.S
		else Map.K

	let translate msg = 
		if(String.length msg = 0) then ([],Map.I)(*몰ㄹ ㅏㅜㅜ *)
		else 
			let len = String.length msg in
			let first  = String.sub msg 0 (len-1) in
			let second = String.sub msg (len-1) 1 in
			(t1(first,0),(t2 second))
		
	let run avt msg =( 
		match msg with
		"move north" -> Move North (*xy축이 우리랑 다르니까 처리 *)
		| "move south" -> Move South
		| "move east" -> Move East
		| "move west" -> Move West
		| _ -> 
			(match avt with
			ExperimentAvatar -> ((print_string "exp");Accelerate)
			| _ ->
				(match msg with
				"collect self" -> Collect Self
				|"collect left" -> Collect Left
				|"collect right" -> Collect Right
				| _ -> 
				  (match avt with
					SendAvatar -> ((print_string "sned");if (msg = "donotcollect") then Accelerate else Send)
					| SmartAvatar -> let (a,b) = translate(msg) in Transform(a,b)
					| AlienAvatar -> 
						let (a,b) = (msg.[0],msg.[1]) in
						if(a='a') then
							if(b='n')then AddArsenic North
							else if(b='s')then AddArsenic South
							else if(b='e')then AddArsenic East
							else AddArsenic West
						else 
							if(b='n')then RemoveArsenic North
							else if(b='s')then RemoveArsenic South
							else if(b='e')then RemoveArsenic East
							else RemoveArsenic West

					| _ -> Accelerate
					)
				)
			)
	)
			

	let rec isBlankSolution s = match s with Map.A(a,b) -> isBlankSolution(a) && isBlankSolution(b) | Map.V a -> false | _ -> true
	let isBlankGrid g = (match g with Map.Nothing -> true | Map.Sol s -> isBlankSolution s)

	let countGrid(grid,ar) = (
		let rec countSolution(s,ar) = 
			match s with
			 Map.S -> ar.(1)<-ar.(1)+1
			| Map.K -> ar.(2)<-ar.(2)+1
			| Map.I -> ar.(3)<-ar.(3)+1
			| Map.A(a,b)-> ar.(4)<-ar.(4)+1;countSolution(a,ar);countSolution(b,ar)
			| Map.V a-> (
				if(a="acid")then ar.(6)<-ar.(6)+1
				else if (a="double") then ar.(7)<-ar.(7)+1
				else if (a="As") then ar.(8)<-ar.(8)+1
				else if (a="triple") then ar.(9)<-ar.(9)+1
				else ar.(5)<-ar.(5)+1
				)
		in
			match grid with
			Map.Nothing -> countSolution(Map.I,ar)
			|Map.Sol s -> countSolution(s,ar)
	)

	let makeHuffmanTree map = (*map으로 허프만트리를 만듭니다 *)
	(
		let height = Map.height map in
		let width = Map.width map in
		let getWeight w = 
			match w with
			Tree (a,b,c) -> a
			| Leaf (a,b) -> a
		in
		let rec count(x,y,ar) = (*count ar에 x,y 를 누적시킵니다 *)
			if (x<height && y<width) then
				(if (isBlankGrid (Map.get map y x)) then ((ar.(0)<-ar.(0)+1);count(x,y+1,ar) )
				else (countGrid((Map.get map y x),ar);count(x,y+1,ar)))
			else if (x<height && y>=width) then count(x+1,0,ar)
			else ar
		in
		let rec makeLeaf (ar,li,num) = 
			match li with 
			[] -> []
			| h::t -> (* ((pss h);(print_int ar.(num));*)Leaf(ar.(num),h)::makeLeaf(ar,t,num+1)
		in
		let rec makeTree li = 
			if(List.length li = 1) then List.hd li
			else
				let sorted = List.sort (fun a b -> if(getWeight(a)>getWeight(b)) then 1 else -1) li in
				let first = List.hd sorted in
				let second = List.hd (List.tl sorted) in
				let remain = List.tl (List.tl sorted) in
					makeTree(Tree(getWeight(first)+getWeight(second),first,second)::remain)
		in
		let makeHuffmanEncoder(tree) =
			let rec findPosition(a,b,c) = (*b에서 a를 찾음 *)
				match b with
				[] -> 0
				| h::t -> if(a=h)then c else findPosition(a,t,c+1)
			in
			let rec makeHuffmanList(tree,dir,depth) = 
				match tree with
				Tree(a,b,c) -> makeHuffmanList(b,dir*2,depth+1)@makeHuffmanList(c,dir*2+1,depth+1)
				|Leaf(a,b) -> [(b,intToString(depth)^intToString(dir/256)^intToString(dir mod 256));]
			in
			List.sort (fun a b -> let (a1,a2)=a in let (b1,b2)=b in if(findPosition(a1,typeOfSolution,0)>findPosition(b1,typeOfSolution,0))then 1 else -1) (makeHuffmanList(tree,0,0))
		in	
		let countArray = Array.make numberOfSolution 0 in
(*			((pb (isBlankGrid(Map.get map 0 0))); *)
			 makeHuffmanEncoder(makeTree(makeLeaf(count(0,0,countArray),typeOfSolution,0)))
	)


	let rec makeHuffmanString(depth, s) = (* s끝 뒤에 depth만큼을 스트링으로 리턴 *)
		if(depth = 0) then ""
	    else if(s mod 2 == 0) then makeHuffmanString(depth-1,s/2)^"0"
	    else makeHuffmanString(depth-1,s/2)^"1"

	let rec compressString s = 
		if(String.length s = 0) then ""
		else 
			let con = String.sub s 0 8 in 
			let zero = charToInt('0') in
				intToString((charToInt(con.[0])-zero)*128+(charToInt(con.[1])-zero)*64+(charToInt(con.[2])-zero)*32+(charToInt(con.[3])-zero)*16+(charToInt(con.[4])-zero)*8+(charToInt(con.[5])-zero)*4+(charToInt(con.[6])-zero)*2+(charToInt(con.[7])-zero))^compressString(String.sub s 8 ((String.length s)-8))

	let rec makePriorityMatrix(cmap, avtpos) =
		let rec apply (x,y,a) = 
			if(x<maxXPos && y<maxYPos) then 
(*				(if(cmap.(x).(y) > distance((x,y),a)) then *)
				 (cmap.(x).(y) <- distance((x,y),a);apply(x,y+1,a))
			else if(x<maxXPos && y>=maxYPos) then apply(x+1,0,a)
			else cmap
		in
		apply(0,0,avtpos)
(* makePriorityMatrix(cmap,t)	*)
	
	let makePriorityList(cmap) = 
		let rec getlist (x,y) = 
			if(x<maxXPos && y<maxYPos) then (x,y,cmap.(x).(y))::getlist(x,y+1)
			else if(x<maxXPos && y>=maxYPos) then getlist(x+1,0)
			else []
		in
		List.sort 
		 (fun x y -> let (a,b,c) = x in let (d,e,f) = y in if(c>f) then 1 else if(c=f && a>d) then 1 else if(c=f && a=d && b>e) then 1 else -1 ) 
		 (getlist(0,0))

	let createMessage msg map = 
		let rec stringfyHuffmanEncoder h = 
			match h with
			[] -> ""
			| (a,b)::t -> b^stringfyHuffmanEncoder(t)
		in
		let huffmanEncoder = makeHuffmanTree(map) in (*hE = (용액 * 3바이트 스트링)의 리스트 *)
		let she = stringfyHuffmanEncoder huffmanEncoder in 
		let mapping = List.map (fun w -> let (a,b)=w in (a,makeHuffmanString(charToInt(b.[0]),charToInt(b.[1])*256 + charToInt(b.[2])))) huffmanEncoder in
		let rec findMapping(s, ma) = 
				match ma with
				[] -> ""
				| (a,b)::t -> if(a=s) then b else findMapping(s,t)
		in
		let encodeGrid (pos, ma) = (*좌표랑 매핑 *)
			let rec encodeSolution (s, ma) =
				match s with
				 Map.S -> findMapping(S,ma)
				| Map.K -> findMapping(K,ma)
				| Map.I -> findMapping(I,ma)
				| Map.A(a,b)-> findMapping(A,ma)^encodeSolution(a,ma)^encodeSolution(b,ma)
				| Map.V a-> 
					if(a="acid") then findMapping(C,ma)
					else if (a="triple") then findMapping(T,ma)
					else if (a="double") then findMapping(D,ma)
					else if (a="As") then findMapping(E,ma)
					else findMapping(V,ma)
			in
			let (x,y) = pos in
			let grid = Map.get map y x in
		(*		(print_int x);(print_int y);*)if(isBlankGrid(grid)) then findMapping(B,ma)
				else match grid with
					Map.Sol s -> encodeSolution(s,ma)
					| _ -> findMapping(I,ma) (*워닝방지*)
		in
		let ret(startx, starty, index, remain) = (*string * rem return *)
			let plist = makePriorityList(makePriorityMatrix(Array.make_matrix (Map.height map) (Map.width map) 999, (startx, starty))) in
			let rec dropn (l, num) = 
				if(num=0)then l
				else dropn(List.tl l, num-1)
			in
			let toEncode = dropn(plist,index) in

			let rec ret2 (enc, r) = 
				if(r>=0 && List.length enc >=0)then (
						if(List.length enc>0) then
							let (x,y,z) = List.hd enc in
							let en2 = encodeGrid((x,y),mapping) in
							let en = (if(String.length en2>1500) then (*100바이트를 다써도 안되면 *) findMapping(B,mapping) else en2) in
							if(r-String.length en <0) then (String.make (r-(r/8)*8) '0', (r-(r/8)*8))
							else let (a,b) = ret2(List.tl enc, (r- (String.length en))) in
								(en^a,b) 
						else
							(String.make (r-(r/8)*8) '0',(r-(r/8)*8)))
				else ("",0)
			in
				ret2(toEncode,remain)
		in
		let rec pp p =
			match p with
			[] -> ()
			| (a,b)::t -> ((pss a);(ps " ");(ps b);(ps " ");pp(t))
		in
		let (pmsg,rem) = ret(charToInt(msg.[1]),charToInt(msg.[2]),charToInt(msg.[3])*256 + charToInt(msg.[4]),1600 - 31 * 8) in (*마지막 자를 비트 *)
			(* ((pp mapping);*)
		let msg = compressString(pmsg) in
(*			((ps pmsg);(ps "버릴비트 :");(print_int rem);(ps ("\n"^msg));(print_int (String.length msg));*)she^intToString(rem)^msg
end


(* 20바이트 허프만코드를보내 ->100바이트 16바이트 84바이트 *)

