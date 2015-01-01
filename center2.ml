open Server
open Avatar2
open Random

(*let reactFull g = (* grid 타입을 받아서 굳은 용액이 될 때까지 반응시키면서 몇번 반응하는지 세는 함수 (grid, 반응 횟수)를 반환한다. *)
	let rec reactFullwn (g, n) =
		if (snd (reactOnce g)) = false then (g, n)
		else (reactFullwn ((fst (reactOnce g)), (n + 1)))
	in
	if (snd (reactOnce g)) = false then (g, 0)
	else (reactFullwn ((fst (reactOnce g)), 1))

let reactFulln g =
	let (a, b) = (reactFull g) in
	b
*)
(*변환아바타 없으면 변환용액 가치 0*)
module Center2 : ICenter
(*with type center = int * (avatarType list) * int *)=
struct
	(* AI관련 상수 *)

	(* ai 지능관련 상수 *)
	
	let constantConsiderGrid = 10
	let constantConsiderGridAccelerate = 1

	let constantConsiderMaxTurn = 20
	let constantConsiderMaxTransformTurn = 10


	(* ai 용액 값어치 관련 상수 *)
	let maxTurn = 150
	let valueOfAcid = -20
	let valueOfSolution = 60 (*바로캘수있는거 *)
	let valueOfDoubleSolution = 120
	let valueOfTripleSolution = 200 
	let valueLossPerDistance = 3

	let valueOfAs = -20

	(* ai 용액 보정 관련 상수 *)
	let constantAdjacentGrid = 6 (* divider *)
	let constantSecondAdjacentGrid = 10 (* divider *)

	let valueOfTransform = 20


	(* ai 불가능한 그리드 관련 상수 *)
	let valueOfUnreachableGrid = -20(*거리가 안되서 못가는거 *)
	let valueOfUncollectableGrid = -20 (*아무것도 없어서 못줍는거 잇어도 숨겨져잇거나 *)
	let valueOfUnknownGrid = 0 (* 모르는거 *)
	let valueOfInexistGrid = -15 (* 범위를 벗어난 그리드 *)

	(*ai 움직임 관련 상수 *)

	let constantMaximumDistance = 4
	let constantLossOfAdjacentPositionA = 15 (* multiplier *)
	let constantLossOfAdjacentPositionB = 1 (* dividerr *)


	(* 맵 업데이트 관련 상수 *)
	let constantCollectable = 1000
	let constantApproach = 40
	let constantUpdatePerTurn = 6

	(* 이거 이상이면 콜렉트함 *)
	let constantCollect = 0
	let constantLossOfAdjacentEnemy = 20


	let constantNearestDistanceAs = 1

	type huffmanType = B | S | K | I | A | V | C | D | R | T

	type solutionType = Bm | Sm | Km | Im | Vm | Am of solutionType * solutionType| Cm | Dm | Rm | Tm
	type centerMap = Unknown | Known of solutionType
	
    let ps a = (print_string (a^"\n"))
	let pi a = ((print_int a);(print_string "\n"))
	let pc a = let (x,y) = a in (print_int x);(print_string ", ");(print_int y);(print_string "  ")
	let rec print_coords(l) = match l with [] -> () | h::t -> (pc(h);print_coords(t))

	let rec printSolution s =
    match s with
    Bm -> print_string "B"
    | Sm -> print_string "S"
    | Km -> print_string "K"
    | Im -> print_string "I"
    | Vm -> print_string "V"
    | Am (a, b) -> ((print_string "A(");printSolution(a);(print_string " ");printSolution(b);(print_string ")"))
    | Cm -> print_string "C"
    | Dm -> print_string "D"
	| Rm -> print_string "R"
	| Tm -> print_string "T"
	let maxXPos = 15
	let maxYPos = 30


    let rec printMap (x, y, map) = 
      if(x<maxXPos && y<maxYPos) then (match map.(x).(y) with
                Unknown -> ((pc (x,y));(print_string "-> unknown\n");printMap(x,y+1,map))
                | Known s -> ((pc (x,y));(print_string "-> ");(printSolution(s));(ps " ");printMap(x,y+1,map)))
        else if(x<maxXPos && y>=maxYPos) then printMap(x+1,0,map)
        else ()

	type  center = int * (avatarType list) *  (int * int) list * int  * centerMap array array * (int * int * int) list * bool array array * MessageList.msgList * (int * int) * (int * int) list


	(* 아바타개수 / 아바타리스트 / 아바타 위치/턴 / 맵 / 가중치맵리스트 /필요ㅓㅄ는 부울어레이 / 저번메세지리스트 / 시작위치 / 적위치 (4개) *)  

	let distance(a,b) = let (x1,y1) = a in 	let (x2,y2) = b in	abs(x1-x2)+abs(y1-y2)

	let  mapGridToSolutionType g =
		let rec mgtst s =
		match s with
		Map.S -> Sm
		| Map.K -> Km
		| Map.I -> Im
		| Map.V a -> if (a = "acid") then Cm
					 else if (a = "double") then Dm
					 else if (a = "As") then Rm
					 else if (a = "triple") then Tm
					 else Vm
		| Map.A (a, b) -> (Am ((mgtst a), (mgtst b)))
		in
	match g with
	Map.Nothing -> Known Bm
	| Map.Sol s ->
	(
		match s with
		Map.S -> Known Sm
		| Map.K -> Known Km
		| Map.I -> Known Im
		| Map.V a -> if (a = "acid") then Known Cm
					 else if (a = "double") then Known Dm
					 else if(a="As") then Known Rm
					 else if(a="triple") then Known Tm
					 else Known Vm
		| Map.A (a, b) -> Known (Am ((mgtst a), (mgtst b)))
	)

	let collectSolutionBool cm =
		match cm with
		Unknown -> false
		| Known s ->
		(
		match s with
		Vm -> true
		| Cm -> true
		| Dm -> true
		| Am(Vm, _) -> true
		| Am(Cm, _) -> true
		| Am(Dm, _) -> true
		| Am(_, Vm) -> true
		| Am(_, Cm) -> true
		| Am(_, Dm) -> true
		| Am(Tm, _) -> true
		| Am(_, Tm) -> true
		| _ -> false
		)

	let collectSolutionBool3 cm =
		match cm with
		Unknown -> false
		| Known s ->
		(
		match s with
		Vm -> true
		| Dm -> true
		| Am(Vm, _) -> true
		| Am(Dm, _) -> true
		| Am(_, Vm) -> true
		| Am(_, Dm) -> true
		| Am(Tm, _) -> true
		| Am(_, Tm) -> true
		| _ -> false
		)

	let rec collectSolutionBool2 cm =
		match cm with
		Unknown -> false
		| Known s ->
		(
		 let rec ss t = 
		match t with
		Vm -> true
		| Dm -> true
		| Am(Vm, _) -> true
		| Am(Dm, _) -> true
		| Am(_, Vm) -> true
		| Am(_, Dm) -> true
		| Am(Tm, _) -> true
		| Am(_, Tm) -> true

		| Am(a,b) -> ss(a) or ss(b)
		| _ -> false
		in
		ss s)

	let bb cm = 
		match cm with
		Unknown -> false
		| Known Rm -> true
		| _ -> false
	

	let aa cm =
		match cm with
		| Known Cm -> true
		| Known Am(Cm, _) -> true
		| Known Am(_,Cm) -> true
		| _ -> false

	let rec ddd(a, b, n) =
		match b with
		[] -> 0
		| h::t -> if(a=h)then n else ddd(a,t,n+1)

	let ppm (map,posl) = 
		let ppg(x,y) = 
			if(List.mem (x,y) posl) then ((print_int(ddd((x,y),posl,0)));(print_string " "))
			else if(bb map.(x).(y))then print_string("! ")
			else if(aa map.(x).(y)) then print_string("C ")
			else if(collectSolutionBool3 map.(x).(y))then print_string("# ")
			else if(collectSolutionBool2 map.(x).(y)) then print_string("+ ")
			else print_string(". ")
		in
		let rec line(x,y)=  
		if(y<maxYPos) then (ppg(x,y);line(x,y+1))
		else (ps " ")
		in
		let rec h(x) =
		if(x<maxXPos) then (line(x,0);h(x+1))
		else (ps " ")
		in
		h(0)

	let parray a = 
		let rec ppg(x,y) =
		if(0<=x && 0<=y && x<maxXPos && y<maxYPos) then ((Printf.printf "%5d" a.(x).(y));ppg(x,y+1))
		else if (x<maxXPos && y>=maxYPos) then ((ps  " ");ppg(x+1,0))
		else ()
		in
		ppg(0,0)

	let parray2 a = 
		let rec ppg(x,y) =
		if(0<=x && 0<=y && x<maxXPos && y<maxYPos) then let (xx,yy) = a.(x).(y) in ((Printf.printf "(%2d,%2d)" xx yy);ppg(x,y+1))
		else if (x<maxXPos && y>=maxYPos) then ((ps  " ");ppg(x+1,0))
		else ()
		in
		ppg(0,0)


	let collectSolutionString cm =
		match cm with
		Unknown -> "collect self"
		| Known s ->
		(
		match s with
		Vm -> "collect self"
		| Cm -> "collect self"
		| Dm -> "collect self"
		| Am(Vm, _) -> "collect left"
		| Am(Cm, _) -> "collect left"
		| Am(Dm, _) -> "collect left"
		| Am(_, Vm) -> "collect right"
		| Am(_, Cm) -> "collect right"
		| Am(_, Dm) -> "collect right"
		| _ -> "collect self"
		)
		
	let typeOfSolution = [B;S;K;I;A;V;C;D;R;T;]  
	let numberOfTypeOfSolution = 10
	let numberOfCoordOnce = 9

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

	let make l t s = 
		let (x,y) = s in
		let start = (y,x) in
		let enemy = (y,29-x) in
		let avtpos = [start;start;start;start;start;start;] in
		let avtpos2 = [enemy;enemy;enemy;enemy;enemy;enemy;] in
		(List.length l, l, avtpos, 0, Array.make_matrix maxXPos maxYPos Unknown, (makePriorityList(makePriorityMatrix(Array.make_matrix maxXPos maxYPos 999, start))), Array.make_matrix maxXPos maxYPos false, MessageList.create (List.length l), start, avtpos2)

	let rec getCoordinateWithP(x, y, pmap, vmap, p, num) = (* vmap이 false면서 pmap이 p인 좌표를 num 개 리스트로 반환*)
		if(num = 0) then []
		else if(x<maxXPos && y<maxYPos) then (if (pmap.(x).(y) = p && vmap.(x).(y) = false) then (x,y)::getCoordinateWithP(x, y+1, pmap, vmap, p, num-1) else getCoordinateWithP(x,y+1,pmap,vmap,p,num))
		else if(x<maxXPos && y>=maxYPos) then getCoordinateWithP(x+1, 0, pmap, vmap, p, num)
		else []

	let getMax a = (*중 최대값구하기 *) Array.fold_left max 0 a
	let getMin a = (*중 최소값구하기 *) Array.fold_left min 999 a

	let rec getValuableCoordinate(pmap, vmap, num, minp, maxp) =  (* pmap의 가중치중 vmap이 false인것중 가장 가중치높은걸 num개 구해서 페어 리스트로 반환 *)
		let gc = getCoordinateWithP(0,0,pmap,vmap,minp,num) in (* 가치가 p인걸 구한다 *)
			if(List.length gc < num) then  (*num개를 다 발견하면*)
				(if (minp+1<=maxp) then List.append gc (getValuableCoordinate(pmap,vmap,num-(List.length gc),minp+1,maxp)) (* 맵을 덜돌았으면 *)
				 else gc) (*맵을 다돌았으면 *)
			else gc

 (* 맵 좌표 받은건 priority 999로 하기 *)
 (* acid는 밟지말고 피해가기 *)
	let charToString a = (*char을 스트링으로바꿈 *)
		String.make 1 a

	let intToString a = (*int를 스트링으로바꿈 *)
		String.make 1 (Char.chr a)

	let charToInt a =
		(Char.code a)


	let ps a = (print_string (a^"\n"))
	let pi a = ((print_int a);(print_string "\n"))

	let rec stringfyValuableCoordinate(cl) = (* 좌표를 문자로 *)
		match cl with 
		[] -> ""
		| h::t -> let (a,b) = h in intToString(a)^intToString(b)^stringfyValuableCoordinate(t)
	let rec makeHuffmanString(depth, s) = (* s끝 뒤에 depth만큼을 스트링으로 리턴 *)
		if(depth = 0) then ""
		else if(s mod 2 == 0) then makeHuffmanString(depth-1,s/2)^"0"
		else makeHuffmanString(depth-1,s/2)^"1"

	let rec makeHuffmanList(c,num,tos) = (* 16바이트 문자열 c를 허프만 리스트로 바꿉니다. *)
		if(num>= numberOfTypeOfSolution * 3) then [] (* 다했으면 리턴 *)
		else (List.hd tos, makeHuffmanString(charToInt(c.[num]),charToInt(c.[num+1])*256+charToInt(c.[num+2])))::makeHuffmanList(c,num+3,List.tl tos)

	let rec decompressChar (c, i) =
		if (i=0)then ""
		else if (c mod 2 = 1) then decompressChar(c/2,i-1)^"1"
		else decompressChar(c/2,i-1)^"0"
	 
	let rec decompressString(s) = 
		if (String.length s =0) then ""
		else decompressChar(charToInt(s.[0]),8)^decompressString(String.sub s 1 ((String.length s)-1))

	let rec huffmanDecode(decoder, psource, vcl, cmap, vmap) = (* decoder 로 source를 해독하여 그걸 cmap에 넣음 *)
		if(List.length vcl = 0) then vcl
		else	
			let rec decodeOnce (decoder, source, num) = (* 용액과 남은 문자열의 페어를 돌려줌 *)
				if(num<=String.length source) then 
					(let l = List.filter 
							(fun so -> 
							 	(match so with 
								 	(a,b) -> 
										(if (b=(String.sub source 0 num)) then true else false)
										)
								) decoder in
						if (List.length l=0) then decodeOnce(decoder, source, num+1)
						else 
							let (a,b) = (List.hd l) in
								match a with
								 A -> (
										 let (l, remain) = decodeOnce(decoder, (String.sub source num ((String.length source)-num)),1) in
										 	let (r, remain2) = decodeOnce(decoder, remain, 1) in
												(Am(l, r),remain2)
								  )
								| T -> (Tm,String.sub source num ((String.length source)-num))
								| S -> (Sm,String.sub source num ((String.length source)-num))
								| K -> (Km,String.sub source num ((String.length source)-num))
								| I -> (Im,String.sub source num ((String.length source)-num))
								| C -> (Cm,String.sub source num ((String.length source)-num))
								| D -> (Dm,String.sub source num ((String.length source)-num))
								| B -> (Bm,String.sub source num ((String.length source)-num))
								| V -> (Vm,String.sub source num ((String.length source)-num))
								| R -> (Rm,String.sub source num ((String.length source)-num))
	)
				else (Im, "DecodeOnceFail")
			in
				if(String.length psource > 0) then
					let (sol, remain) = decodeOnce(decoder, psource, 1) in
						let (x, y, z) = List.hd vcl in
							if(remain = "DecodeOnceFail")then vcl (*((ps "DecodeFAIL!!!!");print_string("Source : ");(ps psource))*)
							else ((cmap.(x).(y) <- Known sol);(vmap.(x).(y)<- true);(huffmanDecode(decoder,remain,List.tl vcl,cmap, vmap)))
				else vcl

	let rec writeValuableCoordinate vcl = (* vcl 을 문자로씁니다 *)
		match vcl with
		[] -> ""
		| (x,y)::t -> intToString(x)^intToString(y)^writeValuableCoordinate(t)


(*주변맵도보자*)
	let rec reactOnce a = (*한번반응한 용액을돌려줌 *)
		match a with
		| Am(Im, s) -> s
		| Am(Am(Km, s1), s2) -> s1
		| Am(Am(Am(Sm, s1), s2), s3) -> Am(Am(s1, s3), Am(s2, s3))
		| Am(s1, s2) -> (* 아직 논의를 거치지 않은 반응 순서, 추후에 달라질 수 있다. *)
			(if (s1 = (reactOnce s1)) then Am(s1, reactOnce s2)
			else Am((reactOnce s1), s2))
		| _ -> a
	let rec reactable a = 
		let b = reactOnce a in
		if(a=b)then false
		else true
	let reactableGrid a = 
		match a with
		Unknown -> false
		| Known s -> reactable s

	let reactOnceAll mmap = 
		let rec roa (x,y,cmap) = 
			if(x<maxXPos && y<maxYPos) then 
				match cmap.(x).(y) with
				Unknown -> roa(x,y+1,cmap)
				| Known s -> (
						( cmap.(x).(y) <- Known(reactOnce(s))); roa(x,y+1,cmap)
						 )
			else if(x<maxXPos && y>=maxYPos) then roa(x+1,0,cmap)
			else ()
		in
		roa(0,0,mmap)


(* Am Sm Km Im Cm Dm Bm Vm *)

	let rec getFrontListElements(l,num) = 
		if (num = 0) then []
		else 
			match l with
			[] -> []
			| h::t -> h::getFrontListElements(t,num-1)

	let rec getValueOfSolution(s, solpos, avtpos, after, turn, avt) = (*현재 turn에 solpos avtpos이고 after이후 용액 s avt : avt타입  *) 
		if(turn+after>maxTurn || after > constantConsiderMaxTurn + distance(solpos,avtpos)) then valueOfUnreachableGrid
		else if(distance(solpos,avtpos)>after) then getValueOfSolution(reactOnce s, solpos, avtpos, after+1, turn,avt) 
		else match s with
		| Cm -> valueOfAcid - valueLossPerDistance * distance(solpos,avtpos)
		| Vm -> valueOfSolution - valueLossPerDistance * distance(solpos,avtpos)
		| Dm -> valueOfDoubleSolution - valueLossPerDistance * distance(solpos,avtpos)
		| Tm -> valueOfTripleSolution - valueLossPerDistance * distance(solpos,avtpos)
		| Rm -> valueOfAs
		| Am(a,b) -> 
			if (a=Vm) then valueOfSolution - valueLossPerDistance * distance(solpos,avtpos) + getValueOfSolution(b, solpos, solpos, 0, turn+after+1,avt)
			else if (b=Vm) then valueOfSolution - valueLossPerDistance * distance(solpos,avtpos) + getValueOfSolution(a, solpos, solpos, 0, turn+after+1, avt) (* 전에말햇던 특이한케이스존재가능 *)
			else if (a=Dm) then valueOfDoubleSolution - valueLossPerDistance * distance(solpos,avtpos) + getValueOfSolution(b, solpos, solpos, 0 ,turn+after+1, avt)
			else if (b=Dm) then valueOfDoubleSolution - valueLossPerDistance * distance(solpos,avtpos) + getValueOfSolution(a, solpos, solpos, 0 ,turn+after+1, avt)
			else if (a=Tm) then valueOfTripleSolution - valueLossPerDistance * distance(solpos,avtpos) + getValueOfSolution(b, solpos, solpos, 0 ,turn+after+1, avt)
			else if (b=Tm) then valueOfTripleSolution - valueLossPerDistance * distance(solpos,avtpos) + getValueOfSolution(a, solpos, solpos, 0 ,turn+after+1, avt)

			else (
				let tv = getValueOfTransform(s, solpos, avtpos, after, turn, avt) in
				let re = reactOnce s in
					if(re=s) then tv (*변화 더없으면 0*)
					else max tv (getValueOfSolution(re,solpos,avtpos,after+1,turn,avt)))
		| _ -> valueOfUncollectableGrid
	and getValueOfTransform(s, solpos, avtpos, after, turn, avt) = (*용액과 아바타를 받아서 변형고려한 것과 그때 변형턴수 리턴 *)
	if(avt = 1) then
			if(turn+after>maxTurn || after > constantConsiderMaxTransformTurn + distance (solpos, avtpos)) then valueOfUnreachableGrid
			else if(reactable(s)) then valueOfUnknownGrid (* 반응이안끝낫을때만  *)
			else 
				let rec getConvertableList(s) = (*한번바꾼거 리스트를 돌려준다 *)
				match s with
				| Cm -> [Sm;Km;Im]
				| Vm -> [Sm;Km;Im]
				| Dm -> [Sm;Km;Im]
				| Sm -> [Km;Im;]
				| Km -> [Sm;Im;]
				| Im -> [Sm;Km;]
				| Tm -> []
				| Bm -> []
				| Rm -> []
				| Am(a,b) ->(
					let rec attachleft(left, li) = 
						match li with
						[] -> []
						| h::t -> Am(left,h)::attachleft(left, t)
					in
					let rec attachright(li, right) = 
						match li with
						[] -> []
						| h::t -> Am(h,right)::attachright(t,right)
					in
					let llist = attachleft(a,getConvertableList(b)) in
					let rlist = attachright(getConvertableList(a),b) in
						llist@rlist
					)
				in
				let once = (List.filter reactable (getConvertableList(s))) in	
				let rec loop li = 
					match li with
					[] -> -9999999
					| h::t -> max (getValueOfSolution(h,solpos,solpos,after+1,turn,avt)) (loop(t))
				in
				let k = loop(once) in
(*					if(k - valueOfTransform > 0) then k - valueOfTransform
					else *k *)
					k - valueOfTransform
		else valueOfUnknownGrid


	let rec getMinimumDistance(pos, poslist) = (* poslist 의 1 2 3 5번만쓰기 *) 
		match poslist with
		[] -> 999
		| h::t -> 
			let d = let l = List.length poslist in if(l=5 || l=4 || l=3 || l=1) then distance(pos,h)  else 999 in
			let m = getMinimumDistance(pos,t) in
			if(m>d) then d
			else m

	let rec getMinimumDistanceCoord(pos, poslist) = (* poslist 의 1 2 3 5번만쓰기 *) 
		match poslist with
		[] -> (99,99)
		| h::t -> 
			let m = let l = List.length poslist in if(l=5 || l=4 || l=3 || l=1) then h else (99,99) in
			let d = getMinimumDistanceCoord(pos,t)  in
			if(distance(pos,m)>distance(pos,d)) then d
			else h


	let getValueOfGrid(g,gridpos, avtpos,turn, avt, enemypos) = (* 그리드, 그리드위치 아바타위치 게임턴 *)
		match g with
		 Unknown -> valueOfUnknownGrid
		| Known s -> 
			let enemydistance = getMinimumDistance(gridpos,enemypos) in
			let mydistance = distance(gridpos,avtpos) in
			let va = (if (enemydistance > mydistance) then 0 else (mydistance - enemydistance)) in
			getValueOfSolution(s, gridpos, avtpos, 0, turn, avt) - constantLossOfAdjacentEnemy * va
		
	let applyAdjacentGrid input = (* array리턴 *)
		let getArrayValue(ar,x,y) = 
			if(0<=x && x<maxXPos && 0<=y && y<maxYPos) then ar.(x).(y)
			else valueOfInexistGrid
		in
		let rec aag(x,y,output) = 
			if(x<maxXPos && y<maxYPos) then 
	((output.(x).(y)<-input.(x).(y)+
	  (getArrayValue(input,x-1,y)
	   +getArrayValue(input,x+1,y)
	   +getArrayValue(input,x,y-1)
	   +getArrayValue(input,x,y+1))
	   / constantAdjacentGrid + 
	   (getArrayValue(input,x-2,y)
		+ getArrayValue(input,x-1,y-1)
		+ getArrayValue(input,x,y-2)
		+ getArrayValue(input,x+1,y-1)
		+ getArrayValue(input,x+2,y)
		+ getArrayValue(input,x+1,y+1)
		+ getArrayValue(input,x,y+2)
		+ getArrayValue(input,x-1,y+1)
		+ getArrayValue(input,x-2,y)
 		) / constantSecondAdjacentGrid
	   );(aag(x,y+1,output)))
			else if(x<maxXPos && y>=maxYPos) then aag(x+1,0,output)
			else output 
		in
			aag(0,0,Array.make_matrix maxXPos maxYPos 0)
	
	let getValuableGrid(pos, map, turn, avt, enemypos) = (*pos : 현재 아바타 위치 avt : 아바타 타입  1: 스마트  2 : 촉진*)
		let rec array2list(input,x,y) =  
			if(x<maxXPos && y<maxYPos) then 
				(match map.(x).(y) with
				 Unknown -> array2list(input,x,y+1)
				 | _ -> (x,y,input.(x).(y))::array2list(input,x,y+1))
			else if(x<maxXPos && y>=maxYPos) then array2list(input,x+1,0)
			else []
		in
		let rec compute(ar,x,y) = 
			if(x<maxXPos && y<maxYPos) then ((ar.(x).(y) <- getValueOfGrid(map.(x).(y),(x,y),pos,turn,avt,enemypos));compute(ar,x,y+1))
			else if(x<maxXPos && y<=maxYPos) then compute(ar,x+1,0)
			else applyAdjacentGrid(ar)
		in
		let cc = 
			(if (avt=2) then constantConsiderGridAccelerate
			 else constantConsiderGrid)
		in
		getFrontListElements(
				(List.sort 
				 (fun q w -> let (a,b,c) = q in let (d,e,f) = w in if(c>f)then -1 else 1 ) 
				 (array2list(
				  (compute(
				   (Array.make_matrix maxXPos maxYPos 0),0,0)),0,0)))
				,cc)

(* x,y, v*)
	let addValue(l, c) = 
		if (List.mem c l) then 0
		else 1

	let rec lossOfAdjacentPosition(p) = 
		let rec loap(p2, pos) = 
			match p2 with 
			[] -> 0
			| h::t -> (if(h=(-1,-1)) then loap(t,pos) else 
					let k = distance(h,pos) in
					let sq x = x* x in
					let d = (if(k>=constantMaximumDistance) then 0 else sq(constantMaximumDistance - k) * constantLossOfAdjacentPositionA / constantLossOfAdjacentPositionB) in
					
					d + loap(t,pos))
		in
		match p with
		[] -> 0
		| h::t -> (if(h=(-1,-1))then lossOfAdjacentPosition(t) else loap(t,h) + lossOfAdjacentPosition(t))

	let rec nnmp (cmap, x, y) = (*모르는게 하나라도있으면 true*)
		if(x<maxXPos && y<maxYPos) then
			(match cmap.(x).(y) with
			 Unknown -> true
			 | _ -> nnmp(cmap,x,y+1))
		else if (x<maxXPos && y>=maxYPos) then nnmp(cmap,x+1,0)
		else false

	let needNewMap(turn,cmap)= (*새 맵이 필요할지 판단하는 함수 현재맵과 턴보고 가져오자 *)
		if(nnmp(cmap,0,0)=false) then false (*다 알면 더알필요없다 *)
		else
		let rec countmap (x,y) = 
			if(x<maxXPos && y<maxYPos) then 
				if(collectSolutionBool(cmap.(x).(y))) then 
					countmap(x,y+1)+1
				else 
					countmap(x,y+1)
			else if(x<maxXPos && y>=maxYPos) then countmap(x+1,0)
			else 0
		in
		if(countmap(0,0) > constantCollectable) then false
		else true
		

	let needUpdateMap(turn,avtpos,enemypos) = 
		let rec rr(pos,pl ) = 
			match pl with
			[] -> false
			| h::t -> if(distance(pos,h)<constantApproach) then true
						else rr(pos,t)
		in
		let rec rrr(pl1, pl2) = 
			match pl1 with
			[] -> false
			| h::t -> if(rr(h,pl2)) then  true
					  else rrr(t,pl2)
		in
		(turn mod constantUpdatePerTurn = 0) && rrr(avtpos,enemypos)

	let needNewMapPosition turn cmap =
		if( nnmp(cmap,0,0) || true ) then true
		else false

	let canEnter(g) =  (*true*)
		match g with 
		Unknown -> true
		| Known Rm -> true
		| _  -> true 

	let getDirection(nowpos, newpos, cmap) =
		let short = Array.make_matrix maxXPos maxYPos 999 in
		let trace = Array.make_matrix maxXPos maxYPos (-1,-1) in
		let rec getlastpos(pos) = 
			let (x,y) = pos in
			if(pos = (-1,-1)) then nowpos (*못가는경우 *)
			else
				let (nx,ny) = trace.(x).(y) in
				if((nx,ny) = nowpos) then (x,y)
				else getlastpos((nx,ny))
		in
		let acid(x, y) =
			if(0<=x && x<maxXPos && 0<=y && y<maxYPos) then 
				match cmap.(x).(y) with
				Known Cm -> 1
				| Known Rm -> 9000 
				| Known Am(Cm, _) -> 1
				| Known Am(_, Cm) -> 1
				| _ -> 0
			else 100
		in
		let rec update(ar,xc,yc,vc,q,pl) = 
			match pl with
				[] -> q
				| (px,py)::t ->
					let xx = xc+px in
					let yy = yc+py in
					let va = vc + 1 + acid(xx,yy) in
					if(0<=xx && xx<maxXPos && 0<=yy && yy<maxYPos) then
						if(ar.(xx).(yy)=va && canEnter(cmap.(xx).(yy)) && Random.int(2) = 0) then ((ar.(xx).(yy)<-va);(trace.(xx).(yy)<-(xc,yc));(update(ar,xc,yc,vc,q@[(xx,yy);],t))) 
						else if(ar.(xx).(yy)>va && canEnter(cmap.(xx).(yy)))then ((ar.(xx).(yy)<-va);(trace.(xx).(yy)<-(xc,yc));(update(ar,xc,yc,vc,q@[(xx,yy);],t)))
						else (update(ar,xc,yc,vc,q,t))
					else (update(ar,xc,yc,vc,q,t))
		in
		let rec bfs(queue) = 
			match queue with 
			[] -> ()
			| h::t  ->
				let (x,y) = h in
				if((x,y)=newpos) then ()
				else 
				let v = short.(x).(y) in
				let newqueue = update(short,x,y,v,t,[(1,0);(-1,0);(0,1);(0,-1);]) in
				bfs(newqueue)
		in
		let (startx, starty) = nowpos in
		    ((short.(startx).(starty)<-0);(trace.(startx).(starty)<-(-1,-1));bfs([nowpos;]);
	(*			(parray short);(parray2 trace); *)let (lx,ly) = getlastpos(newpos) in
				let (x,y) = nowpos in
				if(x<lx) then "move south"
				else if(x>lx) then "move north"
				else if(y<ly) then "move east"
				else "move west"
			)


(*
		let rec recur
		let (x1,y1) = nowpos in
			let (x2,y2) = newpos in
			
				let r = Random.int(2) in
	                     if(x1<x2 && y1<y2) then (if r=0 then "move south" else "move east")
					else if(x1<x2 && y1>y2) then (if r=0 then "move south" else "move west")
					else if(x1=x2) then (if(y1>y2) then "move west" else "move east")
					else if(x1>x2 && y1<y2) then (if r=0 then "move north" else "move east")
					else if(x1>x2 && y1>y2) then (if r=0 then "move north" else "move west")
					else  (if(x1>x2) then "move north" else "move south")
*)
	let getWhereToCollect(startpos,plist) =
		let (x,y) = startpos in
			if(List.length plist = 0) then "donotcollect"
			else 
				let num = maxXPos * maxYPos - (List.length plist) in
				let n1 = num / 256 in
				let n2 = num mod 256 in
				"A" ^ intToString(x) ^ intToString(y) ^ intToString(n1) ^ intToString(n2)

	let getWhereToUpdate(avtpos, enemypos) = 
		let rec rr(a, b) = (*가 리스트 *)
		match b with
		[] -> ((1,1),999)
		| h::t -> let (xy,z) = rr(a, t) in
					if(distance(h,a) > z) then (xy,z)
					else (h,distance(h,a))
		in
		let	rec rrr(a,b) = 
		match a with
		[] -> ((1,1),999)
	 	| h::t -> let (xy,z) = rr(h,b) in
				  let (xxyy,zz) = rrr(t,b) in
				  if(xy>xxyy) then (xxyy,zz)
				  else (xy,z)
		in
		let ((x,y),t) = rrr(avtpos,enemypos) in
			"B"^intToString(x)^intToString(y)^intToString(0)^intToString(0)


	(*
		let rec gwtc(x, y, cmap) =
			if(x+y>maxXPos+maxYPos-2) then "donotcollect"
			else if(0<=x && 0<=y && x<maxXPos && y<maxYPos) then 
				(match cmap.(x).(y) with
				 Unknown -> intToString(x)^intToString(y)
				 | _ -> gwtc(x+1,y-1,cmap)
				)
			else if(y<0) then gwtc(0,x,cmap)
			else gwtc(x+1,y-1,cmap)
		in gwtc(0,0,cmap)
*)
	let getValuableCoordinateNEW(cmap) = 
		let rec gvcn(x,y,cmap) =
			if(x+y>maxXPos+maxYPos-2) then []
			else if(0<=x && 0<=y && x<maxXPos && y<maxYPos) then 
				(match cmap.(x).(y) with
				 Unknown -> (x,y)::gvcn(x+1,y-1,cmap)
				 | _ -> gvcn(x+1,y-1,cmap)
				)
			else if(y<0) then gvcn(0,x,cmap)
			else gvcn(x+1,y-1,cmap)
		in gvcn(0,0,cmap)

	let convertSolutionString(grid,turn) =  
		let rec getConvertableList(s) = (*한번바꾼거 리스트를 돌려준다 *)
			match s with
			| Cm -> [(Sm,"S");(Km,"K");(Im,"I")]
			| Vm -> [(Sm,"S");(Km,"K");(Im,"I")]
			| Dm -> [(Sm,"K");(Km,"K");(Im,"I")]
			| Sm -> [(Km,"K");(Im,"I");]
			| Km -> [(Sm,"S");(Im,"I");]
			| Im -> [(Sm,"S");(Km,"K");]
			| Tm -> []
			| Bm -> []
			| Rm -> []
			| Am(a,b) ->(
				let rec attachleft(left, li) = (*왼쪽은 고정  *)
					match li with
					[] -> []
					| (h1,h2)::t -> (Am(left,h1),"R"^h2)::attachleft(left, t)
				in
				let rec attachright(li, right) = 
					match li with
					[] -> []
					| (h1,h2)::t -> (Am(h1,right),"L"^h2)::attachright(t,right)
				in
				let llist = attachleft(a,getConvertableList(b)) in
				let rlist = attachright(getConvertableList(a),b) in
					llist@rlist
				)
		in
			match grid with
			Unknown -> "collect self" (*이럴 경우 없음 *)
			| Known s -> ((*(printSolution s); *)
				let once = (List.filter (fun x -> let (a,b) = x in reactable a) (getConvertableList(s))) in
(*				(print_o once); *)
				if(List.length once = 0) then "collect self"
				else (
					let vai = List.map (fun x -> let (a,b) = x in (getValueOfSolution(a,(0,0),(0,0),1,turn,1),b)) once in (* value * index *)
					let sorted = List.sort (fun q w -> let (a,b) = q in let (c,d) = w in if (a>c) then 1 else -1) vai in
					let (r1,r2) = List.hd sorted in
					r2		
					))

	let ai (n, turn, avtList, avtPosition, cmap, startpos, plist, enemyPosition) = (* 애들한테 보낼 스트링 집합 리턴  *)
		let rec findMove(i, n, li, positions, value) = (*찾긔... 위치와 최종값 v 리턴 *) 
			if(i>=n) then (* end *)
				(positions, value - lossOfAdjacentPosition(positions)) 
			else (
				let first = List.hd li in
				match first with
				[] -> findMove(i+1, n, List.tl li, positions@[(-1,-1)], value) (*처음게비었다 그럼 이거 -1 -1 세팅후 ㄱㄱ  *)
				 | h::t -> (*아니면 h선택, h무시  *)
				 	let (x,y,v) = h in
						let (list1,value1) = findMove(i+1, n, (List.tl li) , positions@[(x,y)], value + addValue(positions,(x,y)) * v) in
							if(t = []) then (list1,value1)
							else
								let (list2,value2) = findMove(i, n, t::(List.tl li), positions, value) in
									if(value1>value2) then (list1,value1)
									else (list2,value2)
				 )
					 (* 선택 *)
		in
		let rec getValuableGridList i =
			if(i>=n) then []
			else 
			match List.nth avtList i with
			SendAvatar->
				if (needNewMap(turn,cmap)) then []::getValuableGridList(i+1)
			(*	else if(needUpdateMap(turn,avtPosition,enemyPosition)) then []::getValuableGridList(i+1) *)
				 else getValuableGrid((List.nth avtPosition i), cmap, turn, 0,enemyPosition)::getValuableGridList(i+1)
			|CollectAvatar->
				getValuableGrid((List.nth avtPosition i), cmap, turn, 0,enemyPosition)::getValuableGridList(i+1)
			|SmartAvatar->
				getValuableGrid((List.nth avtPosition i), cmap, turn, 1, enemyPosition)::getValuableGridList(i+1)
			|ExperimentAvatar-> 
(*				getValuableGrid((List.nth avtPosition i), cmap, turn, 2)::getValuableGridList(i+1) *)
				[]::getValuableGridList(i+1)
	   		|AlienAvatar -> 
				[]::getValuableGridList(i+1)
		in
		let rec findNearestAs(x,y, nowpos)= (*우리편과 일정거리이하인  비소 찾기 flag, 그때우리편과 거리, 그때비소좌표 *)
			if(x<maxXPos && y<maxYPos) then 
				match cmap.(x).(y) with
				Known Rm -> 
					let md = getMinimumDistance((x,y),avtPosition) in
					if(md<=constantNearestDistanceAs) then
						let (flag, dd, asp) = findNearestAs(x,y+1,nowpos) in
						if(md<dd) then (true,md,(x,y))
						else if(md=dd && distance(nowpos,(x,y)) < distance(nowpos,asp)) then (true,md,(x,y))
						else (flag,dd,asp)
					else (findNearestAs(x,y+1,nowpos))
	(*이번것이 더 가까우면 *)
				| _ -> findNearestAs(x,y+1,nowpos)
			else if(x<maxXPos && y>=maxYPos) then findNearestAs(x+1,0,nowpos)
			else
				(false, 999, (99,99))
		in
		let makeMessage result =  
			let (positions, value) = result in (* (ps " ");(print_coords avtPosition);(ps " "); *)
			let rec loop(i,msg, didc) = (*didc : 이번에 채광 명령을 내린 좌표들 *)
				if(i>=n)then msg
				else(
					let movepos = List.nth positions i in
					let nowpos = List.nth avtPosition i in
					let (x,y) = nowpos in 
					match List.nth avtList i with
					SendAvatar-> 
						if(movepos = (-1,-1)) then 
					(*		if(needNewMap(turn,cmap)) then*) loop((i+1),(MessageList.setMessage msg i (getWhereToCollect(startpos, plist))),didc)
						(*	else loop((i+1),(MessageList.setMessage msg i (getWhereToUpdate(avtPosition, enemyPosition))),didc)*)
						else if( getValueOfGrid(cmap.(x).(y),(0,0),(0,0),0,turn,enemyPosition)>constantCollect && List.mem (x,y) didc = false) then loop(i+1,MessageList.setMessage msg i (collectSolutionString(cmap.(x).(y))),(x,y)::didc)
						else loop(i+1,MessageList.setMessage msg i (getDirection(nowpos,movepos,cmap)),didc) (* 아니면 무브 *)
					|CollectAvatar-> 
						if(getValueOfGrid(cmap.(x).(y),(0,0),(0,0),0,turn,enemyPosition)>constantCollect && List.mem (x,y) didc= false) then loop(i+1,MessageList.setMessage msg i (collectSolutionString(cmap.(x).(y))),(x,y)::didc)
						else if(turn = 0) then loop(i+1,MessageList.setMessage msg i (getDirection(nowpos, (x,y+ ((i mod 2) * 2 -1) ), cmap)), didc) (* 이건 시작위치 바뀌면 수정 *)
						else if(movepos = (-1,-1)) then loop(i+1,MessageList.setMessage msg i "Stay",didc)
						else loop(i+1,MessageList.setMessage msg i (getDirection(nowpos, movepos, cmap)),didc)
					|SmartAvatar->
						if(turn = 0) then loop(i+1,MessageList.setMessage msg i (getDirection(nowpos, (x-1,y), cmap)), didc) 
						else if(getValueOfGrid(cmap.(x).(y),(0,0),(0,0),0,turn,enemyPosition)>constantCollect && List.mem (x,y) didc = false) then loop(i+1,MessageList.setMessage msg i (collectSolutionString(cmap.(x).(y))),(x,y)::didc)
						else if(movepos = nowpos) then loop(i+1,MessageList.setMessage msg i (convertSolutionString(cmap.(x).(y),turn)) ,didc)
						else loop(i+1,MessageList.setMessage msg i (getDirection(nowpos, movepos, cmap)),didc)
					|ExperimentAvatar->
(*						if(turn = 0) then loop(i+1,MessageList.setMessage msg i (getDirection(nowpos, (x-1,y), cmap)), didc) 
						else *)if(reactableGrid(cmap.(x).(y))) then loop(i+1,(MessageList.setMessage msg i "Accelerate"),didc) 
						else loop(i+1,MessageList.setMessage msg i (getDirection(nowpos, List.nth avtPosition 1, cmap)),didc)
					|AlienAvatar ->
						if(turn = 0) then loop(i+1,MessageList.setMessage msg i (getDirection(nowpos, (x+1,y), cmap)), didc)
						else
							let isas (x,y) = 
								if(x>=0 && y>=0 && x<maxXPos && y<maxYPos) then
									if(cmap.(x).(y) = Known Rm) then true
									else false
								else false
							in
							let isblank (x,y) = 
								if(x>=0 && y>=0 && x<maxXPos && y<maxYPos) then
									if(cmap.(x).(y) = Known Bm) then true
									else false
								else false
							in

							let rec getAdjacentAs (li,strli)= (*주변에 우리편이랑 가까운게 있으면 그거 없앤다. *)
								match li with
								[] -> "z"
								|(px,py)::t ->
									let nx = x+px in
									let ny = y+py in
									if(isas(nx,ny) && getMinimumDistance((nx,ny),avtPosition) <= constantNearestDistanceAs) then
										List.hd strli
									else getAdjacentAs(t,List.tl strli)
							in
							let rec getAdjacentBlank (li,strli2)= (*주변에 적이랑 가까운데 잇으면 거기 놓는다 *)
								match li with
								[] -> "z"
								| (px,py)::t ->
									let nx = x+px in
									let ny = y+py in
									if(isblank(nx,ny) && getMinimumDistance((nx,ny),enemyPosition) <= constantNearestDistanceAs) then
										List.hd strli2
									else getAdjacentBlank(t,List.tl strli2)
							in

							let ga = getAdjacentAs([(-1,0);(0,1);(1,0);(0,-1);],["n";"e";"s";"w";]) in
							if(ga="z") then (*가까운게없다*)
								let gb = getAdjacentBlank([(-1,0);(0,1);(1,0);(0,-1);],["n";"e";"s";"w";]) in
								if(gb="z") then (*가까운게 없다 *)
									let (flag,_,asp) = findNearestAs(0,0,(x,y)) in
									if(flag) then (*우리편과 일정거리이내인 비소가있으면 *)
										loop(i+1,MessageList.setMessage msg i (getDirection(nowpos, asp, cmap)),didc) (*거기로감 *)
									else (*주변에 놓을자리 지울자리도없고 우리편주변에 비소도없고 오갈데없는외로운신세가되었다면 적한테간다 *)
										loop(i+1,MessageList.setMessage msg i (getDirection(nowpos, (List.nth enemyPosition 5) ,cmap)),didc)

								else
									loop(i+1,MessageList.setMessage msg i ("a"^gb),didc) (*그 방향에 놓기 *)			
							else
								loop(i+1,MessageList.setMessage msg i ("d"^ga),didc) (*그 방향걸 지우기 *)													
								


				)
			in loop(0, (MessageList.create n), [])
		in
		let rec print_vgg v = 
			match v with 
			[] -> (ps ("EndOfLine"))
			| (a,b,c)::t -> ((print_int a);(print_string " ");(print_int b);(print_string " ");(print_int c);(print_string ", ");(print_vgg t))
		in
		let rec print_vgrid v = 
			match v with
			[] -> ()
			| h::t -> ((print_vgg h);(ps " ");(print_vgrid t))
		in
	(*		let rec print_move f = 
			let (a,b) = f in
			print_coords a
		in *)
		let vgl = getValuableGridList(0) in
		let fm = findMove(0, n, vgl, [], 0) in
(*		(print_vgrid vgl);(print_string "움직임 : ");(print_move fm);*)makeMessage(fm)


	let rec updateosition(avtList, index, pos) =  (* index를 업데이트 *)
		match avtList with
		[] -> [] (*이럴경우없음 *)
		| h::t -> (if (index = 0) then pos::t else h::updateosition(t, index-1, pos))

	let rec print_huffman h = 
		match h with
		[] -> ()
		| (a,b)::t -> ((ps b);(print_huffman t))

	let rec updateEnemyList s = 
		if(s = "") then []
		else (charToInt(s.[1]),charToInt(s.[0]))::updateEnemyList (String.sub s 2 ((String.length s) - 2))

	let run c info = 
		let (n, avtList, avtPosition, turn, cmap, pl, vmap, lastmsg, startpos, enemypos) = c in
			(Random.self_init()); (*(ps "My Turn START!!!!");			*)
(*			try *)
			let rec loop (i,al,plist, enemylist) = (*al : newpoositionlist *)
				if (i>=2*n) then (*(ps "턴");(pi turn);(ps "을 수행중.. 전턴 메세지를 다 받았습니다");*)(al,plist,enemylist)
				else
					if(i<n) then 
						let (x,y) = List.nth al i in
						let lstr = MessageList.getMessage lastmsg i in
						match InfoFromServer.getInfo info i with
						InfoFromServer.Invalid s -> loop(i+1,al,plist,enemylist)
						| InfoFromServer.Moved (mx,my) -> (
							match lstr with 
							"move south" -> (loop(i+1,updateosition(al,i,(x+1,y)),plist,enemylist))
							| "move north" -> (loop(i+1,updateosition(al,i,(x-1,y)),plist,enemylist))
							| "move east" ->(loop(i+1,updateosition(al,i,(x,y+1)),plist,enemylist))
							| "move west" -> (loop(i+1,updateosition(al,i,(x,y-1)),plist,enemylist))
							| _ -> (loop(i+1,al,plist,enemylist)))
						| InfoFromServer.Collected (mg,s) -> ((cmap.(x).(y)<-mapGridToSolutionType(mg));(loop(i+1,al,plist,enemylist)))
						| InfoFromServer.Accelerated mg -> (loop(i+1,al,plist,enemylist))
						| InfoFromServer.Transformed mg ->((cmap.(x).(y)<-mapGridToSolutionType(mg));(loop(i+1,al,plist,enemylist)))
						| InfoFromServer.Sending s -> (* 받은 s로 맵을 업데이트 *)
(*							if(lstr.[0] = 'A') then (*초기 맵 읽어오기 요청이었으면 *)*)
								let huffmanDecoder = makeHuffmanList(String.sub s 0 (numberOfTypeOfSolution *3), 0, typeOfSolution) in
								let rem = charToInt(s.[(numberOfTypeOfSolution * 3)]) in
								let vcl = plist in
								let raw_des = decompressString(String.sub s (numberOfTypeOfSolution * 3+1)  ((String.length s)-(numberOfTypeOfSolution * 3 +1))) in
								let des = String.sub raw_des 0 ((String.length raw_des)-rem) in
(*			*(printMap(0,0,cmap));*)(loop(i+1,al,(huffmanDecode(huffmanDecoder,des, vcl, cmap, vmap)),enemylist))
(*						else (*중간에 맵 업데이트 요청이었으면 앞에 8바이트는 적위치를 나타낸다*)
							let newenemylist = updateEnemyList(String.sub s 0 numberOfEnemy * 2) in 
							let huffmanDecoder = makeHuffmanList(String.sub s (numberOfEnemy * 2) (numberOfTypeOfSolution *2), 0, typeOfSolution) in
							let rem = charToInt(s.[(numberOfTypeOfSolution * 2 + numberOfEnemy * 2)]) in
							let vcl = makePriorityList(makePriorityMatrix(Array.make_matrix maxXPos maxYPos 999, (charToInt(lstr.[1]),charToInt(lstr.[2])))) in
							let raw_des =  decompressString(String.sub s (numberOfTypeOfSolution * 2+1)  ((String.length s)-(numberOfTypeOfSolution * 2 +1))) in
							let des = String.sub raw_des 0 ((String.length raw_des)-rem) in
							let _ = huffmanDecode(huffmanDecoder,des,vcl,cmap,vmap) in
							loop(i+1,al,plist,enemylist)*)
						| InfoFromServer.AddedArsenic (ax,ay)-> ((cmap.(ay).(ax)<-Known Rm);(loop(i+1,al,plist,enemylist)))
						| InfoFromServer.RemovedArsenic (rx,ry) -> ((cmap.(ry).(rx)<-Known Bm);(loop(i+1,al,plist,enemylist)))
	
				else  (* 0 실험 똑똑 채집 채집 외계 전송 *)
(*						((ps "slfkjwelkfjlkwefl");(print_int (i-n));*)let enemynum = i - n in
						let (x,y) = List.nth enemylist enemynum in
						match InfoFromServer.getInfo info i with
						InfoFromServer.Invalid s -> (loop(i+1,al,plist,enemylist))
						| InfoFromServer.Moved (mx,my) -> (loop(i+1,al,plist,updateosition(enemylist,enemynum,(my,mx))))
						| InfoFromServer.Collected (mg,s) -> ((cmap.(x).(y)<-mapGridToSolutionType(mg));(loop(i+1,al,plist,enemylist)))
						| InfoFromServer.Accelerated mg -> ((cmap.(x).(y)<-mapGridToSolutionType(mg));(loop(i+1,al,plist,enemylist)))
						| InfoFromServer.Transformed mg -> ((cmap.(x).(y)<-mapGridToSolutionType(mg));(loop(i+1,al,plist,enemylist)))
						| InfoFromServer.Sending s -> loop(i+1,al,plist,enemylist)
						| InfoFromServer.AddedArsenic (ax,ay)-> ((cmap.(ay).(ax)<-Known Rm);(loop(i+1,al,plist,enemylist)))
						| InfoFromServer.RemovedArsenic (rx,ry) -> ((cmap.(ry).(rx)<-Known Bm);(loop(i+1,al,plist,enemylist)))
(*						| InfoFromServer.Stay ->  (loop(i+1,al,plist,enemylist))) *) 

			in
			let (newPositionList,newplist,newEnemyList) = loop(0,avtPosition, pl,enemypos) in
				((reactOnceAll(cmap));((reactOnceAll(cmap))); (*print_coords(newPositionList); ps("enemy!!"); print_coords(newEnemyList); ps("map!!!!!!)"); ppm(cmap,newPositionList);*)
				let msg = ai(n,turn,avtList,newPositionList,cmap,startpos,newplist,newEnemyList) in
(*					((print_int turn);(print_string "My Turn END!!!!!");*)(n,avtList,newPositionList,turn+1,cmap,newplist,vmap,msg,startpos,newEnemyList),msg)
end
