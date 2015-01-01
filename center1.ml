open Server

module Center1: ICenter =
struct
	type center = int * (Server.avatarType list) * int
	
	(* Center.make에는 아바타들의 리스트와, 총 몇 턴인지, 시작위치를 알려준다 *)
	let make lst maxTurn startLoc = (List.length lst, lst, maxTurn)
	
	let run c info =
		let (n, avtList, turn) = c in
		( (n, avtList, turn+1), (MessageList.create n) )
end	
