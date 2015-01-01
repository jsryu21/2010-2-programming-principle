open Server

module Center1: ICenter =
struct
	type center = int * (Server.avatarType list) * int
	
	(* Center.make���� �ƹ�Ÿ���� ����Ʈ��, �� �� ������, ������ġ�� �˷��ش� *)
	let make lst maxTurn startLoc = (List.length lst, lst, maxTurn)
	
	let run c info =
		let (n, avtList, turn) = c in
		( (n, avtList, turn+1), (MessageList.create n) )
end	
