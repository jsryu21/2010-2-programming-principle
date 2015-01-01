open Server
open Server

module Avatar1: IAvatar =
struct
	(* 본부에서 받아온 메시지를 해석해서 cmd로 바꿔 주는 함수 *)	
	let _ = Random.self_init ()
	let run avt msg = 
		match Random.int 3 with
		| 0 ->
		begin
			match Random.int 4 with
			| 0 -> Move East
			| 1 -> Move East
			| 2 -> Move South
			| 3 -> Move South
			| _ -> Move South
		end
		| _ -> (Collect Self)
	
	(* 전송 아바타가 맵을 만들어서 본부에 보낼때 쓰이는 함수 *)
	let createMessage msg map = "!"
end
