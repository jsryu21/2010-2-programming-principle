open Server
open Server

module Avatar1: IAvatar =
struct
	(* ���ο��� �޾ƿ� �޽����� �ؼ��ؼ� cmd�� �ٲ� �ִ� �Լ� *)	
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
	
	(* ���� �ƹ�Ÿ�� ���� ���� ���ο� ������ ���̴� �Լ� *)
	let createMessage msg map = "!"
end
