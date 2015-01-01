module type IMap =
  sig
    type t
    type sol = S | K | I | V of string | A of sol * sol
    type grid = Nothing | Sol of sol
    val height : t -> int
    val width : t -> int
    val get : t -> int -> int -> grid
    val numOfAvatars : t -> int
    val locOfAvatar : t -> int -> int * int
    val getAcid : t -> int -> bool
    val setAcid : t -> int -> bool -> t
    val set : t -> int -> int -> grid -> t
    val setAvatar : t -> int -> int -> int -> t
    val make : int -> int -> int -> int * int -> int -> int * int -> t
    val swap : t -> t
  end
module Map : IMap
module type IInfoFromServer =
  sig
    type t
    type cmdResult =
        Invalid of string
      | Moved of int * int
      | Collected of Map.grid * string
      | Accelerated of Map.grid
      | Transformed of Map.grid
      | Sending of string
      | AddedArsenic of int * int
      | RemovedArsenic of int * int
    val turn : t -> int
    val create : int -> int -> t
    val setInfo : t -> int -> cmdResult -> t
    val getInfo : t -> int -> cmdResult
  end
module InfoFromServer : IInfoFromServer
module type IMessageList =
  sig
    type msg = string
    type msgList
    val create : int -> msgList
    val setMessage : msgList -> int -> msg -> msgList
    val getMessage : msgList -> int -> msg
  end
module MessageList : IMessageList
type avatarType =
    SendAvatar
  | CollectAvatar
  | SmartAvatar
  | ExperimentAvatar
  | AlienAvatar
type direction = North | East | West | South
type whereToCollect = Self | Left | Right
type side = L | R
type cmd =
    Move of direction
  | Collect of whereToCollect
  | Accelerate
  | Transform of side list * Map.sol
  | Send
  | AddArsenic of direction
  | RemoveArsenic of direction
  | Stay
module type IAvatar =
  sig
    val run : avatarType -> MessageList.msg -> cmd
    val createMessage : MessageList.msg -> Map.t -> string
  end
module type ICenter =
  sig
    type center
    val make : avatarType list -> int -> int * int -> center
    val run : center -> InfoFromServer.t -> center * MessageList.msgList
  end
