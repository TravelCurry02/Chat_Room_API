-module(chat_room_serializer).

-callback fastlane(chat_room_broadcast:t()) -> term().

-callback encode(chat_room_message:t() | chat_room_reply:t()) -> term().

-callback decode(binary(), [{atom(), term()}]) -> chat_room_message:t().
