-module(chat_room_serializer).

%% Translates a `chat_room_broadcast:t()` to fastlane format
-callback fastlane(chat_room_broadcast:t()) -> term().

%% Encodes `chat_room_message:t()` or `chat_room_reply:t()` to transport representation
-callback encode(chat_room_message:t() | chat_room_reply:t()) -> term().

%% Decodes data into `chat_room_broadcast:t()` spec
-callback decode(binary(), [{atom(), term()}]) -> chat_room_message:t().
