-module(ebus_serializer).

-callback fastlane(ebus_broadcast:t()) -> term().

-callback encode(ebus_message:t() | ebus_reply:t()) -> term().

-callback decode(binary(), [{atom(), term()}]) -> ebus_message:t().
