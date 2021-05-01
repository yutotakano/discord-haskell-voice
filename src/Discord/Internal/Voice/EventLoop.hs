module Discord.Internal.Voice.EventLoop where

import qualified Data.Text as T
import           Wuss               ( runSecureClient )

data VoiceWebsocketException = VoiceWebsocketCouldNotConnect T.Text
                             | VoiceWebsocketEventParseError T.Text
                             | VoiceWebsocketUnexpected VoiceWebsocketReceivable T.Text
                             | VoiceWebsocketConnection ConnectionException T.Text
                             deriving (Show)


type DiscordHandleVoice = (Chan (Either VoiceWebsocketException Event), Chan VoiceWebsocketSendable)
