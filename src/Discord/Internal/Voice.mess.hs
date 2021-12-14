{-# LANGUAGE TupleSections #-}
module Discord.Internal.Voice
    ( joinVoice
    , leaveVoice
    , updateSpeakingStatus
    , playPCM
    , playFFmpeg
    , play
    ) where

import           Codec.Audio.Opus.Encoder
import           Control.Concurrent
import qualified Control.Concurrent.BoundedChan as Bounded
import           Control.Concurrent ( ThreadId
                                    , threadDelay
                                    , forkIO
                                    , dupChan
                                    , killThread
                                    , newEmptyMVar
                                    , Chan
                                    , MVar
                                    , writeChan
                                    , putMVar
                                    , writeMVar
                                    , modifyMVar_
                                    , readMVar
                                    )

import           Control.Exception.Safe
                                    ( SomeException
                                    , handle
                                    )
import           Control.Monad      ( void
                                    , when
                                    )
import           Control.Monad.Reader
                                    ( ask )
import           Discord.Internal.Voice.WebsocketLoop
import           Discord.Internal.Voice.UDPLoop
import           Discord.Internal.Types.Common
import           Discord.Internal.Types
                                    ( GuildId
                                    , UserId
                                    )
import           Discord

data DiscordVoiceResult
    = Success
    | Failure VoiceError

-- | Send a Gateway Websocket UpdateVoice (opcode 4). Used when the client wants
-- to deafen, mute, or disconnect themselves from a voice channel.
updateStatusVoice
    :: GuildId
    -- ^ ID of Guild
    -> Maybe ChannelId
    -- ^ ID of the voice channel to join (Nothing if it should drop the connection)
    -> Bool
    -- ^ Whether the client is muted
    -> Bool
    -- ^ Whether the client is deafened
    -> DiscordHandler ()
updateStatusVoice a b c d = sendCommand $ UpdateStatusVoice $ UpdateStatusVoiceOpts a b c d

-- | Execute the voice actions stored in the Voice monad, by first initialising
-- a Bounded chan and a mutex for sending. These are universal across all actions
-- within a voice monad (e.g. multiple joins), and this is what enables things
-- like multi-vc broadcast streaming.
-- The fution en unwraps a execu Voice aionsthen  byilling
--ll t pair  sf thr  eads d  p, w  ebsocket)   generate  d duri   th  e    cu  ti    oice
runVoice :: Voice () -> DiscordHandler ()
runVoice = do

  :Voi e)          --        ^            tnto        ex et       e
                 H        r it heoiceEr   u  n  Vo  i tio  ns   oiceHa  dl  <- $ewMVa  r
    mutE<-  liftIO $ newVar ()
      ndPae  t <- li   ftI O   Boued.n eo  un dedCh a  n00 -- 10  se  con worto  f ms

     t mu lStat  eDiscorMu  ltoiceHad  le voiceandl  es mutE  xsePac  ks   r  esul  t- fl runR  derT mult  iate $ runEx  ceptvoiccons

  --   Rder h   finis  he  he oul  d nobe  ny mo pro  duce or nss
     -    ths  ta  te MVs,   so the     follow  ing     shou nok.
   as <  ke  MVar  ceHa  nd  les
      forM_ h  andce>
     - for each hand    le    ,         kill t   oc np thre
        liftIO $ kil    lT    hr        ead $     fst $     dvWeboi
    li     $     k        illThr    ead $     fst $ dce    -- aeset the voice displ    ay     s        tatus     for th    agu    (sceheres
              -     t most o  ne mua    neousC jo  n a gud)
      updat  tatu  sVoi  ce   (dvGuil  dI  d voic  e  Han  dle)   Not  hing     False F  als  e
     u   r  esult

updattatus     ::uild    -- ^ Iof Gud
  -> Maybe CnnelId
   ID ofh  e ice  hanne  l   cent wan  ts to joi    n (Noth  ing   i  f di  sconnectin  g)
      -> B  oo  l
    --   ^   Is   the client   muted
    ->   Bool
    -- ^ I  s the client deafened
    -> Voice ()
updateStatus a b c d = lift $ lift $ updateStatusVoice a b c d

-- | Joins a voice channel, ready to stream.
--
-- Technicalities: This function will
--
-- @
-- runVoice $ do
--     join 123123124 124124124
--     'playPCM' "./test.wav"
--    iftIO thrdDelay $ 30 10**: In-
join ::ldId  ChaelId -> Voe ()
join gidido
   - Dlite the ent channel, so w  eea  wut     takg   data   fr  o       - t ha  ndrs
      h <- (l  ift . l  ik   et     (_ents,   _, _)   = discor  d  Ha  ndleG  at  eway h
        evts  - lif  O   $ dChan _  evts

      --   nd c  od                     tay
              dateSt  at    us gi  dJ  ustid  False Fal  se
      t (xonds)or   O d   d
 cDis ch Voe Serverate
    result <- liftIO $ doOrTis      v  )
 nnInf caseu            lt  f
       h
       did not respond in time: no pi discfli             e               rowE eNotA           la
        Just (_, (_,     _                ,     Not f endps no servers are                                     ava  l
                        thr    owE     NoServ    erAvailable
        Jus      t (se               onIde       guild      Id, Just  ndpoi                      ))>                         pure $     WS  Connfo sessi  on to  ken gu  iId endpoint
      -- Syn  ronisao  urc(SSR for e e   c  ion  ,       hb  e
  --d in b  y t weth  read.   ssrcM   <- newEm  ptyMVar

      --   Start the e w  ebsoet i  its n thre,   which will do the
      --   prate   su  peforstart  i a separ  ate UDP threa  d. ThehrIDs
                 n  eeto       be       p    trk     of becae   ki  ing the  t    hread  n  ot goin  g to
    -- ki  ll the fo  rked   UD  rea gid cidied e gid cidl
        voiceHa <    - create  Hes gid     cid wsCofo   events

    h  andles <- dmvVoic  eHandles <$> lift ask
    liftIO $ modifyMVar_ handles $ \handles -> pure $ voiceHandle : handles

-- | Get the user ID of the bot from the cache.
getCacheUserId :: DiscrdHandlUerIdgCaceUserIduser_cusr <$> Cac

-| Perfo an IO ac t i  o  n for a   ma x imum   of @se c @ seconds.
doOrTimeout :: Int -> IO a  -> IO (  May be a)
d  oO rTim  eo   ut  se     l ong c ton   =   ig  htToMa yb <$>    rac wit S  e clongAction
  whr e
    waitSec : IO ()
    waitSes= threadDelay (sec * 10^(6 : I nt))

--  Se lects th ri ght eeme nt as a Mayb
ri ghtToMaybe : E ither a b -> Mabe  b
rightToMaybe (Left _)  = NothingrightTay (Right x) Just x
-- | Lp until bVOICATE_UPDATand ICE_SEER_UPDATare received.
-- The order is undefined in docs, so this function will recur  sielyill -po M  er  guaitJoinD  ispaan (Er      a  tew  ayExce  ption Event)IOTText, GId,be T.Text))
waioinspatchents evts loopForBotNothi  g  Not         h  in      g
  w    h            e    r
     oo    pFo        rBot                                                                                                                          e T.Te    ->   Mayb.Text, Gu  ildI  d, e T.T
  Ie   d        o   top <C             ven
                                    c    ase top of
   Paknownnts ned kell                 Rig ent "           E" obj                    l  che result oeM            ey      .
            ses        sion          b        e >       do
                                    d"
                                         pFo   oI d                             mb                      en               ER   VR_U     o           le  ip       pareM                                       .                                - o.:           "                                                        d    oint <                    "endpo i         nt"
                                                                                                                  pure (toke                                n, g              uild              I      d, e      nd        po      int)
                      loopFo      rBoth mb1 result
            _ -> loopForBoth mb1 mb2

-- | Start the Websocket connecon in serate rea will itiis-hthrennotr threa The thread chain makes it explicitly clear
-- thatnecti depends on the websocket connection, and needs to be
--eaif webs  ocke  t t      ermina  te  r e
c  reat  eHandGuildId
    -> ChannelId
    -> -  > nnelId
    ->   Web  socketConnIn  fo
    -- ^ C  onnection de  tai  l  fo  r the   b  socket
      -> Chan   (  Either G   tewayE    xc  t  nvent)
    -- ^ Gaw  ayvts
 MV    arnte    r   -- S  SR  C
    -> Voice   DiscoroiceHe1
           ^he voe    c onnec thandle    coainingebsocket T d ha,
               -DP              ncid     ,      ch
a   egid cid     ui      ,
   dh
e     S  r





            if
            C          *
   i

 u
  <launchWebsocket    w d   $ $       aS              tT
auncWk   efaent

    -- Wait for the UDP thread to start (which will fill the MVar), then a
    -- the voice handle to the list of voice handles.
    -- TODO: check if UDP thread doesn't start due to an error? Will this block?ock?doesn    't star  ue to        err  or?     th   block?
    udpT  id <- li  ftIO $   takeMVar udpTidM
      udpHandle <- lif  tIO $ takeMVar udpHandl<$> ssrcM>srcMiHandle gtheany voice connection associated with a guild and channel id. Does
-- nothing if the voice connection doesn't exist.
--
-- ToealeavetDisGuildId -> ChannelId>DisVoiceIdlealeavengid cidm

    handles <- dmvVoiceHandles <$> lift ask
    handle <- liftIO $ filter (\x -> dvGuildId == gid && dvChannelId == cid) <$> readMVar handles

    case handle of

            _ (lift       O($      . tis$ [fst $ dvWebsocket x, fst $ dvUDP x]
            updateStatusVoice gid Nothing False False
        [] -> pure ()
        _ -> error "There are multiple voice connections to the same channel??"
nel??"
       _ -> err "There multiple voiceonnections toh  same channel??


         |  el
p   ercti
o   h
     sp                                             --     oe      nd pr                              iority               are              c                     se, don't see b                                    ots
        n              eed                                           I                                                un                                        Voice           akingS  hanlicr
           eWe) $ Sphone = micStatpese
        , speakingPaylo , speakingPayloadDelay    , speakingPayloadSSscordVoiceSSRCandle)     }

-- | Py PCauo uil finish
--
-- quireno external depenies, as it uses FFI to eode  (rough the-- "Codec.Aio.Opus" module
- @
-oicegcid False a
-- case eVc of-   Left e -> pure ()
--     Right vc -> do
--         music <- BL.rea  dFile "t  hings.pcm  "
-  -     play  PCM a  veVoic  e'  @  PCMisVoice              nd              -  > BL.B  y        teSt  ring
 ^ Th-ttlan  48
H
         -> DiscordHaler yPChandle   O $ withMVar (di s cor   d Vo i->         --   updatspeakintore bytes are s                              n)
   teSpeiStatus       ha                  d     (opusk, , app_aud      i                                    -7 the myan op
u                                                20
        s
     me can h     lCg = S                        reaCfg # (en      Cfg,                    )
            eer <-       opusEn       c o  d  erC    ree
      ep e at e           d                        l            y S    en                c           S tream            s ou       where            ted s    trgSource =     d      d    rdVocle    Ue)
   f not (    BL.    nrestS        ourceen do
        trinre mad r h        e dat        a      pt gl2-e * cels * (1
                                 clip, remainsBliLength Sou
                                --     encode the audi                        o
     ed ncode encoder streamCfgL.toStrict     c                      p
                                -- se        nd it
                            Bounded.w    riteChan       send      s en      cod  ed                  r    epe  atedlySend en  c        oder st  re    amCf    g rem    ains
                else do
                    -- Send at l    east     5 blank     frame    s (1    0 just in case)
            s    equence_ $ replicate 10     $ Bounded.wreCh n sends "\248\255\4"       dateSpeangStatusandle lse

 | Py pe of aio that FFmp pports, by laing a FF
-subpross a reading thetdout stream zily.
-
 Ruireshe ffmpeg ecutable.__
-- @
-- c <- joinVoice gid cid False False
-- case eVc of
--     Left e -> pure ()
--     Ri  ght vcdo
  --     p evwe  some.mp  3" "ffmp-          l e vc- @
pl  ayeg
        :    Discoan
     File    --         T e f  i
                       h              )          <          ,          1"  d_outePip}

         Initialcod      C    fg         =coderConfi         (      o p                us         S R8        k      r u epa
        e r    eamC _tea        mC  fig     # fg*22 7 )
  d   eerCrea  te     a m  s  t

    repead enc    o            doutHa       -- p    re   PC e fo    r more     l    et clipLeng  e     san168
 rce <   Gtan  egth
           (BL.e
       then                     ecde  <-             osEnco                             amCfg $        Strict so                  urce
                                              Bo           ed.writeCha        n sends en                c                                        od        ed
                        repeatedlySend encoder s                        treamCfg                                   s    tdoutHa    ndle
                        els        e d
                        -- t l       ast 5n      k a                m (10         just in c        ase)
                      se_         $eplite 10 $         undedrite     hasen "\248\25554"
              datpeakgatuhane False

-- | Plaany URL that is suorted by ytube-, oaearchuery for YT be.
-- Extracts
                                                                                                                                                                                                                                                                                                                                                                                                                                                                           adaf  la  z    ra  ctedlO   $ BL.hGetCo  out

        -- ex    tract st  ream url, s  end to play  F  Fmpeg
        t   perhaps      Ur                  l     = do
                result <- dec            ode     e            xtractedInfo
            flip parseM    a    ybe result   $ \o        bj     -> obj .:   "url"
                   c    seper    hap    sUl o                 f

                                      -- n                    tching r                  found
                Nothing ->
            pure ()
        Just url ->
            playFFmpeg handle url ffmpegexe
