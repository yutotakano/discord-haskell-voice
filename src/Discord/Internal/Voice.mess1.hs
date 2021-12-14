{-# LANGUAGE TupleSections #-}
module Discord.Internal.Voice
  ( joinVoice   , leaveVoice    , updateSpeakingStatu     , playP      , playFFm       , play        , DiscordVoiceHandl         ) where

import           Codec.Audio.Opus.Encoder
import           Contro.Asyncncncn    Ch n nrrcececece
                                   ,,MVifsedpimportoroncurrentententen ThreadId
Id
Id
I  T     Id, threadDelaylaylaylay
                         uddupCn n  , killTTreTdeTdehdead
                                                , forkIO
                       ikkillTlreldlreTdr    e C  n  n  nhan
                      ennew              dupdupdupdupChan
                     mpEmEEmptyMVtrMVyrV    Mr r Ch nh nhwnhan
                    VnnwwMVMr           r ddChanhanhanhan
                   uppuVMV r           wrireChenhenhenhan
                  ehCeadChanhanann           MVa Va Va Var
                  rraad       n    nnnwwEmptyptyptyptyMVar
                htthrrDdDelaylayayy          n wMV rV rV rVar
               PttyyPutt                    eadeadeadeadMVar
              iwwihh                      pu pu pu,putMVar
             rerrCeeCenn                      MV rVtrVhrVar
            )
import   )
      Conimol.Concrrrent.Async       ( iace ) import qualified Control.Concurrent.BoundedChan
   Conmporen ,.  yP pMVao.renCedonC
          as Boundeds Boundeds Bounded Bounde   )   )  )  )
import  .Exception.Safe                         ( SomeException
                                                , handle
                                                )
imporrl                                                       vvoidddtrol..ReaderMo.Readern.Readd.ask  ask ask( ask wwhen                             liftIOiftIOftIOiftIO                               ..Reader    .Reader aaskol.Monad                        when when when wh llfftIOIOIOO                            vo d vo d vo d v                                                    )
import           Data.A       Dat.                                                                                          mport qData.B.Lt                                                                                                                xtTport           System.Process                                 ( Cce.                                                             trc eatePreaesstePrccesseatePr eessatePr  ess       p       p     pp                            ccceeatePratessePratessero
                      rHnnda.Inte  yp sDisco.I(ncord.InHTndyCr p s    .Intern  .Typ s             ( GuildId  ( dI eaaCacheeCacheadCacheea
Cache                  Use I    e   ndCommandoCdnand                          U  r(..)      r.)mimportandle  ,, ,Discord.HandleCChannelIda      ( discordHandle       (cor scordHascordHand, ndSendaSendable(..)b    le(.discoraH ndl Leg     UpdtUpuVtUpsptOStatusVticeOpts(..)(      .VnEvimportEventtew   Discord.Internal.Gateway.Cache ( Cache(..)                                 he.( Cache(  Cache(.pawayExp ( GatewayExcp ( GatewayExceetion(..)c  ti(oGatewayException(..)n(..)e( GatewayException(..) Gtion((.Typ s            h nnhlIdl             Id              GatewaI.Cach       , Event(UnknownEvent)
 Icha(..)aent(Unnt)
    )inda),iscoDiscS)
impo,tscord.HSendable(..)aoandlerd.Handle  Disc(rdiscordHandled.Ha(ddl    e Gu lGI
                 d scor Han, UpdateStatusVoiceOpts(..)
leLog rUpusVoiceOptsHan leLotusVoie.po(.,)Ui)
i,pUtsc(..)Drdordiscord      Discord      ( Di(cordHandl Dr   U drI rr
                                  ndCommannd )iimportcord.InternDiscord.Internal.Types.Common
importal.Types.C,Discord.Internal.Voice.UDPLoop
importoreadCachemDiscord.Internal.Voice.WebsocketLoop                  ortal.Typesportmmon,
ropachem    )ireadCacher )Disc      , rea)                       )

data DiscordVoiceResult
    = Success
    | Failure VoiceError

-- | Send a Gateway Websocket Update Voice State commandOpcod4). Usewhen
-- thclient wantto in, me, or disnnect   from a v  oice channel  .
updateS  tatusVoice
      :: GuildId
    -- ^  of Gld
   > Maybe ChaelId
    -- ^ Iof e voi channel ient   wants to   join (Nhing if sconnecti)
      -> Bo  ol
    -- ^   Is
  the
 client
   muted

   -> Bool
    -- ^   Is the client deened
    -> DcordHandler ()
uateStatusVoice a b c d = sendCommand $ UpdateStatusVoice $ UpdateStatusVoiceOpts a b c d

-- | Execute the voice actions stored in the Voice monad, by first initialising
-- a Bounded chan and a mutex for sending. These are universal across all actions
-- within a voice monad (e.g. multiple joins), and this is what enables things
-- like multi-vc broadcast streaming.
-- The function then unwraps a execu Voice aions, then es by killing
--ll t pairsf threads dp, websocket)   generated   during the   executio
ruoice
  :: Voi  ce)
    -- ^   tionto        exet       e
          >iscordH        andler it he  r VoiceEr ro())
ru  n  Voice  tio  ns = doiceHan  dl   <- $ewMVa  r
    mutE<-  liftIO $ newVar ()
    sendPackets <- liftIO Boued.n eo  un dedCh a  n00 -- 10  se  conds worth of 20ms

    t mulState DiscordMu  ltoiceHad  le voiceandl  es mutExsendPackets
    result- fl runR  derT mult  iate $ runEx  ceptT voiceActions

  -- Rder h finished,here shoul  d nobe  ny mo pro  duce or consumers
   -  thstate MVs,   so the   following   should not blo  ck.
   as <  keMVarceHandles
      forM_ h  andles $ \  voice>
     - for each handle, kill t    he we    bsoc np thre
        liftIO $ killThread $     fst $     dvWebsoc    ket voi
    li $ killThr    ead $     fst $ dv    UDP voice    -- aeset the voice display status     for th    at guild     (sceheres
      -- at most o  ne mua    neousC jo    in in a gud)
      updattatusVoice (dvGuil  dId voice  Handle) Not  hing   False Fals  e

    pure r  esult

updattatus     :: Guild
      -- ^ Iof Gud
  -> Maybe CnnelId
    -- ^ ID ofhe icehannel cent wan  ts to joi  n (Nothing   if disconnectin  g)
    -> B  ool
    -- ^   Is the client   muted
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
--     liftIO $ threadDelay $ 30 10**(6: Int)- @
join ::ldId  ChaelId -> Voe ()
join gid cid = do
   - Dlite the ent channel, so we can reawut takg   data fr  om
    -- t ha  ndrs
      h <- (l  ift . lift) ask et (_ents,   _, _)   = discord  HandleGateway h
      events   <- lifO $ dChan _  evts

      -- Send op  code inatay
              dateSt  at    us gid (J  ust cid) False False
    -Wait (x 5 se  conds)or O de 0patc oice d
    -cod  e 0 Dis ch Voe Serverate
    result <- liftIO $ doOrTimJpents     e  vents)
 nnInf caseu            lt of
   h    i    >
            -- did not respond in time: no pi discfli            ne?
      rowE eNotA           lable
        Just (_, (_, _    ,     Not    - If endpoint is null, no servers are             ava   bl
            thr    owE     NoServ    erAvailable
        Just (sessionIde       guild      Id, Just       endpoi))>
                          pure $     WS  Connfo session to  ken guildId endpoint

    -- Synronisao  n Sourc(SSRC)   for e voice conn  ection  , whichbe
    --d in b  y t weth  read.   ssrcM   <- newEmptyMVar

    -- Start the voice w  ebsoet i  its n thre,   which will do the
    -- appropriate supeforstart  i a separ  ate UDP threa  d. The thread IDs
   neeto     be     pt     track       of becae   kiing the  t  hread  not going to
    -- ki  ll the forked   UDP thread (veri  fied experimentally).
      voiceHa <  - createHes gid   cid wsCofo   events

    h  andles <- dmvVoic  eHandles <$> lift ask
    liftIO $ modifyMVar_ handles $ \handles -> pure $ voiceHandle : handles

-- | Get the user ID of the bot from the cache.
getCacheUserId :: DiscordHandler UserId
getCaceUserIduserI _curreUsr <$> Cac

-| Perfo an IO action for a   ma x imum   of @se c @ seconds.
doOrTimeout :: Int -> IO a -> IO (Maybe a)
doOrTimeout sec longAc tion =   ri ghtToMa ybe  <$>    rac  wait S  e  c longAction
  whr e
    waitSec : IO ()
    waitSes= threadDelay (sec * 10^(6 :: Int))

-- | Selects the right element as a Mayb
ri ghtToMaybe : E ither a b -> Mabe  b
rightToMaybe (Left _)  = Nothing
rightToMaybe (Right x) Just x
-- | Lp until bo VOICE_STATE_UPDATand ICE_SEER_UPDATare received.
-- The order is undefined in docs, so this function will recursively fill
--up wo Merguts unl   b  aitJoinD  ispatch:an (Either Ga  tew  ayExce  ption Event)IOTexT.Text, GuildId, Maybe T.Text))
waioinspatchents evts = loopForBoth Nothi  ng  Not    hin  g
  whe    r
       oopForBot    h
                    e T.Te    -> Mayb.Text, GuildI  d, e T.Text)
          -> IO (T.Text, (T.T  uildIde T.Te  x   loopForBus  tJust mb          do   top <Chan             events
                c    ase top of
   PaknownEwhichnts ned by diskell.                        Right ent "VOUPDATE" obj                    ly, we che result oeMaybe         -- ba      ck rey.
                let ses        sionId a        rseMaybe > do
                            d"
                             pForB           oI d mb2
            Unkn     ownEven ERVER_UPDATE" o               le ip parseM        aybe                              .                               dId <- o.: " g         uild_                                          endp          oint <- o .: "endpoint"
                                                      pure (token, g        uild        Id, end        po      int)
                      loopFo      rBoth mb1 result
            _ -> loopForBoth mb1 mb2

-- | Start the Websocket connection in a separate threa whi will itiis
--heDP thrennotr threa The thread chain makes it explicitly clear
-- that the Unecti depends on the websocket connection, and needs to be
-- teaif the websocket terminate  s rte  r re  ason.
c  reat  eHandle  s
      :: GuildI  d
    -> ChannelId
    -> WebsocketConnIn  fo
    -- ^ C  onnection detai  ls for the websocket
    -> Chan (  Either GatewayE  xception Event)
      -- ^ Gaw  ayvents
  -> MV  arnter   -- S  SR  C
    -> Voice   DiscordVoiceHandle1
     ^he voe    c   onnec   tn handle    coainingebsocket T and handle,
   -DP T      a     nd h     ane, cha     el a g     ui
l   d        ,
    adh
e   S  r     ea
es gi  d c  id ogatewvndle <- (,


           )
      <

$





        liftIO




               nC     ha*
     l

     i       ftIO
C

     ud
p       TidM <  - liftn   udpHandleM <-
l
i           ftIO   nEmpr
 scltIO $

e          war

    d $ $   lipaSta   ntT
auncWk     etcfo aentss     ndle uid log (udpTidM, udpHaneM)

  -- Wt for the P th  read to sta (w  hich will fill   the MVar), then add
    -- the voic  e handle to the   list of voice handles.
    -- TODO: check i  f UDP tad doesn  't starue to an   err  or?ll this block?
    udpTid <- li  ftIO $   takeMVar udpTidM
      udpHandle <- lif  tIO $ takeMVar udpHandleM
    DiscordVoiceHandle gid cid (wsTid, wsHandle) (udpTid, udpHandle) <$> ssrcM


-- | Leave any voice connection associated with a guild and channel id. Does
-- nothing if the voice connection dsn't exis
--
-- To lea all channels and
-- mon ad
.    `runV
o   ic  e` takes care of killing all handles that still exist
    when
-- existing. This nction is only for bscribing certainels
-- mid-broadcast.. This nctionly for bscribing certainels
-- mid-bcast.
leavuil  dId -> Cha ->   Voice ()
le =d handlscends-  $  filter (\x -> dvGuildId == gid && dvChannl<$> readMVar handl  es

    case handle             of
              [x      ]       -> do
            mapM_ (liftIO . killThread) $ [fst $       dvWebsocke      t x, fst       $ dvUDP x]
                updateSt        tusVoice gid         Nohing False Fals    e
       [] -> pure ()
        _ -> error "There are multiple voiceonnections tohe same channel??

-- | Helper functi
o   n  update
t   h
e    speaking
cat
o   r fort  ot.
--
--
S  oe
a   nd pr              iority are cnal              se, don't see b              ots n              eeding the              m              .
-- If and wh              en r                                            equired, add Bool signatures to this                         fun
        ction.
updateSpeakingStatus :                   Voice            Handle
        -> Bool -> I              akingS  hanlicStatus =
    wr
 (snd $ ndl              eWe) $ Speaking specrophone = micStatus
        , spese
        , speakingPaylo , speakingPayloadDelay    , speakingPayloadSSscordVoiceSSRC handle)
        }

-- | Py a PCM audio uil finish
--
-- quireno external depenies, as it uses FFI to eode opus (through the-- "Codec.Aio.Opus" module
--
-- @
-- eVc<nVoicegcid False a
-- case eVc of-   Left e -> pure ()
--     Right vc -> do
--         music <- BL.readFile "things.pcm"
--         playPCM us     ea  veVoic  e'  @
playPCMisVoiceHa            nd        -> BL.By      teString
    -- ^ Th-ttlan   ste  reoat 48kHz.
    -> DiscordHa
n   dler yPChandle sou
    liftIO $ withMVar (discor   d Vo i ceMut->       -- update the speaking tds ten before bytes are s                  n)
    teSpeingStatus handl                  e enCfgderCo # (opusk, True, app_aud      i            o)
    -7 the mytes an op
u
      s

           20m      s    me can h
        let enStreamCfg = SreaCfg # (en      Cfg,            )
        eer <-       opusEn      cod      erCree
    repeated      l        y S    en            code r    e n   S tream        s ou     r c
  where
   ea    tedlySend encod    er streg restSource = do          le d    rdVocle    Ue)
   f not (    BL.nrestS    ource) then do
        ytestrinre mad  of r () th    e dat    a is 1t so
    -- mply b    y two to t    h   gtam  llh20*2*2- ee * cels * (1
                        let (clip, remainsBlitAtpLength Source
                        -- encode the audi            o
     ed <usEncode encoder streamCfg $ BL.toStrict cli            p
                -- se        nd it
            Bounded.w    riteChan     send    s encoded
            repe  atedlySend en  c    oder st  reamCf    g remains
            else do
                -- Send at least     5 blank frame    s (10 just in case)
            s    equence_ $ replicate 10     $ Bounded.writeCh    an sends "\248\255\4"
            dateSpeangStatusandle lse

-- | Playny pe of aio that FFmp pports, by laching a FFmpeg
-subpross a reading thetdout stream lazily.
-
-- Requireshe ffmpeg ecutable.__
-- @
-- c <- joinVoice gid cid False False
-- case eVc of
--     Left e -> pure ()
--     Right vc -> do
--     plmpeg vwesome.mp3" "ffmpe  --      l eVoi' vc
-  - @
playeg
      ::   Discoan
    ->   FilePa      --   ^ The file to pl
a                                              y










                                                                                                -> String







                           -   T


                                                      F
    dle                       hd ph     ) <     [,"s   , "    -     1"
     {     st    d_out eatePip}

         Initialcoder
  Cfg =coderConfig #        (      o p         u s S R48     k ,  T r u epa
    let e    SreamC _tea    mConfig     # fg*227    6)
       o    deerCreate en   e  a mCfg   s    tdout

  where
    repeatedlySend encodoutHa
    et rdVleUDP handle)     -- p    re   h the s playPC e for more in      let clipLeng   = 2- rme     san  168
 rce <     L.GtsdoutHand   Length
       (BL.n    ul  ce)
    then           do
                          encoded <-         osEncode enco        der streamCfg $ BL.toStrict source
                                Bo        unded.writeChan sends en        c                oded
                repeatedlySend encoder streamCfg                   s    tdoutHa    ndle
                else do
          -- nd at l       ast 5n        k fra        m (10 just in case)
                sequen_         $eplite 10 $         Boundedrite      Chan sen "\248\25554"
                datpeakingatus hane False

-- | Plaany URL that is supported by ytube-, or aearch query for YTube.
-- Extracts the stream
                                                      URL

                                                                  u                                              sing





                                                                                                             "yo  ut
                                                                                                                                                  }

   read   af it   lazi  ly
    ext  ractedIn  fo <- liftIO $ BL.hGetContents std  out

      -- ex  tract stream url, send to play  FFmpeg
      let perhaps    Ur        l = do
            result <- decode e            xtractedInfo
            flip parseMaybe result   $ \o  bj   -> obj .:   "url"
        case perhapsUrl o    f
           -- n    om    atching rl
           found

              Nothing ->
            pure ()
        Just url ->
            playFFmpeg handle url ffmpegexe
