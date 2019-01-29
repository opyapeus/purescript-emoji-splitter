module Data.String.EmojiSplitter
  ( splitEmoji
  , splitEmoji'
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.List (List(..), fromFoldable, toUnfoldable, (:))
import Data.String (CodePoint, fromCodePointArray, singleton, toCodePointArray)
import Data.Traversable (traverse)

data EmojiElement
  = RI CodePoint -- RegionalIndicator
  | ZWJ CodePoint -- ZeroWidthJoiner
  | EM CodePoint -- EmojiModifier
  | EVS CodePoint -- EmojiVS
  | EK CodePoint -- EnclosingKeycap
  | T CodePoint -- Tags
  | TT CodePoint -- TermTag
  | E CodePoint -- Emoji

instance showEmojiElement :: Show EmojiElement where
  show (RI a) = "Regional Indicator: " <> show a
  show (ZWJ a) = "Zero Width Joiner: " <> show a
  show (EM a) = "Emoji Modifier: " <> show a
  show (EVS a) = "Emoji VS: " <> show a
  show (EK a) = "Enclosing Keycap: " <> show a
  show (T a) = "Tags: " <> show a
  show (TT a) = "Term Tag: " <> show a
  show (E a) = "Emoji: " <> show a

type ParseResult =
  { parsed :: String
  , remains :: List EmojiElement
  }

type ErrorMsg = String

-- | Split emojis into emoji array
splitEmoji :: String -> Either ErrorMsg (Array String)
splitEmoji = map toUnfoldable <<< splitEmoji'

-- | Split emojis into emoji list
splitEmoji' :: String -> Either ErrorMsg (List String)
splitEmoji' s = do
  let codePoints = toCodePointArray' s
  elems <- traverse codeToElem codePoints
  parse elems

toCodePointArray' :: String -> List CodePoint
toCodePointArray' = fromFoldable <<< toCodePointArray

parse :: List EmojiElement -> Either ErrorMsg (List String)
parse es = case consume es of
  Right { parsed: s, remains: Nil } -> Right $ pure s
  Right { parsed: s, remains: es' } -> Cons <$> Right s <*> parse es'
  Left msg -> Left msg

consume :: List EmojiElement -> Either ErrorMsg ParseResult
-- Regional Indicator
consume ((RI a):(RI b):xs)
  = Right { parsed: fromCodePointArray [a, b], remains: xs }
-- ZWJ Sequence
consume ((E a):(T b):(T c):(T d):xs) = case xs of
  (T e):(T f):(TT g):xs' -> Right { parsed: fromCodePointArray [a, b, c, d, e, f, g], remains: xs' } -- ex: Emoji_Tag_Sequence; England
  _ -> Left $ "next expects Tags"
consume ((E a):(ZWJ b):(E c):(ZWJ d):xs) = case xs of
  (E e):(ZWJ f):(E g):xs' -> Right { parsed: fromCodePointArray [a, b, c, d, e, f, g], remains: xs' } -- ex: family: man, woman, girl, boy
  (E e):xs' -> Right { parsed: fromCodePointArray [a, b, c, d, e], remains: xs' } -- ex: family: man, boy, boy
  _ -> Left $ "next expects Emoji"
consume ((E a):(EM b):(ZWJ c):(E d):xs) = case xs of
  (EVS e):xs' ->Right { parsed: fromCodePointArray [a, b, c, d, e], remains: xs' } -- ex: man health worker: light skin tone
  _ -> Right { parsed: fromCodePointArray [a, b, c, d], remains: xs } -- ex: man farmer: light skin tone
consume ((E a):(ZWJ b):(E c):(EVS d):xs) = case xs of
  (ZWJ e):(E f):(ZWJ g):(E h):xs' -> Right { parsed: fromCodePointArray [a, b, c, d, e, f, g, h], remains: xs' } -- ex: kiss: woman, man
  (ZWJ e):(E f):xs' -> Right { parsed: fromCodePointArray [a, b, c, d, e, f], remains: xs' } -- ex: couple with heart: woman, man
  _ -> Right { parsed: fromCodePointArray [a, b, c, d], remains: xs } -- ex: man health worker
consume ((E a):(EVS b):(ZWJ c):(E d):xs) = case xs of
  (EVS e):xs' -> Right { parsed: fromCodePointArray [a, b, c, d, e], remains: xs' } -- ex: eye in speech bubble
  _ -> Right { parsed: fromCodePointArray [a, b, c, d], remains: xs } -- ex: rainbow flag
consume ((E a):(ZWJ b):(E c):xs) -- ex: man farmer
  = Right { parsed: fromCodePointArray [a, b, c], remains: xs }
-- Sequence
consume ((E a):(EVS b):(EK c):xs) -- ex: keycap
  = Right { parsed: fromCodePointArray [a, b, c], remains: xs }
consume ((E a):(EVS b):xs) -- ex: NUMBER SIGN
  = Right { parsed: fromCodePointArray [a, b], remains: xs }
consume ((E a):(EM b):xs) -- ex: index pointing up: light skin tone
  = Right { parsed: fromCodePointArray [a, b], remains: xs }
consume ((E a):xs) -- ex: grinning face
  = Right { parsed: singleton a, remains: xs }
-- end
consume Nil = Right $ { parsed: "", remains: Nil }
-- not match
consume (e:_) = Left $ "start with " <> show e <> " is not match."

inEq :: Int -> Int -> Int -> Boolean
inEq low high i = low <= i && i <= high

-- Ref: http://unicode.org/reports/tr51/#EBNF_and_Regex
-- Ref: http://unicode.org/reports/tr51/#emoji_data
codeToElem :: CodePoint -> Either ErrorMsg EmojiElement
codeToElem c = f $ fromEnum c
  where
    f :: Int -> Either ErrorMsg EmojiElement
    f i
      | inEq 0X1F1E6 0X1F1FF i = Right $ RI c
      | inEq 0X1F3FB 0X1F3FF i = Right $ EM c
      | 0X200D == i = Right $ ZWJ c
      | 0XFE0E == i = Right $ EVS c -- text presentation
      | 0XFE0F == i = Right $ EVS c -- emoji presentation
      | 0X20E3 == i = Right $ EK c
      | inEq 0XE0020 0XE007E i = Right $ T c
      | 0XE007F == i = Right $ TT c
      | 0X0023 == i = Right $ E c -- number sign
      | 0X002A == i = Right $ E c -- asterisk
      | inEq 0X0030 0X0039 i = Right $ E c -- digit zero..digit nine
      | 0X00A9 == i = Right $ E c -- copyright
      | 0X00AE == i = Right $ E c -- registered
      | 0X203C == i = Right $ E c -- double exclamation mark
      | 0X2049 == i = Right $ E c -- exclamation question mark
      | 0X2122 == i = Right $ E c -- trade mark
      | 0X2139 == i = Right $ E c -- information
      | inEq 0X2194 0X2199 i = Right $ E c -- left-right arrow..down-left arrow
      | inEq 0X21A9 0X21AA i = Right $ E c -- right arrow curving left..left arrow curving right
      | inEq 0X231A 0X231B i = Right $ E c -- watch..hourglass done
      | 0X2328 == i = Right $ E c -- keyboard
      | 0X23CF == i = Right $ E c -- eject button
      | inEq 0X23E9 0X23F3 i = Right $ E c -- fast-forward button..hourglass not done
      | inEq 0X23F8 0X23FA i = Right $ E c -- pause button..record button
      | 0X24C2 == i = Right $ E c -- circled M
      | inEq 0X25AA 0X25AB i = Right $ E c -- black small square..white small square
      | 0X25B6 == i = Right $ E c -- play button
      | 0X25C0 == i = Right $ E c -- reverse button
      | inEq 0X25FB 0X25FE i = Right $ E c -- white medium square..black medium-small square
      | inEq 0X2600 0X2604 i = Right $ E c -- sun..comet
      | 0X260E == i = Right $ E c -- telephone
      | 0X2611 == i = Right $ E c -- ballot box with check
      | inEq 0X2614 0X2615 i = Right $ E c -- umbrella with rain drops..hot beverage
      | 0X2618 == i = Right $ E c -- shamrock
      | 0X261D == i = Right $ E c -- index pointing up
      | 0X2620 == i = Right $ E c -- skull and crossbones
      | inEq 0X2622 0X2623 i = Right $ E c -- radioactive..biohazard
      | 0X2626 == i = Right $ E c -- orthodox cross
      | 0X262A == i = Right $ E c -- star and crescent
      | inEq 0X262E 0X262F i = Right $ E c -- peace symbol..yin yang
      | inEq 0X2638 0X263A i = Right $ E c -- wheel of dharma..smiling face
      | 0X2640 == i = Right $ E c -- female sign
      | 0X2642 == i = Right $ E c -- male sign
      | inEq 0X2648 0X2653 i = Right $ E c -- Aries..Pisces
      | inEq 0X265F 0X2660 i = Right $ E c -- chess pawn..spade suit
      | 0X2663 == i = Right $ E c -- club suit
      | inEq 0X2665 0X2666 i = Right $ E c -- heart suit..diamond suit
      | 0X2668 == i = Right $ E c -- hot springs
      | 0X267B == i = Right $ E c -- recycling symbol
      | inEq 0X267E 0X267F i = Right $ E c -- infinity..wheelchair symbol
      | inEq 0X2692 0X2697 i = Right $ E c -- hammer and pick..alembic
      | 0X2699 == i = Right $ E c -- gear
      | inEq 0X269B 0X269C i = Right $ E c -- atom symbol..fleur-de-lis
      | inEq 0X26A0 0X26A1 i = Right $ E c -- warning..high voltage
      | inEq 0X26AA 0X26AB i = Right $ E c -- white circle..black circle
      | inEq 0X26B0 0X26B1 i = Right $ E c -- coffin..funeral urn
      | inEq 0X26BD 0X26BE i = Right $ E c -- soccer ball..baseball
      | inEq 0X26C4 0X26C5 i = Right $ E c -- snowman without snow..sun behind cloud
      | 0X26C8 == i = Right $ E c -- cloud with lightning and rain
      | 0X26CE == i = Right $ E c -- Ophiuchus
      | 0X26CF == i = Right $ E c -- pick
      | 0X26D1 == i = Right $ E c -- rescue worker’s helmet
      | inEq 0X26D3 0X26D4 i = Right $ E c -- chains..no entry
      | inEq 0X26E9 0X26EA i = Right $ E c -- shinto shrine..church
      | inEq 0X26F0 0X26F5 i = Right $ E c -- mountain..sailboat
      | inEq 0X26F7 0X26FA i = Right $ E c -- skier..tent
      | 0X26FD == i = Right $ E c -- fuel pump
      | 0X2702 == i = Right $ E c -- scissors
      | 0X2705 == i = Right $ E c -- white heavy check mark
      | inEq 0X2708 0X2709 i = Right $ E c -- airplane..envelope
      | inEq 0X270A 0X270B i = Right $ E c -- raised fist..raised hand
      | inEq 0X270C 0X270D i = Right $ E c -- victory hand..writing hand
      | 0X270F == i = Right $ E c -- pencil
      | 0X2712 == i = Right $ E c -- black nib
      | 0X2714 == i = Right $ E c -- heavy check mark
      | 0X2716 == i = Right $ E c -- heavy multiplication x
      | 0X271D == i = Right $ E c -- latin cross
      | 0X2721 == i = Right $ E c -- star of David
      | 0X2728 == i = Right $ E c -- sparkles
      | inEq 0X2733 0X2734 i = Right $ E c -- eight-spoked asterisk..eight-pointed star
      | 0X2744 == i = Right $ E c -- snowflake
      | 0X2747 == i = Right $ E c -- sparkle
      | 0X274C == i = Right $ E c -- cross mark
      | 0X274E == i = Right $ E c -- cross mark button
      | inEq 0X2753 0X2755 i = Right $ E c -- question mark..white exclamation mark
      | 0X2757 == i = Right $ E c -- exclamation mark
      | inEq 0X2763 0X2764 i = Right $ E c -- heavy heart exclamation..red heart
      | inEq 0X2795 0X2797 i = Right $ E c -- heavy plus sign..heavy division sign
      | 0X27A1 == i = Right $ E c -- right arrow
      | 0X27B0 == i = Right $ E c -- curly loop
      | 0X27BF == i = Right $ E c -- double curly loop
      | inEq 0X2934 0X2935 i = Right $ E c -- right arrow curving up..right arrow curving down
      | inEq 0X2B05 0X2B07 i = Right $ E c -- left arrow..down arrow
      | inEq 0X2B1B 0X2B1C i = Right $ E c -- black large square..white large square
      | 0X2B50 == i = Right $ E c -- star
      | 0X2B55 == i = Right $ E c -- heavy large circle
      | 0X3030 == i = Right $ E c -- wavy dash
      | 0X303D == i = Right $ E c -- part alternation mark
      | 0X3297 == i = Right $ E c -- Japanese “congratulations” button
      | 0X3299 == i = Right $ E c -- Japanese “secret” button
      | 0X1F004 == i = Right $ E c -- mahjong red dragon
      | 0X1F0CF == i = Right $ E c -- joker
      | inEq 0X1F170 0X1F171 i = Right $ E c -- A button (blood type)..B button (blood type)
      | 0X1F17E == i = Right $ E c -- O button (blood type)
      | 0X1F17F == i = Right $ E c -- P button
      | 0X1F18E == i = Right $ E c -- AB button (blood type)
      | inEq 0X1F191 0X1F19A i = Right $ E c -- CL button..VS button
      -- | inEq 0X1F1E6 0X1F1FF i = Right $ E c -- regional indicator symbol letter a..regional indicator symbol letter z
      | inEq 0X1F201 0X1F202 i = Right $ E c -- Japanese “here” button..Japanese “service charge” button
      | 0X1F21A == i = Right $ E c -- Japanese “free of charge” button
      | 0X1F22F == i = Right $ E c -- Japanese “reserved” button
      | inEq 0X1F232 0X1F23A i = Right $ E c -- Japanese “prohibited” button..Japanese “open for business” button
      | inEq 0X1F250 0X1F251 i = Right $ E c -- Japanese “bargain” button..Japanese “acceptable” button
      | inEq 0X1F300 0X1F320 i = Right $ E c -- cyclone..shooting star
      | 0X1F321 == i = Right $ E c -- thermometer
      | inEq 0X1F324 0X1F32C i = Right $ E c -- sun behind small cloud..wind face
      | inEq 0X1F32D 0X1F32F i = Right $ E c -- hot dog..burrito
      | inEq 0X1F330 0X1F335 i = Right $ E c -- chestnut..cactus
      | 0X1F336 == i = Right $ E c -- hot pepper
      | inEq 0X1F337 0X1F37C i = Right $ E c -- tulip..baby bottle
      | 0X1F37D == i = Right $ E c -- fork and knife with plate
      | inEq 0X1F37E 0X1F37F i = Right $ E c -- bottle with popping cork..popcorn
      | inEq 0X1F380 0X1F393 i = Right $ E c -- ribbon..graduation cap
      | inEq 0X1F396 0X1F397 i = Right $ E c -- military medal..reminder ribbon
      | inEq 0X1F399 0X1F39B i = Right $ E c -- studio microphone..control knobs
      | inEq 0X1F39E 0X1F39F i = Right $ E c -- film frames..admission tickets
      | inEq 0X1F3A0 0X1F3C4 i = Right $ E c -- carousel horse..person surfing
      | 0X1F3C5 == i = Right $ E c -- sports medal
      | inEq 0X1F3C6 0X1F3CA i = Right $ E c -- trophy..person swimming
      | inEq 0X1F3CB 0X1F3CE i = Right $ E c -- person lifting weights..racing car
      | inEq 0X1F3CF 0X1F3D3 i = Right $ E c -- cricket game..ping pong
      | inEq 0X1F3D4 0X1F3DF i = Right $ E c -- snow-capped mountain..stadium
      | inEq 0X1F3E0 0X1F3F0 i = Right $ E c -- house..castle
      | inEq 0X1F3F3 0X1F3F5 i = Right $ E c -- white flag..rosette
      | 0X1F3F7 == i = Right $ E c -- label
      | inEq 0X1F3F8 0X1F3FA i = Right $ E c -- badminton..
      -- | inEq 0X1F3F8 0X1F3FF i = Right $ E c -- badminton..dark skin tone
      | inEq 0X1F400 0X1F43E i = Right $ E c -- rat..paw prints
      | 0X1F43F == i = Right $ E c -- chipmunk
      | 0X1F440 == i = Right $ E c -- eyes
      | 0X1F441 == i = Right $ E c -- eye
      | inEq 0X1F442 0X1F4F7 i = Right $ E c -- ear..camera
      | 0X1F4F8 == i = Right $ E c -- camera with flash
      | inEq 0X1F4F9 0X1F4FC i = Right $ E c -- video camera..videocassette
      | 0X1F4FD == i = Right $ E c -- film projector
      | 0X1F4FF == i = Right $ E c -- prayer beads
      | inEq 0X1F500 0X1F53D i = Right $ E c -- shuffle tracks button..downwards button
      | inEq 0X1F549 0X1F54A i = Right $ E c -- om..dove
      | inEq 0X1F54B 0X1F54E i = Right $ E c -- kaaba..menorah
      | inEq 0X1F550 0X1F567 i = Right $ E c -- one o’clock..twelve-thirty
      | inEq 0X1F56F 0X1F570 i = Right $ E c -- candle..mantelpiece clock
      | inEq 0X1F573 0X1F579 i = Right $ E c -- hole..joystick
      | 0X1F57A == i = Right $ E c -- man dancing
      | 0X1F587 == i = Right $ E c -- linked paperclips
      | inEq 0X1F58A 0X1F58D i = Right $ E c -- pen..crayon
      | 0X1F590 == i = Right $ E c -- hand with fingers splayed
      | inEq 0X1F595 0X1F596 i = Right $ E c -- middle finger..vulcan salute
      | 0X1F5A4 == i = Right $ E c -- black heart
      | 0X1F5A5 == i = Right $ E c -- desktop computer
      | 0X1F5A8 == i = Right $ E c -- printer
      | inEq 0X1F5B1 0X1F5B2 i = Right $ E c -- computer mouse..trackball
      | 0X1F5BC == i = Right $ E c -- framed picture
      | inEq 0X1F5C2 0X1F5C4 i = Right $ E c -- card index dividers..file cabinet
      | inEq 0X1F5D1 0X1F5D3 i = Right $ E c -- wastebasket..spiral calendar
      | inEq 0X1F5DC 0X1F5DE i = Right $ E c -- clamp..rolled-up newspaper
      | 0X1F5E1 == i = Right $ E c -- dagger
      | 0X1F5E3 == i = Right $ E c -- speaking head
      | 0X1F5E8 == i = Right $ E c -- left speech bubble
      | 0X1F5EF == i = Right $ E c -- right anger bubble
      | 0X1F5F3 == i = Right $ E c -- ballot box with ballot
      | 0X1F5FA == i = Right $ E c -- world map
      | inEq 0X1F5FB 0X1F5FF i = Right $ E c -- mount fuji..moai
      | 0X1F600 == i = Right $ E c -- grinning face
      | inEq 0X1F601 0X1F610 i = Right $ E c -- beaming face with smiling eyes..neutral face
      | 0X1F611 == i = Right $ E c -- expressionless face
      | inEq 0X1F612 0X1F614 i = Right $ E c -- unamused face..pensive face
      | 0X1F615 == i = Right $ E c -- confused face
      | 0X1F616 == i = Right $ E c -- confounded face
      | 0X1F617 == i = Right $ E c -- kissing face
      | 0X1F618 == i = Right $ E c -- face blowing a kiss
      | 0X1F619 == i = Right $ E c -- kissing face with smiling eyes
      | 0X1F61A == i = Right $ E c -- kissing face with closed eyes
      | 0X1F61B == i = Right $ E c -- face with tongue
      | inEq 0X1F61C 0X1F61E i = Right $ E c -- winking face with tongue..disappointed face
      | 0X1F61F == i = Right $ E c -- worried face
      | inEq 0X1F620 0X1F625 i = Right $ E c -- angry face..sad but relieved face
      | inEq 0X1F626 0X1F627 i = Right $ E c -- frowning face with open mouth..anguished face
      | inEq 0X1F628 0X1F62B i = Right $ E c -- fearful face..tired face
      | 0X1F62C == i = Right $ E c -- grimacing face
      | 0X1F62D == i = Right $ E c -- loudly crying face
      | inEq 0X1F62E 0X1F62F i = Right $ E c -- face with open mouth..hushed face
      | inEq 0X1F630 0X1F633 i = Right $ E c -- anxious face with sweat..flushed face
      | 0X1F634 == i = Right $ E c -- sleeping face
      | inEq 0X1F635 0X1F640 i = Right $ E c -- dizzy face..weary cat face
      | inEq 0X1F641 0X1F642 i = Right $ E c -- slightly frowning face..slightly smiling face
      | inEq 0X1F643 0X1F644 i = Right $ E c -- upside-down face..face with rolling eyes
      | inEq 0X1F645 0X1F64F i = Right $ E c -- person gesturing NO..folded hands
      | inEq 0X1F680 0X1F6C5 i = Right $ E c -- rocket..left luggage
      | inEq 0X1F6CB 0X1F6CF i = Right $ E c -- couch and lamp..bed
      | 0X1F6D0 == i = Right $ E c -- place of worship
      | inEq 0X1F6D1 0X1F6D2 i = Right $ E c -- stop sign..shopping cart
      | inEq 0X1F6E0 0X1F6E5 i = Right $ E c -- hammer and wrench..motor boat
      | 0X1F6E9 == i = Right $ E c -- small airplane
      | inEq 0X1F6EB 0X1F6EC i = Right $ E c -- airplane departure..airplane arrival
      | 0X1F6F0 == i = Right $ E c -- satellite
      | 0X1F6F3 == i = Right $ E c -- passenger ship
      | inEq 0X1F6F4 0X1F6F6 i = Right $ E c -- kick scooter..canoe
      | inEq 0X1F6F7 0X1F6F8 i = Right $ E c -- sled..flying saucer
      | 0X1F6F9 == i = Right $ E c -- skateboard
      | inEq 0X1F910 0X1F918 i = Right $ E c -- zipper-mouth face..sign of the horns
      | inEq 0X1F919 0X1F91E i = Right $ E c -- call me hand..crossed fingers
      | 0X1F91F == i = Right $ E c -- love-you gesture
      | inEq 0X1F920 0X1F927 i = Right $ E c -- cowboy hat face..sneezing face
      | inEq 0X1F928 0X1F92F i = Right $ E c -- face with raised eyebrow..exploding head
      | 0X1F930 == i = Right $ E c -- pregnant woman
      | inEq 0X1F931 0X1F932 i = Right $ E c -- breast-feeding..palms up together
      | inEq 0X1F933 0X1F93A i = Right $ E c -- selfie..person fencing
      | inEq 0X1F93C 0X1F93E i = Right $ E c -- people wrestling..person playing handball
      | inEq 0X1F940 0X1F945 i = Right $ E c -- wilted flower..goal net
      | inEq 0X1F947 0X1F94B i = Right $ E c -- 1st place medal..martial arts uniform
      | 0X1F94C == i = Right $ E c -- curling stone
      | inEq 0X1F94D 0X1F94F i = Right $ E c -- lacrosse..flying disc
      | inEq 0X1F950 0X1F95E i = Right $ E c -- croissant..pancakes
      | inEq 0X1F95F 0X1F96B i = Right $ E c -- dumpling..canned food
      | inEq 0X1F96C 0X1F970 i = Right $ E c -- leafy green..smiling face with 3 hearts
      | inEq 0X1F973 0X1F976 i = Right $ E c -- partying face..cold face
      | 0X1F97A == i = Right $ E c -- pleading face
      | inEq 0X1F97C 0X1F97F i = Right $ E c -- lab coat..woman’s flat shoe
      | inEq 0X1F980 0X1F984 i = Right $ E c -- crab..unicorn face
      | inEq 0X1F985 0X1F991 i = Right $ E c -- eagle..squid
      | inEq 0X1F992 0X1F997 i = Right $ E c -- giraffe..cricket
      | inEq 0X1F998 0X1F9A2 i = Right $ E c -- kangaroo..swan
      | inEq 0X1F9B0 0X1F9B9 i = Right $ E c -- red-haired..supervillain
      | 0X1F9C0 == i = Right $ E c -- cheese wedge
      | inEq 0X1F9C1 0X1F9C2 i = Right $ E c -- cupcake..salt
      | inEq 0X1F9D0 0X1F9E6 i = Right $ E c -- face with monocle..socks
      | inEq 0X1F9E7 0X1F9FF i = Right $ E c -- red envelope..nazar amulet
      | otherwise = Left $ "can not convert " <> show c <> " to emoji element."
