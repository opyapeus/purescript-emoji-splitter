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
      | inEq 0x1F1E6 0x1F1FF i = Right $ RI c
      | inEq 0x1F3FB 0x1F3FF i = Right $ EM c
      | 0x200D == i = Right $ ZWJ c
      | 0xFE0E == i = Right $ EVS c -- text presentation
      | 0xFE0F == i = Right $ EVS c -- emoji presentation
      | 0x20E3 == i = Right $ EK c
      | inEq 0xE0020 0xE007E i = Right $ T c
      | 0xE007F == i = Right $ TT c
      | 0x0023 == i = Right $ E c -- number sign
      | 0x002A == i = Right $ E c -- asterisk
      | inEq 0x0030 0x0039 i = Right $ E c -- digit zero..digit nine
      | 0x00A9 == i = Right $ E c -- copyright
      | 0x00AE == i = Right $ E c -- registered
      | 0x203C == i = Right $ E c -- double exclamation mark
      | 0x2049 == i = Right $ E c -- exclamation question mark
      | 0x2122 == i = Right $ E c -- trade mark
      | 0x2139 == i = Right $ E c -- information
      | inEq 0x2194 0x2199 i = Right $ E c -- left-right arrow..down-left arrow
      | inEq 0x21A9 0x21AA i = Right $ E c -- right arrow curving left..left arrow curving right
      | inEq 0x231A 0x231B i = Right $ E c -- watch..hourglass done
      | 0x2328 == i = Right $ E c -- keyboard
      | 0x23CF == i = Right $ E c -- eject button
      | inEq 0x23E9 0x23F3 i = Right $ E c -- fast-forward button..hourglass not done
      | inEq 0x23F8 0x23FA i = Right $ E c -- pause button..record button
      | 0x24C2 == i = Right $ E c -- circled M
      | inEq 0x25AA 0x25AB i = Right $ E c -- black small square..white small square
      | 0x25B6 == i = Right $ E c -- play button
      | 0x25C0 == i = Right $ E c -- reverse button
      | inEq 0x25FB 0x25FE i = Right $ E c -- white medium square..black medium-small square
      | inEq 0x2600 0x2604 i = Right $ E c -- sun..comet
      | 0x260E == i = Right $ E c -- telephone
      | 0x2611 == i = Right $ E c -- ballot box with check
      | inEq 0x2614 0x2615 i = Right $ E c -- umbrella with rain drops..hot beverage
      | 0x2618 == i = Right $ E c -- shamrock
      | 0x261D == i = Right $ E c -- index pointing up
      | 0x2620 == i = Right $ E c -- skull and crossbones
      | inEq 0x2622 0x2623 i = Right $ E c -- radioactive..biohazard
      | 0x2626 == i = Right $ E c -- orthodox cross
      | 0x262A == i = Right $ E c -- star and crescent
      | inEq 0x262E 0x262F i = Right $ E c -- peace symbol..yin yang
      | inEq 0x2638 0x263A i = Right $ E c -- wheel of dharma..smiling face
      | 0x2640 == i = Right $ E c -- female sign
      | 0x2642 == i = Right $ E c -- male sign
      | inEq 0x2648 0x2653 i = Right $ E c -- Aries..Pisces
      | inEq 0x265F 0x2660 i = Right $ E c -- chess pawn..spade suit
      | 0x2663 == i = Right $ E c -- club suit
      | inEq 0x2665 0x2666 i = Right $ E c -- heart suit..diamond suit
      | 0x2668 == i = Right $ E c -- hot springs
      | 0x267B == i = Right $ E c -- recycling symbol
      | inEq 0x267E 0x267F i = Right $ E c -- infinity..wheelchair symbol
      | inEq 0x2692 0x2697 i = Right $ E c -- hammer and pick..alembic
      | 0x2699 == i = Right $ E c -- gear
      | inEq 0x269B 0x269C i = Right $ E c -- atom symbol..fleur-de-lis
      | inEq 0x26A0 0x26A1 i = Right $ E c -- warning..high voltage
      | inEq 0x26AA 0x26AB i = Right $ E c -- white circle..black circle
      | inEq 0x26B0 0x26B1 i = Right $ E c -- coffin..funeral urn
      | inEq 0x26BD 0x26BE i = Right $ E c -- soccer ball..baseball
      | inEq 0x26C4 0x26C5 i = Right $ E c -- snowman without snow..sun behind cloud
      | 0x26C8 == i = Right $ E c -- cloud with lightning and rain
      | 0x26CE == i = Right $ E c -- Ophiuchus
      | 0x26CF == i = Right $ E c -- pick
      | 0x26D1 == i = Right $ E c -- rescue worker’s helmet
      | inEq 0x26D3 0x26D4 i = Right $ E c -- chains..no entry
      | inEq 0x26E9 0x26EA i = Right $ E c -- shinto shrine..church
      | inEq 0x26F0 0x26F5 i = Right $ E c -- mountain..sailboat
      | inEq 0x26F7 0x26FA i = Right $ E c -- skier..tent
      | 0x26FD == i = Right $ E c -- fuel pump
      | 0x2702 == i = Right $ E c -- scissors
      | 0x2705 == i = Right $ E c -- white heavy check mark
      | inEq 0x2708 0x2709 i = Right $ E c -- airplane..envelope
      | inEq 0x270A 0x270B i = Right $ E c -- raised fist..raised hand
      | inEq 0x270C 0x270D i = Right $ E c -- victory hand..writing hand
      | 0x270F == i = Right $ E c -- pencil
      | 0x2712 == i = Right $ E c -- black nib
      | 0x2714 == i = Right $ E c -- heavy check mark
      | 0x2716 == i = Right $ E c -- heavy multiplication x
      | 0x271D == i = Right $ E c -- latin cross
      | 0x2721 == i = Right $ E c -- star of David
      | 0x2728 == i = Right $ E c -- sparkles
      | inEq 0x2733 0x2734 i = Right $ E c -- eight-spoked asterisk..eight-pointed star
      | 0x2744 == i = Right $ E c -- snowflake
      | 0x2747 == i = Right $ E c -- sparkle
      | 0x274C == i = Right $ E c -- cross mark
      | 0x274E == i = Right $ E c -- cross mark button
      | inEq 0x2753 0x2755 i = Right $ E c -- question mark..white exclamation mark
      | 0x2757 == i = Right $ E c -- exclamation mark
      | inEq 0x2763 0x2764 i = Right $ E c -- heavy heart exclamation..red heart
      | inEq 0x2795 0x2797 i = Right $ E c -- heavy plus sign..heavy division sign
      | 0x27A1 == i = Right $ E c -- right arrow
      | 0x27B0 == i = Right $ E c -- curly loop
      | 0x27BF == i = Right $ E c -- double curly loop
      | inEq 0x2934 0x2935 i = Right $ E c -- right arrow curving up..right arrow curving down
      | inEq 0x2B05 0x2B07 i = Right $ E c -- left arrow..down arrow
      | inEq 0x2B1B 0x2B1C i = Right $ E c -- black large square..white large square
      | 0x2B50 == i = Right $ E c -- star
      | 0x2B55 == i = Right $ E c -- heavy large circle
      | 0x3030 == i = Right $ E c -- wavy dash
      | 0x303D == i = Right $ E c -- part alternation mark
      | 0x3297 == i = Right $ E c -- Japanese “congratulations” button
      | 0x3299 == i = Right $ E c -- Japanese “secret” button
      | 0x1F004 == i = Right $ E c -- mahjong red dragon
      | 0x1F0CF == i = Right $ E c -- joker
      | inEq 0x1F170 0x1F171 i = Right $ E c -- A button (blood type)..B button (blood type)
      | 0x1F17E == i = Right $ E c -- O button (blood type)
      | 0x1F17F == i = Right $ E c -- P button
      | 0x1F18E == i = Right $ E c -- AB button (blood type)
      | inEq 0x1F191 0x1F19A i = Right $ E c -- CL button..VS button
      -- | inEq 0x1F1E6 0x1F1FF i = Right $ E c -- regional indicator symbol letter a..regional indicator symbol letter z
      | inEq 0x1F201 0x1F202 i = Right $ E c -- Japanese “here” button..Japanese “service charge” button
      | 0x1F21A == i = Right $ E c -- Japanese “free of charge” button
      | 0x1F22F == i = Right $ E c -- Japanese “reserved” button
      | inEq 0x1F232 0x1F23A i = Right $ E c -- Japanese “prohibited” button..Japanese “open for business” button
      | inEq 0x1F250 0x1F251 i = Right $ E c -- Japanese “bargain” button..Japanese “acceptable” button
      | inEq 0x1F300 0x1F320 i = Right $ E c -- cyclone..shooting star
      | 0x1F321 == i = Right $ E c -- thermometer
      | inEq 0x1F324 0x1F32C i = Right $ E c -- sun behind small cloud..wind face
      | inEq 0x1F32D 0x1F32F i = Right $ E c -- hot dog..burrito
      | inEq 0x1F330 0x1F335 i = Right $ E c -- chestnut..cactus
      | 0x1F336 == i = Right $ E c -- hot pepper
      | inEq 0x1F337 0x1F37C i = Right $ E c -- tulip..baby bottle
      | 0x1F37D == i = Right $ E c -- fork and knife with plate
      | inEq 0x1F37E 0x1F37F i = Right $ E c -- bottle with popping cork..popcorn
      | inEq 0x1F380 0x1F393 i = Right $ E c -- ribbon..graduation cap
      | inEq 0x1F396 0x1F397 i = Right $ E c -- military medal..reminder ribbon
      | inEq 0x1F399 0x1F39B i = Right $ E c -- studio microphone..control knobs
      | inEq 0x1F39E 0x1F39F i = Right $ E c -- film frames..admission tickets
      | inEq 0x1F3A0 0x1F3C4 i = Right $ E c -- carousel horse..person surfing
      | 0x1F3C5 == i = Right $ E c -- sports medal
      | inEq 0x1F3C6 0x1F3CA i = Right $ E c -- trophy..person swimming
      | inEq 0x1F3CB 0x1F3CE i = Right $ E c -- person lifting weights..racing car
      | inEq 0x1F3CF 0x1F3D3 i = Right $ E c -- cricket game..ping pong
      | inEq 0x1F3D4 0x1F3DF i = Right $ E c -- snow-capped mountain..stadium
      | inEq 0x1F3E0 0x1F3F0 i = Right $ E c -- house..castle
      | inEq 0x1F3F3 0x1F3F5 i = Right $ E c -- white flag..rosette
      | 0x1F3F7 == i = Right $ E c -- label
      | inEq 0x1F3F8 0x1F3FA i = Right $ E c -- badminton..
      -- | inEq 0x1F3F8 0x1F3FF i = Right $ E c -- badminton..dark skin tone
      | inEq 0x1F400 0x1F43E i = Right $ E c -- rat..paw prints
      | 0x1F43F == i = Right $ E c -- chipmunk
      | 0x1F440 == i = Right $ E c -- eyes
      | 0x1F441 == i = Right $ E c -- eye
      | inEq 0x1F442 0x1F4F7 i = Right $ E c -- ear..camera
      | 0x1F4F8 == i = Right $ E c -- camera with flash
      | inEq 0x1F4F9 0x1F4FC i = Right $ E c -- video camera..videocassette
      | 0x1F4FD == i = Right $ E c -- film projector
      | 0x1F4FF == i = Right $ E c -- prayer beads
      | inEq 0x1F500 0x1F53D i = Right $ E c -- shuffle tracks button..downwards button
      | inEq 0x1F549 0x1F54A i = Right $ E c -- om..dove
      | inEq 0x1F54B 0x1F54E i = Right $ E c -- kaaba..menorah
      | inEq 0x1F550 0x1F567 i = Right $ E c -- one o’clock..twelve-thirty
      | inEq 0x1F56F 0x1F570 i = Right $ E c -- candle..mantelpiece clock
      | inEq 0x1F573 0x1F579 i = Right $ E c -- hole..joystick
      | 0x1F57A == i = Right $ E c -- man dancing
      | 0x1F587 == i = Right $ E c -- linked paperclips
      | inEq 0x1F58A 0x1F58D i = Right $ E c -- pen..crayon
      | 0x1F590 == i = Right $ E c -- hand with fingers splayed
      | inEq 0x1F595 0x1F596 i = Right $ E c -- middle finger..vulcan salute
      | 0x1F5A4 == i = Right $ E c -- black heart
      | 0x1F5A5 == i = Right $ E c -- desktop computer
      | 0x1F5A8 == i = Right $ E c -- printer
      | inEq 0x1F5B1 0x1F5B2 i = Right $ E c -- computer mouse..trackball
      | 0x1F5BC == i = Right $ E c -- framed picture
      | inEq 0x1F5C2 0x1F5C4 i = Right $ E c -- card index dividers..file cabinet
      | inEq 0x1F5D1 0x1F5D3 i = Right $ E c -- wastebasket..spiral calendar
      | inEq 0x1F5DC 0x1F5DE i = Right $ E c -- clamp..rolled-up newspaper
      | 0x1F5E1 == i = Right $ E c -- dagger
      | 0x1F5E3 == i = Right $ E c -- speaking head
      | 0x1F5E8 == i = Right $ E c -- left speech bubble
      | 0x1F5EF == i = Right $ E c -- right anger bubble
      | 0x1F5F3 == i = Right $ E c -- ballot box with ballot
      | 0x1F5FA == i = Right $ E c -- world map
      | inEq 0x1F5FB 0x1F5FF i = Right $ E c -- mount fuji..moai
      | 0x1F600 == i = Right $ E c -- grinning face
      | inEq 0x1F601 0x1F610 i = Right $ E c -- beaming face with smiling eyes..neutral face
      | 0x1F611 == i = Right $ E c -- expressionless face
      | inEq 0x1F612 0x1F614 i = Right $ E c -- unamused face..pensive face
      | 0x1F615 == i = Right $ E c -- confused face
      | 0x1F616 == i = Right $ E c -- confounded face
      | 0x1F617 == i = Right $ E c -- kissing face
      | 0x1F618 == i = Right $ E c -- face blowing a kiss
      | 0x1F619 == i = Right $ E c -- kissing face with smiling eyes
      | 0x1F61A == i = Right $ E c -- kissing face with closed eyes
      | 0x1F61B == i = Right $ E c -- face with tongue
      | inEq 0x1F61C 0x1F61E i = Right $ E c -- winking face with tongue..disappointed face
      | 0x1F61F == i = Right $ E c -- worried face
      | inEq 0x1F620 0x1F625 i = Right $ E c -- angry face..sad but relieved face
      | inEq 0x1F626 0x1F627 i = Right $ E c -- frowning face with open mouth..anguished face
      | inEq 0x1F628 0x1F62B i = Right $ E c -- fearful face..tired face
      | 0x1F62C == i = Right $ E c -- grimacing face
      | 0x1F62D == i = Right $ E c -- loudly crying face
      | inEq 0x1F62E 0x1F62F i = Right $ E c -- face with open mouth..hushed face
      | inEq 0x1F630 0x1F633 i = Right $ E c -- anxious face with sweat..flushed face
      | 0x1F634 == i = Right $ E c -- sleeping face
      | inEq 0x1F635 0x1F640 i = Right $ E c -- dizzy face..weary cat face
      | inEq 0x1F641 0x1F642 i = Right $ E c -- slightly frowning face..slightly smiling face
      | inEq 0x1F643 0x1F644 i = Right $ E c -- upside-down face..face with rolling eyes
      | inEq 0x1F645 0x1F64F i = Right $ E c -- person gesturing NO..folded hands
      | inEq 0x1F680 0x1F6C5 i = Right $ E c -- rocket..left luggage
      | inEq 0x1F6CB 0x1F6CF i = Right $ E c -- couch and lamp..bed
      | 0x1F6D0 == i = Right $ E c -- place of worship
      | inEq 0x1F6D1 0x1F6D2 i = Right $ E c -- stop sign..shopping cart
      | inEq 0x1F6E0 0x1F6E5 i = Right $ E c -- hammer and wrench..motor boat
      | 0x1F6E9 == i = Right $ E c -- small airplane
      | inEq 0x1F6EB 0x1F6EC i = Right $ E c -- airplane departure..airplane arrival
      | 0x1F6F0 == i = Right $ E c -- satellite
      | 0x1F6F3 == i = Right $ E c -- passenger ship
      | inEq 0x1F6F4 0x1F6F6 i = Right $ E c -- kick scooter..canoe
      | inEq 0x1F6F7 0x1F6F8 i = Right $ E c -- sled..flying saucer
      | 0x1F6F9 == i = Right $ E c -- skateboard
      | inEq 0x1F910 0x1F918 i = Right $ E c -- zipper-mouth face..sign of the horns
      | inEq 0x1F919 0x1F91E i = Right $ E c -- call me hand..crossed fingers
      | 0x1F91F == i = Right $ E c -- love-you gesture
      | inEq 0x1F920 0x1F927 i = Right $ E c -- cowboy hat face..sneezing face
      | inEq 0x1F928 0x1F92F i = Right $ E c -- face with raised eyebrow..exploding head
      | 0x1F930 == i = Right $ E c -- pregnant woman
      | inEq 0x1F931 0x1F932 i = Right $ E c -- breast-feeding..palms up together
      | inEq 0x1F933 0x1F93A i = Right $ E c -- selfie..person fencing
      | inEq 0x1F93C 0x1F93E i = Right $ E c -- people wrestling..person playing handball
      | inEq 0x1F940 0x1F945 i = Right $ E c -- wilted flower..goal net
      | inEq 0x1F947 0x1F94B i = Right $ E c -- 1st place medal..martial arts uniform
      | 0x1F94C == i = Right $ E c -- curling stone
      | inEq 0x1F94D 0x1F94F i = Right $ E c -- lacrosse..flying disc
      | inEq 0x1F950 0x1F95E i = Right $ E c -- croissant..pancakes
      | inEq 0x1F95F 0x1F96B i = Right $ E c -- dumpling..canned food
      | inEq 0x1F96C 0x1F970 i = Right $ E c -- leafy green..smiling face with 3 hearts
      | inEq 0x1F973 0x1F976 i = Right $ E c -- partying face..cold face
      | 0x1F97A == i = Right $ E c -- pleading face
      | inEq 0x1F97C 0x1F97F i = Right $ E c -- lab coat..woman’s flat shoe
      | inEq 0x1F980 0x1F984 i = Right $ E c -- crab..unicorn face
      | inEq 0x1F985 0x1F991 i = Right $ E c -- eagle..squid
      | inEq 0x1F992 0x1F997 i = Right $ E c -- giraffe..cricket
      | inEq 0x1F998 0x1F9A2 i = Right $ E c -- kangaroo..swan
      | inEq 0x1F9B0 0x1F9B9 i = Right $ E c -- red-haired..supervillain
      | 0x1F9C0 == i = Right $ E c -- cheese wedge
      | inEq 0x1F9C1 0x1F9C2 i = Right $ E c -- cupcake..salt
      | inEq 0x1F9D0 0x1F9E6 i = Right $ E c -- face with monocle..socks
      | inEq 0x1F9E7 0x1F9FF i = Right $ E c -- red envelope..nazar amulet
      | otherwise = Left $ "can not convert " <> show c <> " to emoji element."
