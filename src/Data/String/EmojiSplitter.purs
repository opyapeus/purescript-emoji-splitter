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

-- NOTE: http://unicode.org/reports/tr51/#EBNF_and_Regex
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

-- NOTE: http://www.unicode.org/Public/emoji/12.0/emoji-data.txt
codeToElem :: CodePoint -> Either ErrorMsg EmojiElement
codeToElem c = f $ fromEnum c
  where
    f :: Int -> Either ErrorMsg EmojiElement
    f i
      | inEq 0x1F3FB 0x1F3FF i = Right $ EM c
      | 0x200D == i = Right $ ZWJ c
      | 0xFE0E == i = Right $ EVS c -- text presentation
      | 0xFE0F == i = Right $ EVS c -- emoji presentation
      | 0x20E3 == i = Right $ EK c
      | inEq 0xE0020 0xE007E i = Right $ T c
      | 0xE007F == i = Right $ TT c
      -- # All omitted code points have Emoji=No 
      -- # @missing: 0000..10FFFF  ; Emoji ; No
      | (==) 0x0023 i = Right $ E c
      | (==) 0x002A i = Right $ E c
      | inEq 0x0030 0x0039 i = Right $ E c
      | (==) 0x00A9 i = Right $ E c
      | (==) 0x00AE i = Right $ E c
      | (==) 0x203C i = Right $ E c
      | (==) 0x2049 i = Right $ E c
      | (==) 0x2122 i = Right $ E c
      | (==) 0x2139 i = Right $ E c
      | inEq 0x2194 0x2199 i = Right $ E c
      | inEq 0x21A9 0x21AA i = Right $ E c
      | inEq 0x231A 0x231B i = Right $ E c
      | (==) 0x2328 i = Right $ E c
      | (==) 0x23CF i = Right $ E c
      | inEq 0x23E9 0x23F3 i = Right $ E c
      | inEq 0x23F8 0x23FA i = Right $ E c
      | (==) 0x24C2 i = Right $ E c
      | inEq 0x25AA 0x25AB i = Right $ E c
      | (==) 0x25B6 i = Right $ E c
      | (==) 0x25C0 i = Right $ E c
      | inEq 0x25FB 0x25FE i = Right $ E c
      | inEq 0x2600 0x2604 i = Right $ E c
      | (==) 0x260E i = Right $ E c
      | (==) 0x2611 i = Right $ E c
      | inEq 0x2614 0x2615 i = Right $ E c
      | (==) 0x2618 i = Right $ E c
      | (==) 0x261D i = Right $ E c
      | (==) 0x2620 i = Right $ E c
      | inEq 0x2622 0x2623 i = Right $ E c
      | (==) 0x2626 i = Right $ E c
      | (==) 0x262A i = Right $ E c
      | inEq 0x262E 0x262F i = Right $ E c
      | inEq 0x2638 0x263A i = Right $ E c
      | (==) 0x2640 i = Right $ E c
      | (==) 0x2642 i = Right $ E c
      | inEq 0x2648 0x2653 i = Right $ E c
      | inEq 0x265F 0x2660 i = Right $ E c
      | (==) 0x2663 i = Right $ E c
      | inEq 0x2665 0x2666 i = Right $ E c
      | (==) 0x2668 i = Right $ E c
      | (==) 0x267B i = Right $ E c
      | inEq 0x267E 0x267F i = Right $ E c
      | inEq 0x2692 0x2697 i = Right $ E c
      | (==) 0x2699 i = Right $ E c
      | inEq 0x269B 0x269C i = Right $ E c
      | inEq 0x26A0 0x26A1 i = Right $ E c
      | inEq 0x26AA 0x26AB i = Right $ E c
      | inEq 0x26B0 0x26B1 i = Right $ E c
      | inEq 0x26BD 0x26BE i = Right $ E c
      | inEq 0x26C4 0x26C5 i = Right $ E c
      | (==) 0x26C8 i = Right $ E c
      | (==) 0x26CE i = Right $ E c
      | (==) 0x26CF i = Right $ E c
      | (==) 0x26D1 i = Right $ E c
      | inEq 0x26D3 0x26D4 i = Right $ E c
      | inEq 0x26E9 0x26EA i = Right $ E c
      | inEq 0x26F0 0x26F5 i = Right $ E c
      | inEq 0x26F7 0x26FA i = Right $ E c
      | (==) 0x26FD i = Right $ E c
      | (==) 0x2702 i = Right $ E c
      | (==) 0x2705 i = Right $ E c
      | inEq 0x2708 0x2709 i = Right $ E c
      | inEq 0x270A 0x270B i = Right $ E c
      | inEq 0x270C 0x270D i = Right $ E c
      | (==) 0x270F i = Right $ E c
      | (==) 0x2712 i = Right $ E c
      | (==) 0x2714 i = Right $ E c
      | (==) 0x2716 i = Right $ E c
      | (==) 0x271D i = Right $ E c
      | (==) 0x2721 i = Right $ E c
      | (==) 0x2728 i = Right $ E c
      | inEq 0x2733 0x2734 i = Right $ E c
      | (==) 0x2744 i = Right $ E c
      | (==) 0x2747 i = Right $ E c
      | (==) 0x274C i = Right $ E c
      | (==) 0x274E i = Right $ E c
      | inEq 0x2753 0x2755 i = Right $ E c
      | (==) 0x2757 i = Right $ E c
      | inEq 0x2763 0x2764 i = Right $ E c
      | inEq 0x2795 0x2797 i = Right $ E c
      | (==) 0x27A1 i = Right $ E c
      | (==) 0x27B0 i = Right $ E c
      | (==) 0x27BF i = Right $ E c
      | inEq 0x2934 0x2935 i = Right $ E c
      | inEq 0x2B05 0x2B07 i = Right $ E c
      | inEq 0x2B1B 0x2B1C i = Right $ E c
      | (==) 0x2B50 i = Right $ E c
      | (==) 0x2B55 i = Right $ E c
      | (==) 0x3030 i = Right $ E c
      | (==) 0x303D i = Right $ E c
      | (==) 0x3297 i = Right $ E c
      | (==) 0x3299 i = Right $ E c
      | (==) 0x1F004 i = Right $ E c
      | (==) 0x1F0CF i = Right $ E c
      | inEq 0x1F170 0x1F171 i = Right $ E c
      | (==) 0x1F17E i = Right $ E c
      | (==) 0x1F17F i = Right $ E c
      | (==) 0x1F18E i = Right $ E c
      | inEq 0x1F191 0x1F19A i = Right $ E c
      | inEq 0x1F1E6 0x1F1FF i = Right $ RI c -- regional indicator
      | inEq 0x1F201 0x1F202 i = Right $ E c
      | (==) 0x1F21A i = Right $ E c
      | (==) 0x1F22F i = Right $ E c
      | inEq 0x1F232 0x1F23A i = Right $ E c
      | inEq 0x1F250 0x1F251 i = Right $ E c
      | inEq 0x1F300 0x1F320 i = Right $ E c
      | (==) 0x1F321 i = Right $ E c
      | inEq 0x1F324 0x1F32C i = Right $ E c
      | inEq 0x1F32D 0x1F32F i = Right $ E c
      | inEq 0x1F330 0x1F335 i = Right $ E c
      | (==) 0x1F336 i = Right $ E c
      | inEq 0x1F337 0x1F37C i = Right $ E c
      | (==) 0x1F37D i = Right $ E c
      | inEq 0x1F37E 0x1F37F i = Right $ E c
      | inEq 0x1F380 0x1F393 i = Right $ E c
      | inEq 0x1F396 0x1F397 i = Right $ E c
      | inEq 0x1F399 0x1F39B i = Right $ E c
      | inEq 0x1F39E 0x1F39F i = Right $ E c
      | inEq 0x1F3A0 0x1F3C4 i = Right $ E c
      | (==) 0x1F3C5 i = Right $ E c
      | inEq 0x1F3C6 0x1F3CA i = Right $ E c
      | inEq 0x1F3CB 0x1F3CE i = Right $ E c
      | inEq 0x1F3CF 0x1F3D3 i = Right $ E c
      | inEq 0x1F3D4 0x1F3DF i = Right $ E c
      | inEq 0x1F3E0 0x1F3F0 i = Right $ E c
      | inEq 0x1F3F3 0x1F3F5 i = Right $ E c
      | (==) 0x1F3F7 i = Right $ E c
      | inEq 0x1F3F8 0x1F3FF i = Right $ E c
      | inEq 0x1F400 0x1F43E i = Right $ E c
      | (==) 0x1F43F i = Right $ E c
      | (==) 0x1F440 i = Right $ E c
      | (==) 0x1F441 i = Right $ E c
      | inEq 0x1F442 0x1F4F7 i = Right $ E c
      | (==) 0x1F4F8 i = Right $ E c
      | inEq 0x1F4F9 0x1F4FC i = Right $ E c
      | (==) 0x1F4FD i = Right $ E c
      | (==) 0x1F4FF i = Right $ E c
      | inEq 0x1F500 0x1F53D i = Right $ E c
      | inEq 0x1F549 0x1F54A i = Right $ E c
      | inEq 0x1F54B 0x1F54E i = Right $ E c
      | inEq 0x1F550 0x1F567 i = Right $ E c
      | inEq 0x1F56F 0x1F570 i = Right $ E c
      | inEq 0x1F573 0x1F579 i = Right $ E c
      | (==) 0x1F57A i = Right $ E c
      | (==) 0x1F587 i = Right $ E c
      | inEq 0x1F58A 0x1F58D i = Right $ E c
      | (==) 0x1F590 i = Right $ E c
      | inEq 0x1F595 0x1F596 i = Right $ E c
      | (==) 0x1F5A4 i = Right $ E c
      | (==) 0x1F5A5 i = Right $ E c
      | (==) 0x1F5A8 i = Right $ E c
      | inEq 0x1F5B1 0x1F5B2 i = Right $ E c
      | (==) 0x1F5BC i = Right $ E c
      | inEq 0x1F5C2 0x1F5C4 i = Right $ E c
      | inEq 0x1F5D1 0x1F5D3 i = Right $ E c
      | inEq 0x1F5DC 0x1F5DE i = Right $ E c
      | (==) 0x1F5E1 i = Right $ E c
      | (==) 0x1F5E3 i = Right $ E c
      | (==) 0x1F5E8 i = Right $ E c
      | (==) 0x1F5EF i = Right $ E c
      | (==) 0x1F5F3 i = Right $ E c
      | (==) 0x1F5FA i = Right $ E c
      | inEq 0x1F5FB 0x1F5FF i = Right $ E c
      | (==) 0x1F600 i = Right $ E c
      | inEq 0x1F601 0x1F610 i = Right $ E c
      | (==) 0x1F611 i = Right $ E c
      | inEq 0x1F612 0x1F614 i = Right $ E c
      | (==) 0x1F615 i = Right $ E c
      | (==) 0x1F616 i = Right $ E c
      | (==) 0x1F617 i = Right $ E c
      | (==) 0x1F618 i = Right $ E c
      | (==) 0x1F619 i = Right $ E c
      | (==) 0x1F61A i = Right $ E c
      | (==) 0x1F61B i = Right $ E c
      | inEq 0x1F61C 0x1F61E i = Right $ E c
      | (==) 0x1F61F i = Right $ E c
      | inEq 0x1F620 0x1F625 i = Right $ E c
      | inEq 0x1F626 0x1F627 i = Right $ E c
      | inEq 0x1F628 0x1F62B i = Right $ E c
      | (==) 0x1F62C i = Right $ E c
      | (==) 0x1F62D i = Right $ E c
      | inEq 0x1F62E 0x1F62F i = Right $ E c
      | inEq 0x1F630 0x1F633 i = Right $ E c
      | (==) 0x1F634 i = Right $ E c
      | inEq 0x1F635 0x1F640 i = Right $ E c
      | inEq 0x1F641 0x1F642 i = Right $ E c
      | inEq 0x1F643 0x1F644 i = Right $ E c
      | inEq 0x1F645 0x1F64F i = Right $ E c
      | inEq 0x1F680 0x1F6C5 i = Right $ E c
      | inEq 0x1F6CB 0x1F6CF i = Right $ E c
      | (==) 0x1F6D0 i = Right $ E c
      | inEq 0x1F6D1 0x1F6D2 i = Right $ E c
      | (==) 0x1F6D5 i = Right $ E c
      | inEq 0x1F6E0 0x1F6E5 i = Right $ E c
      | (==) 0x1F6E9 i = Right $ E c
      | inEq 0x1F6EB 0x1F6EC i = Right $ E c
      | (==) 0x1F6F0 i = Right $ E c
      | (==) 0x1F6F3 i = Right $ E c
      | inEq 0x1F6F4 0x1F6F6 i = Right $ E c
      | inEq 0x1F6F7 0x1F6F8 i = Right $ E c
      | (==) 0x1F6F9 i = Right $ E c
      | (==) 0x1F6FA i = Right $ E c
      | inEq 0x1F7E0 0x1F7EB i = Right $ E c
      | inEq 0x1F90D 0x1F90F i = Right $ E c
      | inEq 0x1F910 0x1F918 i = Right $ E c
      | inEq 0x1F919 0x1F91E i = Right $ E c
      | (==) 0x1F91F i = Right $ E c
      | inEq 0x1F920 0x1F927 i = Right $ E c
      | inEq 0x1F928 0x1F92F i = Right $ E c
      | (==) 0x1F930 i = Right $ E c
      | inEq 0x1F931 0x1F932 i = Right $ E c
      | inEq 0x1F933 0x1F93A i = Right $ E c
      | inEq 0x1F93C 0x1F93E i = Right $ E c
      | (==) 0x1F93F i = Right $ E c
      | inEq 0x1F940 0x1F945 i = Right $ E c
      | inEq 0x1F947 0x1F94B i = Right $ E c
      | (==) 0x1F94C i = Right $ E c
      | inEq 0x1F94D 0x1F94F i = Right $ E c
      | inEq 0x1F950 0x1F95E i = Right $ E c
      | inEq 0x1F95F 0x1F96B i = Right $ E c
      | inEq 0x1F96C 0x1F970 i = Right $ E c
      | (==) 0x1F971 i = Right $ E c
      | inEq 0x1F973 0x1F976 i = Right $ E c
      | (==) 0x1F97A i = Right $ E c
      | (==) 0x1F97B i = Right $ E c
      | inEq 0x1F97C 0x1F97F i = Right $ E c
      | inEq 0x1F980 0x1F984 i = Right $ E c
      | inEq 0x1F985 0x1F991 i = Right $ E c
      | inEq 0x1F992 0x1F997 i = Right $ E c
      | inEq 0x1F998 0x1F9A2 i = Right $ E c
      | inEq 0x1F9A5 0x1F9AA i = Right $ E c
      | inEq 0x1F9AE 0x1F9AF i = Right $ E c
      | inEq 0x1F9B0 0x1F9B9 i = Right $ E c
      | inEq 0x1F9BA 0x1F9BF i = Right $ E c
      | (==) 0x1F9C0 i = Right $ E c
      | inEq 0x1F9C1 0x1F9C2 i = Right $ E c
      | inEq 0x1F9C3 0x1F9CA i = Right $ E c
      | inEq 0x1F9CD 0x1F9CF i = Right $ E c
      | inEq 0x1F9D0 0x1F9E6 i = Right $ E c
      | inEq 0x1F9E7 0x1F9FF i = Right $ E c
      | inEq 0x1FA70 0x1FA73 i = Right $ E c
      | inEq 0x1FA78 0x1FA7A i = Right $ E c
      | inEq 0x1FA80 0x1FA82 i = Right $ E c
      | inEq 0x1FA90 0x1FA95 i = Right $ E c
      | otherwise = Left $ "can not convert " <> show c <> " to emoji element."
