# Emoji Splitter

[![Build status](https://travis-ci.org/opyapeus/purescript-emoji-splitter.svg?branch=master)](https://travis-ci.org/opyapeus/purescript-emoji-splitter)

Split emojis into emoji list.

```PureScript
> import Data.String.CodeUnits (toCharArray)
> toCharArray "😍🏳️‍🌈👨🏻‍🌾☝🏿"
['�','�','�','�','️','‍','�','�','�','�','�','�','‍','�','�','☝','�','�']

> import Data.String.CodePoints (singleton, toCodePointArray)
> singleton <$> toCodePointArray "😍🏳️‍🌈👨🏻‍🌾☝🏿"
["😍","🏳","️","‍","🌈","👨","🏻","‍","🌾","☝","🏿"]

> import Data.String.EmojiSplitter (splitEmoji)
> splitEmoji "😍🏳️‍🌈👨🏻‍🌾☝🏿"
(Right ["😍","🏳️‍🌈","👨🏻‍🌾","☝🏿"])
```

※ Only emoji unicodes are supported.

Reference:
[UTS #51: Unicode Emoji #EBNF_and_Regex](http://unicode.org/reports/tr51/#EBNF_and_Regex)

## Installation

```sh
bower install purescript-emoji-splitter
```

## Documentation

Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-emoji-splitter).
