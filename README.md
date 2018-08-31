# Emoji Splitter

[![Build status](https://travis-ci.org/opyapeus/purescript-emoji-splitter.svg?branch=master)](https://travis-ci.org/opyapeus/purescript-emoji-splitter)

Split emojis into emoji list.

```PureScript
> import Data.String.EmojiSplitter
> splitEmoji "😍🏳️‍🌈👨🏻‍🌾☝🏿👨‍👩‍👧‍👧♨️"
(Right ["😍", "🏳️‍🌈", "👨🏻‍🌾", "☝🏿", "👨‍👩‍👧‍👧", "♨️"])
```

Reference:
[UTS #51: Unicode Emoji #EBNF_and_Regex](http://unicode.org/reports/tr51/#EBNF_and_Regex)

## Installation

```sh
bower install purescript-emoji-splitter
```

## Documentation

Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-emoji-splitter).
