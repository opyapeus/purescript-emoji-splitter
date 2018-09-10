module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.String.EmojiSplitter (splitEmoji)
import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal, expectFailure)
import Test.Unit.Main (runTest)

-- NOTE: rough test
main :: Effect Unit
main =
  runTest do
    suite "Emoji ZWJ Sequence" do
      test "Family" do
        equal (Right ["👨‍❤️‍👨", "👨‍❤️‍💋‍👨", "👨‍👦", "👨‍👦‍👦", "👨‍👨‍👦‍👦"]) $ splitEmoji "👨‍❤️‍👨👨‍❤️‍💋‍👨👨‍👦👨‍👦‍👦👨‍👨‍👦‍👦"
      test "Gendered Role, with object" do
        equal (Right ["👨‍⚕️", "👨‍🌾", "👨🏻‍⚕️", "👨🏻‍🌾"]) $ splitEmoji "👨‍⚕️👨‍🌾👨🏻‍⚕️👨🏻‍🌾"
      test "Gendered Role" do
        equal (Right ["👮‍♀️", "👮🏻‍♀️", "👱‍♀️", "👱🏻‍♀️"]) $ splitEmoji "👮‍♀️👮🏻‍♀️👱‍♀️👱🏻‍♀️"
      test "Gendered Activity" do
        equal (Right ["⛹🏻‍♀️", "🏃‍♀️", "🏃🏻‍♀️", "🏄‍♀️"]) $ splitEmoji "⛹🏻‍♀️🏃‍♀️🏃🏻‍♀️🏄‍♀️"
      test "Gendered Gestures" do
        equal (Right ["💁‍♀️", "💁🏻‍♀️", "🙅‍♀️", "🙅🏻‍♀️"]) $ splitEmoji "💁‍♀️💁🏻‍♀️🙅‍♀️🙅🏻‍♀️"
      test "Other" do
        equal (Right ["🏳️‍🌈", "🏴‍☠️", "👁️‍🗨️"]) $ splitEmoji "🏳️‍🌈🏴‍☠️👁️‍🗨️"
    suite "Sequence" do
      test "Emoji Keycap Sequence" do
        equal (Right ["#️⃣", "*️⃣", "0️⃣", "1️⃣"]) $ splitEmoji "#️⃣*️⃣0️⃣1️⃣"
      test "Emoji Flag Sequence" do
        equal (Right ["🏴󠁧󠁢󠁥󠁮󠁧󠁿", "🏴󠁧󠁢󠁳󠁣󠁴󠁿", "🏴󠁧󠁢󠁷󠁬󠁳󠁿"]) $ splitEmoji "🏴󠁧󠁢󠁥󠁮󠁧󠁿🏴󠁧󠁢󠁳󠁣󠁴󠁿🏴󠁧󠁢󠁷󠁬󠁳󠁿"
      test "Emoji Modifier Sequence" do
        equal (Right ["☝🏻", "☝🏿", "☝🏻", "☝🏿"]) $ splitEmoji "☝🏻☝🏿☝🏻☝🏿"
    suite "checks" do
      test "presentation" do
        equal (Right ["☠️", "☠", "☠️", "☠"]) $ splitEmoji "☠️☠☠️☠"
      test "ex a" do
        equal (Right ["♨️", "😍", "👷‍♂️", "♨"]) $ splitEmoji "♨️😍👷‍♂️♨"
      test "ex b" do
        equal (Right ["🔟", "➡", "🀄", "☀"]) $ splitEmoji "🔟➡🀄☀"
      test "ex c" do
        equal (Right ["👨‍👩‍👧‍👧", "🚴", "🧘‍♀️", "🧞‍♂️"]) $ splitEmoji "👨‍👩‍👧‍👧🚴🧘‍♀️🧞‍♂️"
      test "ex d" do
        equal (Right ["👩🏾‍💻", "👩🏻‍🦰", "🙆🏿‍♀️", "🦹🏻‍♀️"]) $ splitEmoji "👩🏾‍💻👩🏻‍🦰🙆🏿‍♀️🦹🏻‍♀️"
      test "ex e" do
        equal (Right ["😍","🏳️‍🌈","👨🏻‍🌾","☝🏿"]) $ splitEmoji "😍🏳️‍🌈👨🏻‍🌾☝🏿"
      test "ex fa" do
        expectFailure "should not equal" $ equal (Right ["a"]) $ splitEmoji "a"
      test "ex fb" do
        expectFailure "should not equal" $ equal (Right [" "]) $ splitEmoji " "
      test "ex fc" do
        expectFailure "should not equal" $ equal (Right ["絵"]) $ splitEmoji "絵"
