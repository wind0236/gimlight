{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Localization.Texts.Actors.Electria
  ( electriaBeforeQuest,
    electriaReject,
    electriaAccept,
    electriaNotCompleted,
    electriaCompleted,
    electriaAfterCompletion,
  )
where

import Gimlight.Localization (MultilingualText, multilingualText)

electriaBeforeQuest :: MultilingualText
electriaBeforeQuest =
  multilingualText
    "Hey, did you hear that monsters are emerging from the Bats Cave? Some said that in that cave, not only bats but also other monsters live. So, can you investigate the cave? If you kill at least three monsters, I'll be glad."
    "ねえ、蝙蝠の洞窟から化け物が湧き出てるって聞いた？どうやら蝙蝠だけじゃなくて他の生物も住んでいるみたい。あの洞窟を調査してくれない？もし3匹以上の怪物を殺してきてくれたら嬉しいな。"

electriaReject :: MultilingualText
electriaReject =
  multilingualText
    "Ah, I see. If you change your mind, ask me again."
    "そう。もし気が変わったらもう一度話しかけてね。"

electriaAccept :: MultilingualText
electriaAccept =
  multilingualText
    "Thank you! Be careful not to be killed!"
    "ありがとう！殺されないように気をつけてね！"

electriaNotCompleted :: MultilingualText
electriaNotCompleted =
  multilingualText "Hi, how's your investigation going?" "あら，調査の様子はどう？"

electriaCompleted :: MultilingualText
electriaCompleted =
  multilingualText
    "Thanks for the investigation! I'm surprised because there are orcs and trolls in the cave. I wonder what happened in the cave."
    "調査ありがとう！あの洞窟にオークやらトロールやらがいるなんて驚いたわ。一体何があったのかしら。"

electriaAfterCompletion :: MultilingualText
electriaAfterCompletion =
  multilingualText "Hi, thank you for the other day." "あら、この前はありがとね。"
