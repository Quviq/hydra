{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Test.LogFilterSpec where

import Control.Lens ((^?))
import Data.Aeson (Value (Array, String), decode)
import Data.Aeson.Lens (key)
import qualified Data.ByteString.Lazy as LBS
import Hydra.LogFilter (filterLog)
import Hydra.Prelude
import Test.Hydra.Prelude

entry :: LBS.ByteString
entry = "{\"message\":{\"node\":{\"by\":10,\"event\":{\"message\":{\"transactions\":[{\"witnesses\":{\"scripts\":{},\"keys\":[\"820082582028c440059807b853b2d44ee8358086a10837b9418efab69aaef4c9e981d2fd3f5840c4c1a02d2ecb38bc0e4efa93ea9147ed30132437b8f2d89f47f7e84723ea5969c43b62c1043eb20e35f7ddef4334cb093ece1d5d0ecee9358a21f44937ed6005\"]},\"body\":{\"outputs\":[{\"address\":\"addr_test1qryc674js99w50kjf30heds8eqqe0vre3d8487swgrmd7q5a8uwp3k06h9vg32z7lrnzjvpey9eymx7zq8atvz755sjqcguqss\",\"value\":{\"lovelace\":31288501}},{\"address\":\"addr_test1qzzdm3uyvzgutwzkqfv23d92gkksgt03ywknqqcxdg6r830dtlu6gds4r96mejvaa93439tl0rhrwj0vp4kujxuprhuqszkmcs\",\"value\":{\"lovelace\":0}}],\"mint\":{\"lovelace\":0},\"auxiliaryDataHash\":null,\"withdrawals\":[],\"certificates\":[],\"fees\":0,\"inputs\":[\"03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314#22\"],\"validity\":{\"notBefore\":1,\"notAfter\":2}},\"id\":\"6cba5394ec8a1a1161758a33089661383143283d0121e4a293ed51a0272cfbc4\",\"auxiliaryData\":null}],\"snapshotNumber\":3,\"party\":30,\"tag\":\"ReqSn\"},\"tag\":\"NetworkEvent\"},\"tag\":\"ProcessedEvent\"},\"tag\":\"Node\"},\"timestamp\":\"2021-09-08T10:05:05.919304349Z\",\"namespace\":\"HydraNode-1\",\"threadId\":18}"

spec :: Spec
spec = parallel $ do
  it "injects timestamp, threadId and namespace" $ do
    (decode entry >>= filterLog >>= (^? key "timestamp"))
      `shouldBe` Just (String "2021-09-08T10:05:05.919304349Z")

  it "replaces transactions by their ids in ReqSn" $ do
    (decode entry >>= filterLog >>= (^? key "event" . key "message" . key "transactions"))
      `shouldBe` Just (Array [String "6cba5394ec8a1a1161758a33089661383143283d0121e4a293ed51a0272cfbc4"])