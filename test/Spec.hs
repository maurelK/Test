{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import MyDocFormat
import ParserJson (parseDocument)
import ParserXml (parseDocumentXml)
import ParserMarkdown (parseDocumentMarkdown)

main :: IO ()
main = hspec $ do
  describe "ParserJson" $ do
    it "parses a simple JSON document" $ do
      let input = "{ \"header\": { \"title\": \"Test\" }, \"body\": [\"Hello\"] }"
      parseDocument input `shouldSatisfy` (/= Nothing)

    it "parses the complex JSON document from examples/syntaxe.json" $ do
      content <- readFile "examples/syntaxe.json"
      parseDocument content `shouldSatisfy` (/= Nothing)

  describe "ParserXml" $ do
    it "parses a simple XML document" $ do
      let input = "<document><header title=\"Test\"></header><body><text>Hello</text></body></document>"
      parseDocumentXml input `shouldSatisfy` (/= Nothing)

  describe "ParserMarkdown" $ do
    it "parses a simple Markdown document" $ do
      let input = "---\ntitle: Test\n---\nHello\n"
      parseDocumentMarkdown input `shouldSatisfy` (/= Nothing)
