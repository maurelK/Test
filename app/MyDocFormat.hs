{-
-- EPITECH PROJECT, 2024
-- n4s_controls
-- File description:
-- mydocFormat file
-}

module MyDocFormat where

--data Header = Header
--    {   title :: String
--    ,   author :: Maybe String
--    ,   date :: Maybe String
--    } deriving (Show, Eq)

data Header = Header String (Maybe String) (Maybe String)
  deriving (Show, Eq)

data Content
  = Text String
  | Bold Content
  | Italic Content
  | Paragraph [Content]
  | Section String [Content]
  | CodeBlock String
  deriving (Show, Eq)


data Item = Item Content
    deriving (Show, Eq)

data Document = Document Header [Content]
    deriving (Show, Eq)
