{-
-- EPITECH PROJECT, 2024
-- n4s_controls
-- File description:
-- mydocFormat file
-}

module MyDocFormat where

data Header = Header
    {   title :: String
    ,   author :: Maybe String
    ,   date ::Maybe String
    } deriving (Show, Eq)

data Content
    = Text String
    | Italic Content
    | Bold Content
    | Code Content
    | Link String String 
    | Image String  String
    | Paragraph [Content]
    | Section (Maybe String) [Content]
    | CodeBlock String
    | List [Item]
    deriving (Show, Eq)

data Item = Item Content
    deriving (Show, Eq)

data Document = Document Header [Content]
    deriving (Show, Eq)
