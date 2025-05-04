{-
-- EPITECH PROJECT, 2024
-- n4s_controls
-- File description:
-- mydocFormat file
-}
module MyDocFormat where

-- Header contains title and optional author/date
data Header = Header
    { title  :: String          -- Required document title
    , author :: Maybe String    -- Optional author
    , date   :: Maybe String    -- Optional date
    } deriving (Show, Eq)

-- Main content types
data Content
    = Text String               -- Plain text content
    | Italic Content            -- Italic formatted content
    | Bold Content              -- Bold formatted content
    | Code Content              -- Inline code content
    | Link LinkData             -- Hyperlink with text and URL
    | Image ImageData           -- Image with alt text and URL
    | Paragraph [Content]       -- Group of content forming a paragraph
    | Section SectionData       -- Document section with title and content
    | CodeBlock String          -- Block of code (multiline)
    | List ListData             -- Ordered or unordered list
    deriving (Show, Eq)

-- Link structure
data LinkData = LinkData
    { linkText :: [Content]     -- Display text (can be formatted)
    , linkUrl  :: String        -- Destination URL
    } deriving (Show, Eq)

-- Image structure
data ImageData = ImageData
    { imageAlt  :: String       -- Alternative text
    , imageUrl  :: String       -- Image source URL
    } deriving (Show, Eq)

-- Section structure
data SectionData = SectionData
    { sectionTitle :: Maybe String  -- Optional section title
    , sectionContent :: [Content]   -- Nested content
    } deriving (Show, Eq)

-- List structure
data ListData = ListData
    { listType   :: ListType    -- Ordered or unordered
    , listItems  :: [ListItem]  -- List items
    } deriving (Show, Eq)

data ListType
    = Ordered     -- Numbered list
    | Unordered   -- Bulleted list
    deriving (Show, Eq)

-- Single list item
data ListItem = ListItem
    { itemContent :: [Content]  -- Content within the item
    } deriving (Show, Eq)

-- Complete document structure
data Document = Document
    { docHeader :: Header       -- Document header
    , docBody   :: [Content]    -- Main content body
    } deriving (Show, Eq)