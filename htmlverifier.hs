import System.IO

-- List of all the opening and closing tags.
openingTags = ["<html>", "<head>", "<body>", "<title>", "<a>", "<div>", "<h1>", "<h2>", "<h3>", "<p>", "<ul>", "<li>"]
closingTags = ["</html>", "</head>", "</body>", "</title>", "</a>", "</div>", "</h1>", "</h2>", "</h3>", "</p>", "</ul>", "</li>"]
-- Creates a list of tuples where, for each tuple, the first element is an opening tag, and the second element is its corresponding closing tag
matchingTags = zip openingTags closingTags

-- Defining a stack that allows strings and its methods
newtype Stack a = Stack [String] deriving Show

emptyStack :: Stack String
emptyStack = Stack []

isEmpty :: Stack String -> Bool
isEmpty (Stack s) = null s

push :: String -> Stack String -> Stack String
push x (Stack s) = Stack (x:s)

peek :: Stack String -> String
peek (Stack []) = ""
peek (Stack s) = head s

pop :: Stack String -> (String, Stack String)
pop (Stack (x:xs)) = (x, Stack xs)

-- Will scan through a given string and when it finds a '<' character, it will put together a tag until it finds a '>' character
findTag :: String -> [String] -> [String]
findTag [] listOfTags = reverse listOfTags
findTag (x:xs) listOfTags = 
    if x /= '<' then findTag xs listOfTags
    else removeLeadingSpaces xs listOfTags

-- Once we find a '>' we might have spaces before the text of the tag, we will just ignore them.
removeLeadingSpaces :: String -> [String] -> [String]
removeLeadingSpaces [] listOfTags = reverse listOfTags
removeLeadingSpaces (x:xs) listOfTags = 
    if x == ' ' then removeLeadingSpaces xs listOfTags
    else getTag "<" (x:xs) listOfTags

-- We will append every character together until we find a space or a '>'
getTag :: String -> String -> [String] -> [String]
getTag _ [] listOfTags = reverse listOfTags
getTag tag (x:xs) listOfTags 
    | x == ' ' = ignoreTrailingSpaces tag (x:xs) listOfTags
    | x /= '>' = getTag (x:tag) xs listOfTags
    | otherwise = findTag xs (reverse('>':tag):listOfTags)

-- We will ignore every character until we find a '>'
ignoreTrailingSpaces :: String -> String -> [String] -> [String]
ignoreTrailingSpaces _ [] listOfTags = listOfTags
ignoreTrailingSpaces tag (x:xs) listOfTags
    | x /= '>' = ignoreTrailingSpaces tag xs listOfTags
    | otherwise = findTag xs (reverse('>':tag):listOfTags)

-- List of all the tags that can nest within a given tag
isAccepted :: String -> String -> Bool
isAccepted outerTag innerTag =
    case outerTag of
        "<html>" -> innerTag `elem` ["<head>", "<body>"]
        "<head>" -> innerTag == "<title>"
        "<body>" -> innerTag `elem` ["<a>", "<br>", "<div>", "<h1>", "<h2>", "<h3>", "<hr>", "<p>", "<ul>"]
        "<div>" -> innerTag `elem` ["<a>", "<br>", "<div>", "<h1>", "<h2>", "<h3>", "<hr>", "<p>", "<ul>"]
        "<li>" -> innerTag `elem` ["<a>", "<br>", "<div>", "<h1>", "<h2>", "<h3>", "<hr>", "<p>", "<ul>"]
        "<h1>" -> innerTag == "<br>"
        "<h2>" -> innerTag == "<br>"
        "<h3>" -> innerTag == "<br>"
        "<ul>" -> innerTag == "<li>"
        "<p>" -> innerTag `elem` ["<a>", "<br>"]
        _ -> False

-- Returns the correspondent closing tag given an opening tag.
closingTag :: String -> [(String, String)] -> String
closingTag _ [] = undefined
closingTag tag (x:xs) 
    | fst x == tag = snd x
    | otherwise = closingTag tag xs

-- Starts checking the nesting of the html file.
-- It first checks that the first that, if there is one, is an html tag
checkNesting :: [String] -> Stack String -> IO ()
checkNesting [] _ = putStrLn "No tags in the file."
checkNesting (x:xs) stack 
    | x == "<html>" = checkNestingNoHTML xs (push x stack)
    | otherwise = putStrLn "First tag is not an html tag"

-- Once we have had an html tag, we won't be accepting any more html tags,
-- But we will keep checking the nesting of the rest of the tags
checkNestingNoHTML :: [String] -> Stack String -> IO ()
checkNestingNoHTML [] stack = if isEmpty stack then putStrLn "Valid HTML file!"
                                               else putStrLn "Tags are not balanced. Unvalid HTML file"
checkNestingNoHTML (x:xs) stack 
    |  x == "<html>" = putStrLn "Can't have more than one html tag"
    |  x == "<head>" = checkNestingNoHead xs (push x stack)
    |  x == "<body>" = checkNestingNoBody xs (push x stack)
    | otherwise = putStrLn (x ++ " is not accepted in " ++ peek stack)

-- If we found a head tag in the html file, don't accept any more head tags and keep checking the nesting of 
-- the remaining tags
checkNestingNoHead :: [String] -> Stack String -> IO ()
checkNestingNoHead [] stack = if isEmpty stack then putStrLn "Valid HTML file!"
                                               else putStrLn "Tags are not balanced. Unvalid HTML file"
checkNestingNoHead (x:xs) stack
    |  x `elem` ["<html>", "<head>"] = putStrLn ("Can't have more than one" ++ x ++ "tag")
    |  x `elem` openingTags = if isAccepted (peek stack) x
                                then if x == "<body>" then checkNestingNoBody xs (push x stack)
                                                      else checkNestingNoHead xs (push x stack)
                                else putStrLn (x ++ " is not accepted in " ++ peek stack)
    |  x `elem` closingTags = if closingTag (peek stack) matchingTags == x
                                then checkNestingNoHead xs (snd (pop stack))
                                else putStrLn ("Can't close a tag you haven't opened! " ++ x) 
    | x `elem` ["<br>", "<hr>"] = if isAccepted (peek stack) x
                                    then checkNestingNoHead xs stack
                                    else putStrLn (x ++ " tags are not accepted in: " ++ peek stack)
    | otherwise = putStrLn ("Not a valid tag: " ++ x)

-- If we found a body tag, that means we can't have any more body tags, but we also can't have head tags
-- as head tags can't be put after a body tag in a valid html file
checkNestingNoBody :: [String] -> Stack String -> IO ()
checkNestingNoBody [] stack = if isEmpty stack then putStrLn "Valid HTML file!"
                                               else putStrLn "Tags are not balanced. Unvalid HTML file"
checkNestingNoBody (x:xs) stack
    |  x `elem` ["<html>", "<head>", "<body>"] = putStrLn ("Can't have more than one " ++ x ++ "opened in the same file")
    |  x `elem` openingTags = if isAccepted (peek stack) x
                                then checkNestingNoBody xs (push x stack)
                                else putStrLn (x ++ " tags are not accepted in: " ++ peek stack)
    |  x `elem` closingTags = if closingTag (peek stack) matchingTags == x
                                then checkNestingNoBody xs (snd (pop stack))
                                else putStrLn ("Can't close a tag you haven't opened! " ++ x)
    | x `elem` ["<br>", "<hr>"] = if isAccepted (peek stack) x
                                    then checkNestingNoBody xs stack
                                    else putStrLn (x ++ " tags are not accepted in: " ++ peek stack)
    | otherwise = putStrLn (x ++ " is not a valid tag.")    

-- Read file.html file, place all its tags into a list "listOfTags", and then check the nesting of every tag in there.
main = do
    contents <- readFile "file.html"
    let listOfTags = findTag contents []
    checkNesting listOfTags emptyStack
    return ()
