module Main where 
import System.IO
import System.Environment
import System.Exit
import System.IO.Error
import Text.Read


--program wyświetla zawartość pliku podanego w argumencie w linii poleceń


main :: IO ()
main = do
    args <- getArgs
    if null args then do
        hPutStrLn stderr "Brak argumentu."
        exitFailure
    else 
        catchIOError (do
        handle <- openFile (args !! 0) ReadMode
        contents <-(hGetContents handle)
        putStr contents
        hClose handle)
        (\_ -> do 
            hPutStrLn stderr "Nie można otworzyć pliku."
            exitFailure)

-- program wyświetla sumę liczb całkowitych z pliku

mainF' :: (String -> IO()) ->IO ()
mainF' f = do
    args <- getArgs
    if null args then do
        hPutStrLn stderr "Brak argumentu."
        exitFailure
    else 
        catchIOError (do
        handle <- openFile (args !! 0) ReadMode
        contents <-(hGetContents handle)
        f contents
        hClose handle)
        (\_ -> do 
            hPutStrLn stderr "Nie można otworzyć pliku."
            exitFailure)

printSum :: String -> IO ()
printSum contents = do
    let parts = map readMaybe (words contents) :: [Maybe Integer]
    case sumMaybe parts of
        Nothing -> do
            hPutStrLn stderr "W pliku nie występują tylko liczby."
            exitFailure
        Just s -> print s

hPrintSum :: Handle -> String -> IO ()
hPrintSum handle contents = do
    let parts = map readMaybe (words contents) :: [Maybe Integer]
    case sumMaybe parts of
        Nothing -> do
            hPutStrLn stderr "W pliku nie występują tylko liczby."
            exitFailure
        Just s -> hPrint handle s

sumMaybe :: [Maybe Integer] -> Maybe Integer
sumMaybe [] = Just 0
-- sumMaybe (Nothing:_) = Nothing
-- sumMaybe (Just x: t) = 
--     case sumMaybe t of
--         Nothing -> Nothing
--         Just y -> Just(x + y)

sumMaybe (h:t) = do
    x <- h
    y <- sumMaybe t
    return (x + y)

--Plik wynikowy ma zawierać sumy dla każdego wejścia w pliku wyjściowym gdzie plik wyściowy będzie drugim argumentem wiersza poleceń


main'' :: IO ()
main'' = do
    args <- getArgs
    if length args < 2 then do
        hPutStrLn stderr "Brak argumentu."
        exitFailure
    else 
        catchIOError (do
        handle <- openFile (args !! 0) ReadMode
        handleOut <- openFile (args !! 1) WriteMode
        contents <-(hGetContents handle)
        hPrintSums handleOut contents
        putStr contents
        hClose handle
        hClose handleOut)
        (\_ -> do 
            hPutStrLn stderr "Nie można otworzyć pliku."
            exitFailure)


hPrintSums :: Handle -> String -> IO ()
hPrintSums handle contents = do 
    let rows = lines contents
    hPrintRows handle rows

hPrintRows :: Handle -> [String] -> IO ()
hPrintRows _ [] = return ()
hPrintRows handle (h:t) = do
    hPrintSum handle h
    hPrintRows handle t