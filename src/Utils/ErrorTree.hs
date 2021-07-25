module Utils.ErrorTree where


data ErrorTree = ErrorTree
  { msg :: String
  , causedBy :: [ErrorTree]
  } deriving Show

printErrorTree :: ErrorTree -> IO ()
printErrorTree (ErrorTree m c) = do
  putStrLn $ "Error: " ++ m

  putStrLn "Error stack is:"
  mapM_ (printErrorTree' $ "  - ") c

  where
    printErrorTree' :: String -> ErrorTree -> IO ()
    printErrorTree' prefix (ErrorTree m c) = do
      putStr prefix
      putStrLn m

      mapM_ (printErrorTree' $ "  " ++ prefix) c

