-- simple calculator to recompute a word from its goedel number--

module Goedel(getfromgoedelnumber, testmodule) where
    import Control.Lens
    import Data.Char
    import Data.List
    import Data.Map
    import Math.NumberTheory.Primes

    error_message_input        = "ERROR: The number must be a positive Integer!\n\t'-> Valid Characters includes \"[a-b]\"" 
    error_message_decodingalph = "ERROR: This number contains invalid characters!\n\t'-> Valid Characters includes \"[a-b]\""

    getfromgoedelnumber :: Integer -> Either String String
    getfromgoedelnumber number
        | number <= 0 =Left error_message_input
        | otherwise = Right (to_string $ count_occurences ((sort_list . (getlist number)) []) empty)

    getlist :: Integer -> [Integer] -> [Integer]
    getlist number acc
        | number == 1 = acc
        | otherwise = let (next_number, p) = compute_new number (nextPrime 1 :: Prime Integer) in
            getlist next_number (p:acc)
        where
            compute_new :: Integer -> Prime Integer-> (Integer, Integer)
            compute_new input p
                | input `mod` (unPrime p) == 0  = (input `div` (unPrime p), (unPrime p :: Integer))
                | otherwise                     = compute_new input (succ (p :: Prime Integer))

    sort_list :: [Integer] -> [Integer]
    sort_list lst = sort lst
    
    count_occurences :: [Integer] -> Map Integer Integer -> [Integer]
    count_occurences []    mp = Data.List.map (\(x,y) -> y) $ toList mp
    count_occurences (h:t) mp = count_occurences t (insertWithKey (\_ _ value -> value + 1) h 1 mp)

    -- ASCII Version
    -- to_string :: [Integer] -> String
    -- to_string number = Data.List.map (\e -> chr $fromIntegral e) number

    -- Alphabet Version
    to_string :: [Integer] -> String
    to_string numbers = create_string numbers ""
        where
            alphabet = "abcdefghijklmnopqrstuvwxyz"

            create_string :: [Integer] -> String -> String
            create_string []          acc = reverse acc
            create_string (h:t) acc = case (alphabet ^? element ((fromIntegral h) - 1)) of
                Just e -> create_string t (e:acc)
                Nothing -> error_message_decodingalph

    testmodule :: Bool
    testmodule = let test_list = [intfact_test_1, countocc_test_1, goedel_test_1] in
            Prelude.foldl (&&) True test_list
        where
            intfact_test_1 = ((Prelude.foldr (*) 1  $ getlist 210 []) == 210)

            countocc_test_1 = (count_occurences [] empty == [])
            
            goedel_test_1 = "lmu" == (let lmu = 3113912109375000000000000 in 
                to_string $ count_occurences ((sort_list . (getlist lmu)) []) empty)





