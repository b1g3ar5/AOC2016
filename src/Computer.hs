module Computer where


import Data.Map ( Map, (!), insert, empty )


data Computer = Computer { rs :: Map Reg Int
                         , ins :: [Instruction]
                         , pos :: Int
                         , out :: [Int]} deriving (Show)


data Reg = A | B | C | D deriving (Show, Read, Eq, Enum, Bounded, Ord)


data Instruction = Out (Either Reg Int) | Skip (Either Reg Int)  (Either Reg Int) | Cpy (Either Reg Int)  Reg | Inc Reg | Dec Reg | Jmp (Either Reg Int)  (Either Reg Int) | Tgl Reg deriving (Eq, Show)


itemApply :: Int -> (a->a) -> [a] -> [a]
itemApply pos f list
  | pos >= length list = list
  | otherwise = take pos list ++ f (list!!pos) : drop (pos+1) list


run :: (Computer -> Bool) -> Computer -> Computer
run isFinished c@(Computer rs is pos out)
  | pos >= length is = c
  | isFinished c = c
  | otherwise = run isFinished (Computer newRs newIs newPos newOut)
  where
    getInt (Left r) = rs ! r
    getInt (Right i) = i
    (newRs, newPos, newIs, newOut) = case is !! pos of
      Cpy eri r -> (insert r (getInt eri) rs, pos+1, is, out)
      Inc r -> (insert r (rs!r + 1) rs, pos+1, is, out)
      Dec r -> (insert r (rs!r - 1) rs, pos+1, is, out)
      Jmp eri i -> (rs, if getInt eri /= 0 then pos + getInt i else pos+1, is, out)
      Tgl r -> (rs, pos+1, itemApply (pos + rs!r) toggle is, out)
      Skip _ _ -> (rs, pos+1, is, out)
      Out eri -> (rs, pos+1, is, out ++ [getInt eri])


toggle :: Instruction -> Instruction
toggle (Inc r) = Dec r
toggle (Dec r) = Inc r
toggle (Tgl r) = Inc r
toggle (Cpy eri r) = Jmp eri (Left r)
toggle (Jmp eri (Left r)) = Cpy eri r
toggle (Jmp eri (Right i)) = Skip eri (Right i)
toggle (Skip eri (Right i)) = Jmp eri (Right i)
toggle (Out x) = Out x


parseInstruction :: String -> Instruction
parseInstruction s
  | id == "inc" = Inc $ go1 $ pieces!!1
  | id == "dec" = Dec $ go1 $ pieces!!1
  | id == "cpy" = Cpy (go $ pieces!!1) $ go1 $ pieces!!2
  | id == "jnz" = Jmp (go $ pieces!!1) $ go $ pieces!!2
  | id == "tgl" = Tgl (go1 $ pieces!!1) 
  | id == "out" = Out (go $ pieces!!1) 
  | otherwise = error "No such instruction"
  where
    pieces = words s  
    id = head pieces
    go :: String -> Either Reg Int
    go s 
      | s == "a" = Left A
      | s == "b" = Left B
      | s == "c" = Left C
      | s == "d" = Left D
      | otherwise = Right $ read s
    go1 :: String -> Reg
    go1 s
      | s == "a" = A
      | s == "b" = B
      | s == "c" = C
      | s == "d" = D
      | otherwise = error $ "This is not a register: " ++ s
