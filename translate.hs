data Stmt = Conti | Break
          | Exp String
          | Goto Int -- only support number label
          | Label Int -- only support number label
          | If String [Stmt] [Stmt] -- If condition true-statements false-statements
          | While String [Stmt] -- While condition statements
          | Do [Stmt] String -- Do statements condition
          | For String String String [Stmt] -- For initial condition increase statements
          | Switch String [(String, [Stmt])] [Stmt] -- Switch expression [(constant, statements)] default-statements
            deriving (Show)

-- simplified statement
data SStmt = SExp String
           | SLabel Int
           | SGoto Int
           | SIf String SStmt -- If condition true-statement
             deriving (Show)

newtype State a = S (Int -> (a, Int))

app :: State a -> Int -> (a, Int)
app (S st) i = st i

instance Functor State where
    -- fmap :: (a -> b) -> State a -> State b
    fmap g st = S (\s -> let (x, s') = app st s in (g x, s'))

instance Applicative State where
    -- pure :: a -> State a
    pure x = S (\s -> (x, s))
    
    -- (<*>) :: State (a -> b) -> State a -> State b
    stf <*> stx = S (\s -> 
        let (f, s0) = app stf s in 
            let (x, s1) = app stx s0 in (f x, s1))

instance Monad State where
    -- (>>=) :: State a -> (a -> State b) -> State b
    st >>= f = S (\s -> let (x, s0) = app st s in app (f x) s0)

newLabel :: State Int
newLabel = S (\s -> (s, s + 1))

transAll :: [Stmt] -> State [SStmt]
transAll [] = return []
transAll (s : ss) = (++) <$> trans s <*> transAll ss

trans :: Stmt -> State [SStmt]
trans (Exp s) = return [SExp s]
trans (Goto s) = return [SGoto s]
trans (Label s) = return [SLabel s]
trans (If cond ts [])
    = trans (If ("!(" ++ cond ++ ")") [] ts)
trans (If cond [] fs)
    = do end <- newLabel
         fs' <- transAll fs
         return (SIf cond (SGoto end) : fs' ++ [SLabel end])
trans (If cond ts fs)
    = do tsl <- newLabel; end <- newLabel
         ts' <- transAll ts; fs' <- transAll fs
         return (SIf cond (SGoto tsl) : fs' 
              ++ SGoto end : SLabel tsl : ts' ++ [SLabel end])
-- trans (While cond []) = return []
trans (While cond sts)
    = do start <- newLabel
         end <- newLabel
         let neosts = replace sts start end
         sts' <- trans (If cond (neosts ++ [Goto start]) [])
         return (SLabel start : sts' ++ [SLabel end])
-- trans (Do [] cond) = return []
trans (Do sts cond)
    = do start <- newLabel
         end <- newLabel
         let neosts = replace sts start end
         sts' <- transAll neosts
         return (SLabel start : sts' ++ [SIf cond (SGoto start), SLabel end])
trans (For init cond inc sts)
    = do incl <- newLabel
         start <- newLabel
         end <- newLabel
         let neosts = replace sts incl end ++ [Label incl, Exp inc, Goto start]
         sts' <- trans (If cond neosts [])
         return (SExp init : SLabel start : sts' ++ [SLabel end])
trans (Switch exp [] deft) 
    = transAll deft
trans (Switch exp [(const, code)] deft)
    = trans (If (exp ++ " == " ++ const) code deft)
trans (Switch exp ((const, code) : xs) deft)
    = trans (If (exp ++ " == " ++ const) code [(Switch exp xs deft)])

-- replace continue or break, don't replace for loop statements
replace :: [Stmt] -> Int -> Int -> [Stmt]
replace (Conti : rt) ct bk = Goto ct : replace rt ct bk 
replace (Exp "continue" : rt) ct bk = replace (Conti : rt) ct bk 
replace (Break : rt) ct bk = Goto bk : replace rt ct bk
replace (Exp "break" : rt) ct bk = replace (Break : rt) ct bk
replace (If cond ts fs : rt) ct bk = If cond (replace ts ct bk) (replace fs ct bk) : replace rt ct bk
-- replace (Switch exp [] deft : rt) ct bk = Switch exp [] (replace deft ct bk) : replace rt ct bk
-- replace (Switch exp ((const, code) : xs) deft : rt) ct bk 
--     = Switch exp ((const, (replace code ct bk)) : xs') deft' : rt'
--     where 
--         (Switch _ xs' deft') : rt' = replace (Switch exp xs deft : rt) ct bk
replace [] _ _ = []
-- skip other kinds of statements
replace (s : ss) ct bk = s : replace ss ct bk

-- print sstmts
printElements :: [SStmt] -> IO ()
printElements (SExp x : xs) = putStrLn (x ++ ";") >> printElements xs
printElements (SLabel x : xs) = putStrLn ("L" ++ show x ++ ":;") >> printElements xs
printElements (SGoto x : xs) = putStrLn ("goto L" ++ show x ++ ";") >> printElements xs
printElements (SIf cond st : xs) = putStr ("if (" ++ cond ++ ") ") >> printElements (st : xs)
printElements [] = return ()

-- use: ts s0
ts :: [Stmt] -> IO ()
ts s = let (ss, _) = app (transAll s) 0 in printElements ss


-- examples

s0 = [Exp "int i = 0", Exp "++i", Exp "printf(\"%d\\n\", i)"]

s1 = [Exp "int i = 0", 
    If "i == 0" [Exp "printf(\"%d\\n\", j + 1)"] []]

s2 = [Exp "int i = 0", 
    While "i < 100" 
       [Exp "printf(\"%d\\n\", i)",
        If "i == 66" [Break] [], 
        Exp "++i"]]

s3 = [Exp "int i = 0", Do [Exp "++i", If "i % 2 == 0" [Conti] [], Exp "printf(\"%d\\n\", i)"] "i < 100"]

s4 = [Exp "int i = 0", 
    Switch "i" 
       [("0", [Exp "printf(\"%d = 0\\n\", i)"]), 
        ("1", [Exp "printf(\"%d = 1\\n\", i)"]), 
        ("2", [Exp "printf(\"%d = 2\\n\", i)"])] 
       [Exp "printf(\"other\\n\")"]]

s4' = [Exp "int i = 0", 
    Switch "i" 
       [("0", [Break]), 
        ("1", [Conti])] 
       [Break]]

-- perfect numbers 1 - 10000
s5 = [Exp "int i = 1", 
    While "i <= 10000" 
       [Exp "int acc = 0", 
        (For "int j = 1" "j < i" "++j" 
           [If "i % j == 0" [Exp "acc += j"] []]), 
        (If "i == acc" [Exp "printf(\"%d\\n\", i)"] []), 
        (Exp "++i")]]

s6 = [If "j % 2 == 0" [If "j % 3 == 0" [Exp "printf(\"%d\\n\", j)", Exp "print j + 1"] []] [], Exp "int i = 0"]