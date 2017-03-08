data Stmt = Conti | Break
          | Exp String
          | Goto Integer -- only support number label
          | Label Integer -- only support number label
          | If String [Stmt] [Stmt] -- If condition true-statements false-statements
          | While String [Stmt] -- While condition statements
          | Do [Stmt] String -- Do statements condition
          | For String String String [Stmt] -- For initial condition increase statements
          | Switch String [(String, [Stmt])] [Stmt] -- Switch expression [(constant, statements)] default-statements
            deriving (Show)

-- simplified statement
data SStmt = SExp String
           | SLabel Integer
           | SGoto Integer
           | SIf String SStmt -- If condition true-statement
             deriving (Show)

trans :: [Stmt] -> Integer -> ([SStmt], Integer)
trans (Exp s : rt) e 
    = (SExp s : rt', e') where (rt', e') = trans rt e
trans (Goto s : rt) e
    = (SGoto s : rt', e') where (rt', e') = trans rt e
trans (Label s : rt) e 
    = (SLabel s : rt', e') where (rt', e') = trans rt e
-- optimizations for if 
-- one expression in true-statement, no expression in false-statement
trans (If cond [Exp s] [] : rt) e 
    = (SIf cond (SExp s) : rt', e') where (rt', e') = trans rt e
trans (If cond [Goto g] [] : rt) e 
    = (SIf cond (SGoto g) : rt', e') where (rt', e') = trans rt e
-- no true-statement or false-statement
trans (If cond [] [] : rt) e
    = trans rt e
-- only false-statement
trans (If cond [] fs : rt) e
    = (SIf cond (SGoto e) : fs' ++ SLabel e : rt', e1)
    where
        (fs', e0) = trans fs (e + 1)
        (rt', e1) = trans rt e0
-- only true-statement
trans (If cond ts [] : rt) e
    = trans (If ("!(" ++ cond ++ ")") [] ts : rt) e
-- general if statement
trans (If cond ts fs : rt) e 
    = (SIf cond (SGoto e) : fs'
    ++ SGoto (e + 1) : SLabel e : ts'
    ++ SLabel (e + 1) : rt', e2)
    where
        (fs', e0) = trans fs (e + 2)
        (ts', e1) = trans ts e0
        (rt', e2) = trans rt e1
trans (While cond [] : rt) e 
    = trans rt e
trans (While cond sts : rt) e 
    = (SLabel e : sts' ++ SLabel (e + 1) : rt', e1)
    where
        neosts = replace sts e (e + 1)
        (sts', e0) = trans [If cond (neosts ++ [Goto e]) []] (e + 2)
        (rt', e1) = trans rt e0
trans (Do [] cond : rt) e
    = trans rt e
trans (Do sts cond : rt) e 
    = (SLabel e : sts' ++ SIf cond (SGoto e) : SLabel (e + 1) : rt', e1)
    where
        neosts = replace sts e (e + 1)
        (sts', e0) = trans neosts (e + 2)
        (rt', e1) = trans rt e0
trans (For init cond inc sts : rt) e 
    = (SExp init : SLabel e : sts' ++ SLabel (e + 2) : rt', e1)
    where
        neosts = replace sts (e + 1) (e + 2) ++ [Label (e + 1), Exp inc, Goto e]
        (sts', e0) = trans [If cond neosts []] (e + 3)
        (rt', e1) = trans rt e0
trans (Switch exp [] deft : rt) e
    = trans (deft ++ rt) e
trans (Switch exp [(const, code)] deft : rt) e
    = trans (If (exp ++ " == " ++ const) code deft : rt) e
trans (Switch exp ((const, code) : xs) deft : rt) e 
    = trans (If (exp ++ " == " ++ const) code [(Switch exp xs deft)] : rt) e
-- trans (Conti : rt) e = let (rt', e') = trans rt e in (SExp "continue" : rt', e')
-- trans (Break : rt) e = let (rt', e') = trans rt e in (SExp "break" : rt', e')
trans [] e = ([], e)

-- replace continue or break, don't replace for loop statements
replace :: [Stmt] -> Integer -> Integer -> [Stmt]
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
ts s = let (ss, _) = trans s 0 in printElements ss


-- examples

s0 = [Exp "int i = 0", Exp "++i", Exp "print i"]

s1 = [Exp "int i = 0", 
    If "i == 0" [Exp "print j + 1"] []]

s2 = [Exp "int i = 0", 
    While "i < 100" 
       [Exp "print i",
        If "i == 66" [Break] [], 
        Exp "++i"]]

s3 = [Exp "int i = 0", Do [Exp "++i", If "i % 2 == 0" [Conti] [], Exp "print i"] "i < 100"]

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
