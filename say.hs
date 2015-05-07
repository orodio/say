splitInt :: Int -> [Int]
splitInt ns = [castInt x | x<- show ns]
  where castInt n = read [n] :: Int

ones :: Int -> String
ones n = ["","one","two","three","four","five","six","seven","eight","nine"] !! n;

teens :: Int -> String
teens n | n <= 9 = ones n
teens n = ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"] !! (index n)
  where index = last . splitInt

tens :: Int -> String
tens n | n <= 19 = teens n
tens 20 = "twenty"
tens 30 = "thirty"
tens 40 = "fourty"
tens 50 = "fifty"
tens 60 = "sixty"
tens 70 = "seventy"
tens 80 = "eighty"
tens 90 = "ninety"
tens n  =
    let sp = splitInt n
        xx = (tens . (*10) . head) sp
        x  = (tens . last) sp
    in xx ++ "-" ++ x

hundreds :: Int -> String
hundreds n | n <= 99 = tens n
hundreds n = let sp    = splitInt n
                 xxx   = ones $ sp !! 0
                 xx    = tens $ ((sp !! 1) * 10) + (sp !! 2)
                 inner = if (n - ((sp !! 0) * 100)) > 0
                            then " hundred and " ++ xx
                            else " hundred"
             in xxx ++ inner

say :: Int -> String
say 0 = "zero"
say n
  | n < 0          = "Not In Range"
  | n > 9999999999 = "Not In Range"
say n = hundreds n
