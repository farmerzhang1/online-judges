module Sprintf

%access export
%default total
data Format = Ch Format
            | Num Format
            | Flt Format
            | Str Format
            | Lit String Format
            | End

PrintfType : List Char -> Type
PrintfType ('%'::'c'::str) = Char -> PrintfType str
PrintfType ('%'::'f'::str) = Double -> PrintfType str
PrintfType ('%'::'s'::str) = String -> PrintfType str
PrintfType ('%'::'d'::str) = Int -> PrintfType str
PrintfType (c::str) = PrintfType str
PrintfType [] = String

helper : (chars : List Char) -> (acc : String) -> PrintfType chars
helper ('%'::'c'::str) acc = \c => helper str $ acc ++ (singleton c)
helper ('%'::'f'::str) acc = \d => helper str $ acc ++ (show d)
helper ('%'::'s'::str) acc = \s => helper str $ acc ++ s
helper ('%'::'d'::str) acc = \n => helper str $ acc ++ (show n)
helper (c::str) acc = helper str $ acc ++ (singleton c)
helper [] acc = acc

formatter : List Char -> Format
formatter ('%'::'s'::str) = Str $ formatter str
formatter ('%'::'d'::str) = Num $ formatter str
formatter ('%'::'c'::str) = Ch $ formatter str
formatter ('%'::'f'::str) = Flt $ formatter str
formatter (c::str) = case formatter str of
                        Lit s fmt => Lit (singleton c ++ s) fmt
                        _ => Lit (cast c) $ formatter str
formatter [] = End

sprintf : (str: String) -> PrintfType $ unpack str
sprintf str = helper _ ""