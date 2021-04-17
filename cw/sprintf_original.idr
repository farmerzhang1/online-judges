module Sprintf
%access public export
%default total
data Format = Ch Format
            | Num Format
            | Flt Format
            | Str Format
            | Lit String Format
            | End
PrintfType : Format -> Type
PrintfType (Ch fmt) = (c: Char) -> PrintfType fmt
PrintfType (Flt fmt) = (d :Double) -> PrintfType fmt
PrintfType (Str fmt) = (s : String) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType (Num fmt) = (n : Integer) -> PrintfType fmt
PrintfType End = String

helper : (fmt : Format) -> (acc : String) -> PrintfType fmt
helper (Ch fmt) acc = \c => helper fmt $ acc ++ (cast c)
helper (Num fmt) acc = \n => helper fmt $ acc ++ (show n)
helper (Flt fmt) acc = \d => helper fmt $ acc ++ (show d)
helper (Str fmt) acc = \s => helper fmt $ acc ++ s
helper (Lit s fmt) acc = helper fmt $ acc ++ s
helper End acc = acc

formatter : List Char -> Format
formatter ('%'::'s'::str) = Str $ formatter str
formatter ('%'::'d'::str) = Num $ formatter str
formatter ('%'::'c'::str) = Ch $ formatter str
formatter ('%'::'f'::str) = Flt $ formatter str
formatter ('%'::'%'::str) = case formatter str of
                                Lit s fmt => Lit (singleton '%' ++ s) fmt
                                _ => Lit (singleton '%') $ formatter str
formatter (c::str) = case formatter str of
                        Lit s fmt => Lit (singleton c ++ s) fmt
                        _ => Lit (singleton c) $ formatter str
formatter [] = End
sprintf : (str: String) -> PrintfType $ formatter $ unpack str
sprintf str = helper _ ""