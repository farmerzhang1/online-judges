module Solution

%access export
%default total

random_fact_about_filtering :
  (l : List a) -> (f : a -> Bool) -> (g : a -> Bool) ->
  length (filter (\x => f x || g x) l) `LTE`
    length (filter f l) + length (filter g l)
random_fact_about_filtering = ?prf