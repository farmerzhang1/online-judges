module DndPem

%access export
%default total

--- You need this to get the final submit work
-- 排中律 a -> Void的意思是不存在a implies Void
-- 類型體操...
public export
AxiomPEM : Type
AxiomPEM = {a : Type} -> {b : Type} -> (a -> b) -> ((a -> Void) -> b) -> b

public export
AxiomDNE : Type
AxiomDNE = {a : Type} -> ((a -> Void) -> Void) -> a -- this can also be b

from : AxiomDNE -> AxiomPEM
from dne ab avb = dne $ \bv => bv (avb $ bv . ab)

to : AxiomPEM -> AxiomDNE
to pem notnota_orb = let test = absurd . notnota_orb in pem id test
-- 我完全不理解這個！！！why id?