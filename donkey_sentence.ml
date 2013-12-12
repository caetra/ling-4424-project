(* "Donkey sentences" are sentences with the structure illustrated by:
    Every farmer who owns a donkey beats it.
    Basically the problem with these in semantics is that the quantifiers don't work intuitively when translated to logic.
    The way we'd like to write it is:
    for all x (farmer(x)^there exists y (donkey(y)^owns(x,y)) ==> beats(x,y))
    But that doesn't work because we don't know what y is in beats(x,y)
    Normally, "every" means "for all" and "a" means "there exists".
    But with the donkey sentence, both become "for all": the farmer does not beat an arbitrary donkey, he beats the donkey that he owns.
    for all x, y farmer(x)^donkey(y)^owns(x,y) ==> beats(x,y)
    Or in words, we restate the sentence as:
    If a farmer owns a donkey, then he beats it. *)
    
(* grammar...

    S -> NP VP
    NP -> NP SubjRel
    NP -> Det N
    SubjRel -> who VP
    VP -> Vt NP