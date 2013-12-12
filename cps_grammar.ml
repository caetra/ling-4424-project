(* continuized grammar for the final project *)

(* mostly following van Eijck and Unger chapter 11 *)

(* Apply to x to raise to CPS, then apply a continuation to get value *)
(* Type is 'a -> ('a -> 'b) -> 'b *)
let cps_terminal x cont = cont x

(* The grammar from Barker 2002:
    S -> NP VP
    NP -> Det N
    NP -> John
    NP -> Mary
    NP -> everyone
    NP -> someone
    Det -> every
    Det -> some
    N -> man
    N -> woman
    VP -> Vt NP
    VP -> left
    Vt -> saw *)
    
type category = S | NP | VP | Det | N | Vt

type 'a term = (category * 'a)

type entity = John | Mary | Dave | Anna (* adding a couple names for variety *)

(* quick and dirty hardcoding, arbitrary truth values *)
let leave = function
  | John -> true
  | Mary -> false
  | Dave -> true
  | Anna -> true

let see x y = match (x,y) with
  | (John, Mary) -> true
  | (Mary, John) -> true
  | (John, Dave) -> true
  | (Dave, John) -> false
  | (John, Anna) -> false
  | (Anna, John) -> false
  | (Mary, Dave) -> false
  | (Dave, Mary) -> false
  | (Mary, Anna) -> false
  | (Anna, Mary) -> true
  | (Dave, Anna) -> true
  | (Anna, Dave) -> true
  (* everyone saw themselves *)
  | (_, _) -> true
(* see x y -> true means that y saw x *)
(* for the purposes of testing inverse scoping, these are constructed so that
   'every man saw a woman' is true linearly scoped
   but false inverse scoped ('some woman was seen by every man') *)

(* for things like 'every man', 'a woman', gender is a property *)
let male = function
  | John -> true
  | Dave -> true
  | _ -> false
  
let female x = not (male x)

(* Continuize this grammar, start by continuizing the terminals *)
let john = (NP, (cps_terminal John))
let mary = (NP, (cps_terminal Mary))
let dave = (NP, (cps_terminal Dave))
let anna = (NP, (cps_terminal Anna))

(* Quantifiers! 
   Use built-in list functions on list of everyone to apply continuation
   function to all of them.
   List.for_all f lst returns if f is true for all elements of lst
   List.exists f lst returns if f is true for at least one element *)
let all_entities = [John; Dave; Mary; Anna]
let everyone = (NP, fun cont -> List.for_all cont all_entities)
let someone = (NP, fun cont -> List.exists cont all_entities)

(* Quantified dets
   Basically it returns something that works like 'everyone'
   But with the list of entities filtered *)
let every = (Det, fun cont x -> cont (fun y -> List.for_all x (List.filter y all_entities)))
let a = (Det, fun cont x -> cont (fun y -> List.exists x (List.filter y all_entities)))

let man = (N, (cps_terminal male))
let woman = (N, (cps_terminal female))

let left = (VP, (cps_terminal leave))
let saw = (Vt, (cps_terminal see))

(* how do we do the binary rules? since everything is a function in CPS, a binary rule is function application.
   so we need something to apply functions when they're CPSed.
   intuitively, evaluate one, make continuation, give that to other to evaluate, make continuation *)
let cps_apply f g = fun cont -> g (fun b -> f (fun a -> cont (a b)))

(* alternate version for inverse scope
   evaluates f and g in reverse order *)
let cps_apply' f g = fun cont -> f (fun a -> g (fun b -> cont (a b)))

(*let np det n = match (det, n) with
  | ((Det, a), (N, b)) -> (NP, (cps_apply a b)) 
  | _ -> failwith "shouldn't happen"*)
  
let np det n = match (det, n) with
  | ((Det, a), (N, b)) -> (NP, (a b)) 
  | _ -> failwith "shouldn't happen"

let vp vt np = match (vt, np) with
  | ((Vt, a), (NP, b)) -> (VP, (cps_apply a b))
  | _ -> failwith "shouldn't happen"
  
(* finally work up to sentence *)
let sentence np vp = match (np, vp) with
  | ((NP, a), (VP, b)) -> (S, (cps_apply b a)) (* note that we reverse order here *)
  | _ -> failwith "shouldn't happen"
  
(* inverse scope version *)
let sentence' np vp = match (np, vp) with
  | ((NP, a), (VP, b)) -> (S, (cps_apply' b a)) (* note that we reverse order here *)
  | _ -> failwith "shouldn't happen"

(* to get truth values out *)
let identity = fun x -> x
let eval words cont = (snd words) cont
(* eval (sentence whatever) identity evaluates truth of whatever *)

(* try non-quantified sentences
# eval (sentence john left) identity;;
- : bool = true
# eval (sentence mary left) identity;;
- : bool = false
# eval (sentence dave (vp saw john)) identity;;     
- : bool = true
# eval (sentence john (vp saw dave)) identity;;
- : bool = false
# eval (sentence john (vp saw john)) identity;;
- : bool = true
*)

(* try simple quantifiers
# eval (sentence everyone left) identity;;
- : bool = false
# eval (sentence someone left) identity;; 
- : bool = true
# eval (sentence (np every man) left) identity;;  
- : bool = true
# eval (sentence (np every woman) left) identity;;
- : bool = false
# eval (sentence (np a woman) left) identity;;    
- : bool = true
# eval (sentence (np a man) left) identity;;      
- : bool = true
*)

(* okay, now for inverse scoping *)

(*
# eval (sentence (np every man) (vp saw (np a woman))) identity;;
- : bool = true
This is true under the way we evaluate it by default:
'Every man' evaluates first
John saw Mary, and Dave saw Anna
So John saw a woman, and Dave saw a woman.
So by linear scope, for every man, there is some woman (not necessarily the same) seen by that man.*)

(*
# eval (sentence' (np every man) (vp saw (np a woman))) identity;;
- : bool = false
Using the inverse scoped version of sentence, this now evaluates to false
'A woman' evaluates first
Mary was seen by John, but not Dave
Anna was seen by Dave, but not John
So there is no woman such that every man saw that same woman.
Thus, this sentence is false when read with the inverse scoping. *)