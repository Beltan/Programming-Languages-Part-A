(* 1 *)
fun only_capitals list = List.filter(fn x => Char.isUpper(String.sub(x, 0))) list

(* 2 *)
fun longest_string1 list = List.foldl (fn (x,y) => if String.size x > String.size y then x else y) "" list

(* 3 *)
fun longest_string2 list = List.foldl (fn (x,y) => if String.size x >= String.size y then x else y) "" list

(* 4 *)
fun longest_string_helper f list = List.foldl (fn (x, y) => if f(String.size x, String.size y) then x else y) "" list

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

(* 5 *)
val longest_capitalized = longest_string3 o only_capitals

(* 6 *)
val rev_string = String.implode o List.rev o String.explode

exception NoAnswer

(* 7 *)
fun first_answer f list =
    case list of
	[] => raise NoAnswer
      | x::list' => case (f x) of
			NONE => first_answer f list'
		      | SOME v => v
		      
(* 8 *)
fun all_answers f list =
    let fun helper acc list =
		   case list of
		       [] => SOME acc
		     | x::list' => case (f x) of
				       NONE => NONE
				     | SOME v => helper (v @ acc) list'
    in
	helper [] list
    end

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let val r = g f1 f2 
    in
	case p of
	    Wildcard => f1 ()
	  | Variable x => f2 x
	  | TupleP ps => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _ => 0
    end

(* 9a *)
val count_wildcards = g (fn _ => 1) (fn _ => 0)

(* 9b *)
val count_wild_and_variable_lengths = g (fn _ => 1) String.size

(* 9c *)
fun count_some_var(v, p) = g (fn _ => 0) (fn x => if v = x then 1 else 0) p

(* 10 *)
fun check_pat pat =
    let fun new_list pat =
            case pat of
		Variable x => [x]
              | TupleP ps => List.foldl (fn (p, vs) => new_list p @ vs) [] ps
              | ConstructorP(_, p) => new_list p
              | _ => []
        fun unique xs =
            case xs of
		[] => true
              | x::xs' => (not (List.exists (fn y => y = x) xs')) andalso unique xs'
    in
        unique (new_list pat)
    end
	
(* 11 *)
fun match (v, p) =
    case (v, p) of
        (_, Wildcard) => SOME []
      | (vs, Variable x) => SOME [(x,vs)]
      | (Unit, UnitP) => SOME []
      | (Const x, ConstP y) => if x = y then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if length vs = length ps
                                 then let val x = ListPair.zip(vs, ps)
                                      in all_answers match x
                                      end
                                 else NONE
      | (Constructor (x, v), ConstructorP (x', p)) => if x = x' then match(v, p) else NONE
      | _ => NONE

(* 12 *)
fun first_match v ps =
    SOME (first_answer (fn x => match(v, x)) ps) handle NoAnswer => NONE
		
datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string
	     
(* Challenge problem not implemented *)
