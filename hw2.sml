fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* 1a *)
fun all_except_option(str : string, list : string list) =
    case list of
	[] => NONE
      | x::list' => if same_string(str, x)
                    then SOME list'
                    else case all_except_option(str, list') of
                             NONE => NONE
			   | SOME i => SOME(x::i)

(* 1b *)
fun get_substitutions1(list : string list list, str : string) =
    case list of
	[] => []
      | x::list' => case all_except_option(str, x) of
			NONE => get_substitutions1(list', str)
		      | SOME i => i @ get_substitutions1(list', str)

(* 1c *)
fun get_substitutions2(list : string list list, str : string) =
    let fun helper(list : string list list, prev_list : string list) =
	    case list of
		[] => prev_list
	      | x::list' => case all_except_option(str, x) of
				NONE => helper(list', prev_list)
			      | SOME i => helper(list', prev_list @ i)
    in
	helper(list, [])
    end

(* 1d *)
fun similar_names(list : string list list, {first : string, last : string, middle : string}) =
    let val names = first :: get_substitutions2(list, first)
	fun helper(names : string list) =
	    case names of
		[] => []
	      | x::names' => {first = x, last = last, middle = middle} :: helper(names')
    in
	helper(names)
    end
    
(* 2 *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* 2a *)
fun card_color(c : card) =
    case c of
	(Clubs, _) => Black
      | (Spades, _) => Black
      | (_, _) => Red

(* 2b *)
fun card_value(c : card) =
    case c of
	(_, Ace) => 11
      | (_, Num i) => i
      | (_, _) => 10
      
(* 2c *)
fun remove_card(cs : card list, c : card, e : exn) =
    case cs of
	[] => raise e
      | x::cs' => if x = c
		  then cs'
		  else x :: remove_card(cs', c, e)

(* 2d *)
fun all_same_color(cs : card list) =
    case cs of
        [] => true
      | [_] => true
      | head::neck::tail => card_color head = card_color neck andalso all_same_color(neck::tail)

(* 2e *)
fun sum_cards(cs : card list) =
    let fun helper(cs : card list, acc : int) =
	    case cs of
		[] => acc
	      | x::cs' => helper(cs', card_value(x) + acc)
    in
	helper(cs, 0)
    end

(* 2f *)
fun score(cs : card list, goal : int) =
    let val sum = sum_cards(cs)
	val preliminary_score = if sum > goal
				then 3 * (sum - goal)
				else goal - sum
    in
	if all_same_color(cs)
	then preliminary_score div 2
	else preliminary_score
    end

(* 2g *)
fun officiate (cs : card list, ms : move list, goal : int) =
    let fun held_cards(cs : card list, m : move, held : card list) =
	    case m of
	        Discard i => remove_card(held, i, IllegalMove)
	      | Draw => case cs of
			    x::cs' => x :: held
	fun score_helper(cs : card list, ms : move list, goal : int, held : card list) =
	    if sum_cards(held) > goal
	    then score(held, goal)
	    else case ms of
		     [] => score(held, goal)
		   | x::ms' => case x of
				   Discard i => score_helper(cs, ms', goal, held_cards(cs, x, held))
				 | Draw => case cs of
					       [] => score(held, goal)
					     | y::cs' => score_helper(cs', ms', goal, held_cards(cs, x, held))
    in
	score_helper(cs, ms, goal, [])
    end

(* 3a *)
fun score_sum(sum : int, color : bool, goal : int) =
    let val preliminary_score = if sum > goal
				then 3 * (sum - goal)
				else goal - sum
    in
	if color
	then preliminary_score div 2
	else preliminary_score
    end

fun score_challenge(cs : card list, goal : int) =
    let fun ace_counter(cs) =
	    case cs of
		[] => 0
	      | x::cs' => case x of
			      (_, Ace) => ace_counter(cs') + 1
			    | (_, _) => ace_counter(cs')
	val aces = ace_counter(cs)
	val color = all_same_color(cs)
	val sum = sum_cards(cs)
	val score = score_sum(sum, color, goal)
	fun compare(aces, sum, goal, score) =
	    if aces < 1
	    then score
	    else let val aces = aces - 1
		     val sum = sum - 10
		     val score2 = score_sum(sum, color, goal)
		     val new_score = if score < score2
				     then score
				     else score2
		 in
		     compare(aces, sum, goal, new_score)
		 end
    in
	compare(aces, sum, goal, score)
    end

fun card_value1(c : card) =
    case c of
	(_, Ace) => 1
      | (_, Num i) => i
      | (_, _) => 10

fun sum_cards1(cs : card list) =
    let fun helper(cs : card list, acc : int) =
	    case cs of
		[] => acc
	      | x::cs' => helper(cs', card_value1(x) + acc)
    in
	helper(cs, 0)
    end

fun officiate_challenge (cs : card list, ms : move list, goal : int) =
    let fun held_cards(cs : card list, m : move, held : card list) =
	    case m of
	        Discard i => remove_card(held, i, IllegalMove)
	      | Draw => case cs of
			    x::cs' => x :: held
	fun score_helper(cs : card list, ms : move list, goal : int, held : card list) =
	    if sum_cards1(held) > goal
	    then score_challenge(held, goal)
	    else case ms of
		     [] => score_challenge(held, goal)
		   | x::ms' => case x of
				   Discard i => score_helper(cs, ms', goal, held_cards(cs, x, held))
				 | Draw => case cs of
					       [] => score_challenge(held, goal)
					     | y::cs' => score_helper(cs', ms', goal, held_cards(cs, x, held))
    in
	score_helper(cs, ms, goal, [])
    end

(* 3b *)
fun rev xs =
    let fun aux(xs, acc) =
            case xs of
                [] => acc
              | x::xs' => aux(xs', x::acc)
    in
        aux(xs, [])
    end

fun careful_player (cs : card list, goal) =
    let fun held_cards(cs : card list, ms : move list, held : card list) =
	    case ms of
		[] => held
	      | x::ms' => case x of
			      Discard i => remove_card(held, i, IllegalMove)
			    | Draw => case cs of
					  x::cs' => x :: held
	fun cheat(cs : card list, held : card list, goal : int, sum : int) =
	    case cs of
		[] => (Spades, Num 0)
	      | x::cs' => case held of
			      [] => (Spades, Num 0)
			    | y::held' => if sum - card_value(y) + card_value(x) = goal
					  then y
					  else cheat(cs, held', goal, sum)
	fun score_helper(cs : card list, goal : int, held : card list, ms : move list) =
	    if sum_cards(held) > goal - 10
	    then if sum_cards(held) = goal
		 then ms
		 else let val new_card = cheat(cs, held, goal, sum_cards(held))
		      in
			  if new_card = (Spades, Num 0)
			  then ms
			  else Draw :: Discard new_card :: ms
		      end
	    else case cs of
		     [] => Draw :: ms
		   | x::cs' => score_helper(cs', goal, held_cards(cs, Draw :: ms, held), Draw :: ms)
    in
	rev(score_helper(cs, goal, [], []))
    end
