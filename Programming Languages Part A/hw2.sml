(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2
		       
(*
   Write a function all_except_option, which takes a string and a string list
   Return NONE if the string is not in the list, 
   else return SOME lst where lst is identical to the argument list except the string is not in it. 
   You may assume the string is in the list at most once. 
   Use same_string, provided to you, to compare strings.
   Sample solution is around 8 lines.
*)	     

fun all_except_option (str, strs) =
    case strs of
	[] => NONE
     | x::xs => case same_string(str, x) of
		true => SOME xs
	      | false => case all_except_option(str, xs) of
			     NONE => NONE
			   | SOME y => SOME(x::y)

(*
   Write a function get_substitutions1, which takes a string list list
   (a list of list of strings, the substitutions) and a string s
   returns a string list. The result has all the strings that are in some list in substitutions that also has s
   but s itself should not be in the result. 
   Example:get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred")
   Answer: ["Fredrick","Freddie","F"] 
*) 
					   
fun get_substitutions1 (sub, s) =
    case sub of
	[] => []
      | x::xs => case all_except_option(s, x) of
		     NONE => get_substitutions1(xs, s)
		   | SOME y => y @ get_substitutions1(xs,s)

						     
(* Write a function get_substitutions2, which is like get_substitutions1
 except it uses a tail-recursive local helper function. *)

fun get_substitutions2 (sub, s) =
    let fun aux (sub, acc) =
	    case sub of
		[] => acc
	      | x::xs => case all_except_option(s, x) of
			     NONE => aux(xs, acc)
			   | SOME y => aux(xs, acc @ y)
    in
	aux(sub, [])
    end

(*
    Write a function similar_names, which takes a string list list of substitutions (as in parts (b) and (c))
    and a full name of type {first:string,middle:string,last:string}
    returns a list of full names (type {first:string,middle:string,last:string} list). 
    The result is all the full names you can produce by substituting for the first name (and only the first name)
    using substitutions and parts (b) or (c). 
    The answer should begin with the original name (then have 0 or more other names). 
    Example:
    similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"})
    Answer: [{first="Fred", last="Smith", middle="W"},
                 {first="Fredrick", last="Smith", middle="W"},
                 {first="Freddie", last="Smith", middle="W"},
                 {first="F", last="Smith", middle="W"}]
*)
type name = {first: string, middle: string, last: string}

fun similar_names (sub, name) =
    let val {first = f, middle = m, last = l} = name
	fun aux (sub, acc) =
	    case sub of
		[] => acc
	      | x::xs => aux(xs, acc @ [{first = x, middle = m, last = l}])
    in
	aux(get_substitutions2(sub, f), [name])
    end

	
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(*
   Write a function card_color, which takes a card and returns its color 
   (spades and clubs are black, diamonds and hearts are red). 
   Note: One case-expression is enough.
*)
fun card_color card =
    case card of
        (Clubs,_)    => Black
      | (Diamonds,_) => Red
      | (Hearts,_)   => Red
      | (Spades,_)   => Black

(*
    Write a function card_value, which takes a card and returns its value 
    (numbered cards have their number as the value, aces are 11,
    everything else is 10). Note: One case-expression is enough.
 *)
fun card_value card =
    case card of
	(_,Jack) => 10
      | (_,Queen) => 10
      | (_,King) => 10
      | (_,Ace) => 11 
      | (_,Num n) => n
		   
(*
   Write a function remove_card, which takes a list of cards cs, a card c, and an exception e. 
   It returns a list that has all the elements of cs except c. 
   If c is in the list more than once, remove only the first one. 
   If c is not in the list, raise the exception e. 
   You can compare cards with =.
*)

fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
      | x::xs => case x = c of
		     true => xs
		   | false => case remove_card(xs, c, e) of
				  [] => [x]
				| y::ys => x::y::ys

(*
   Write a function all_same_color, which takes a list of cards
   returns true if all the cards in the list are the same color.
   Hint: An elegant solution is very similar to one of the functions using nested pattern-matching in the lectures.
*)

fun all_same_color (cards) = 
    case cards of
	[] => true
      | x::[] => true
      | x::y::xs => case card_color(x) = card_color(y) of
			   false => false
			 | true => all_same_color(y::xs)

(*
   Write a function sum_cards, which takes a list of cards
   returns the sum of their values. Use a locally defined helper function that is tail recursive.
   (Take “calls use a constant amount of stack space” as a requirement for this problem.)
*)

fun sum_cards (cards) =
    let fun aux (cards, acc) =
	    case cards of
		[] => acc
	      | x::xs => aux(xs, acc + card_value(x)) 
    in
	aux(cards, 0)
    end
	
(*
   Write a function score, which takes a card list (the held-cards) 
   and an int (the goal) and computes the score as described above.
*)

fun score (held, goal) =
    let val sum = sum_cards(held)
	fun game (held) =
	    case sum > goal of
		 true => (sum - goal) * 3
               | false => goal - sum 
    in
	case all_same_color(held) of
	    true => game(held) div 2
	  | false => game(held) 	    
    end

(*
   Write a function officiate, which “runs a game.” 
   It takes a card list (the card-list) a move list (what the player “does” at each point), and an int (the goal)
   returns the score at the end of the game after processing (some or all of) the moves in the move list in order. 
   Use a locally defined recursive helper function that takes several arguments that together represent the current state of the game. 
   As described above:
   • The game starts with the held-cards being the empty list.
   • The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
   • If the player discards some card c, play continues (i.e., make a recursive call)
   with the held-cards not having c and the card-list unchanged. 
   If c is not in the held-cards, raise the IllegalMove exception.
   • If the player draws and the card-list is (already) empty, the game is over. 
   Else if drawing causes the sum of the held-cards to exceed the goal, the game is over (after drawing). 
   Else play continues with a larger held-cards and a smaller card-list.
*)

fun officiate (cards, move, goal) =
    let fun game (cards, move, held) =
	case move of
	    [] => held
	  | x::xs => case x of
			 Discard card => game(cards, xs, remove_card(held, card, IllegalMove))
		       | Draw => case cards of
				     [] => held
				   | y::_ => case sum_cards(y::held) > goal of
                                                 true => y::held
                                               | false => game(remove_card(cards, y, IllegalMove), xs, y::held)   
    in
	score(game(cards, move, []), goal)
    end



		
