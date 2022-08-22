(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2;

(* put your solutions for problem 1 here *)
fun all_except_option(s:string, slist:string list) =
let  fun helper (s:string,[], hdlist: string list) = NONE
| helper (s:string,slist:string list, hdlist: string list) =
if (same_string(s,hd slist)) then
   SOME (hdlist@(tl slist))
   else
   helper(s,tl slist,hdlist@[hd slist])
   in
   helper(s,slist,[])
   end;

   all_except_option("asd",["asd","asdasd","sdsadw","wwwww"]);


fun get_substitutions1 ([], _) = []
  | get_substitutions1 (smat: string list list, s: string) =
    case (all_except_option (s, hd smat)) of
         NONE => get_substitutions1 (tl smat, s)
       | SOME slist => slist @ get_substitutions1 (tl smat, s);


get_substitutions1([["asd","asdasd","sdsadw","wwwww"],["asds","asdasd","sdsadw","wwwww"],["asd","f","x","n"]],"asd");


fun get_substitutions2 (smat: string list list, s: string) =
let

  fun check (NONE) = []
    | check (SOME slist) = slist

  fun get_sub (sm,[]) = sm
    | get_sub (sm,smat) =
      get_sub (sm @ check (all_except_option (s, hd smat)),tl smat)
in
  get_sub ([],smat)
end;


get_substitutions2([["asd","asdasd","sdsadw","wwwww"],["asds","asdasd","sdsadw","wwwww"],["asd","f","x","n"]],"asd");


fun similar_names (substitutions, {first=first, middle=middle, last=last}) =
let

  fun generate_names ([], acc) = acc
    | generate_names (a::tll, acc) = generate_names (tll, acc @ [{first=a, middle=middle, last=last}])
in
  generate_names (get_substitutions2 (substitutions, first),
                  [{first=first, middle=middle, last=last}])
end;


similar_names([["asd","asdasd","sdsadw","wwwww"],["asds","5455d","sd123w","wasdw"],["asd","f","x","n"]],{first="asd", middle="Smerechuk", last="Mykolayovich"});

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove





(* put your solutions for problem 2 here *)

fun card_color (suit, _) =
  case suit of
       Clubs => Black
     | Diamonds => Red
     | Hearts => Red
     | Spades => Black;

fun card_value (suit,rank) =
  case rank of
   Ace => 11
   | Num x => x
   | _ => 10;


fun remove_card (cs, c, e) =
let
  fun removeR (_, []) = raise e
    | removeR (al, a::l) =
    if a = c
    then
      al @ l
    else
      removeR(al @ [a], l)
in
  removeR ([], cs)
end;

fun all_same_color (cs) =
  case cs of
   [] => true
   | _::[] => true
   | a::l =>
      card_color a = card_color (hd l) andalso (all_same_color (hd l::tl l));

fun sum_cards (cs) =
let
  fun sum ([], sm) = sm
    | sum (a::l, sm) = sum (l, card_value a + sm)
in
  sum (cs, 0)
end;


fun score (cs, goal) =
let
  fun abs (x) = if x >= 0 then x else ~x
  val is_all_same_color = all_same_color cs
  val sum = sum_cards cs
  val is_greater = sum > goal
  val base = abs (sum - goal)
in
  case (is_all_same_color, is_greater) of
       (false, false) => base
     | (false, true) => 3 * base
     | (true, false) => base div 2
     | (true, true) => 3 * base div 2
end;
score([(Clubs, Num 1), (Clubs, King)],1);