
(*  
    Write a function is_older that takes two dates and evaluates to true or 
    false. It evaluates to true if
    the First argument is a date that comes before the second argument.
    (If the two dates are the same,the result is false.)
 *)

fun is_older (dx: int * int * int, dy: int * int * int) =
    if dx = dy then false
    else if #1 dx < #1 dy then true
    else if #1 dx = #1 dy andalso #2 dx < #2 dy then true
    else if #2 dx = #2 dy andalso #3 dx < #3 dy then true
    else false


(*
    Write a function number_in_month that takes a list of dates and a month
    (i.e., an int)
    returns how many dates in the list are in the given month.
*)
							       
fun number_in_month (dates: (int * int * int) list, month: int) =
    if null dates then 0
    else (if #2 (hd dates) = month then 1 else 0) 
          + number_in_month(tl dates, month)

(*
    Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list)
    returns the number of dates in the list of dates that are in any of the months in the list of months. 
    Assume the list of months has no number repeated.
    Hint: Use your answer to the previous problem.
*)

fun number_in_months (dates: (int * int * int) list, months: int list) =
    if null dates then 0
    else if null months then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

							     
(*
    Write a function dates_in_month that takes a list of dates and a month (i.e., an int)
    returns a list holding the dates from the argument list of dates that are in the month. 
    The returned list should contain dates in the order they were originally given.
*)

fun dates_in_month (dates: (int * int * int) list, month: int) =
    if null dates then []
    else
	let val rest_of_dates = dates_in_month(tl dates, month)
	in
	    if #2 (hd dates) = month
	    then hd dates :: rest_of_dates
	    else rest_of_dates
	end

(*
    Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
    returns a list holding the dates from the argument list of dates that are in any of the months in the list of months. 
    Assume the list of months has no number repeated.
    Hint: Use your answer to the previous problem and SML’s list-append operator (@).
*)

fun dates_in_months (dates: (int * int * int) list, months: int list) =
    if null dates then []
    else if null months then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(*
   Write a function get_nth that takes a list of strings and an int n
   returns the nth element of the list where the head of the list is 1st.
   Do not worry about the case where the list has too few elements.
   your function may apply hd or tl to the empty list in this case, which is okay.
*)

fun get_nth (strings: string list, n: int) =
    if n = 1 then hd strings
    else get_nth(tl strings, n - 1)

(*
    Write a function date_to_string that takes a date
    returns a string of the form January 20, 2013 (for example).
    Use the operator ^ for concatenating strings 
    and the library function Int.toString for converting an int to a string. 
    For producing the month part, do not use a bunch of conditionals.
    Instead, use a list holding 12 strings and your answer to the previous problem.
    For consistency, put a comma following the day and use capitalized English month names: 
    January, February, March, April, May, June, July, August, September, October, November, December.
*)

fun date_to_string (date: int * int * int) =
    let val months = ["January", "February", "March", "April", "May", "June", "July",
		      "August", "September", "October", "November", "Decembe"]
    in get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

(*
    Write a function number_before_reaching_sum that takes an int called sum,
    which you can assume is positive, and an int list, which you can assume contains all positive numbers
    You should return an int n such that the first n elements of the list add to less than sum,
    but the first n + 1 elements of the list add to sum or more. 
    Assume the entire list sums to more than the passed in value; 
    it is okay for an exception to occur if this is not the case.
*)

fun number_before_reaching_sum (sum: int, numbers: int list) =
    if null numbers then 0
    else if hd numbers > sum then 0
    else
	let fun helper (i: int, s: int, nums: int list) =
		if s <= 0 then i - 1
                else
		    helper(i + 1, s - hd nums, tl nums)
	in helper(0, sum, numbers)
	end

 (*
     Write a function what_month that takes a day of year
     (i.e., an int between 1 and 365) 
     returns what month that day is in (1 for January, 2 for February, etc.).
     Use a list holding 12 integers and your answer to the previous problem.
 *)

fun what_month (day: int) =
    let val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in number_before_reaching_sum(day, days) + 1
    end

(*
    Write a function month_range that takes two days of the year day1 and day2
    returns an int list [m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ...
    and mn is the month of day day2.
    Note the result will have length day2 - day1 + 1 or length 0 if day1>day2.
*)

fun month_range (day1: int, day2: int) =
    if day1 > day2 then []
    else what_month(day1) :: month_range(day1 + 1, day2)
					
(*
    Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. 
    It evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list.
*)

fun oldest (dates: (int * int * int) list) =
    if null dates then NONE
    else if null (tl dates) then SOME(hd dates)
    else
	let fun oldest (d: (int * int * int) list) =
		if null (tl d) then hd d
		else
		    let val tail = oldest(tl d)
		    in if is_older(hd d, tail) then hd d else tail
		    end
	in SOME(oldest(dates))
	end		  
				 
(*
    Challenge Problem:
    Write functions number_in_months_challenge and dates_in_months_challenge 
    that are like your solutions to problems 3 and 5 
    except having a month in the second argument multiple times has no more effect than having it once. 
    (Hint: Remove duplicates, then use previous work.)	
*)

fun remove_duplicates (months: int list) =
    if null months then []
    else
	(* remove all the elements that are same as hd months *)
	let fun filter(target: int, m: int list) =
		if null m then []
		else
		    let
			val rest_of_months = filter(target, tl m)
		    in
			if target = hd m then rest_of_months
			else hd m :: rest_of_months 
		    end
	in
	   hd months :: remove_duplicates(filter(hd months, tl months))
	end
	    
fun number_in_months_challenge (dates: (int * int * int) list, months: int list) =
    number_in_months(dates, remove_duplicates(months))

fun dates_in_months_challenge (dates: (int * int * int) list, months: int list) =
    dates_in_months(dates, remove_duplicates(months))
	 

(*
    Challenge Problem: 
    Write a function reasonable_date that takes a date and determines if it describes a real date in the common era. 
    A “real date” has a positive year (year 0 did not exist), a month between 1 and 12, and a day appropriate for the month. 
    Solutions should properly handle leap years.
    Leap years are years that are either divisible by 400 or divisible by 4 but not divisible by 100. 
    (Do not worry about days possibly lost in the conversion to the Gregorian calendar in the Late 1500s.)
*)

fun reasonable_date (date: int * int * int) =
    let
	val year = #1 date
	val month = #2 date
	val day = #3 date
	val bigMonths = [1, 3, 5, 7, 8, 19, 12]
	val shortMonths = [4, 6, 9, 11]
	fun checkMonth (target: int, months: int list) =
	    if null months then false
	    else if target = hd months then true					
	    else checkMonth(target, tl months)
	fun isLeap (year: int) =
	    if year mod 400 = 0 orelse year mod 4 = 0 andalso year mod 100 <> 0
	    then true else false       
    in
	if year <= 0 orelse month < 1 orelse month > 12 orelse day < 1 then false							
	else if checkMonth(month, bigMonths) andalso day > 31 then false
	else if checkMonth(month, shortMonths) andalso day > 30 then false
	else if isLeap(year) andalso month = 2 andalso day > 29 then false
	else if not (isLeap(year)) andalso month = 2 andalso day > 28 then false							
	else true
    end

	    
