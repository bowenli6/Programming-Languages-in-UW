

fun is_older (dx: int * int * int, dy: int * int * int) =
    if dx = dy then false
    else if #1 dx < #1 dy then true
    else if #1 dx = #1 dy andalso #2 dx < #2 dy then true
    else if #2 dx = #2 dy andalso #3 dx < #3 dy then true
    else false

						       
fun number_in_month (dates: (int * int * int) list, month: int) =
    if null dates then 0
    else (if #2 (hd dates) = month then 1 else 0) 
          + number_in_month(tl dates, month)


fun number_in_months (dates: (int * int * int) list, months: int list) =
    if null dates then 0
    else if null months then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)



fun dates_in_month (dates: (int * int * int) list, month: int) =
    if null dates then []
    else
	let val rest_of_dates = dates_in_month(tl dates, month)
	in
	    if #2 (hd dates) = month
	    then hd dates :: rest_of_dates
	    else rest_of_dates
	end

fun dates_in_months (dates: (int * int * int) list, months: int list) =
    if null dates then []
    else if null months then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)


fun get_nth (strings: string list, n: int) =
    if n = 1 then hd strings
    else get_nth(tl strings, n - 1)

		
fun date_to_string (date: int * int * int) =
    let val months = ["January", "February", "March", "April", "May", "June", "July",
		      "August", "September", "October", "November", "Decembe"]
    in get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end


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


fun what_month (day: int) =
    let val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in number_before_reaching_sum(day, days) + 1
    end

	
fun month_range (day1: int, day2: int) =
    if day1 > day2 then []
    else what_month(day1) :: month_range(day1 + 1, day2)
					


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

	    
