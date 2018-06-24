(*1.Assumes reasonable dates*)
fun is_older (date1 : int * int * int, date2 : int * int * int) =
    if #1 date1 < #1 date2
    then true
    else if #1 date1 = #1 date2 andalso #2 date1 < #2 date2
    then true
    else if #2 date1 = #2 date2 andalso #3 date1 < #3 date2
    then true
    else false

(*2.Assumes reasonable dates and a reasonable month*)	     
fun number_in_month (list_of_dates : (int * int * int) list, month : int) =
    if null list_of_dates
    then 0
    else if #2 (hd list_of_dates) = month
    then number_in_month(tl list_of_dates, month) + 1
    else number_in_month(tl list_of_dates, month)

(*3.Assumes reasonable dates and reasonable non-repeating months*)	    
fun number_in_months (list_of_dates : (int * int * int) list, list_of_months : int list) =
    if null list_of_months
    then 0
    else number_in_months(list_of_dates, tl list_of_months) + number_in_month(list_of_dates, hd list_of_months)
	    
(*4.Assumes reasonable dates and a reasonable month*)	    
fun dates_in_month (list_of_dates : (int * int * int) list, month : int) =
    if null list_of_dates
    then []
    else if #2 (hd list_of_dates) = month
    then (hd list_of_dates) :: dates_in_month(tl list_of_dates, month)
    else dates_in_month(tl list_of_dates, month)

(*5.Assumes reasonable dates and reasonable non-repeating months*)	    
fun dates_in_months (list_of_dates : (int * int * int) list, list_of_months : int list) =
    if null list_of_months
    then []
    else dates_in_month(list_of_dates, hd list_of_months) @ dates_in_months(list_of_dates, tl list_of_months)

(*6.Assumes a non-empty list and an index > 0*)
fun get_nth (list_of_strings : string list, index : int) =
    if index = 1
    then hd list_of_strings
    else get_nth(tl list_of_strings, index - 1)

(*7.Assumes a valid date*)
fun date_to_string (date : int * int * int) =
    let	val english_months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(english_months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

(*8.Assumes that all numbers are positive and that the sum of the number of the list is bigger than sum*)
fun number_before_reaching_sum (sum : int, positive_list : int list) =
    if sum <= hd positive_list
    then 0
    else number_before_reaching_sum(sum - hd positive_list, tl positive_list) + 1

(*9.Assumes a reasonable day*)
fun what_month (day : int) =
    let	val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum(day, month_days) + 1
    end

(*10.Assumes reasonable days*)
fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

(*11.Assumes reasonable dates*)
fun oldest (list_of_dates : (int * int * int) list) =
    if null list_of_dates
    then NONE
    else let fun oldest_nonempty (list_of_dates : (int * int * int) list) =
		 if null (tl list_of_dates)
		 then hd list_of_dates
		 else let val current_oldest = oldest_nonempty(tl list_of_dates)
		      in
			  if is_older(hd list_of_dates, current_oldest)
			  then hd list_of_dates
			  else current_oldest
		      end
	 in
	     SOME (oldest_nonempty list_of_dates)
	 end

(*12.Assumes reasonable dates and reasonable months*)
fun find_duplicates (duplicate : int, list_of_months : int list) =
    if null list_of_months
    then false
    else if hd list_of_months = duplicate
    then true
    else find_duplicates(duplicate, tl list_of_months)

fun delete_duplicates (list_of_months : int list) =
    if null list_of_months
    then []
    else if find_duplicates(hd list_of_months, tl list_of_months)
    then delete_duplicates(tl list_of_months)
    else (hd list_of_months) :: delete_duplicates(tl list_of_months)

fun number_in_months_challenge (list_of_dates : (int * int * int) list, list_of_months : int list) =
    number_in_months(list_of_dates, delete_duplicates(list_of_months))

fun dates_in_months_challenge (list_of_dates : (int * int * int) list, list_of_months : int list) =
    dates_in_months(list_of_dates, delete_duplicates(list_of_months))		    

(*13.Checks for a reasonable date*)		    
fun leap_year (year : int) =
    year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)

fun day_of_month (year : int, month : int, day : int) =
    if month <= 12 andalso month >= 1 andalso day >= 1
    then ((month = 1 orelse month = 3 orelse month = 5 orelse month = 7 orelse month = 8 orelse month = 10 orelse month = 12) andalso day <= 31) orelse ((month = 4 orelse month = 6 orelse month = 9 orelse month = 11) andalso day <= 30) orelse month = 2 andalso ((day <= 28 andalso not (leap_year(year))) orelse (day <=29 andalso leap_year(year)))
    else false

fun reasonable_date (date : int * int * int) =
    #1 date > 0 andalso day_of_month(#1 date, #2 date, #3 date)
