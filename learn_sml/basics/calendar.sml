(*
 * Function related to calendar dates (day, month, year)
 * Year > 0, 1 <= Month <= 12, day <= 31
 * 1 <= Day of year <= 365 (33 === February 2nd) [Leap Year?]
 *)


 (* Pr1: First date comes before the second. 
  * Lexicographic comparison seems to be the way to go ...most precise
  *)

 fun isOlder(dt1 : (int * int * int), dt2 : (int * int * int)) = 
    let
      val yr1 = #1 dt1
      val yr2 = #1 dt2
      val mon1 = #2 dt1
      val mon2 = #2 dt2
      val day1 = #3 dt1
      val day2 = #3 dt2
    in
      yr1 < yr2 orelse yr1 > yr2 orelse false
      (* ( 
        (* mon1 < mon2 orelse not (mon1 > mon2) orelse ( *)
            (* day1 < day2 orelse not (day1 > day2) 
            orelse not(yr1 = yr2 andalso mon1 = mon2 andalso day1 = day2) *)
            (* orelse false *)
            (* false *)
        )
      ) *)
    end