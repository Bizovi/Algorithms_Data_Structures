use "calendar.sml";

val test11 = isOlder ((2010, 05, 01), (2011, 05, 01))
val test12 = isOlder ((2010, 05, 01), (2010, 05, 01))
val test13 = isOlder ((2011, 05, 01), (2011, 05, 11))
val test14 = isOlder ((2011, 05, 01), (2011, 04, 01))
val test15 = isOlder ((2011, 03, 01), (2011, 04, 01))