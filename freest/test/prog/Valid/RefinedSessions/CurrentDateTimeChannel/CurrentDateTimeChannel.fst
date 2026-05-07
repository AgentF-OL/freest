module CurrentDateTimeChannel where

type Year, Month, Day, Hour, Minute, Second, Date, Time : *T
type DateTimeServer, DateTimeClient : 1C

type Year   = {year:   Int | year >= 1970}
type Month  = {month:  Int | 1 <= month && month <= 12}
type Day    = {day:    Int | 1 <= day && day <= 31}
type Hour   = {hour:   Int | 0 <= hour && hour < 24}
type Minute = {minute: Int | 0 <= minute && minute < 60}
type Second = {second: Int | 0 <= second && second < 60}

type Date = (Year, Month, Day)
type Time = (Hour, Minute, Second)

type DateTimeServer = &{ Date: Skip; !Year; !Month;  !Day;    Skip
                       , Time: Skip; !Hour; !Minute; !Second; Skip
                       }
                       ; Wait
type DateTimeClient = Dual DateTimeServer

dateTimeServer :  DateTimeServer -> () -- server is stuck in time
dateTimeServer c =
  case c of
    &Date c -> c |> send 2024 |> send 5 |> send 19 |> wait
    &Time c -> c |> send 17 |> send 0 |> send 42 |> wait

dateClient : DateTimeClient -> Date
dateClient c =
  let c = select Date c in
  let (year, c) = receive c in
  let (month, c) = receive c in
  let (day, c) = receive c in
  close c; (year, month, day)

timeClient : DateTimeClient -> Time
timeClient c =
  let c = select Time c in
  let (hour, c) = receive c in
  let (minute, c) = receive c in
  let (second, c) = receive c in
  close c; (hour, minute, second)

startDateClient : (DateTimeClient -> Date) -> Date
startDateClient client =
  let (w,r) = channel @DateTimeClient in
  fork (\(_ : ()) 1-> dateTimeServer r);
  client w

startTimeClient : (DateTimeClient -> Time) -> Time
startTimeClient client =
  let (w,r) = channel @DateTimeClient in
  fork (\(_ : ()) 1-> dateTimeServer r);
  client w

main : (Date, Time)
main =
  let date = startDateClient dateClient in
  let time = startTimeClient timeClient in
  (date, time)
