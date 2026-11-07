type Qty, Price : *T
type Qty = Float
type Price = Float

type Order : 1C
type Order = +{Cake: !Qty, Pizza: !Qty} ; Close

orderCake : Float -> Order -1-> ()
orderCake qty c = cancel c

receiveOrder : Dual Order -> Price
receiveOrder c =
  try
    case c of
      &Cake c ->
        try receiveAndWait c as qty in 5.00 *. qty
        unless print "Order cancelled: missing quantity of cake."; 0.00
      &Pizza c ->
        try receiveAndWait c as qty in 7.00 *. qty
        unless print "Order cancelled: missing quantity of pizza."; 0.00
  as price in
    price
  unless
    print "Order cancelled: no dessert selected."; 0.00

_ = forkWith (orderCake 3.0) |> receiveOrder |> print