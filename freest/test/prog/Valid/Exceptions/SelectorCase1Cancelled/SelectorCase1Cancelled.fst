module SelectorCase1Cancelled where

type Qty, Price : *T
type Qty = Int
type Price = Float

type Order : 1C
type Order = +{Cake: !Qty, Pizza: !Qty} ; Close

orderCake : Order -> Int -> ()
orderCake c qty = c |> select Cake |> drop

receiveOrder : Dual Order -> Price
receiveOrder c =
  try
    case c of
      &Cake c ->
        try receiveAndWait c as qty in 5.00 * qty
        otherwise print "Order cancelled: missing quantity of cake."; 0.00
      &Pizza c ->
        try receiveAndWait c as qty in 7.00 * qty
        otherwise print "Order cancelled: missing quantity of pizza."; 0.00
  otherwise
    print "Order cancelled: no dessert selected."; 0.00

_ = forkWith (orderCake 3) |> receiveOrder