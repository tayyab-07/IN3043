module Orders where

import Data.List
import Data.Map 
import Data.Maybe

type Customer = String
type Product = String

-- An order of some positive quantity of a product by a customer
data Order = Order Customer Product Double
    deriving (Show)

-- A delivery to the supplier of some quantity quantity of a product
data Delivery = Delivery Product Double
    deriving (Show)

-- NOTE --
-- We didn`t know if by the statement in the coursework "You may assume that the quantity is greater than zero" 
-- meant that the the quantities of products would be greater than 0 or
-- if you meant that the list of Orders and list of Deliveries would be greater than 0 (not an empty list)
-- With this in mind, we decided, just in case, to add lines for each function that look like:
-- "numProducts [] = []" which prevent the solution from crashing if there is an empty list
--  

-- ----- QUESTION 1 -----
-- All customers who submitted an order, with the number of different products each of them ordered.
--
-- STRATEGY USED: --
-- Find all unique products in [Order] 
-- Remove duplicates from that list using "nub"
-- Use "sum" to add all unique products and stores the value in "num"
--
uniqueProducts :: [Order] -> [(Customer,Product)]
uniqueProducts orders = 
   [ (c, p) | (c, p) <- (nub [ (c, p) | (Order c p q) <- orders])]

numProducts :: [Order] -> [(Customer, Int)]
numProducts [] = []
numProducts orders = 
   nub [(c, num) | (c, _) <- uniqueProducts orders, 
   let 
   num = 
      sum [1 | (c1, p1) <- uniqueProducts orders, 
      c == c1] ]


-- ----- QUESTION 2 -----
-- All products that have been ordered, with the total quantity of each.
--
-- STATEGY USED: --
-- Function to map all product quantities to the second element in a tuple
-- e.g. Chair, 3, 5, 8, 2...
-- Function using "sum" to add quantities of mapped products found using Map.Lookup
-- "nub" to remove dupliactes and then "sum" to get final summed quantities from the list
--
mapProductQuantity :: [(Product, Double)] -> Data.Map.Map Product [Double]  
mapProductQuantity mpq =  
   Data.Map.fromListWith (++) $ Data.List.map (\(a,b) -> (a,[b])) mpq

calculateSum :: Product -> [(Product, Double)] -> Double
calculateSum p pQuantity = 
   sum $ fromJust (Data.Map.lookup p $ mapProductQuantity pQuantity)

productQuantities :: [Order] ->  [(Product, Double)]
productQuantities [] = []
productQuantities orders = 
   nub [ (p, q1) | (Order c p q) <- (orders), 
   let 
   q1 = 
      sum ( [ calculateSum p1 [ (p1, q1) ] 
      | (Order c1 p1 q1) <- (orders), 
      p == p1]) ]


-- ----- QUESTION 3 -----
-- The customers and products for which the customer has ordered
-- more than half the total quantity for that product.
--
-- STRATEGY USED: -- 
-- Used ProductQuantities to get total quantity
-- Compared total quantities between customers to find which customer had more than half
--
majority :: [Order] -> [(Customer,Product)]
majority [] = []
majority orders = 
   [ (c, p) | (Order c p q) <-  orders, 
   (pQuantity, q1)  <-  (productQuantities orders), 
   p == pQuantity && q > (q1/2) ]


-- ----- QUESTION 4 -----
-- Products for which the total quantity ordered exceeds the
-- total quantity delivered, with the difference in quantity.
--
-- STRATEGY USED: --
-- Fucntion that does the same thing as ProductQuantities but for deliveries
-- If the product is the same and ordered quantity is higher than delivery
-- Then do order quantity minus delivery quantity
--
deliveryQuantity :: [Delivery] ->  [(Product, Double)]
deliveryQuantity [] = []
deliveryQuantity deliveries = 
   nub [ (p, q1) | (Delivery p q) <- deliveries,  
   let 
   q1 = 
      sum ( [ calculateSum p1 [ (p1, q1) ]
      | (Delivery p1 q1) <- deliveries,
      p == p1 ] ) ]

shortfall :: [Order] -> [Delivery] -> [(Product, Double)]
shortfall [] [] = []
shortfall orders deliveries = 
   [ (p1, difference) | (p1, q1) <- productQuantities orders, 
   (p2, q2) <- deliveryQuantity deliveries, 
   p1 == p2 && q1 > q2, 
   let 
   difference = 
      q1 - q2 ]


-- ----- QUESTION 5 -----
-- Allocation of quantities of products to customers.
--
-- No customer should be allocated more of a given product than they have
-- ordered.  If a sufficient quantity of a product has been delivered to
-- satisfy all orders, each customer should receive the total quantity
-- they ordered.  If not, the available quantity of the product should
-- be shared between customers in proportion to the amount ordered.
-- For example, if the delivered quantity of a product is half the the
-- total ordered quantity of that product, each customer would receive
-- half of what they ordered.
--
-- STRATEGY USED: --
-- Function to be able to divide floating point numbers without losing precision
-- Get total ordered quantity
-- Get total delivery quantity
-- If delivery is less then ordered
-- Then divide using function to work out how much each customer should receive
-- 
precision :: Double -> Int -> Double
precision d n = 
   fromInteger (round (d * (10^n))) / (10^n)

share :: [Order] -> [Delivery] -> [(Customer, Product, Double)]
share [] [] = []
share orders deliveries = 
   [ variableOrderedQuantity | (Order c p q) <- orders, 
   (p1, q1) <- productQuantities orders, 
   (p2, q2) <- deliveryQuantity deliveries, 
   p == p1,  
   p == p2, 
   let 
   variableOrderedQuantity = 
      if (q1 - q2) >= 0   then ( c, p, (precision (q2/q1*q) (2)) )   else (c, p, q) ]
