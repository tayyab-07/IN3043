module Orders where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

type Customer = String
type Product = String

-- An order of some positive quantity of a product by a customer
data Order = Order Customer Product Double
    deriving (Show)

-- A delivery to the supplier of some quantity quantity of a product
data Delivery = Delivery Product Double
    deriving (Show)

frequencyMap :: Ord a => [a] -> Map a Int
frequencyMap xs = Map.unionsWith (+) [Map.singleton x 1 | x <- xs]

frequency :: [Char] -> [(Char, Int)]
frequency ws = [(head g, length g) | g <- group (sort ws)]

-- All customers who submitted an order, with the number of different
-- products each of them ordered.
numProducts :: [Order] -> [(Customer, Int)]
numProducts [(Order c p n)] = [(head o, length o) | o <- group (sort [c])]

-- All products that have been ordered, with the total quantity of each.
productQuantities :: [Order] -> [(Product, Double)]
productQuantities [(Order c p n)] = undefined

productQuantities1 :: Ord [orders] => [Order] -> Map orders Int 
productQuantities1 [(Order c p n)] = Map.unionsWith (+) [Map.singleton o 1 | o <- [p]]

-- The customers and products for which the customer has ordered
-- more than half the total quantity for that product.
majority :: [Order] -> [(Customer, Product)]
majority orders = undefined

-- Products for which the total quantity ordered exceeds the
-- total quantity delivered, with the difference in quantity.
shortfall :: [Order] -> [Delivery] -> [(Product, Double)]
shortfall orders deliveries = undefined

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
share :: [Order] -> [Delivery] -> [(Customer, Product, Double)]
share orders deliveries = undefined
