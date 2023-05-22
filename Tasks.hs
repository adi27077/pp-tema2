
-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array
import Data.Maybe
import Data.Time.Calendar.OrdinalDate (fromMondayStartWeek)

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String

-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}


-- Task 1

compute_average_steps :: Table -> Table
compute_average_steps m = ["Name","Average Number of Steps"] : map compute_average_row (tail m)

--functii care calculeaza media de pasi unei linii
compute_average_row :: Row -> Row
compute_average_row r = head r : [compute_average r]

compute_average :: Row -> Value
compute_average r = printf "%.2f" ((read (foldr compute_sum "0" (tail r)) :: Float) / 8)

--functie ajutatoare pt fold care calculeaza suma
compute_sum :: Value -> Value -> Value
compute_sum v acc = show (read v + read acc)


-- Task 2

-- Number of people who have achieved their goal:
get_passed_people_num :: Table -> Int
get_passed_people_num m = foldr (check_achieve . compute_sum_row) 0 (tail m)

compute_sum_row :: Row -> Row
compute_sum_row r = [foldr compute_sum "0" (tail r)]

--functie ajutatoare pt fold folosita pt numararea persoanelor
--care au indeplinit conditia de pasi
check_achieve :: Row -> Int -> Int
check_achieve v acc
              | read (head v) >= 1000 = acc + 1
              | otherwise = acc


-- Percentage of people who have achieved their:
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m = fromIntegral (get_passed_people_num m) / get_total_people_num m

--functie care calculeaza nr total al persoanelor din tabel
get_total_people_num :: Table -> Float
get_total_people_num m = foldr (\v acc -> acc + 1) 0.0 (tail m)


-- Average number of daily steps
get_steps_avg :: Table -> Float
get_steps_avg m = foldr ((\v acc -> acc + read (head v)) . compute_sum_row) 0 (tail m) / get_total_people_num m



-- Task 3

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h m = ["H10","H11","H12","H13","H14","H15","H16","H17"] : [f_aux get_avg_steps_hour (map tail (tail m)) []]

get_avg_steps_hour :: Table -> Value
get_avg_steps_hour t = printf "%.2f" ((read (foldr (\v acc -> show (read (head v) + read acc)) "0" t) :: Float) / (get_total_people_num t + 1))

--functie auxiliara care construieste tabelul cerut
f_aux :: (Table -> Value) -> Table -> Row -> Row
f_aux f ([]:_) acc = acc
f_aux f t acc = acc ++ [f t] ++ f_aux f (map tail t) acc


-- Task 4

get_activ_summary :: Table -> Table
get_activ_summary m = ["column","range1","range2","range3"] : compute_minutes (map (drop 3) (tail m))

compute_minutes :: Table -> Table
compute_minutes t = [comp_vact_min t] ++ [comp_fair_min (map tail t)] ++ [comp_light_min (map (drop 2) t)]

--functii auxiliare care construiesc randurile pt fiecare categorie
comp_vact_min :: Table -> Row
comp_vact_min t = ["VeryActiveMinutes"] ++ [comp_low t] ++ [comp_med t] ++ [comp_high t]

comp_fair_min :: Table -> Row
comp_fair_min t = ["FairlyActiveMinutes"] ++ [comp_low t] ++ [comp_med t] ++ [comp_high t]

comp_light_min :: Table -> Row
comp_light_min t = ["LightlyActiveMinutes"] ++ [comp_low t] ++ [comp_med t] ++ [comp_high t]

--functii care numara persoanele din fiecare categorie
comp_low :: Table -> Value
comp_low = foldr check_low_range "0"

comp_med :: Table -> Value
comp_med = foldr check_med_range "0"

comp_high :: Table -> Value
comp_high = foldr check_high_range "0"

--functiile folosite de fold pt numararea persoanelor din fiecare range
check_low_range :: Row -> Value -> Value
check_low_range r acc
       | read (head r) < 50 = show (read acc + 1)
       | otherwise = acc

check_med_range :: Row -> Value -> Value
check_med_range r acc
       | read (head r) >= 50 && read (head r) < 100 = show (read acc + 1)
       | otherwise = acc

check_high_range :: Row -> Value -> Value
check_high_range r acc
       | read (head r) >= 100 && read (head r) < 500 = show (read acc + 1)
       | otherwise = acc

-- Task 5

get_ranking :: Table -> Table
get_ranking m = ["Name","Total Steps"] : sortBy sort_cmp  (map (reverse . drop 4 . reverse)  (tail m))

--comparator pt sort
sort_cmp :: Row -> Row -> Ordering
sort_cmp a b
        | head (tail a) < head (tail b) = GT
        | head (tail a) > head (tail b) = LT
        | head a < head b = GT
        | head a > head b = LT
        | otherwise = EQ

-- Task 6

get_steps_diff_table :: Table -> Table
get_steps_diff_table m = ["Name","Average first 4h","Average last 4h","Difference"] : sort_step_diff (tail m)

sort_step_diff :: Table -> Table
sort_step_diff m = sortBy sort_cmp_diff (map (compute_diff_row . compute_average4_row) m)

--functie care construieste linia cu cele 2 medii pe 4 ore
compute_average4_row :: Row -> Row
compute_average4_row r = [head r] ++ [average_4h (tail r)] ++ [average_4h (drop 5 r)]

--functie care calculeaza media pe 4 ore
average_4h :: Row -> Value
average_4h r = printf "%.2f" (((read (head r) :: Float) + (read (r !! 1) :: Float) + (read (r !! 2) :: Float) + (read (r !! 3) :: Float)) / 4)

--functie care adauga la linie diferenta dintre cele 2 medii
compute_diff_row :: Row -> Row
compute_diff_row r = r ++ [printf "%.2f" (abs ((read (r !! 1) :: Float) - (read (r !! 2) :: Float)))]

--comparator pt sort
sort_cmp_diff :: Row -> Row -> Ordering
sort_cmp_diff a b
         | (read (a !! 3) :: Float) < (read (b !! 3) :: Float) = GT
         | (read (a !! 3) :: Float) > (read (b !! 3) :: Float) = LT
         | head a < head b = GT
         | head a > head b = LT
         | otherwise = EQ



-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f = map (map f)


-- Task 8

-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = s : map f m


get_sleep_total :: Row -> Row
get_sleep_total r = head r : [printf "%.2f" (read (foldr (\v acc -> show (read acc + read v)) "0" (tail r)) :: Float)]


{-
    TASK SET 2
-}

-- Task 1

tsort :: ColumnName -> Table -> Table
tsort column table = head table : sortBy (sort_cmp2 (find_column_no column table)) (tail table)

{-functie care intoarce un nr ce reprezinta a cata coloana este in tabel
este cea data-}
find_column_no :: ColumnName -> Table -> Int
find_column_no column table = fromMaybe 0 (elemIndex column (head table))

--functie de comparatie - cazul pt valorile numerice
sort_cmp2 :: Int -> Row -> Row -> Ordering
sort_cmp2 n a b
          | (read (a !! n) :: Float) < (read (b !! n) :: Float) = LT
          | (read (a !! n) :: Float) > (read (b !! n) :: Float) = GT
          | otherwise = sort_cmp_name a b

{-functie de comparatie - cazule pt name
(prima coloana, atunci cand valorile de pe coloana specificata
sunt egale)-}
sort_cmp_name :: Row -> Row -> Ordering
sort_cmp_name a b
             | head a < head b = LT
             | head a > head b = GT
             | otherwise = EQ

-- Task 2

{-daca numele coloanelor coincid, concatenam rowurile din t2 la t1,
altfel lasam t1 neschimbat-}
vunion :: Table -> Table -> Table
vunion t1 t2
       | head t1 == head t2 = t1 ++ tail t2
       | otherwise = t1

-- Task 3

{-daca tabelele nu au acelasi nr de row-uri, facem pad la cel mai mic
cu row-uri de valori goale (["","","",...])-}
hunion :: Table -> Table -> Table
hunion t1 t2
       | length t1 > length t2 = zipWith (++) t1 (fill_empty t2 (length t1 - length t2))
       | length t1 < length t2 = zipWith (++) (fill_empty t1 (length t2 - length t1)) t2
       | otherwise = zipWith (++) t1 t2

--functie care face pad la un tabel cu row-uri goale
fill_empty :: Table -> Int -> Table
fill_empty t n
       | n > 0 = fill_empty (t ++ [empty_row (length (head t))]) (n - 1)
       | otherwise = t

--generare row cu valori goale
empty_row :: Int -> Row
empty_row 1 = [""]
empty_row i = "" : empty_row (i - 1)


-- Task 4

--n-am facut
tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column t1 t2 = (head t1 ++ drop 1 (head t2)) : tjoin_aux (tail t1) (tail t2)

tjoin_aux :: Table -> Table -> Table
tjoin_aux t1 = map (\ ht2 -> find_int1 t1 (head ht2) ++ tail ht2)

find_int1 :: Table -> Value -> Row
find_int1 t1 name = fromJust (find (\x -> head x == name) t1)



-- Task 5

cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian new_row_function new_column_names t1 t2 = new_column_names : cartesian_aux new_row_function (tail t1) (tail t2)

--functie care ia cate un row din t1 si face produs cartezian cu t2
cartesian_aux :: (Row -> Row -> Row) -> Table -> Table -> Table
cartesian_aux f (t1:t1s) t2 = cartesian_row f t1 t2 ++ cartesian_aux f t1s t2
cartesian_aux f [] _ = []

{-functie care face produs (aplicand functia primita ca parametru)
pe 2 row-uri-}
cartesian_row :: (Row -> Row -> Row) -> Row -> Table -> Table
cartesian_row f r = map (f r)

-- Task 6

{-functie care genereaza tabel format doar din coloanele specificate
care foloseste foldr cu o coloana vida ca acumulator si zipwith pt a
adauga la fiecare row urmatoarea valoare dintr-o coloana care trebuie
sa apara in tabel-}
projection :: [ColumnName] -> Table -> Table
projection cs t
  = foldr
      (\ c -> zipWith (++) (get_column (find_column_no c t) t))
      (replicate (length t) []) cs

--functie care intoarce coloana n din tabel
get_column :: Int -> Table -> Table
get_column n = map (drop n . take (n + 1))

-- Task 7

{-functie care genereaza tabelul cerut folosind foldr cu
functia de verificare conditie-}
filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t = head t : foldr (check_row condition (find_column_no key_column t)) [] (tail t)

{-functia care verifica conditia data si adauga row-ul la acumulator
daca aceasta indeplineste conditia-}
check_row :: (Value -> Bool) -> Int -> Row -> Table -> Table
check_row cond n r acc
                   | cond (r !! max 0 n) = r : acc
                   | otherwise = acc

