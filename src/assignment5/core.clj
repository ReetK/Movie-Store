;;1. Add New movies: To add new movies, use:
;;     (addMovie id name price quantity)
;;
;;
;;2. Remove a movie in the inventory (including all its copies)
;;     (removeAll id)
;;
;;
;;3. Rent a movie in the inventory
;;     (rentMovie id name renter)
;;
;;
;;4. Add copies of existing movies
;;     (copyAdding id number_of_copies)
;;
;;
;;5. Change the price of the Movie
;;     (changePrices id new_price)
;;
;;
;;6. Find Price/Quantity using id or name  (Note1: This will only work with GUI as these methods displays answer in the textfield)
;;    a. By Name
;;           i. Price:
;;              (findPrice name)
;;           ii. Quantity
;;              (findQuantity name)
;;           iii. Price and Quantity
;;              (findPrQ name)
;;    (Note2: Here, all the data is case sen)
;;
;;
;;7. To display the GUI:
;;       (seesaw/show! window)



(ns assignment5.core
  (:gen-class)
  (:require [seesaw.core :as seesaw]
            [seesaw.swingx :as swingx]
            [seesaw.table :as table]
            [seesaw.dev :as dev]
            [clj-time.core :as time]
            [clj-time.format :as f]))


;;(spit "movies.clj" (pr-str '()))
;;(spit "availmovies.clj" (pr-str '()))
;;(spit "rentedmovies.clj" (pr-str '()))


;;***************************************TABLES********************************************;;

(def rentedMoviesTable
  (let [r (binding [*read-eval* false]
            (read-string (slurp "rentedmovies.clj")))]
   (swingx/table-x
   :horizontal-scroll-enabled? true
   :model [:columns [{:key :id :text "ID"}
                     {:key :name :text "Movie Name"}
                     {:key :renter :text "Renter's Name"}
                     {:key :due-date :text "Due Date"}]
           :rows r])))





(def availableMoviesTable
  (let [r (binding [*read-eval* false]
            (read-string (slurp "availmovies.clj")))]
   (swingx/table-x
   :horizontal-scroll-enabled? true
   :model [:columns [{:key :id :text "ID"}
                     {:key :name :text "Movie Name"}
                     {:key :price :text "Price"}
                     {:key :quantity :text "Quantity"}]
           :rows r])))


(def allMoviesTable
  (let [r (binding [*read-eval* false]
            (read-string (slurp "movies.clj")))]
   (swingx/table-x
   :horizontal-scroll-enabled? true
   :model [:columns [{:key :id :text "ID"}
                     {:key :name :text "Movie Name"}
                     {:key :price :text "Price"}
                     {:key :quantity :text "Quantity"}]
           :rows r])))



;;********************************************************************************************;;




(def custom-formatter (f/formatter "MM-dd-yyyy"))

(defn String->Number [str]
  (let [n (read-string str)]
       (if (number? n) n nil)))


(defn writeMovieFile
  [file]
  (with-open [w (clojure.java.io/writer "movies.clj")]
    (binding [*out* w]
      (pr file))))



;;*********************************Some Table Functions to Update tables**************************************;;



(defn findRowinAvail
  [id value]
  (if (< value (seesaw.table/row-count availableMoviesTable))
    (if (= (get (seesaw.table/value-at availableMoviesTable value) :id) id)
      value
      (findRowinAvail id (inc value)))))

(defn findRowinRent
  [id renter value]
  (if (< value (seesaw.table/row-count rentedMoviesTable))
    (if (= (get (seesaw.table/value-at rentedMoviesTable value) :id) id)
      (if (= (get (seesaw.table/value-at rentedMoviesTable value) :renter) renter)
        value
        (findRowinRent id (inc value))))))

(defn findQuantityinAvail
  [id value]
  (if (< value (seesaw.table/row-count availableMoviesTable))
    (if (= (get (seesaw.table/value-at availableMoviesTable value) :id) id)
      (get (seesaw.table/value-at availableMoviesTable value) :quantity)
      (findQuantityinAvail id (inc value)))))

(defn findQuantityinAll
  [id value]
  (if (< value (seesaw.table/row-count allMoviesTable))
    (if (= (get (seesaw.table/value-at allMoviesTable value) :id) id)
      (get (seesaw.table/value-at allMoviesTable value) :quantity)
      (findQuantityinAll id (inc value)))))



(defn findRowinAll
  [id value]
  (if (< value (seesaw.table/row-count allMoviesTable))
    (if (= (get (seesaw.table/value-at allMoviesTable value) :id) id)
      value
      (findRowinAll id (inc value)))))


(defn updateinAvailQ
  [id quantity]
  (seesaw.table/update-at! availableMoviesTable (findRowinAvail id 0) {:quantity quantity}))

(defn updateinAvailP
  [id price]
  (seesaw.table/update-at! availableMoviesTable (findRowinAvail id 0) {:price price}))

(defn insertinAvail
  [id name price quantity]
  (seesaw.table/insert-at! availableMoviesTable (seesaw.table/row-count availableMoviesTable) {:id id :name name :price price :quantity quantity}))

(defn removefromAvail
  [id]
  (seesaw.table/remove-at! availableMoviesTable (findRowinAvail id 0)))

(defn removefromRent
  [id renter]
  (seesaw.table/remove-at! rentedMoviesTable (findRowinRent id renter 0)))

(defn insertinRent
  [id name renter]
  (seesaw.table/insert-at! rentedMoviesTable (seesaw.table/row-count rentedMoviesTable) {:id id :name name :renter renter :due-date (f/unparse custom-formatter (time/plus (time/now) (time/weeks 2)))}))

(defn updateinAllQ
  [id quantity]
  (seesaw.table/update-at! allMoviesTable (findRowinAll id 0) {:quantity quantity}))

(defn updateinAllP
  [id price]
  (seesaw.table/update-at! allMoviesTable (findRowinAll id 0) {:price price}))

(defn insertinAll
  [id name price quantity]
  (seesaw.table/insert-at! allMoviesTable (seesaw.table/row-count allMoviesTable) {:id id :name name :price price :quantity quantity}))

(defn removefromAll
  [id]
  (seesaw.table/remove-at! allMoviesTable (findRowinAll id 0)))


;;*******************************************************************************************;;


;;*********************************AVAILABLE MOVIES***********************************;;


(defn no-duplicate
  [old new]
   (if (= (count old) 0)
     new
     (if (= (take-last 1 old) (take 1 new))
       (no-duplicate (drop-last old) new)
       (no-duplicate (drop-last old) (concat (take-last 1 old) new)))))

(defn no-dup
  [x]
   (no-duplicate x '()))



(defn decrementing
  [v]
  (assoc v 3 (- (get v 3) 1)))

(decrementing [1 "hi" 2 3])

(defn removing1
  [id list newlist value]
  (if (not (empty? newlist))
    (if (= (get (first newlist) 0) id)
      (with-open [w (clojure.java.io/writer "availmovies.clj")]
      (binding [*out* w]
        (pr (concat (take value list) (vector (decrementing (first newlist))) (rest newlist)))))
      (removing1 id list (rest newlist) (inc value)))))



(defn remove1Movie
  [id]
  (let [r1 (binding [*read-eval* false]
            (read-string (slurp "availmovies.clj")))]
    (if (not (empty? r1))
      (removing1 id r1 r1 0))))


(defn rentMovie
  [id name renter]
  (let [r (binding [*read-eval* false]
            (read-string (slurp "rentedmovies.clj")))]
    (with-open [w (clojure.java.io/writer "rentedmovies.clj")]
      (binding [*out* w]
        (pr (concat r [[id name renter (f/unparse custom-formatter (time/plus (time/now) (time/weeks 2)))]]))))
    (remove1Movie id)))



(defn removeId1
  [id list newlist value]
  (if (not (empty? newlist))
    (if (= (get (first newlist) 0) id)
      (with-open [w (clojure.java.io/writer "movies.clj")]
      (binding [*out* w]
        (pr (concat (take value list) (rest newlist)))))
      (removeId1 id list (rest newlist) (inc value)))))

(defn removeId2
  [id list newlist value]
  (if (not (empty? newlist))
    (if (= (get (first newlist) 0) id)
      (with-open [w (clojure.java.io/writer "availmovies.clj")]
      (binding [*out* w]
        (pr (concat (take value list) (rest newlist)))))
      (removeId2 id list (rest newlist) (inc value)))))


(defn removed
  [id]
  (let [r (binding [*read-eval* false]
            (read-string (slurp "movies.clj")))]
    (if (not (empty? r))
      (removeId1 id r r 0))))

(defn removes
  [id]
  (let [r1 (binding [*read-eval* false]
            (read-string (slurp "availmovies.clj")))]
    (if (not (empty? r1))
      (removeId2 id r1 r1 0))))

;;(seesaw/listen availableMoviesTable
;;               :selection (fn [event] "You selected " (seesaw/selection event)))

(def name2
  (seesaw/label :text "Name"))

(def text7
  (seesaw/text :columns 20 :editable? false))

(def rent1
  (seesaw/button :enabled? false :text "Rent the movie.."))

(def remove2
  (seesaw/button :enabled? false :text "Remove the movie.." ))

(def availableOther
  (seesaw/grid-panel
   :columns 6
   :items [name2 text7 "" "" "" rent1
           "" "" "" "" "" ""
           "" "" "" "" "" remove2]))


(seesaw/listen text7
               :document (fn[event]
                             (seesaw/config! rent1 :enabled? true)))




(seesaw/listen remove2
               :action (fn[event]
                         (removes (get (seesaw.table/value-at availableMoviesTable (seesaw/selection availableMoviesTable)) :id))
                         (removed (get (seesaw.table/value-at availableMoviesTable (seesaw/selection availableMoviesTable)) :id))
                         (removefromAll (get (seesaw.table/value-at availableMoviesTable (seesaw/selection availableMoviesTable)) :id))
                         (removefromAvail (get (seesaw.table/value-at availableMoviesTable (seesaw/selection availableMoviesTable)) :id))))

(seesaw/listen rent1
               :action (fn[event]
                         (let [m (seesaw.table/value-at availableMoviesTable (seesaw/selection availableMoviesTable))]
                           (rentMovie (get m :id) (get m :name) (seesaw/value text7))
                           (insertinRent (get m :id) (get m :name) (seesaw/value text7))
                           (updateinAvailQ (get m :id) (dec (get m :quantity)))
                           (seesaw/config! text7 :text ""))))


(seesaw/listen availableMoviesTable
               :selection
               (fn[e]
                 (let [m (seesaw.table/value-at availableMoviesTable (seesaw/selection availableMoviesTable))]
                   (if (not (= (get m :quantity) 0))
                     (and (seesaw/config! text7 :editable? true) (seesaw/config! remove2 :enabled? true))
                     (and (seesaw/config! rent1 :enabled? false) (seesaw/config! remove2 :enabled? false))))))


(def availableMovies
  (seesaw/grid-panel
   :border "Available Movies"
   :columns 1
   :items [(seesaw/scrollable availableMoviesTable)]))

(def availableMovies
  (seesaw/top-bottom-split
   availableMovies
   availableOther))


;;***************************************************************************************;;

;;**************************************Adding Copies************************************;;


(def id3
  (seesaw/label :text "Id"))

(def text8
  (seesaw/text :columns 20))

(def quantity2
  (seesaw/label :text "Copies to add"))

(def text9
  (seesaw/text :columns 20))

(def addCop
  (seesaw/button :text "Add Copies"))


(defn addingCopies
  [v n]
  (assoc v 3 (+ (get v 3) n)))


(defn addCopy
  [id n list newlist value]
  (if (not (empty? newlist))
    (if (= (get (first newlist) 0) id)
      (with-open [w (clojure.java.io/writer "availmovies.clj")]
      (binding [*out* w]
        (pr (concat (take value list) (vector (addingCopies (first newlist) n)) (rest newlist)))))
      (addCopy id n list (rest newlist) (inc value)))))


(defn copyAdd
  [id n]
  (let [r1 (binding [*read-eval* false]
            (read-string (slurp "availmovies.clj")))]
    (if (not (empty? r1))
      (addCopy id n r1 r1 0))))


(defn addCopy1
  [id n list newlist value]
  (if (not (empty? newlist))
    (if (= (get (first newlist) 0) id)
      (with-open [w (clojure.java.io/writer "movies.clj")]
      (binding [*out* w]
        (pr (concat (take value list) (vector (addingCopies (first newlist) n)) (rest newlist)))))
      (addCopy1 id n list (rest newlist) (inc value)))))


(defn copyAdd1
  [id n]
  (let [r1 (binding [*read-eval* false]
            (read-string (slurp "movies.clj")))]
    (if (not (empty? r1))
      (addCopy1 id n r1 r1 0))))


(defn copyAdding
  [id copies]
  (copyAdd id copies)
  (copyAdd1 id copies))


(seesaw/listen addCop
               :action (fn[event]
                         (copyAdd (String->Number (seesaw/value text8)) (String->Number (seesaw/value text9)))
                         (copyAdd1 (String->Number (seesaw/value text8)) (String->Number (seesaw/value text9)))
                         (updateinAllQ (String->Number (seesaw/value text8)) (+ (String->Number (seesaw/value text9)) (findQuantityinAll (String->Number (seesaw/value text8)) 0)))
                         (updateinAvailQ (String->Number (seesaw/value text8)) (+ (String->Number (seesaw/value text9)) (findQuantityinAvail (String->Number (seesaw/value text8)) 0)))
                         (seesaw/text! text8 "")
                         (seesaw/text! text9 "")))


(def addCopies
  (seesaw/grid-panel
   :border "Add Copies of Movie"
   :columns 7
   :items ["" "" "" "" "" "" ""
           "" "" "" "" "" "" ""
           "" "" "" "" "" "" ""
           "" "" id3 "" text8 "" ""
           "" "" "" "" "" "" ""
           "" "" quantity2 "" text9 "" ""
           "" "" "" "" "" "" ""
           "" "" "" "" "" "" ""
           "" "" "" addCop "" "" ""
           "" "" "" "" "" "" ""
           "" "" "" "" "" "" ""
           "" "" "" "" "" "" ""
           "" "" "" "" "" "" ""
           "" "" "" "" "" "" ""]))



;;***************************************************************************************;;

;;****************************************Remove a Movie*********************************;;


(defn removeAll
  [id]
  (removes id)
  (removed id))


(def id2
  (seesaw/label "Id"))

(def text6
  (seesaw/text :columns 20))

(def remove1
  (seesaw/button :text "Remove Movie"))

(seesaw/listen remove1
               :action (fn[event]
                         (removes (String->Number (seesaw/value text6)))
                         (removed (String->Number (seesaw/value text6)))
                         (removefromAvail (String->Number (seesaw/value text6)))
                         (removefromAll (String->Number (seesaw/value text6)))
                         (seesaw/text! text6 "")))


(def removeMovies
  (seesaw/grid-panel
   :border "Remove a movie"
   :columns 7
   :items ["" "" "" "" "" "" ""
           "" "" "" "" "" "" ""
           "" "" "" "" "" "" ""
           "" "" "" "" "" "" ""
           "" "" id2 "" text6 "" ""
           "" "" "" "" "" "" ""
           "" "" "" "" "" "" ""
           "" "" "" "" "" "" ""
           "" "" "" "" "" "" ""
           "" "" "" remove1 "" "" ""
           "" "" "" "" "" "" ""
           "" "" "" "" "" "" ""]))



;;*************************************All Movies***************************************;;


(def allMovies
  (seesaw/grid-panel
   :border "All Movies in the Inventory"
   :columns 1
   :items [(seesaw/scrollable allMoviesTable)]))

;;*******************************************************************************************;;


;;************************************Rented Movies******************************************;;

(defn returns
  [id renter list newlist value]
  (if (not (empty? list))
    (if (= (get (first list) 0) id)
      (with-open [w (clojure.java.io/writer "rentedmovies.clj")]
      (binding [*out* w]
        (pr (concat (take value list) (rest newlist)))))
      (removeId2 id list (rest newlist) (inc value)))))


(defn returning
  [id renter]
  (let [r (binding [*read-eval* false]
            (read-string (slurp "rentedmovies.clj")))]
    (if (not (empty? r))
      (returns id renter r r 0))))


(def return1
  (seesaw/button :text "Return the Movie" :enabled? false))

(seesaw/listen return1
               :action (fn[event]
                         (let [m (seesaw.table/value-at rentedMoviesTable (seesaw/selection rentedMoviesTable))]
                           (copyAdd (get m :id) 1)
                           (updateinAvailQ (get m :id) (inc (findQuantityinAvail (get m :id) 0)))
                           (returning (get m :id) (get m :renter))
                           (removefromRent (get m :id) (get m :renter)))))


(seesaw/listen rentedMoviesTable
        :selection
          (fn[e]
            (seesaw/config! return1 :enabled? true)))


(def rentedOther
  (seesaw/grid-panel
   :columns 6
   :items ["" "" "" "" return1 ""
           "" "" "" "" ""
           "" "" "" "" ""]))

(def rentedMovies
  (seesaw/grid-panel
   :border "Rented Movies"
   :columns 1
   :items [(seesaw/scrollable rentedMoviesTable)]))

(def rentedMovies
  (seesaw/top-bottom-split
   rentedMovies
   rentedOther))


;;******************************FINDING PRICE/QUANTITY********************************;;



(def name3
  (seesaw/label :text "Name"))

(def text12
  (seesaw/text :columns 20))

(def ok
  (seesaw/button :text "Okay" :visible? false))

(def answer
  (seesaw/text :columns 30 :editable? false))




(def id5
  (seesaw/label :text "Name"))

(def text13
  (seesaw/text :columns 20))

(def ok1
  (seesaw/button :text "Okay" :visible? false))

(def answer1
  (seesaw/text :columns 30 :editable? false))



(defn findingPrice
  [name list]
  (if (not (empty? list))
    (if (= (get (first list) 1) name)
      (seesaw/config! answer :text (str "Price is: " (get (first list) 2)))
      (findingPrice name (rest list)))))

(defn findPrice
  [name]
  (let [r (binding [*read-eval* false]
             (read-string (slurp "movies.clj")))]
    (if (not (empty? r))
      (findingPrice name r))))

(defn findingQuantity
  [name list]
  (if (not (empty? list))
    (if (= (get (first list) 1) name)
      (seesaw/config! answer :text (str "Quantity is: " (get (first list) 3)))
      (findingQuantity name (rest list)))))

(defn findQuantity
  [name]
  (let [r (binding [*read-eval* false]
             (read-string (slurp "movies.clj")))]
    (if (not (empty? r))
      (findingQuantity name r))))


(defn findingPQ
  [name list]
  (if (not (empty? list))
    (if (= (get (first list) 1) name)
      (seesaw/config! answer :text (str "Price = " (get (first list) 2) ", Quantity = " (get (first list) 3)))
      (findingPQ name (rest list)))))

(defn findPrQ
  [name]
  (let [r (binding [*read-eval* false]
             (read-string (slurp "movies.clj")))]
    (if (not (empty? r))
      (findingPQ name r))))


(seesaw/listen answer
               :document (fn[event]
                         (seesaw/config! ok :visible? true)))

(def findP
  (seesaw/button :text "Find Price"))

(seesaw/listen findP
               :action (fn[event]
                         (findPrice (seesaw/value text12))))

(def findQ
  (seesaw/button :text "Find Quantity"))

(seesaw/listen findQ
               :action (fn[event]
                         (findQuantity (seesaw/value text12))))

(def findPQ
  (seesaw/button :text "Price/Quantity"))


(seesaw/listen findPQ
               :action (fn[event]
                         (findPrQ (seesaw/value text12))))


(def findbyName
  (seesaw/grid-panel
   :border "Find price/quantity by name"
   :columns 5
   :items ["" "" "" "" ""
           "" "" "" "" ""
           "" "" "" "" findP
           "" "" "" "" ""
           "" name3 text12 "" findQ
           "" "" "" "" ""
           "" "" "" "" findPQ
           "" answer "" "" ""
           "" "" "" "" ""
           "" ok "" "" ""
           "" "" "" "" ""
           "" "" "" "" ""
           "" "" "" "" ""
           "" "" "" "" ""]))


(seesaw/listen ok
               :action (fn[event]
                         (seesaw/config! answer :text "")
                         (seesaw/config! text13 :text "")
                         (seesaw/config! ok :visible? false)))

(seesaw/listen ok1
               :action (fn[event]
                         (seesaw/config! answer1 :text "")
                         (seesaw/config! text13 :text "")
                         (seesaw/config! ok1 :visible? false)))

(defn findingPrice1
  [id list]
  (if (not (empty? list))
    (if (= (get (first list) 0) id)
      (seesaw/config! answer1 :text (str "Price is: " (get (first list) 2)))
      (findingPrice1 id (rest list)))))

(defn findPrice1
  [id]
  (let [r (binding [*read-eval* false]
             (read-string (slurp "movies.clj")))]
    (if (not (empty? r))
      (findingPrice1 id r))))

(defn findingQuantity1
  [id list]
  (if (not (empty? list))
    (if (= (get (first list) 0) id)
      (seesaw/config! answer1 :text (str "Quantity is: " (get (first list) 3)))
      (findingQuantity1 id (rest list)))))

(defn findQuantity1
  [id]
  (let [r (binding [*read-eval* false]
             (read-string (slurp "movies.clj")))]
    (if (not (empty? r))
      (findingQuantity1 id r))))


(defn findingPQ1
  [id list]
  (if (not (empty? list))
    (if (= (get (first list) 0) id)
      (seesaw/config! answer1 :text (str "Price = " (get (first list) 2) ", Quantity = " (get (first list) 3)))
      (findingPQ1 id (rest list)))))

(defn findPrQ1
  [id]
  (let [r (binding [*read-eval* false]
             (read-string (slurp "movies.clj")))]
    (if (not (empty? r))
      (findingPQ1 id r))))


(seesaw/listen answer1
               :document (fn[event]
                         (seesaw/config! ok1 :visible? true)))

(def findP1
  (seesaw/button :text "Find Price"))

(seesaw/listen findP1
               :action (fn[event]
                         (findPrice1 (String->Number (seesaw/value text13)))))

(def findQ1
  (seesaw/button :text "Find Quantity"))

(seesaw/listen findQ1
               :action (fn[event]
                         (findQuantity1 (String->Number (seesaw/value text13)))))

(def findPQ1
  (seesaw/button :text "Find Price/Quantity"))


(seesaw/listen findPQ1
               :action (fn[event]
                         (findPrQ1 (String->Number (seesaw/value text13)))))


(def findbyId
  (seesaw/grid-panel
   :border "Find price/quantity by name"
   :columns 5
   :items ["" "" "" "" ""
           "" "" "" "" ""
           "" "" "" "" findP1
           "" "" "" "" ""
           "" id5 text13 "" findQ1
           "" "" "" "" ""
           "" "" "" "" findPQ1
           "" answer1 "" "" ""
           "" "" "" "" ""
           "" ok1 "" "" ""
           "" "" "" "" ""
           "" "" "" "" ""
           "" "" "" "" ""
           "" "" "" "" ""]))



(def finding
  (seesaw/tabbed-panel
   :border "Find the price/quantity of a Movie"
   :placement :top
   :tabs [{:title "By Name"
           :content findbyName}
          {:title "By Id"
           :content findbyId}]))



;;************************************************************************************;;

;;**********************************Change Price**************************************;;

(defn changingPrice
  [v p]
  (assoc v 2 p))


(defn priceChange
  [id p list newlist value]
  (if (not (empty? newlist))
    (if (= (get (first newlist) 0) id)
      (with-open [w (clojure.java.io/writer "availmovies.clj")]
      (binding [*out* w]
        (pr (concat (take value list) (vector (changingPrice (first newlist) p)) (rest newlist)))))
      (priceChange id p list (rest newlist) (inc value)))))

(defn priceChange1
  [id p list newlist value]
  (if (not (empty? newlist))
    (if (= (get (first newlist) 0) id)
      (with-open [w (clojure.java.io/writer "movies.clj")]
      (binding [*out* w]
        (pr (concat (take value list) (vector (changingPrice (first newlist) p)) (rest newlist)))))
      (priceChange1 id p list (rest newlist) (inc value)))))



(defn changePrice
  [id p]
  (let [r1 (binding [*read-eval* false]
            (read-string (slurp "availmovies.clj")))]
    (if (not (empty? r1))
      (priceChange id p r1 r1 0))))


(defn changePrice1
  [id p]
  (let [r1 (binding [*read-eval* false]
            (read-string (slurp "movies.clj")))]
    (if (not (empty? r1))
      (priceChange1 id p r1 r1 0))))


;;To Change price:

(defn changePrices
  [id p]
  (changePrice id p)
  (changePrice1 id p))



(def id4
  (seesaw/label :text "Id"))

(def text10
  (seesaw/text :columns 20))

(def price2
  (seesaw/label :text "Price"))

(def text11
  (seesaw/text :columns 20))

(def changeP
  (seesaw/button :text "Change Price"))

(seesaw/listen changeP
               :action (fn[event]
                         (changePrice (String->Number (seesaw/value text10)) (String->Number (seesaw/value text11)))
                         (updateinAllP (String->Number (seesaw/value text10)) (String->Number (seesaw/value text11)))
                         (updateinAvailP (String->Number (seesaw/value text10)) (String->Number (seesaw/value text11)))
                         (changePrice1 (String->Number (seesaw/value text10)) (String->Number (seesaw/value text11)))
                         (seesaw/config! text10 :text "")
                         (seesaw/config! text11 :text "")))

(def pricing
  (seesaw/grid-panel
   :border "Change the price of Movie"
   :columns 7
   :items ["" "" "" "" "" "" ""
           "" "" "" "" "" "" ""
           "" "" "" "" "" "" ""
           "" "" id4 "" text10 "" ""
           "" "" "" "" "" "" ""
           "" "" "" "" "" "" ""
           "" "" price2 "" text11 "" ""
           "" "" "" "" "" "" ""
           "" "" "" "" "" "" ""
           "" "" "" changeP "" "" ""
           "" "" "" "" "" "" ""
           "" "" "" "" "" "" ""
           "" "" "" "" "" "" ""
           "" "" "" "" "" "" ""]))

;;****************************************************************************************;;



;;***************************Add Movie****************************************************;;


(defn addMovie
  [id name price quantity]
  (let [r (binding [*read-eval* false]
            (read-string (slurp "movies.clj")))
    r1 (binding [*read-eval* false]
            (read-string (slurp "availmovies.clj")))]
    (with-open [w (clojure.java.io/writer "movies.clj")]
      (binding [*out* w]
        (pr (concat r [[id name price quantity]]))))
    (with-open [w1 (clojure.java.io/writer "availmovies.clj")]
      (binding [*out* w1]
        (pr (concat r1 [[id name price quantity]]))))))


(def id1
  (seesaw/label :text "Id"))

(def name1
  (seesaw/label :text "Name"))

(def price1
  (seesaw/label :text "Price"))

(def quantity1
  (seesaw/label :text "Quantity"))

(def text1
  (seesaw/text :columns 20))

(def text2
  (seesaw/text :columns 20))

(def text3
  (seesaw/text :columns 20))

(def text4
  (seesaw/text :columns 20))

(def add1
  (seesaw/button :text "Add"))

(defn thingstodo
  [n]
  (addMovie (String->Number (seesaw/value text1)) (seesaw/text text2) (String->Number (seesaw/value text3)) (String->Number (seesaw/value text4)))
  (insertinAll (String->Number (seesaw/value text1)) (seesaw/text text2) (String->Number (seesaw/value text3)) (String->Number (seesaw/value text4)))
  (insertinAvail (String->Number (seesaw/value text1)) (seesaw/text text2) (String->Number (seesaw/value text3)) (String->Number (seesaw/value text4)))
  (seesaw/text! text1 "")
  (seesaw/text! text2 "")
  (seesaw/text! text3 "")
  (seesaw/text! text4 ""))


(seesaw/listen add1
               :action (fn[event]
                         (if (or (empty? (seesaw/value text1)) (empty? (seesaw/value text2)) (empty? (seesaw/value text3)) (empty? (seesaw/value text4)))
                           (seesaw/alert "Fill all the field")
                           (thingstodo 0))))

(def addMovies
  (seesaw/grid-panel
   :border "Add a movie"
   :columns 5
   :items ["" id1 "" text1 ""
           "" "" "" "" ""
           "" "" "" "" ""
           "" name1 "" text2 ""
           "" "" "" "" ""
           "" "" "" "" ""
           "" price1 "" text3 ""
           "" "" "" "" ""
           "" "" "" "" ""
           "" quantity1 "" text4 ""
           "" "" "" "" ""
           "" "" add1 "" ""
           "" "" "" "" ""
           "" "" "" "" ""]))


(def tab1
  (seesaw/tabbed-panel
   :placement :top
   :tabs [{:title "All Movies"
           :content allMovies}
          {:title "Rented Movies"
          :content rentedMovies}
          {:title "Available Movies"
           :content availableMovies}
          {:title "Add Movies"
           :content addMovies}
          {:title "Remove Movies"
           :content removeMovies}
          {:title "Add copies of Existing Movies"
           :content addCopies}
          {:title "Change Price of Movie"
           :content pricing}
          {:title "Find Price/Quantity"
           :content finding}]))

(def window
  (seesaw/frame
   :title "Video Store Inventory"
   :content tab1
   :width 1000
   :height 600))

(seesaw/show! window)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
