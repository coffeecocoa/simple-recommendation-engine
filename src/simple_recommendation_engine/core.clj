(ns simple-recommendation-engine.core
	(:require [clojure.set :as set]))

; ketika user milihat item, kemudian menyarankan user
; untuk melihat semua item yang user lain menunjukkan minat 
; di saat mereka juga mengunjungi item yang sama

(defn correlation-matrix-delta 
	"mengambil item dan map yang mirip dan memproses
	 perbedaan untuk diterapkan ke map yang berhubungan"
	[this-item sim-map]

	(let [s-sim-map (dissoc sim-map this-item)] ;;menghapus item jika sim-map berisi item bind ke s-sim-map
		(if (seq s-sim-map)
			;;bila map berisi elemen
			(let [line-this-item {this-item s-sim-map}
				  ;;line-this-item berisi item1 similarity1, item2 similarity2
				  lines-sim-map (into {} (map (fn [[k v]]
				  	                            {k {this-item v}})
				  						  s-sim-map))]
				  ;;lines item1 berisi this-item similarity1
				  ;;item2 berisi this-item similarity2
				  (conj lines-sim-map line-this-item))
			;;else return empty map
			{})))

(defn update-correlation-matrix
	"update informasi yang berhubungan saat menerima item baru"
	[correlation-matrix this-item sim-map]
	(merge-with (partial merge-with +)
		correlation-matrix
		(correlation-matrix-delta this-item sim-map)))

(defn update-coocurrence-matrix
	[correlation-matrix this-item session-map]
	(let [cooc-sim (zipmap (keys session-map) (repeat 1))]
		(update-correlation-matrix correlation-matrix this-item cooc-sim)))

(defn apply-fn-to-map-vals
	"apply func f to map m values , return result map"
	[f m]
	(into {} (map (fn [[k v]] [k (f v)]) m)))

(defn recommend-for-an-item 
	[correlation-matrix this-item pref]
	(if-let [items->this-item (correlation-matrix this-item)]
		(->> items->this-item
			 (apply-fn-to-map-vals (partial * pref)))
		{}))

(defn recommend-for-a-prefs-map
	[correlation-matrix pref-maps]
	(->> pref-maps
		 (map (fn [[item pref]]
		 		  (recommend-for-an-item correlation-matrix item pref)))
		 (reduce (partial merge-with +))))

; recommendion in action 
; 2 users 
; user1 click item1 dan item2
; kemudia mengunjungi item3 dan item4

(def coocurrence-matrix
	(-> {}
		(update-coocurrence-matrix :item1 {})
		(update-coocurrence-matrix :item2 {:item1 2})
		(update-coocurrence-matrix :item3 {})
		(update-coocurrence-matrix :item4 {:item3 2})))

(def user3-prefs-map {:item1 2 :item3 4})
(recommend-for-a-prefs-map coocurrence-matrix user3-prefs-map) ;; output ->> {:item2 2, :item4 4}
