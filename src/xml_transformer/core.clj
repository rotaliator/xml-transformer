(ns xml-transformer.core
  (:require [clojure.xml :as xml]
            [clojure.data.zip.xml :as zip-xml]
            [clojure.zip :as zip]))

(set! *warn-on-reflection* true)

(defn parse-str
  "Parses string to tree of the xml/element struct-map zipped with xml-zip"
  [s]
  (-> (xml/parse (new org.xml.sax.InputSource
                      (new java.io.StringReader s)))
      zip/xml-zip))


(defn remove-nils [m]
  (into {} (remove (comp nil? second) m)))

(defn transform-xml
  "Transforms zipped-xml into map using field-map"
  [zipped-xml field-map]
  (let [as-map (->>
                (for [[field selector] field-map]
                  (let [many?         (-> selector meta :many)
                        value-fn      (-> selector meta :value-fn)
                        node-fn       (-> selector meta :node-fn)
                        nested-fields (-> selector meta :fields)

                        f         (if many? zip-xml/xml-> zip-xml/xml1->)
                        get-value (fn [node]
                                    (when node
                                      (if many?
                                        (map zip-xml/text node)
                                        (zip-xml/text node))))
                        node      (apply f zipped-xml selector)
                        value     (get-value node)
                        value
                        (if (and value (or value-fn node-fn nested-fields))
                          (cond
                            value-fn
                            (if (coll? value)
                              (mapv value-fn value)
                              (value-fn value))

                            node-fn
                            (if many?
                              (mapv node-fn node)
                              (node-fn node))

                            nested-fields
                            (if many?
                              (mapv #(transform-xml % nested-fields) node)
                              (transform-xml node nested-fields)))

                          value)]
                    [field value]))
                (into {}))
        as-map (remove-nils as-map)]
    as-map))
