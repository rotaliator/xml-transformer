(ns xml-transformer.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [xml-transformer.core :refer [transform-xml parse-str]]))

(def flat-xml
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<head>
<attr1>value1</attr1>
<attr2>value2</attr2>
<attr3>42</attr3>
</head>")

(def nested-xml
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<head>
<attr1>value1</attr1>
<attr2>value2</attr2>
<attr3>42</attr3>
<lines>
<line><lineattr1>line1 value1</lineattr1></line>
<line><lineattr1>line2 value1</lineattr1><lineattr2>123</lineattr2></line>
</lines>
</head>")

(def fields1
  {:header/attr1 [:head :attr1]
   :header/attr2 [:head :attr2]})

(def fields2
  {:header/attr3 ^{:value-fn #(Integer/parseInt %)}
   [:head :attr3]})

(declare line-fields)

(def fields3
  {:header/attr1 [:head :attr1]
   :header/attr2 [:head :attr2]
   :header/attr3 ^{:value-fn #(Integer/parseInt %)}
   [:head :attr3]
   :lines ^{:many true :node-fn #(transform-xml % line-fields)}
   [:lines :line]})

(def line-fields
  {:line/attr1
   [:lineattr1]
   :line/number ^{:value-fn #(Integer/parseInt %)}
   [:lineattr2]})

(def fields3-nested
  {:header/attr1 [:head :attr1]
   :header/attr2 [:head :attr2]
   :header/attr3 ^{:value-fn #(Integer/parseInt %)}
   [:head :attr3]
   :lines ^{:many true :fields line-fields}
   [:lines :line]})


(deftest flat-transform
  (testing "Transformation of flat xml"
    (is (= {:header/attr1 "value1", :header/attr2 "value2"}
           (transform-xml (parse-str flat-xml) fields1))))
  (testing "Transformation of flat xml with coercion"
    (is (= {:header/attr3 42}
           (transform-xml (parse-str flat-xml) fields2))))
  (testing "Transformation of nested xml with node coercion"
    (is (= {:header/attr1 "value1",
            :header/attr2 "value2",
            :header/attr3 42,
            :lines        [#:line{:attr1 "line1 value1"}
                           #:line{:attr1 "line2 value1" :number 123}]}
           (transform-xml (parse-str nested-xml) fields3))))
  (testing "Transformation of nested xml with nested fields"
    (is (= {:header/attr1 "value1",
            :header/attr2 "value2",
            :header/attr3 42,
            :lines        [#:line{:attr1 "line1 value1"}
                           #:line{:attr1 "line2 value1" :number 123}]}
           (transform-xml (parse-str nested-xml) fields3-nested)))))
