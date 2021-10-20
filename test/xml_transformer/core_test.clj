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
<not-used></not-used>
</head>")

(def fields1
  {:header/attr1 [:head :attr1]
   :header/attr2 [:head :attr2]
   :header/not-present-in-source [:head :not-there]})

(def fields2
  {:header/attr3 ^{:value-fn #(Integer/parseInt %)}
   [:head :attr3]})

(def line-fields
  {:line/attr1
   [:lineattr1]
   :line/number ^{:value-fn #(Integer/parseInt %)}
   [:lineattr2]})

(def fields3
  {:header/attr1 [:head :attr1]
   :header/attr2 [:head :attr2]
   :header/attr3 ^{:value-fn #(Integer/parseInt %)}
   [:head :attr3]
   :lines ^{:many true :node-fn #(transform-xml % line-fields)}
   [:lines :line]})


(def fields3-nested-fields
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
           (transform-xml (parse-str flat-xml) fields2)))))

(deftest nested-transform
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
           (transform-xml (parse-str nested-xml) fields3-nested-fields)))))

(deftest reduced-attributes
  (testing "Vector of attributes"
    (let [xml-str     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<head>
<attr1>value1</attr1>
<attr2>value2</attr2>
</head>"
          zipped-xml  (parse-str xml-str)
          fields      {:header/attr1       [:head :attr1]
                       :header/attr2       [:head :attr2]
                       :header/attr1+attr2 ^{:reduce-fn vector}
                       [:header/attr1 :header/attr2]}
          transformed (transform-xml zipped-xml fields)]
      (is (= #:header{:attr1       "value1",
                      :attr2       "value2",
                      :attr1+attr2 ["value1" "value2"]}
             transformed))))
  (testing "Vector of 3 attributes"
    (let [xml-str     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<head>
<attr1>value1</attr1>
<attr2>value2</attr2>
<attr3>value3</attr3>
</head>"
          zipped-xml  (parse-str xml-str)
          fields      {:header/attr1             [:head :attr1]
                       :header/attr2             [:head :attr2]
                       :header/attr3             [:head :attr3]
                       :header/attr1+attr2+attr3 ^{:reduce-fn conj :reduce-init []}
                       [:header/attr1 :header/attr2 :header/attr3]}
          transformed (transform-xml zipped-xml fields)]
      (is (= #:header{:attr1       "value1",
                      :attr2       "value2",
                      :attr3       "value3",
                      :attr1+attr2+attr3 ["value1" "value2" "value3"]}
             transformed))))

  (testing "Sum of attributes"
    (let [xml-str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<head>
<value1>11</value1>
<value2>31</value2>
</head>"

          zipped-xml (parse-str xml-str)
          fields     {:header/value1        ^{:value-fn #(Integer/parseInt %)} [:head :value1]
                      :header/value2        ^{:value-fn #(Integer/parseInt %)} [:head :value2]
                      :header/value1+value2 ^{:reduce-fn +}
                      [:header/value1 :header/value2]}

          transformed (transform-xml zipped-xml fields)]
      (is (= #:header{:value1        11,
                      :value2        31,
                      :value1+value2 42}
             transformed)))))
