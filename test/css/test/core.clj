(ns css.test.core
  (:use [css.core] :reload)
  (:use name.choi.joshua.fnparse)
  (:use [clojure.test]))

(defn test-rule
  [rule tokens]
  (binding [*remainder-accessor* remainder-a]
    (rule-match rule
      (fn [_] nil)
      (fn [_ _] nil)
      (struct state-s tokens 0 0))))

(defmacro should-match
  [rule & token-sets]
  (let [assertions (for [tokens token-sets] `(is (test-rule ~rule ~tokens)))]
    `(do ~@assertions)))

(defmacro should-not-match
  [rule & token-sets]
  (let [assertions (for [tokens token-sets] `(is (not (test-rule ~rule ~tokens))))]
    `(do ~@assertions)))

(defn should-make-node
  [rule node tokens]
  (is (= node (test-rule rule tokens))))

(defn debug-rule
  [rule tokens]
  (binding [*remainder-accessor* remainder-a]
    (rule-match rule
      (fn [_] (println "Total failure. There is no useful information I can give."))
      (fn [_ state] (println (str "Error at line " (:line state) ", column " (:column state) ".\n" 
                                  (apply str (take 150 (:remainder state))))))
      (struct state-s tokens 0 0))))

(deftest test-rule-test
  (testing "test-rule produces an error when the tokens aren't matched"
    (is (not (test-rule back-slash "1")))))

(deftest character-literal-test
  (testing "defcls should define the non-alphanumeric characters"
    (should-match back-slash "\\")
    (should-match dot ".")
    (should-match new-line "\n")
    (should-match quotes "'" "\"")
    (should-match zero "0")))

(deftest character-class-test
  (testing "character-classes"
    (should-match (rep+ non-zero-digit) "123456789")
    (should-match (rep+ number) "0123456789")
    (should-match (rep+ letter) "qwertyuiopadsfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM")
    (should-match (rep+ alphanumeric) "1q2w3e4r5t6y7u8i9o0p")
    (should-match (rep+ hexadecimal-digit) "abcdefABCDEF0123456789")
    (should-not-match unescaped-char "\\" "\"")
    (should-match (rep+ unescaped-char) "assdf lasldslfijawfi19&*(!*$^&*£$&^*")
    (should-match (rep+ standard-escapeable-char) "\\/bfnrt")
    (should-match (rep+ standard-escape-sequence) "\\n\\\\\\/\\b\\t\\f\\r")
    (should-match (rep+ double-quote-string-char) "qweruioasvjkoa sgd\\\"\n\t\r\f\\asadf'")
    (should-match (rep+ single-quote-string-char) "qweruioasvjkoa sgd\\'\n\t\r\f\\asadf\"")
    (should-not-match (rep+ double-quote-string-char) "qwert\"qweruio")
    (should-not-match (rep+ single-quote-string-char) "qwert'qwertyui")
    (should-match line-break "\n" "\r")
    (should-match nb-whitespace " \t")
    (should-not-match nb-whitespace " \t\n" " \t\r")
    (should-match whitespace "\n\r \t")))

(deftest selector-matching-test
  (testing "rules to match css selectors"
    (should-match tag "h1" "p" "input" "body" "*")
    (should-not-match tag "h1 p" "P" "$")
    (should-match lang-modifier ":lang(en)" ":lang(en-GB)" ":lang(fr-CA)")
    (should-match other-pseudo-class-modifier ":wrong" ":just-in-case" ":link")
    (should-match pseudo-class-modifier ":first-child" ":link" ":visited" ":active" ":hover" ":focus" ":lang(en)" ":lang(en-GB)" ":lang(fr-CA)" ":wrong" ":just-in-case")
    (should-match class-modifier ".abc1" ".Abc1" "._abc" ".-ABC")
    (should-not-match class-modifier ".123" "#abc")
    (should-match id-modifier "#abc" "#a1-a:h.I")
    (should-not-match id-modifier "#1abc" "#-abc" "#_abc" ".abc")
    (should-match attribute-modifier "[abc=\"example\"]" "[abc=example]" "[abc|=wtf]" "[abc~=123]" "[abc]")
    (should-match modifier ":focus" ":lang(en)" ".Abc1" "#abc" "[abc=example]")
    (should-match element-selector "a:visited" "p[class=whatever]" ".class" "#id")
    (should-match descendant-selector "div.links a" "div#column p[class~=\"highlight\"]" "div.a div.b div.c")
    (should-match child-selector "p   > em" "div.a >div[attr=~val]")
    (should-match sibling-selector "a.brother + a.sister")
    (should-match css-selector "#menu .current_page_item a")
    (should-make-node css-selector {:kind :selector :content "#logo h1 a:hover"} "#logo h1 a:hover")
    (should-make-node css-selector {:kind :selector :content "#menu .current_page_item a"} "#menu .current_page_item a")
    (should-make-node css-selector-list {:kind :selector-list :content (list {:kind :selector :content "#logo h1"}
                                                                         {:kind :selector :content "#logo h2"})} "#logo h1, #logo h2")))

(deftest data-type-literal-test
  (testing "data-type literals"
    (should-make-node color-literal {:kind :color :content "1Ab9f3"} "#1Ab9f3")
    (should-make-node color-literal {:kind :color :content "11AAbb"} "#1Ab")
    (should-make-node color-literal {:kind :color :content "ffff00"} "yellow")
    (should-make-node color-literal {:kind :color :content "800000"} "maroon")
    (should-make-node color-literal {:kind :color :content "c0c0c0"} "silver")
    (should-make-node color-literal {:kind :color :content "ff00ff"} "fuchsia")
    (should-match double-quote-string (str \" "this is a valid\nstring with double quotes \r and some other specials like \t and \\\"" \"))
    (should-match single-quote-string (str \' "this is a valid\nstring with single quotes \r and some other specials like \t and \\'" \'))
    (should-match no-quote-string "this-is-a-valid-no-quote-string")
    (should-match no-quote-string "/paths/are/valid_too.txt")
    (should-match string-literal (str \' "this is a valid\nstring with single quotes \r and some other specials like \t and \\'" \')
                                 (str \" "this is a valid\nstring with double quotes \r and some other specials like \t and \\\"" \")
                                 "this-is-a-valid-no-quote-string")
    (should-make-node string-literal {:kind :string :content "this is a 'string node' & it has a \" in it"} (str \" "this is a 'string node' & it has a \\\" in it" \"))
    (should-make-node string-literal {:kind :string :content "this-is-a-valid-no-quote-string"} "this-is-a-valid-no-quote-string")
    (should-make-node number-literal {:kind :number :content 1} "1")
    (should-make-node number-literal {:kind :number :content 1.01} "1.01")
    (should-make-node number-literal {:kind :number :content -1} "-1")
    (should-make-node unit-literal "px" "px")
    (should-make-node unit-literal "cm" "cm")
    (should-make-node unit-literal "em" "em")
    (should-make-node unit-literal "kHz" "kHz")
    (should-make-node unit-literal "%" "%")
    (should-make-node number-literal {:kind :number-with-unit :content [1 "px"]} "1px")
    (should-make-node number-literal {:kind :number-with-unit :content [1.01 "px"]} "1.01px")
    (should-make-node number-literal {:kind :number-with-unit :content [-1 "px"]} "-1px")
    (should-make-node number-literal {:kind :number-with-unit :content [-1.01 "px"]} "-1.01px")
    (should-make-node boolean-literal {:kind :boolean :content true} "true")
    (should-make-node boolean-literal {:kind :boolean :content false} "false")))

(def sample-rule
  "body {
  margin-top: 50px;
	padding: 0;
	background: #FFFFFF url(images/img01.jpg) repeat-x left top;
	font-size: 12px;
	font-family: Arial, Helvetica, sans-serif;
	text-align: justify;
	color: #5C5C5C;
}")

(def expected-from-sample-rule
  (make-node :css-rule {:selector (debug-rule css-selector-list "body")
                        :declarations (vec (for [l ["margin-top: 50px;" 
                                                    "padding: 0;" 
                                                    "background: #FFFFFF url(images/img01.jpg) repeat-x left top;" 
                                                    "font-size: 12px;" 
                                                    "font-family: Arial, Helvetica, sans-serif;"
                                                    "text-align: justify;"
                                                    "color: #5C5C5C;"]] 
                                             (debug-rule css-rule-declaration l)))}))

(deftest css-rule-matching-test
  (testing "css-rules"
    (should-match start-css-rule "\n\r\t { \n\r\t" "{")
    (should-match end-css-rule "\n\r\t } \n\r\t" "}")
    (should-make-node css-property {:kind :property :content "font-size"} "font-size")
    (should-make-node css-property {:kind :property :content "-moz-border-colors"} "-moz-border-colors")
    (should-make-node arg-list [(make-node :number 1) 
                                (make-node :string "foo")
                                (make-node :string "I'm a longer string")
                                (make-node :number-with-unit [20 "px"])
                                (make-node :boolean true)] "1, foo, \"I'm a longer string\", 20px, true")
    (should-make-node function-call {:kind :function-call :content {:function "test"
                                                                    :args [(make-node :number -0.01)
                                                                           (make-node :boolean false)]}} "test(-0.01,false)")
    (should-make-node string-list (make-node :string-list [(make-node :string "Arial")
                                                           (make-node :string "Helvetica")
                                                           (make-node :string "sans-serif")]) "Arial, Helvetica, sans-serif")
    (should-make-node css-value-list [(make-node :color "FFFFFF")
                                  (make-node :function-call {:function "url" :args [(make-node :string "images/img01.jpg")]})
                                  (make-node :string "repeat-x")
                                  (make-node :string "left")
                                  (make-node :string "top")] "#FFFFFF url(images/img01.jpg) repeat-x left top")
    (should-make-node css-value-list [(make-node :number-with-unit [12 "px"])
                                  (make-node :number-with-unit [20 "px"])
                                  (make-node :number-with-unit [0 "px"])
                                  (make-node :number-with-unit [20 "px"])] "12px 20px 0px 20px")
    (should-make-node css-value-list [(make-node :number-with-unit [12 "px"])
                                  (make-node :string-list [(make-node :string "Arial")
                                                           (make-node :string "Helvetica")
                                                           (make-node :string "sans-serif")])]
                                  "12px Arial, Helvetica, sans-serif")
    (should-make-node css-value-list [(make-node :number-with-unit [1 "px"])
                                  (make-node :string "dashed")
                                  (make-node :color "DFE1E0")] "1px dashed #DFE1E0")
    (should-make-node css-rule-declaration {:kind :css-rule-declaration :content {:property {:kind :property 
                                                                                             :content "margin-top"}
                                                                                  :value {:kind :number-with-unit 
                                                                                          :content [50 "px"]}}} "margin-top: 50px;  \n\t")
    (should-make-node css-rule-declaration {:kind :css-rule-declaration :content {:property {:kind :property 
                                                                                             :content "font"}
                                                                                  :value {:kind :ratio 
                                                                                          :content {:numerator (make-node :number-with-unit [2 "px"])
                                                                                                    :denomenator (make-node :number-with-unit [3 "px"])}}}} "font: 2px/3px;")
    (should-make-node css-rule-declaration {:kind :css-rule-declaration :content {:property {:kind :property :content "background"}
                                                                                  :value (debug-rule css-value-list "#FFFFFF url(images/img01.jpg) repeat-x left top")}}
                      "background: #FFFFFF url(images/img01.jpg) repeat-x left top;")
    (should-make-node css-rule expected-from-sample-rule sample-rule)))

(deftest comments
  (testing "ignoring comments"
    (should-match single-line-comment "//blah blah blah\n")
    (should-match multi-line-comment "/*blah\nblah\nblah*/")))

(def test-file "./default.css")

(deftest css-parsing-test
  (testing "parsing a complete css file"
    (should-match css-text (slurp test-file))))

;;;;;;;; Sass extensions start here

(def nested-rule-sample
"#main p {
  color: #00ff00;

  .redbox {
    background-color: #ff0000;
  }

  pre { font-size: 3em; }
}")

(def expected-from-nested-rule
  (make-node :css-rule {:selector (test-rule css-selector-list "#main p")
                        :declarations (vector (test-rule css-rule-declaration "color: #00ff00;")
                                              (test-rule css-rule ".redbox {\n  background-color: #ff0000;\n}")
                                              (test-rule css-rule "pre { font-size: 3em; }"))}))

(deftest nested-rule-test
  (testing "nested-rules"
    (should-match css-rule nested-rule-sample)
    (should-make-node css-rule expected-from-nested-rule nested-rule-sample))
  (testing "parent selector references"
    (should-make-node css-selector {:kind :selector :content "&:hover"} "&:hover")))

(def nested-property-sample-with-value
"font: 2px/3px {
  family: fantasy;
  size: 30em;
  weight: bold;
}")

(def nested-property-sample-without-value
"font: {
  family: fantasy;
  size: 30em;
  weight: bold;
}")

(deftest nested-properties
  (testing "nested-properties"
    (should-match css-rule-declaration nested-property-sample-with-value)
    (should-match css-rule-declaration nested-property-sample-without-value)))

(def v-ass-in-rule ; sample sass with a variable assignment inside a rule
"a {
  $color: red;
  
  font-size: 10px;
}")

(def expected-from-v-ass-in-rule
  (make-node :css-rule {:selector (test-rule css-selector-list "a")
                        :declarations (vector (test-rule css-expr "$color: red;")
                                              (test-rule css-rule-declaration "font-size: 10px;"))}))

(def v-usage
"a {
  $color: red;
  color: $color;
  font-size: 10px;
}")
  
(def expected-from-v-usage
  (make-node :css-rule {:selector (test-rule css-selector-list "a")
                        :declarations (vector (test-rule css-expr "$color: red;")
                                              (make-node :css-rule-declaration {:property (test-rule css-property "color")
                                                                                :value (make-node :sass-variable "color")})
                                              (test-rule css-rule-declaration "font-size: 10px;"))}))
(def v-in-func "url($my_url)")

(def expected-from-v-in-func
  (make-node :function-call {:function "url"
                             :args (vector (make-node :sass-variable "my_url"))}))

(deftest sass-variables
  (testing "variable-assignment"
    (testing "at root level"
      (should-make-node css-expr (make-node :sass-variable-assignment {:variable (make-node :sass-variable "width")
                                                                       :value    (make-node :number-with-unit [5 "em"])}) "$width: 5em;"))
    (testing "inside a rule"
      (should-make-node css-expr expected-from-v-ass-in-rule v-ass-in-rule)))
  (testing "variable usage"
    (should-make-node css-expr expected-from-v-usage v-usage)
    (should-make-node function-call expected-from-v-in-func v-in-func)))

(run-tests)
