(ns fooheads.css-parser.core
  (:require
    [clojure.string :as str]
    [clojure.zip :as zip]
    [instaparse.core :as insta]
    [instaparse.combinators :as instac]
    [zippo.core :as zippo]))


(def whitespace
  (insta/parser
    "whitespace = #'\\s+'"))


(def parser-simple
  (insta/parser
    "css = element*
     element = key <':'> value <';'>
     key = literal
     value = #'[^;]*'
     literal = #'[-\\w]+'"
    :auto-whitespace whitespace))


(defn parse-simple [s]
  (->>
    (parser-simple s)
    (insta/transform
      {:value str/trim
       :literal str/trim
       :key (comp keyword str/trim)
       :element (fn [k v] {k v})
       :css merge})))



(def example
  (insta/parser
    "strings = {s}+
     s = '\"' #'[^\"]*' '\"'
    "
    :auto-whitespace whitespace))


(example "\"abc\" \"foo\"")

(def css21-additions
  "
  h : #'[0-9a-f]'
  nonascii : #'[\240-\377]'
  unicode : #'\\{h}{1,6}(\r\n|[ \t\r\n\f])?'
  escape : {unicode} | #'[^\r\n\f0-9a-f]'
  nmstart : #'[_a-z]' | {nonascii} | {escape}
  nmchar : #'[_a-z0-9-]' | {nonascii} | {escape}
  nmstart-simple : #'[_a-z]'
  nmchar-simple : #'[_a-z0-9-]'
  string1 : '\"' ( #'[^\n\r\f\"]' | {nl} | {escape})* '\"'
  string2 : '\\'' ( #'[^\n\r\f\\']' | {nl} | {escape})* '\\''
  comment : #'\\/\\*[^*]*\\*+([^/*][^*]*\\*+)*\\/'
  ident : #'-?'{nmstart}{nmchar}*
  ident-simple : #'-?' {nmstart-simple} {nmchar-simple}*
  name : {nmchar}+
  num = #'[0-9]+' | #'[0-9]*' #'\\.' #'[0-9]+'
  string : {string1}|{string2}
  url : (#'[!#$%&*-~]' | {nonascii} | {escape})*
  s : #'[ \t\r\n\f]+'
  w : {s}?
  nl : #'\n|\r\n|\r|\f'

  length-unit : ('px' | 'cm' | 'mm' | 'in' | 'pt' | 'pc')

  S : #'[ \t\r\n\f]+'
  W : S?
  (* IDENT : {ident-simple} *)
  IDENT : #'[\\w-_]+'
  INCLUDES : #'~='
  DASHMATCH : #'|='
  STRING : string
  HASH : #'#'
  PAGE_SYM = 'PAGE'
  COMMA = ','
  FUNCTION = IDENT '('
  NUMBER = num
  PERCENTAGE = num '%'
  LENGTH = num length-unit
  EMS = num 'em'
  EXS = num 'ex'
  ANGLE = num ('deg' | 'rad' | 'grad')
  TIME = num ('ms' | 's')
  FREQ = num ('hz' | 'khz')
  URI = 'url(' {w} {string} {w} ')' | 'url(' {w}{url}{w} ')'
  CHARSET_SYM = '@charset '
  CDO = '<!--'
  CDC = '-->'
  IMPORT_SYM = '@import'
  MEDIA_SYM = '@media'
  IMPORTANT_SYM = '!' (W | COMMENT)* 'important'
  COMMENT = comment
  ")


(comment (instac/ebnf css21-additions))


(def css21-bnf
  "stylesheet
    : [ CHARSET_SYM STRING ';' ]?
      [S|CDO|CDC]* [ import [ CDO S* | CDC S* ]* ]*
      [ [ ruleset | media | page ] [ CDO S* | CDC S* ]* ]*
    ;
  import
    : IMPORT_SYM S*
      [STRING|URI] S* media_list? ';' S*
    ;
  media
    : MEDIA_SYM S* media_list '{' S* ruleset* '}' S*
    ;
  media_list
    : medium [ COMMA S* medium]*
    ;
  medium
    : IDENT S*
    ;
  page
    : PAGE_SYM S* pseudo_page?
      '{' S* declaration? [ ';' S* declaration? ]* '}' S*
    ;
  pseudo_page
    : ':' IDENT S*
    ;
  operator
    : '/' S* | ',' S*
    ;
  combinator
    : '+' S*
    | '>' S*
    ;
  unary_operator
    : '-' | '+'
    ;
  property
    : IDENT S*
    ;
  ruleset
    : selectors declarations
    ;
  selectors
    : selector [ <','> S* selector ]*
    ;
  selector
    : simple_selector [ combinator selector | S+ [ combinator? selector ]? ]?
    ;
  simple_selector
    : element_name [ HASH | class | attrib | pseudo ]*
    | [ HASH | class | attrib | pseudo ]+
    ;
  class
    : '.' IDENT
    ;
  element_name
    : IDENT | '*'
    ;
  attrib
    : '[' S* IDENT S* [ [ '=' | INCLUDES | DASHMATCH ] S*
      [ IDENT | STRING ] S* ]? ']'
    ;
  pseudo
    : ':' [ IDENT | FUNCTION S* [IDENT S*]? ')' ]
    ;
  declarations
    : <'{'> S* declaration? [ <';'> S* declaration? ]* <'}'> S*
    ;
  declaration
    : property <':'> S* expr prio?
    ;
  prio
    : IMPORTANT_SYM S*
    ;
  expr
    : term [ operator? term ]*
    ;
  term
    : unary_operator?
      [ NUMBER S* | PERCENTAGE S* | LENGTH S* | EMS S* | EXS S* | ANGLE S* |
        TIME S* | FREQ S* ]
    / STRING S* / IDENT S* / URI S* / hexcolor / function
    ;
  function
    : FUNCTION S* expr ')' S*
    ;
  (*
   * There is a constraint on the color that it must
   * have either 3 or 6 hex-digits (i.e., [0-9a-fA-F])
   * after the \"#\"; e.g., \"#000\" is OK, but \"#abcd\" is not.
   *)
  hexcolor
    : HASH S*
    ;
  ")


(comment (instac/ebnf css21-bnf))


(def css21-parser
  (insta/parser
    (merge
      (instac/ebnf css21-bnf)
      (instac/ebnf css21-additions))
    :string-ci true
    :start :stylesheet
    :auto-whitespace whitespace))


(defn parse [css & args]
  (apply css21-parser css args))


(defn parse-int [s]
  #?(:clj (Integer/parseInt s)))


(defn garden [css-parse-tree]
  (as->
    (zip/vector-zip css-parse-tree) tree
    (zippo/loc-update
      tree
      (fn [loc] (let [node (zip/node loc)]
                  (let [res
                        (or
                          ;; whitespace
                          (and (vector? node) (= :S (first node)))
                          ;; empty terms (how do they appear?)
                          (and (vector? node) (= :term (first node)) (empty? (rest node))))]
                    res)))
      zip/remove)
    (zip/root tree)


    (insta/transform
      {:num (fn [x] (parse-int x))
       :length-unit (fn [x] (symbol x))
       :LENGTH (fn [n unit] (list unit n))
       :IDENT identity
       :element_name identity
       :simple_selector keyword
       :term identity
       :expr (fn ([arg] arg)
                 ([arg & args] (str/join " " (cons arg args))))

       :property keyword
       :stylesheet identity
       ; :ruleset (fn [& args] args)
       :ruleset (fn [selectors declarations] (conj selectors (merge declarations)))
       ;:ruleset (fn [selector declarations] (conj selector (merge declarations)))
       :declarations merge
       :declaration (fn [k v] {k v})
       :selector (fn
                   ([selector] selector)
                   ([selector & selectors] [selector (vec selectors)]))
       :selectors (fn [& selectors] (vec selectors))}
       ;:selector (fn [selector & selectors] (vec (cons (keyword selector) selectors)))}

      tree)))


(comment
  (garden
    (parse
     "h1, h2 { text-align: center; color: red; }"))

  (parse
    "p {
      text-align: center;
      color: red;
    }")


  (.printStackTrace *e)
  (->
    "li {font: 14px Helvetica Neue,Helvetica,Arial,sans-serif;}"
    (parse)
    (garden))

  (parse "3px" :start :term)

  (->
    (parse
      "li {
        -webkit-text-size-adjust: 100%;
        -webkit-tap-highlight-color: rgba(0,0,0,0);
        word-break: break-all;
        color: #333;
        word-wrap: break-word;
        cursor: text;
        list-style-type: none;
        outline: transparent;
        box-sizing: border-box;
        margin: 2px;
        padding: 0 5px;
        display: inline-block;
        float: left;
        font: 14px Helvetica Neue,Helvetica,Arial,sans-serif;
        height: 26px;
        line-height: 25px;
        border: 1px solid #acacac;
        border-radius: 3px;
        background: linear-gradient(180deg,#f0f9ff 0,#cbebff 47%,#a1dbff);
      }")
    (garden))


  (parse
    "li {
      font: 14px Helvetica Neue,Helvetica,Arial,sans-serif;
      height: 26px;
      line-height: 25px;
      border: 1px solid #acacac;
      border-radius: 3px;
      background: linear-gradient(180deg,#f0f9ff 0,#cbebff 47%,#a1dbff);
    }"))



(parse-simple
  "    -webkit-text-size-adjust: 100%;
    -webkit-tap-highlight-color: rgba(0,0,0,0);
    line-height: 1.42857;
    font-family: Arial;
    font-size: .92rem;
    margin: 0;
    outline: transparent;
    box-sizing: border-box;
    position: relative;
    min-height: 1px;
    float: left;
    width: 16.66667%;
    height: 25px;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
    color: #666;
    padding-top: 5px;
    padding-bottom: 5px;
    padding-left: 5px;
    padding-right: 5px;
    border-left: none;")


