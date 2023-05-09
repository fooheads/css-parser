(ns fooheads.css-parser.core-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [fooheads.css-parser.core :refer [parse garden]]))


(deftest garden-test

  (is (= '[:body {:font-size (px 16)}]
         (garden (parse "body{font-size:16px}"))))

  (is (= '[:h1 :h2 {:font-weight "none"}]
         (garden (parse "h1,h2{font-weight:none}")))))

(garden 
  (parse
   "li {
      font: 14px Helvetica Neue,Helvetica,Arial,sans-serif;
      height: 26px;
      line-height: 25px;
      border: 1px solid #acacac;
      border-radius: 3px;
      background: linear-gradient(180deg,#f0f9ff 0,#cbebff 47%,#a1dbff);
    }"))
       

(garden 
  (parse
   "li {
      font: 14px Helvetica Neue,Helvetica,Arial,sans-serif;
      height: 26px;
      line-height: 25px;
      border: 1px solid #acacac;
      border-radius: 3px;
      background: linear-gradient(180deg,#f0f9ff 0,#cbebff 47%,#a1dbff);
    }"))
       

