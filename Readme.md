# via

- A value of type IO a is an "action" that, when performed, may do some input/output, before delivering a value of type a.
- In Clojure, '(println 1) is an action, a value that when performed may do IO then deliver nil
