(defsystem "advent-of-code"
  :version "0.1.0"
  :author "kinder ferraz"
;;  :email "ferraz.alkindar@gmail.com"
  :license "MIT"
  :depends-on (:alexandria :arrow-macros :str)
  :components ((:module "src"
                :components
                ((:file "main")
                 (:module "day1"
                  :components ((:file "day1")))
                 (:module "day2"
                  :components ((:file "day2")))
                 (:module "day3"
                  :components ((:file "day3")))
                 (:module "day4"
                  :components ((:file "day4")))
                 (:module "day5"
                  :components ((:file "day5")))
                 (:module "day6"
                  :components ((:file "day6")))
                 (:module "day7"
                  :components ((:file "day7"))))))
  :description ""
  :in-order-to ((test-op (test-op "advent-of-code/tests"))))

(defsystem "advent-of-code/tests"
  :author "kinder ferraz"
  :license "ferraz.alkindar@gmail.com"
  :depends-on ("advent-of-code"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for twit-cleaner"
  :perform (test-op (op c) (symbol-call :rove :run c)))
