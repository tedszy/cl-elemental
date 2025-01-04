;;;; cl-elemental.asd

(asdf:defsystem #:cl-elemental
  :description "Describe cl-elemental here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:fiveam)
  :components ((:file "package")
               (:file "cl-elemental")
	       (:file "prime-gaps")))
