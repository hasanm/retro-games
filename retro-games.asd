(asdf:defsystem #:retro-games
  :serial t
  :depends-on (#:cl-ppcre
               #:cl-who
               #:hunchentoot
               #:parenscript)
  :components ((:file "package")
               (:file "retro-games")))
