;; (push #P"/home/p-hasan/src/lisp/lisp-for-web/" asdf:*central-registry*)
;; (require "retro-games")

;; https://raw.githubusercontent.com/adamtornhill/LispForTheWeb/master/web_with_proto_backend.lisp
;; (in-package :cl-user) 
;; ;; 
;; (require :cl-who)
;; (require :hunchentoot)
;; (require :parenscript)
;; ;; (require :elephant)
;; 
;; (defpackage :retro-games
;;   (:use :cl :cl-who :hunchentoot :parenscript))
;; 
(in-package #:retro-games) 

;; Domain model 
;; ============

;; A simple domain model of our games together with 
;; access metods. 

(defclass game () 
  ((name :reader  name 
         :initarg :name)
   (votes :accessor votes 
          :initform 0)))

;; Print helper 
(defmethod print-object ((object game) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name votes) object 
      (format stream "name: ~s with ~d votes" name votes))))

(defmethod vote-for (game) 
  (incf (votes game)))

;; Backend 
(defvar *games* '())

(defun game-from-name (name) 
  (find name *games* :test #'string-equal :key #'name))

(defun game-stored? (game-name) 
  (game-from-name game-name))

(defun games () 
  (sort (copy-list *games*) #'> :key #'votes))

(defun add-game (name) 
  (unless (game-stored? name) 
    (push (make-instance 'game :name name) *games*)))

;; Web server 
(defvar *acceptor* nil)

(defun start-server (port)
  ;; (stop-server)
  (start (setf *acceptor*
               (make-instance 'easy-acceptor
                              :port port))))

(defun stop-server ()
  (when *acceptor*
    (stop *acceptor*)))

;; Start our web server 
;; (defparameter *web-server* 
;;  (start (make-instance 'easy-acceptor :address "localhost" :port 9090)))

(defun publish-static-content () 
  (push (create-static-file-dispatcher-and-handler
         "/logo.jpg" "/home/p-hasan/src/lisp/lisp-for-web/imgs/Commodore64.jpg") *dispatch-table*)
  (push (create-static-file-dispatcher-and-handler
         "/retro.css" "/home/p-hasan/src/lisp/lisp-for-web/css/retro.css") *dispatch-table*))

;; DSL for our webpage
(setf (html-mode) :html5)


(defmacro standard-page ((&key title script) &body body)
  "All pages on the Retro Games site will use the following macro;
   less to type and a uniform look of the pages (defines the header
   and the stylesheet).
   The macro also accepts an optional script argument. When present, the
   script form is expected to expand into valid JavaScript."
  `(with-html-output-to-string
    (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
           (:head
            (:meta :charset "utf-8")
            (:title ,title)
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href "/retro.css")
            ,(when script
               `(:script :type "text/javascript"
                         (str ,script))))
           (:body
            (:div :id "header" ; Retro games header
                  (:img :src "/logo.jpg"
                        :alt "Commodore 64"
                        :class "logo")
                  (:span :class "strapline"
                         "Vote on your favourite Retro Game"))
            ,@body))))
  


;; HTML 
(define-easy-handler (retro-games :uri "/retro-games") () 
  (standard-page (:title "Top Retro Games")
    (:h1 "Vote on your all time favourte retro games!")
    (:p "Missing a game? Make it available for votes " (:a :href "new-game" "here"))
    (:h2 "Current stand") 
    (:div :id "chart" ; Used for CSS Styling of the links 
          (:ol 
           (dolist (game (games))
             (htm 
              (:li (:a :href (format nil "vote?name=~a" (url-encode ; avoid injectino attacks 
                                                         (name game))) "Vote!")
                   (fmt "~A with ~d votes" (escape-string (name game))
                        (votes game)))))))))

(define-easy-handler (new-game :uri "/new-game") () 
  (standard-page (:title "Add a new game" 
                         :script (ps ;client side verfication 
                                   (defvar add-form nil)
                                   (defun validate-game-name (evt) 
                                     "For a more robust event handling mechanism you may want to consider a library" 
                                     (when (= (@ add-form name value) "")
                                       (chain evt (prevent-default))
                                       (alert "Please enter a name.")))
                                   (defun init () 
                                     (setf add-form (chain document 
                                                           (get-element-by-id "addform")))
                                     (chain add-form 
                                            (add-event-listener "submit" validate-game-name false)))
                                   (setf (chain window onload) init)))
    (:h1 "Add a new game to the chart") 
    (:form :action "/game-added" :method "post" :id "addform" 
           (:p "What is the name of the game?" (:br)
               (:input :type "text" :name "name" :class "txt"))
           (:p (:input :type "submit" :value "Add" :class "btn")))))

(define-easy-handler (game-added :uri "/game-added") (name) 
  (unless (or (null name) (zerop (length name))) ;; IN case javascript is turned off. 
    (add-game name))
  (redirect "/retro-games"))

(define-easy-handler (vote :uri "/vote") (name) 
  (when (game-stored? name) 
    (vote-for (game-from-name name)))
  (redirect "/retro-games"))
    

;; (publish-static-content)
;; (start-server 9090)
