(ql:quickload `(:qtools :qtcore :qtgui))

(defpackage #:cad
  (:use #:cl+qt)
  (:export #:main))
(in-package :cad)
(in-readtable :qtools)

(define-widget main-window (Qwidget) ())

(define-subwidget (main-window go-button)
    (q+:make-qpushbutton "Go!" main-window))

(define-subwidget (main-window layout) (q+:make-qhboxlayout main-window)
  (q+:add-widget layout name)
  (q+:add-widget layout go-button))

(with-main-window
    (window `main-window))

(defparameter *name* "bearing-cap")

(defun name-stl (name)
  (concatenate `string "solid " name))


(defun cube-vertices ()
  `((0 0 0)
    (0 0 1)
    (0 1 0)
    (0 1 1)
    (1 0 0)
    (1 0 1)
    (1 1 0)
    (1 1 1)))
