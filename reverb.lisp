;; Package information

(defpackage :reverb (:use :cl :sketch)
            (:export
             #:make-instance))
(in-package :reverb)


;; Global variables

(defparameter *events* (make-hash-table))
(defparameter *instances* (make-hash-table))
(defparameter *bullets* (make-hash-table))
(defparameter *shooting* nil)
;; (defparameter *lock* (bt:make-lock))

;; Functions

(defun print-hash (hash)
  (loop for key being each hash-key of hash
          using (hash-value value)
        collect (list key value)))

(defun index-instance (name instance)
  (setf (gethash name *instances*) instance))

(defun index-bullet (name bullet)
  (setf (gethash name *bullets*) bullet))

(defun delete-instance (name)
  (remhash name *instances*))

(defun delete-bullet (name)
  (remhash name *instances*)
  (remhash name *bullets*))
 
(defun create-bullet (ship)
  (let ((bullet (make-instance 'bullet
                               :x (x ship)
                               :y (y ship)
                               :draw-function (function draw-bullet)))
        (name (intern (concatenate 'string "bullet-" (write-to-string (random 1000000))))))
    (index-bullet name bullet)
    (index-instance name bullet)))

(defun draw-ship (ship name ticks)
  (circle (x ship) (y ship) 25)
  (if *shooting* (progn
                   (create-bullet ship))))

(defun draw-bullet (bullet name ticks)
  (setf (x bullet) (+ 10 (x bullet)))
  (circle (x bullet) (y bullet) 1)
  (if (> (x bullet) 1000)
      (progn (delete-instance name)
             (delete-bullet name))))

(defun draw-sine (sine name ticks)
  (if (< (start-tick sine) ticks)
      (progn 
        (let ((ticks  (* 0.2 (- 800 (- ticks (start-tick sine))))))
          (setf (x sine) (* 5 ticks))
          (setf (y sine) (+ 200 (* 50 (cos ticks))))
          (circle (x sine) (y sine) (width sine))

          (loop for k being each hash-key of *bullets*
                do (progn
                     (if (and (> (+ (x sine) (/ (width sine) 2)) (x (gethash k *bullets*)))
                              (< (- (x sine) (/ (width sine) 2)) (x (gethash k *bullets*)))
                              (> (+ (y sine) (/ (height sine) 2)) (y (gethash k *bullets*)))
                              (< (- (y sine) (/ (height sine) 2)) (y (gethash k *bullets*))))
                         (progn
                           (with-pen (make-pen :fill +RED+)
                             (circle (x sine) (y sine) (width sine)))
                           (delete-bullet k)
                           (setf (health sine) (- (health sine) 1))
                           (print (health sine)))))))
        (if (< (health sine) 1)
            (progn
              (print "delete!")
              (delete-instance name))))))


;; Classes

(defclass game-object ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (draw-function :accessor draw-function :initarg :draw-function))
  (:default-initargs :x 0 :y 0))

(defclass ship (game-object)
  ())

(defclass bullet (game-object)
  ())

(defclass enemy (game-object)
  ((health :accessor health :initarg :health)
   (width :accessor width :initarg :width)
   (height :accessor height :initarg :height)
   (start-tick :accessor start-tick :initarg :start-tick)))

(defclass sine (enemy)
  ()
  (:default-initargs :health 3 :height 12 :width 12))


;; Methods

;; (defmethod initialize-instance :after ((item enemy) &key)

;;   (if (gethash (start-tick item) *events*)
;;       (push (gethash (start-tick item) *events*))
;;       (setf (gethash (start-tick item)) '())
;;       ))

(defmethod draw-game-object ((item game-object) name ticks)
  (funcall (draw-function item) item name ticks))

(defmethod shooting-start ((item ship))
  (setf *shooting* t))

(defmethod shooting-end ((item ship))
  (setf *shooting* nil))



;; Instances

(index-instance 'ship1
                (make-instance 'ship
                               :draw-function (function draw-ship)))
(index-instance 'sine1
                (make-instance 'sine
                               :draw-function (function draw-sine)
                               :start-tick 0))

(index-instance 'sine2
                (make-instance 'sine
                               :draw-function (function draw-sine)
                               :start-tick 100))


;; Sketch

(defsketch reverb
    ((title "Reverb")
     (width 900)
     (height 500)
     (ticks 0)
     (color 0))
  
  (incf ticks)
  (incf color 0.0001)
  
  (background (rgb color color color))
  
  (print *instances*)
  
  (loop for k being each hash-key of *instances*
        do (progn
             (if (gethash k *instances*)
                 (funcall
                  (draw-function (gethash k *instances*))
                  (gethash k *instances*)
                  k
                  ticks)))))

;; (let ((instances *instances*) (bullets *bullets*))
  
;;   )

;; Events

(defmethod kit.sdl2:textinput-event ((window reverb) ts text)
  (with-slots (running) window
    (setf running (not running))))

(defmethod kit.sdl2:close-window ((window reverb))
  (makunbound 'running-window)
  (call-next-method))

(defmethod kit.sdl2:keyboard-event ((window reverb) state timestamp repeat key)
  (when (eql state :keydown)
    (format t "keyboard: ~a, ~a, ~a) " key state (type-of key))))

(defmethod kit.sdl2:mousebutton-event ((window reverb) state timestamp button x y)
  (when (eql state :mousebuttondown)
    (shooting-start (gethash 'ship1 *instances*)))
  (when (eql state :mousebuttonup)
    (shooting-end (gethash 'ship1 *instances*))))

(defmethod kit.sdl2:mousemotion-event ((window reverb) timestamp mask x y xrel yrel)
  (setf (x (gethash 'ship1 *instances*)) x)
  (setf (y (gethash 'ship1 *instances*)) y)
  ;; (format t "mouse: ~D, ~D " x y)
  )


;; Window

(unless (boundp 'running-window)
  (setf running-window (make-instance 'reverb)))
