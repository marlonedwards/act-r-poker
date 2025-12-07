;;;
;;; CARD FORMAT: RankSuit (ex. AS, KH, TD, 2C)
;;;   Ranks: A K Q J T 9 8 7 6 5 4 3 2
;;;   Suits: H = Hearts, D = Diamonds, C = Clubs, S = Spades

;; Load the model from same directory
(defparameter *poker-model-path* 
  (merge-pathnames "poker-model.lisp" 
                   (make-pathname :directory (pathname-directory *load-truename*))))

(load-act-r-model *poker-model-path*)

;; Card deck
(defparameter *ranks* '("A" "K" "Q" "J" "T" "9" "8" "7" "6" "5" "4" "3" "2"))
(defparameter *suits* '("H" "D" "C" "S"))

;; Table layout pixel coordinates
(defparameter *layout*
  '(;; Opponent cards (top center)
    (:opponent-card1 :x 250 :y 80)
    (:opponent-card2 :x 350 :y 80)
    
    ;; Board cards (middle row)
    (:discard :x 150 :y 220)
    (:flop1 :x 250 :y 220)
    (:flop2 :x 330 :y 220)
    (:flop3 :x 410 :y 220)
    (:turn :x 490 :y 220)
    (:river :x 570 :y 220)
    
    ;; Player cards (bottom)
    (:player-card1 :x 250 :y 360)
    (:player-card2 :x 350 :y 360)))

(defvar *poker-window* nil)
; Track what's been added
(defvar *window-cards* nil)  

(defun create-poker-window ()
  "Create poker table window"
  (setf *poker-window* (open-exp-window "Poker Table" 
                                         :visible t
                                         :width 750 
                                         :height 480
                                         :x 50 
                                         :y 50))
  (setf *window-cards* nil)

  (add-text-to-exp-window *poker-window* "Opponent:" 
                          :x 280 :y 50 :font-size 14 :color 'gray)
  
  ;; Board section  
  (add-text-to-exp-window *poker-window* "Board:" 
                          :x 150 :y 190 :font-size 14 :color 'gray)
  (add-text-to-exp-window *poker-window* "|" 
                          :x 220 :y 215 :font-size 24 :color 'gray)
  (add-text-to-exp-window *poker-window* "|" 
                          :x 640 :y 215 :font-size 24 :color 'gray)
  
  ;; Player section
  (add-text-to-exp-window *poker-window* "You:" 
                          :x 290 :y 330 :font-size 14 :color 'gray)
  
  *poker-window*)

(defun get-position (location)
  "Get x,y coordinates for a location"
  (let ((pos (rest (assoc location *layout*))))
    (values (getf pos :x) (getf pos :y))))

(defun random-card ()
  "Generate random card"
  (format nil "~a~a" 
          (nth (random (length *ranks*)) *ranks*)
          (nth (random (length *suits*)) *suits*)))

(defun add-card-delayed (card-text x y)
  "Helper function for scheduled card addition"
  (add-text-to-exp-window *poker-window* card-text
                          :x x :y y
                          :width 60 :height 40
                          :font-size 32 :color 'black))

(defun add-card-to-window (location &optional (text nil) (delay 0))
  "Add a card to the window at specified location"
  (let ((card-text (or text (random-card))))
    (multiple-value-bind (x y) (get-position location)
      (if (> delay 0)
          ;; Schedule for later
          (schedule-event-relative delay 'add-card-delayed
            :params (list card-text x y)
            :module 'poker
            :output 'medium)
        ;; Add immediately
        (add-text-to-exp-window *poker-window* card-text
                                :x x :y y
                                :width 60 :height 40
                                :font-size 32 :color 'black))
      (push (list location card-text) *window-cards*)
      card-text)))

(defun add-placeholder (location)
  (multiple-value-bind (x y) (get-position location)
    (add-text-to-exp-window *poker-window* "XX"
                            :x x :y y
                            :width 60 :height 40
                            :font-size 32 :color 'dark-gray)))

(defun setup-table ()
  "Setup poker table with placeholders"
  (create-poker-window)
  
  ;; Add placeholders for hidden opponent cards
  (add-placeholder :opponent-card1)
  (add-placeholder :opponent-card2)
  
  ;; Add placeholder for discard
  (add-placeholder :discard)
  
  ;; Board cards start hidden
  (add-placeholder :flop1)
  (add-placeholder :flop2)
  (add-placeholder :flop3)
  (add-placeholder :turn)
  (add-placeholder :river))

(defun poker-trial ()
  "One poker trial - deal 2 cards to player"
  (reset)
  
  (unless (current-model)
    (error "No model loaded!"))
  
  (format t "~%=== POKER TRIAL ===~%")
  
  (setup-table)
  
  ;; Install for model
  (install-device *poker-window*)
  
  ;; Deal player cards - immediately (can change in the future)
  (format t "Dealing player cards~%")
  (let ((card1 (add-card-to-window :player-card1 nil 0))
        (card2 (add-card-to-window :player-card2 nil 0)))
    
    (format t "Cards: ~a ~a~%" card1 card2)
    
    ;; Run model
    (run 5)
    
    (format t "~%Trial complete: ~a and ~a~%" card1 card2)
    (list card1 card2)))

(defun poker-experiment (&optional (n-trials 1))
  "Run multiple trials of the poker task"
  (format t "~%Running ~d trial(s) of poker...~%~%" n-trials)
  (let ((results nil))
    (dotimes (i n-trials)
      (format t "~%========== TRIAL ~d ==========~%" (1+ i))
      (push (poker-trial) results)
      (sleep 2))  
    (reverse results)))

(defun test-layout ()
  "Test the table layout with sample cards"
  (format t "~%Testing poker table layout~%")
  (setup-table)
  
  (add-card-to-window :player-card1 "AS")
  (add-card-to-window :player-card2 "KH")
  (add-card-to-window :flop1 "QD")
  (add-card-to-window :flop2 "JC")
  (add-card-to-window :flop3 "TS")
  
  (format t "Table layout displayed with sample cards~%")
  (format t "Player: AS KH~%")
  (format t "Board: QD JC TS~%"))

;; Quick commands
(defun pt () (poker-trial))
(defun tl () (test-layout))

(format t "~%=================================~%")
(format t "Poker Table Experiment Loaded!~%")
(format t "=================================~%")
(format t "Commands:~%")
(format t "  (test-layout) or (tl)  - See table layout~%")
(format t "  (poker-trial) or (pt)  - Run trial~%")
(format t "  (poker-experiment 5)   - Run 5 trials~%")
(format t "~%Try (test-layout) first to see the poker table~%~%")
