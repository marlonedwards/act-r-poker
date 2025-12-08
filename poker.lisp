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

; remaining cards in deck
(defvar *deck* nil)  

(defun make-deck ()
  "Create a fresh shuffled deck"
  (let ((cards nil))
    (dolist (r *ranks*)
      (dolist (s *suits*)
        (push (format nil "~a~a" r s) cards)))
    (setf *deck* (shuffle-list cards))))

(defun shuffle-list (lst)
  "Fisher-Yates shuffle"
  (let ((vec (coerce lst 'vector)))
    (loop for i from (1- (length vec)) downto 1
          do (rotatef (aref vec i) (aref vec (random (1+ i)))))
    (coerce vec 'list)))

(defun deal-card ()
  "Deal one card from deck"
  (if *deck*
      (pop *deck*)
      (progn
        (format t "~%WARNING: Deck empty, reshuffling~%")
        (make-deck)
        (pop *deck*))))

;; Table layout pixel coordinates
(defparameter *layout*
  '(;; Opponent cards (top center)
    (:opponent-card1 :x 250 :y 80)
    (:opponent-card2 :x 350 :y 80)

    ;; Board cards (middle row) - y=220
    (:flop1 :x 250 :y 220)
    (:flop2 :x 330 :y 220)
    (:flop3 :x 410 :y 220)
    (:turn :x 490 :y 220)
    (:river :x 570 :y 220)

    ;; Player cards (bottom) - y=360
    (:player-card1 :x 250 :y 360)
    (:player-card2 :x 350 :y 360)))

(defvar *poker-window* nil)
(defvar *window-cards* nil)
(defvar *game-stage* nil)  ; preflop, flop, turn, river, done
(defvar *dealt-cards* nil) ; store all dealt cards

(defun create-poker-window ()
  "Create poker table window"
  (setf *poker-window* (open-exp-window "Poker Table" 
                                         :visible t
                                         :width 750 
                                         :height 480
                                         :x 50 
                                         :y 50))
  (setf *window-cards* nil)

  ;; Minimal labels - keep far from card search areas
  ;; Player cards at y=360, board cards at y=220
  ;; No labels in those zones to avoid visual confusion
  
  *poker-window*)

(defun get-position (location)
  "Get x,y coordinates for a location"
  (let ((pos (rest (assoc location *layout*))))
    (values (getf pos :x) (getf pos :y))))

(defun random-card ()
  "Deal a card from the deck (no duplicates)"
  (deal-card))

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

(defun add-action-labels ()
  "No visual labels - keeps display clean for model"
  nil)

(defun handle-keypress (model key)
  "Handle keyboard input - advance game stage"
  (declare (ignore model))
  (format t "~%>>> Key pressed: ~a (stage: ~a)~%" key *game-stage*)
  (cond
    ;; Call at any stage
    ((string-equal key "c")
     (cond
       ((eq *game-stage* 'preflop)
        (setf *game-stage* 'flop)
        (format t "Revealing flop...~%")
        (reveal-flop))
       ((eq *game-stage* 'flop)
        (setf *game-stage* 'turn)
        (format t "Revealing turn...~%")
        (reveal-turn))
       ((eq *game-stage* 'turn)
        (setf *game-stage* 'river)
        (format t "Revealing river...~%")
        (reveal-river))
       ((eq *game-stage* 'river)
        (setf *game-stage* 'done)
        (format t "Hand complete!~%"))))
    ;; Fold at any time
    ((string-equal key "f")
     (setf *game-stage* 'done)
     (format t "Player folded.~%"))
    ;; Raise - for now treat same as call
    ((string-equal key "r")
     (format t "Raise! (treating as call for now)~%")
     (handle-keypress model "c"))))

(defun reveal-flop ()
  "Reveal the 3 flop cards"
  (let ((f1 (add-card-to-window :flop1))
        (f2 (add-card-to-window :flop2))
        (f3 (add-card-to-window :flop3)))
    (push (list 'flop f1 f2 f3) *dealt-cards*)
    (format t "Flop: ~a ~a ~a~%" f1 f2 f3)))

(defun reveal-turn ()
  "Reveal the turn card"
  (let ((t1 (add-card-to-window :turn)))
    (push (list 'turn t1) *dealt-cards*)
    (format t "Turn: ~a~%" t1)))

(defun reveal-river ()
  "Reveal the river card"
  (let ((r1 (add-card-to-window :river)))
    (push (list 'river r1) *dealt-cards*)
    (format t "River: ~a~%" r1)))

(defun setup-table ()
  "Setup poker table with placeholders and buttons"
  (create-poker-window)
  (setf *game-stage* 'preflop)
  (setf *dealt-cards* nil)
  (make-deck)  ; fresh shuffled deck

  ;; Add placeholders for hidden opponent cards
  (add-placeholder :opponent-card1)
  (add-placeholder :opponent-card2)

  ;; Board cards: NO placeholders - they appear when revealed
  ;; This prevents model from finding "XX" instead of real cards

  ;; Add key labels
  (add-action-labels))

(defun poker-trial ()
  "One poker trial - full hand with flop/turn/river"
  (reset)

  (unless (current-model)
    (error "No model loaded!"))

  (format t "~%=== POKER TRIAL ===~%")

  (setup-table)

  ;; Deal player cards BEFORE installing device
  (format t "Dealing player cards~%")
  (let ((card1 (add-card-to-window :player-card1 nil 0))
        (card2 (add-card-to-window :player-card2 nil 0)))
    (push (list 'hole card1 card2) *dealt-cards*)
    (format t "Hole cards: ~a ~a~%" card1 card2)

    ;; NOW install window for model (so it sees the cards)
    (install-device *poker-window*)

    ;; Register keypress handler
    (add-act-r-command "poker-key-handler" 'handle-keypress
                       "Handle poker keypresses")
    (monitor-act-r-command "output-key" "poker-key-handler")

    ;; Run model until game is done
    (run 30)

    (format t "~%=== HAND COMPLETE ===~%")
    (format t "Final stage: ~a~%" *game-stage*)
    (format t "All cards dealt: ~a~%" (reverse *dealt-cards*))
    *dealt-cards*))

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
