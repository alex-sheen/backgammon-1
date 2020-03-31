#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
(require (only-in typed/racket/gui/base put-file get-file))
(require typed/test-engine/racket-tests)

;; === data definitions
(define-type Player (U 'Black 'White))

(define-struct OccupiedPoint
  ([color : Player]
   [count : Integer]))

(define-type Point (U OccupiedPoint 'EmptyPoint))

(define-struct Board
  ([points : (Listof Point)]
   [black-bar : Integer]
   [white-bar : Integer]
   [black-off : Integer]
   [white-off : Integer]))

(define-struct Style
  ([checker-radius : Integer]
   [spacing : Integer]
   [black-checker : (Integer -> Image)]
   [white-checker : (Integer -> Image)]
   [dark-point : (Integer Boolean -> Image)]
   [light-point : (Integer Boolean -> Image)]
   [background : (Integer Integer -> Image)]
   [label : (String Integer -> Image)]
   [black-die : (Integer Integer -> Image)]
   [white-die : (Integer Integer -> Image)]))

(define-struct Game
  ([board : Board]
   [turn : Player]
   [moves : (Listof Integer)]))

(define-struct World
  ([game : Game]
   [style : Style]
   [first-white-die : Integer]
   [second-white-die : Integer]
   [first-black-die : Integer]
   [second-black-die : Integer]
   [first-click : BoardLoc]
   [history : (Listof Game)]))

(define-struct PointNum
  ([num : Integer]))

(define-type ClickLoc (U PointNum 'BlackBar 'WhiteBar 'BlackOff 'WhiteOff
                         'BlackDice 'WhiteDice 'Nowhere))

(define-type BoardLoc (U PointNum 'BlackBar 'WhiteBar 'BlackOff 'WhiteOff
                         'Nowhere))

(define none : Image-Color (color 255 255 255 0))

;; === general helper functions
(: player=? : Player Player -> Boolean)
;; player=?: return #t if players are the same, otherwise #f
;; parameter "one": the first player for comparison
;; parameter "two": the second player for comparison
;; output: #t if the players are the same, else #f
(define (player=? one two)
  (local
    {(define string1 : String (symbol->string one))
     (define string2 : String (symbol->string two))}
    (string=? string1 string2)))
(check-expect (player=? 'Black 'Black) #t)
(check-expect (player=? 'White 'White) #t)
(check-expect (player=? 'Black 'White) #f)
(check-expect (player=? 'White 'Black) #f)

(: list-max : (Listof Integer) -> Integer)
;; list-max: return the maximum value in a list of integers
;; parameter "xs": the list of integers
;; output: the maximum value in the list
(define (list-max xs)
  (match xs
    ['() (error "list-max: no maximum in an empty list")]
    [(cons hd '()) hd]
    [(cons hd tl) (if (= (max hd (list-max tl)) hd) hd (list-max tl))]))
(check-expect (list-max (list 1)) 1)
(check-expect (list-max (list 1 3 2)) 3)
(check-expect (list-max (list 2 2 2)) 2)

(: list-min : (Listof Integer) -> Integer)
;; list-min: return the minimum value in a list of integers
;; parameter "xs": the list of integers
;; output: the minimum value in the list
(define (list-min xs)
  (match xs
    ['() (error "list-min: no minimum in an empty list")]
    [(cons hd '()) hd]
    [(cons hd tl) (if (= (min hd (list-min tl)) hd) hd (list-min tl))]))
(check-expect (list-min (list 1)) 1)
(check-expect (list-min (list 2 1 3)) 1)
(check-expect (list-min (list 2 2 2)) 2)

(: replace-at : All (A) Integer A (Listof A) -> (Listof A))
;; replace-at: replace the list element at the given index with a given value
;; parameter "i": the list index
;; parameter "x": the new value
;; parameter "xs": the list
;; output: the list with the replaced value
(define (replace-at i x xs)
  (match xs
    ['() '()]
    [(cons hd tl)
     (if (= i 0) (cons x (replace-at (- i 1) x tl))
         (cons hd (replace-at (- i 1) x tl)))]))
(check-expect (replace-at 0 'Z '(a b c)) '(Z b c))
(check-expect (replace-at 1 'Z '(a b c)) '(a Z c))

(: sublist (All (A) (-> (Listof A) Integer Integer (Listof A))))
;; sublist: return a sublist of the original list given start and end indices
;; parameter "xs": the original list
;; parameter "start": the starting index for the sublist
;; parameter "end": the ending index for the sublist
;; output: the sublist
(define (sublist xs start end)
  (local
    {(: with-i : Integer -> (Listof A))
     (define (with-i i)
       (if (<= i end) (cons (list-ref xs i) (with-i (+ i 1))) '()))}
    (with-i start)))
(check-expect (sublist (list "a" "b" "c" "d" "e") 1 3) (list "b" "c" "d"))
(check-expect (sublist (list 1 2 3 4 5 6 7 8) 0 3) (list 1 2 3 4))

(: string->integer : String -> Integer)
;; string->integer: convert a string representation of an integer to an integer
;; parameter "s": the string representation of an integer
;; output: the integer part of the resulting number only; raise an error if the
;;   string is not a number (function intended to be used with only integers)
(define (string->integer s)
  (local
    {(define conv : (U Complex False) (string->number s))}
    (if (complex? conv) (exact-round (real-part conv))
        (error "string->integer: invalid integer"))))

(: remove-int : Integer (Listof Integer) -> (Listof Integer))
;; remove-int: remove a given integer from a list of integers
;; parameter "i": the integer to be removed from the list
;; parameter "is": the list of integers
;; output: the list of integers with the given value removed
(define (remove-int i is)
  (match is
    ['() (error "remove-int: given integer is not in list")]
    [(cons hd tl) (if (= hd i) tl (cons hd (remove-int i tl)))]))
(check-expect (remove-int 1 (list 1 2 3 4)) (list 2 3 4))
(check-expect (remove-int 1 (list 2 3 1 1 4)) (list 2 3 1 4))

(: remove-max-int : Integer (Listof Integer) -> (Listof Integer))
;; remove-max-int: remove, from a list of integers, the maximum of the values
;;   greater than the given integer
;; parameter "i": the given integer
;; parameter "is": the list of integers
;; output: the list of integers with the appropriate value removed
(define (remove-max-int i is)
  (local
    {(define l : (Listof Integer) (filter (lambda ([m : Integer]) (> m i)) is))}
    (if (= 1 (length l)) (remove-int (first l) is)
        (remove-int (list-max l) is))))
(check-expect (remove-max-int 2 (list 1 6)) (list 1))
(check-expect (remove-max-int 2 (list 3 4)) (list 3))

(: align-top (-> (Listof Image) Image))
;; align-top: call 'beside/align "top"' on all images in a list of images
;; parameter "imgs": the list of images
;; output: the composite image result of having called 'beside/align "top"'
(define (align-top imgs)
  (match imgs
    ['() empty-image]
    [(cons hd tl) (beside/align "top" hd (align-top tl))]))

(: align-bottom (-> (Listof Image) Image))
;; align-bottom: call 'beside/align "bottom"' on all images in a list of images
;; parameter "imgs": the list of images
;; output: the composite image result of having called 'beside/align "bottom"'
(define (align-bottom imgs)
  (match imgs
    ['() empty-image]
    [(cons hd tl) (beside/align "bottom" hd (align-bottom tl))]))

;; === styling the backgammon board
(: checker : Image-Color Image-Color -> (Integer -> Image))
;; checker: return a checker image function, given two colors
;; parameter "c1": the main checker color
;; parameter "c2": the checker highlight color
;; output: a checker image function that takes argument radius
(define (checker c1 c2)
  (lambda ([r : Integer])
    (local
      {(define r1 : Real (* 0.675 r))
       (define r2 : Real (* 0.8 r))}
      (overlay
       (circle r1 "solid" c1) (circle r2 "solid" c2) (circle r "solid" c1)))))

(: die : Image-Color Image-Color -> (Integer Integer -> Image))
;; die: return a die image function, given two colors
;; parameter "c1": the die color
;; parameter "c2": the pip color
;; output: a die image function that takes arguments checker radius and pip
;;   number
(define (die c1 c2)
  (lambda ([r : Integer] [n : Integer])
    (local
      {(define s : Integer (* 2 r))
       (define bg : Image (square s "solid" c1))
       (define w : Real (/ s 5))
       (define a : Real (* -0.5 w))
       (define b : Real (- (* 1.5 w) s))
       (define m : Real (* 0.5 (- w s)))
       (define pip : Image (circle (/ w 2) "solid" c2))
       (define one : Image (overlay pip bg))
       (define two : Image (overlay/xy pip a b (overlay/xy pip b a bg)))
       (define three : Image (overlay pip two))
       (define four : Image (overlay/xy pip b b (overlay/xy pip a a two)))
       (define five : Image (overlay pip four))
       (define six : Image (overlay/xy pip m a (overlay/xy pip m b four)))}
      (cond
        [(= n 0) (square s "solid" none)]
        [(= n 1) one]
        [(= n 2) two]
        [(= n 3) three]
        [(= n 4) four]
        [(= n 5) five]
        [(= n 6) six]
        [else (error "die: invalid die number")]))))

(: point : Image-Color -> (Integer Boolean -> Image))
;; point: return a point image function
;; parameter "c": the point color
;; output: a point image function that takes arguments checker radius and point
;;   direction
(define (point c)
  (lambda ([r : Integer] [point-up? : Boolean])
    (local
      {(define s : Real (sqrt (+ (sqr r) (* 100 (sqr r)))))
       (define a : Real (* 2 (radians->degrees (atan (/ 1 10)))))
       (define tri : Image (isosceles-triangle s a "solid" c))}
      (rotate (if point-up? 0 180) tri))))

(: frame : Image-Color Image-Color -> (Integer Integer -> Image))
;; frame: return a board background image function
;; parameter "c1": the main background color
;; parameter "c2": the background border color
;; output: a background image function that takes arguments checker radius and
;;   point spacing
(define (frame c1 c2)
  (lambda ([r : Integer] [s : Integer])
    (local
      {(define w : Real (+ (* 14 s) (* 26 r)))
       (define h : Real (* 25 r))
       (define scene : Image (rectangle w h "solid" c1))
       (define pocket : Image (rectangle (* 2 r) (* 10 r) "solid" c1))
       (define break : Image (rectangle (* 2 r) (* 5 r) "solid" c2))
       (define borne : Image
         (overlay (above pocket break pocket) (rectangle (* 3 r) h "solid" c2)))
       (define bar : Image (rectangle (* 2 r) h "solid" c2))
       (define border : Image (rectangle (+ w (* 6 r)) (+ h r) "solid" c2))}
      (overlay bar (beside borne scene borne) border))))

(: marker : Image-Color -> (String Integer -> Image))
;; marker: return a checker label image function
;; parameter "c": the color of the label text
;; output: a checker labeling function that takes arguments label text and
;;   checker radius
(define (marker c)
  (lambda ([s : String] [r : Integer])
    (if (byte? r) (text s r c) (text s 255 c))))

;; === original style
(define black-checker : (Integer -> Image)
  (checker 'steelblue (color 105 155 196)))

(define white-checker : (Integer -> Image)
  (checker 'indianred (color 214 122 122)))

(define dark-point : (Integer Boolean -> Image) (point 'darkred))

(define light-point : (Integer Boolean -> Image) (point 'lightblue))

(define background : (Integer Integer -> Image) (frame 'tan 'darkred))

(define label : (String Integer -> Image) (marker 'white))

(define black-die : (Integer Integer -> Image) (die 'steelblue 'white))

(define white-die : (Integer Integer -> Image) (die 'indianred 'white))

;; === original style struct
(define original : Style
  (Style 25 20 black-checker white-checker dark-point light-point background
         label black-die white-die))

;; === classic style
(define c-black-checker : (Integer -> Image)
  (checker 'chocolate (color 228 132 63)))

(define c-white-checker : (Integer -> Image)
  (checker 'moccasin (color 255 247 232)))

(define c-dark-point : (Integer Boolean -> Image) (point 'chocolate))

(define c-light-point : (Integer Boolean -> Image) (point 'moccasin))

(define c-background : (Integer Integer -> Image)
  (frame (color 63 16 16) (color 43 11 11)))

(define c-label : (String Integer -> Image) (marker 'black))

(define c-black-die : (Integer Integer -> Image) (die 'chocolate 'white))

(define c-white-die : (Integer Integer -> Image) (die 'moccasin 'black))

;; === classic style struct
(define classic : Style
  (Style 30 10 c-black-checker c-white-checker c-dark-point c-light-point
         c-background c-label c-black-die c-white-die))

;; === drawing the backgammon board
(: draw-board : Style Board -> Image)
;; draw-board: return image of backgammon board, given style and board structs
;; parameter "s": the board style
;; parameter "b": the board struct
;; output: a backgammon board image
(define (draw-board s b)
  (match* (s b)
    [((Style r s b-c w-c d-pt l-pt bg la _ _)
      (Board pts b-bar w-bar b-off w-off))
     (local
       {(: six-points (-> Boolean (Listof Image)))
        (define (six-points point-up?)
          (local
            {(define p : Image (square s "solid" none))
             (define t-dark : Image (d-pt r #t))
             (define f-dark : Image (d-pt r #f))
             (define t-light : Image (l-pt r #t))
             (define f-light : Image (l-pt r #f))}
            (if point-up?
                (append
                 (build-list 3 (lambda ([i : Integer])
                                 (beside p t-light p t-dark))) (list p))
                (append
                 (build-list 3 (lambda ([i : Integer])
                                 (beside p f-dark p f-light))) (list p)))))
        (: draw-checker : Player -> Image)
        (define (draw-checker p) (if (player=? p 'Black) (b-c r) (w-c r)))
        (: stack : Point -> Image)
        (define (stack p)
          (match p
            ['EmptyPoint (circle r "solid" none)]
            [(OccupiedPoint c n)
             (cond
               [(<= n 5)
                (foldr above empty-image
                       (build-list n (lambda ([i : Integer])
                                       (draw-checker c))))]
               [else
                (overlay (la (number->string n) r)
                         (foldr above empty-image
                                (build-list 5 (lambda ([i : Integer])
                                                (draw-checker c)))))])]))
        (: int-stack : Player Integer -> Image)
        (define (int-stack p n)
          (cond
            [(= n 0) (circle r "solid" none)]
            [(<= n 5)
             (foldr above empty-image (build-list n (lambda ([i : Integer])
                                                      (draw-checker p))))]
            [else
             (overlay (la (number->string n) r)
                      (foldr above empty-image
                             (build-list 5 (lambda ([i : Integer])
                                             (draw-checker p)))))]))
        (: space : (Listof Image) -> (Listof Image))
        (define (space imgs)
          (match imgs
            ['() '()]
            [(cons hd tl) (cons (square s "solid" none) (cons hd (space tl)))]))
        (define stacks : (Listof Image) (space (map stack pts)))
        (define t-stack : Image
          (align-top (list (align-top (sublist stacks 24 35))
                           (square (+ s (* 2 r)) "solid" none)
                           (align-top (sublist stacks 36 47)))))
        (define b-stack : Image
          (align-bottom (list (align-bottom (reverse (sublist stacks 12 23)))
                              (square (+ s (* 2 r)) "solid" none)
                              (align-bottom (reverse (sublist stacks 0 11))))))
        (define t-points : Image (foldr beside empty-image (six-points #t)))
        (define f-points : Image (foldr beside empty-image (six-points #f)))
        (define all-points : Image
          (above
           (beside f-points (square (* 2 r) "solid" none) f-points)
           (square (* 5 r) "solid" none)
           (beside t-points (square (* 2 r) "solid" none) t-points)))
        (define t-x : Real (/ (image-width t-stack) 2))
        (define t-y : Real (/ (image-height t-stack) 2))
        (define b-x : Real (/ (image-width b-stack) 2))
        (define b-y : Real (/ (image-height b-stack) 2))
        (define w : Real (image-width (bg r s)))
        (define h : Real (image-height (bg r s)))
        (define b-bar-stack : Image (int-stack 'Black b-bar))
        (define w-bar-stack : Image (int-stack 'White w-bar))
        (define b-off-stack : Image (int-stack 'Black b-off))
        (define w-off-stack : Image (int-stack 'White w-off))}
       (place-image
        b-bar-stack
        (+ (* 7 s) (* 16 r)) (- h (* 0.5 r) (/ (image-height b-bar-stack) 2))
        (place-image
         w-bar-stack
         (+ (* 7 s) (* 16 r)) (+ (* 0.5 r) (/ (image-height w-bar-stack) 2))
         (place-image
          t-stack
          (+ t-x (* 3 r)) (+ t-y (* 0.5 r))
          (place-image
           b-stack
           (- w (* 3 r) b-x) (- h (* 0.5 r) b-y)
           (place-image
            w-off-stack
            (- w (* 1.5 r)) (- h (* 0.5 r) (/ (image-height w-off-stack) 2))
            (place-image
             b-off-stack
             (- w (* 1.5 r)) (+ (* 0.5 r) (/ (image-height b-off-stack) 2))
             (overlay all-points (bg r s)))))))))]))

;; === sample backgammon boards
(define initial-board
  (Board (append (list (OccupiedPoint 'Black 2))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 5)
                       'EmptyPoint
                       (OccupiedPoint 'White 3))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 5)
                       (OccupiedPoint 'White 5))
                 (make-list 3 'EmptyPoint)
                 (list (OccupiedPoint 'Black 3)
                       'EmptyPoint
                       (OccupiedPoint 'Black 5))
                 (make-list 4 'EmptyPoint)
                 (list (OccupiedPoint 'White 2)))
         0 0 0 0))

(define test-board
  (Board (build-list 24
                     (lambda ([i : Integer])
                       (OccupiedPoint (if (even? i) 'Black 'White) (add1 i))))
         2 26 25 4))

(define white-home
  (Board (append (build-list 4 (lambda ([i : Integer])
                                 (OccupiedPoint 'White (add1 i))))
                 (list (OccupiedPoint 'White 4) (OccupiedPoint 'White 1))
                 (make-list 18 'EmptyPoint))
         0 0 0 0))

(define black-home
  (Board (append (make-list 19 'EmptyPoint)
                 (build-list 4 (lambda ([i : Integer])
                                 (OccupiedPoint 'Black (+ i 2))))
                 (list (OccupiedPoint 'White 1)))
         0 0 1 0))

;; === universe support & game play
(: roll : Integer -> Integer)
;; roll: return sum of one and built-in 'random' function
;; parameter "max": upper-bound for built-in function 'random'
;; output: sum of one and result of built-in 'random' function
(define (roll max)
  (+ (random max) 1))

(: click-where : Style Integer Integer -> ClickLoc)
;; click-where: return aspect of backgammon board clicked on
;; parameter "style": the style of the backgammon board
;; parameter "x": the x-coordinate of the click
;; parameter "y": the y-coordinate of the click
;; output: a ClickLoc that represents the aspect of the backgammon board clicked
(define (click-where style x y)
  (match style
    [(Style r s _ _ _ _ _ _ _ _)
     (local
       {(define a : Integer (- x (* 3 r)))
        (define b : Integer (- x (* 17 r) (* 7 s)))
        (define c : Integer (+ s (* 2 r)))
        (define left : Integer
          (if (> (remainder a c) s) (exact-ceiling (/ a c)) 0))
        (define right : Integer
          (if (> (remainder b c) s) (exact-ceiling (/ b c)) 0))}
       (cond
         [(< (* 0.5 r) y (* 10.5 r))
          (cond
            [(< (+ (* 3 r) s) x (+ (* 15 r) (* 7 s)))
             (if (> left 0) (PointNum (+ 12 left)) 'Nowhere)]
            [(< (+ (* 15 r) (* 7 s)) x (+ (* 17 r) (* 7 s))) 'WhiteBar]
            [(< (+ (* 17 r) (* 7 s)) x (+ (* 29 r) (* 14 s)))
             (if (> right 0) (PointNum (+ 18 right)) 'Nowhere)]
            [(< (+ (* 29.5 r) (* 14 s)) x (+ (* 31.5 r) (* 14 s))) 'BlackOff]
            [else 'Nowhere])]
         [(< (* 10.5 r) y (* 15.5 r))
          (cond
            [(< (+ (* 7 r) (* 2.5 s)) x (+ (* 11 r) (* 4.5 s))) 'WhiteDice]
            [(< (+ (* 21 r) (* 9.5 s)) x (+ (* 25 r) (* 11.5 s))) 'BlackDice]
            [else 'Nowhere])]
         [(< (* 15.5 r) y (* 25.5 r))
          (cond
            [(< (+ (* 3 r) s) x (+ (* 15 r) (* 7 s)))
             (if (> left 0) (PointNum (- 13 left)) 'Nowhere)]
            [(< (+ (* 15 r) (* 7 s)) x (+ (* 17 r) (* 7 s))) 'BlackBar]
            [(< (+ (* 17 r) (* 7 s)) x (+ (* 29 r) (* 14 s)))
             (if (> right 0) (PointNum (- 7 right)) 'Nowhere)]
            [(< (+ (* 29.5 r) (* 14 s)) x (+ (* 31.5 r) (* 14 s))) 'WhiteOff]
            [else 'Nowhere])]
         [else 'Nowhere]))]))
(check-expect (click-where original 38 38) 'Nowhere)
(check-expect (click-where original 120 38) (PointNum 13))
(check-expect (click-where original 190 38) (PointNum 14))
(check-expect (click-where original 260 38) (PointNum 15))
(check-expect (click-where original 330 38) (PointNum 16))
(check-expect (click-where original 400 38) (PointNum 17))
(check-expect (click-where original 470 38) (PointNum 18))
(check-expect (click-where original 540 38) 'WhiteBar)
(check-expect (click-where original 610 38) (PointNum 19))
(check-expect (click-where original 680 38) (PointNum 20))
(check-expect (click-where original 750 38) (PointNum 21))
(check-expect (click-where original 820 38) (PointNum 22))
(check-expect (click-where original 890 38) (PointNum 23))
(check-expect (click-where original 960 38) (PointNum 24))
(check-expect (click-where original 1043 38) 'BlackOff)
(check-expect (click-where original 120 625) (PointNum 12))
(check-expect (click-where original 190 625) (PointNum 11))
(check-expect (click-where original 260 625) (PointNum 10))
(check-expect (click-where original 330 625) (PointNum 9))
(check-expect (click-where original 400 625) (PointNum 8))
(check-expect (click-where original 470 625) (PointNum 7))
(check-expect (click-where original 540 625) 'BlackBar)
(check-expect (click-where original 610 625) (PointNum 6))
(check-expect (click-where original 680 625) (PointNum 5))
(check-expect (click-where original 750 625) (PointNum 4))
(check-expect (click-where original 820 625) (PointNum 3))
(check-expect (click-where original 890 625) (PointNum 2))
(check-expect (click-where original 960 625) (PointNum 1))
(check-expect (click-where original 1043 625) 'WhiteOff)

(: distance : BoardLoc BoardLoc -> Integer)
;; distance: return the die roll needed to move from one location to another
;; parameter "loc1": the origin
;; parameter "loc2": the destination
;; output: the die roll needed to move from "loc1" to "loc2"
(define (distance loc1 loc2)
  (match* (loc1 loc2)
    [((PointNum p) (PointNum q)) (- q p)]
    [((PointNum p) 'BlackOff) (- 25 p)]
    [((PointNum p) 'WhiteOff) (- p)]
    [('BlackBar (PointNum p)) p]
    [('WhiteBar (PointNum p)) (- p 25)]
    [(_ _) (error "distance: not applicable")]))
(check-expect (distance (PointNum 24) (PointNum 19)) -5)
(check-expect (distance (PointNum 19) 'BlackOff) 6)
(check-expect (distance (PointNum 6) 'WhiteOff) -6)
(check-expect (distance 'BlackBar (PointNum 3)) 3)
(check-expect (distance 'WhiteBar (PointNum 21)) -4)

(: home? : Game -> Boolean)
;; home?: return #t if the player whose turn it is has all checkers in the home
;;   quadrant, including any borne off checkers, otherwise #f
;; parameter "game": the current game state
;; output: #t if the player has all checkers in the home quadrant, otherwise #f
(define (home? game)
  (match game
    [(Game (Board pts _ _ b-off w-off) turn _)
     (local
       {(: point->integer : Point -> Integer)
        (define (point->integer pt)
          (match pt
            ['EmptyPoint 0]
            [(OccupiedPoint c n) (if (player=? c turn) n 0)]))
        (define l : (Listof Point)
          (if (player=? turn 'Black) (sublist pts 18 23) (sublist pts 0 5)))
        (define off : Integer (if (player=? turn 'Black) b-off w-off))}
       (= 15 (+ (foldr + 0 (map point->integer l)) off)))]))
(check-expect (home? (Game white-home 'White '())) #t)
(check-expect (home? (Game black-home 'Black '())) #t)
(check-expect (home? (Game initial-board 'White '())) #f)
(check-expect (home? (Game test-board 'Black '())) #f)

(: dice-match? : Game BoardLoc BoardLoc -> Boolean)
;; dice-match?: return #t if a player's proposed move is permitted by their dice
;; parameter "game": the current game state
;; parameter "loc1": the origin location of the move
;; parameter "loc2": the destination location of the move
;; output: #t if the player's proposed move is permitted, otherwise #f
(define (dice-match? game loc1 loc2)
  (match game
    [(Game (Board pts _ _ _ _) turn moves)
     (local
       {(define dist : Integer (distance loc1 loc2))
        (define l : (Listof Point) (if (player=? turn 'Black)
                                       (sublist pts 18 23)
                                       (reverse (sublist pts 0 5))))
        (: max-point : Integer (Listof Point) -> Integer)
        (define (max-point i lpt)
          (match lpt
            ['() (error "max-point: no maximum point")]
            [(cons hd tl)
             (match hd
               ['EmptyPoint (max-point (- i 1) tl)]
               [(OccupiedPoint c _)
                (if (player=? c turn) i (max-point (- i 1) tl))])]))}
       (or (if (player=? turn 'Black)
               (and (positive? dist)
                    (ormap (lambda ([i : Integer]) (= i (abs dist))) moves))
               (and (negative? dist)
                    (ormap (lambda ([i : Integer]) (= i (abs dist))) moves)))
           (match* (loc1 loc2)
             [(_ 'BlackOff)
              (and (ormap (lambda ([i : Integer]) (> i (max-point 6 l))) moves)
                   (= (abs dist) (max-point 6 l)))]
             [(_ 'WhiteOff)
              (and (ormap (lambda ([i : Integer]) (> i (max-point 6 l))) moves)
                   (= (abs dist) (max-point 6 l)))]
             [(_ _) #f])))]))
;; separate check-expects not written for dice-match?, as it is central to
;;   legal-move?

(: legal-move? : Game BoardLoc BoardLoc -> Boolean)
;; legal-move?: return #t if the proposed move is legal, otherwise #f
;; parameter "game": the current game state
;; parameter "loc1": the origin location of the move
;; parameter "loc2": the destination location of the move
;; output: #t if the proposed move is legal, otherwise #f
(define (legal-move? game loc1 loc2)
  (match game
    [(Game (Board pts b-bar w-bar _ _) turn _)
     (if (or (and (player=? turn 'Black) (> b-bar 0))
             (and (player=? turn 'White) (> w-bar 0)))
         (if (player=? turn 'Black)
             (match* (loc1 loc2)
               [('BlackBar (PointNum p))
                (match (list-ref pts (- p 1))
                  [(OccupiedPoint 'White 1) (dice-match? game loc1 loc2)]
                  [(OccupiedPoint 'White _) #f]
                  [_ (dice-match? game loc1 loc2)])]
               [(_ _) #f])
             (match* (loc1 loc2)
               [('WhiteBar (PointNum p))
                (match (list-ref pts (- p 1))
                  [(OccupiedPoint 'Black 1) (dice-match? game loc1 loc2)]
                  [(OccupiedPoint 'Black _) #f]
                  [_ (dice-match? game loc1 loc2)])]
               [(_ _) #f]))
         (match* (loc1 loc2)
           [((PointNum p) (PointNum q))
            (match* ((list-ref pts (- p 1)) (list-ref pts (- q 1)))
              [('EmptyPoint _) #f]
              [((OccupiedPoint c _) 'EmptyPoint)
               (and (player=? c turn) (dice-match? game loc1 loc2))]
              [((OccupiedPoint c1 _) (OccupiedPoint c2 n2))
               (and (player=? c1 turn)
                    (if (player=? c1 c2) (dice-match? game loc1 loc2)
                        (and (= n2 1) (dice-match? game loc1 loc2))))])]
           [((PointNum p) 'BlackOff)
            (and (and (player=? turn 'Black) (home? game))
                 (match (list-ref pts (- p 1))
                   [(OccupiedPoint 'Black _) (dice-match? game loc1 loc2)]
                   [_ #f]))]
           [((PointNum p) 'WhiteOff)
            (and (and (player=? turn 'White) (home? game))
                 (match (list-ref pts (- p 1))
                   [(OccupiedPoint 'White _) (dice-match? game loc1 loc2)]
                   [_ #f]))]
           [(_ _) #f]))]))
(check-expect (legal-move? (Game initial-board 'Black (list 2 4)) (PointNum 1)
                           (PointNum 3)) #t)
(check-expect (legal-move? (Game initial-board 'Black (list 3 4)) (PointNum 1)
                           (PointNum 3)) #f)
(check-expect (legal-move? (Game initial-board 'Black (list 2 4)) (PointNum 12)
                           (PointNum 10)) #f)
(check-expect (legal-move? (Game initial-board 'Black (list 1 4)) (PointNum 12)
                           (PointNum 13)) #f)
(check-expect (legal-move? (Game initial-board 'White (list 2 4)) (PointNum 24)
                           (PointNum 22)) #t)
(check-expect (legal-move? (Game initial-board 'White (list 3 4)) (PointNum 24)
                           (PointNum 22)) #f)
(check-expect (legal-move? (Game initial-board 'White (list 2 4)) (PointNum 13)
                           (PointNum 15)) #f)
(check-expect (legal-move? (Game initial-board 'White (list 1 4)) (PointNum 13)
                           (PointNum 12)) #f)
(check-expect (legal-move? (Game initial-board 'Black (list 1 6)) (PointNum 19)
                           'BlackOff) #f)
(check-expect (legal-move? (Game initial-board 'White (list 1 6)) (PointNum 6)
                           'WhiteOff) #f)
(check-expect (legal-move? (Game white-home 'White (list 1 6)) (PointNum 6)
                           'WhiteOff) #t)
(check-expect (legal-move? (Game black-home 'Black (list 1 5)) (PointNum 20)
                           'BlackOff) #t)
(check-expect (legal-move? (Game black-home 'Black (list 1 4)) (PointNum 20)
                           (PointNum 24)) #t)
(check-expect (legal-move? (Game test-board 'Black (list 3 4)) 'BlackBar
                           (PointNum 3)) #t)
(check-expect (legal-move? (Game test-board 'Black (list 2 4)) (PointNum 1)
                           (PointNum 3)) #f)
(check-expect (legal-move? (Game black-home 'Black (list 1 6)) (PointNum 20)
                           'BlackOff) #t)
(check-expect (legal-move? (Game black-home 'Black (list 1 2)) (PointNum 20)
                           'BlackOff) #f)
(check-expect (legal-move? (Game initial-board 'White '()) (PointNum 24)
                           (PointNum 20)) #f)

(: apply-move : Game BoardLoc BoardLoc -> Game)
;; apply-move: move checker from the proposed origin location to the proposed
;;   destination location on the board, if the move is legal. otherwise, leave
;;   the board unchanged.
;; parameter "game": the current game state
;; parameter "loc1": the origin location of the move
;; parameter "loc2": the destination location of the move
;; output: the game state with the move applied, if it is legal; otherwise an
;;   error is raised
(define (apply-move game loc1 loc2)
  (if (legal-move? game loc1 loc2)
      (match game
        [(Game (Board pts b-bar w-bar b-off w-off) turn moves)
         (local
           {(define dist : Integer (abs (distance loc1 loc2)))
            (define moves-2 : (Listof Integer)
              (if (ormap (lambda ([i : Integer]) (= i dist)) moves)
                  (remove-int dist moves)
                  (remove-max-int dist moves)))
            (: pts-2 : Integer -> (Listof Point))
            (define (pts-2 i)
              (match (list-ref pts (- i 1))
                [(OccupiedPoint _ 1) (replace-at (- i 1) 'EmptyPoint pts)]
                [(OccupiedPoint c n)
                 (replace-at (- i 1) (OccupiedPoint c (- n 1)) pts)]
                [_ pts]))}
           (match* (loc1 loc2)
             [((PointNum p) (PointNum q))
              (match (list-ref pts (- q 1))
                ['EmptyPoint
                 (Game
                  (Board (replace-at (- q 1) (OccupiedPoint turn 1) (pts-2 p))
                         b-bar w-bar b-off w-off) turn moves-2)]
                [(OccupiedPoint c n)
                 (if (player=? c turn)
                     (Game
                      (Board
                       (replace-at (- q 1) (OccupiedPoint c (+ n 1)) (pts-2 p))
                       b-bar w-bar b-off w-off) turn moves-2)
                     (if (player=? c 'Black)
                         (Game
                          (Board
                           (replace-at (- q 1) (OccupiedPoint turn 1) (pts-2 p))
                           (+ b-bar 1) w-bar b-off w-off) turn moves-2)
                         (Game
                          (Board
                           (replace-at (- q 1) (OccupiedPoint turn 1) (pts-2 p))
                           b-bar (+ w-bar 1) b-off w-off) turn moves-2)))])]
             [((PointNum p) 'BlackOff)
              (Game
               (Board (pts-2 p) b-bar w-bar (+ b-off 1) w-off) turn moves-2)]
             [((PointNum p) 'WhiteOff)
              (Game
               (Board (pts-2 p) b-bar w-bar b-off (+ w-off 1)) turn moves-2)]
             [('BlackBar (PointNum p))
              (match (list-ref pts (- p 1))
                ['EmptyPoint
                 (Game (Board (replace-at (- p 1) (OccupiedPoint turn 1) pts)
                              (- b-bar 1) w-bar b-off w-off) turn moves-2)]
                [(OccupiedPoint c n)
                 (if (player=? c turn)
                     (Game
                      (Board (replace-at (- p 1) (OccupiedPoint c (+ n 1)) pts)
                             (- b-bar 1) w-bar b-off w-off) turn moves-2)
                     (Game
                      (Board (replace-at (- p 1) (OccupiedPoint turn 1) pts)
                             (- b-bar 1) (+ w-bar 1) b-off w-off) turn
                                                                  moves-2))])]
             [('WhiteBar (PointNum p))
              (match (list-ref pts (- p 1))
                ['EmptyPoint
                 (Game (Board (replace-at (- p 1) (OccupiedPoint turn 1) pts)
                              b-bar (- w-bar 1) b-off w-off) turn moves-2)]
                [(OccupiedPoint c n)
                 (if (player=? c turn)
                     (Game
                      (Board (replace-at (- p 1) (OccupiedPoint c (+ n 1)) pts)
                             b-bar (- w-bar 1) b-off w-off) turn moves-2)
                     (Game
                      (Board (replace-at (- p 1) (OccupiedPoint turn 1) pts)
                             (+ b-bar 1) (- w-bar 1) b-off w-off)
                      turn moves-2))])]))])
      (error "apply-move: illegal move")))

(: available-moves? : Game -> Boolean)
;; available-moves?: return #t if it is possible for the player whose turn it is
;;   to make a legal move, otherwise #f
;; parameter "game": the current game state
;; output: #t if it is possible for the player to make a move, otherwise #f
(define (available-moves? game)
  (match game
    [(Game (Board pts b-bar w-bar _ _) turn _)
     (local
       {(: start-point? : Point -> Boolean)
        (define (start-point? pt)
          (match pt
            [(OccupiedPoint c _) (player=? c turn)]
            [_ #f]))
        (: start-points : (Listof Point) Integer -> (Listof PointNum))
        (define (start-points lpt i)
          (match lpt
            ['() '()]
            [(cons hd tl)
             (if (start-point? hd)
                 (cons (PointNum (+ i 1)) (start-points tl (+ i 1)))
                 (start-points tl (+ i 1)))]))
        (define start-pts : (Listof PointNum) (start-points pts 0))
        (define starts : (Listof BoardLoc)
          (if (player=? turn 'Black)
              (if (> b-bar 0) (append start-pts (list 'BlackBar)) start-pts)
              (if (> w-bar 0) (append start-pts (list 'WhiteBar)) start-pts)))
        (: end-point? : Point -> Boolean)
        (define (end-point? pt)
          (match pt
            ['EmptyPoint #t]
            [(OccupiedPoint c n) (or (player=? c turn) (= n 1))]))
        (: end-points : (Listof Point) Integer -> (Listof PointNum))
        (define (end-points lpt i)
          (match lpt
            ['() '()]
            [(cons hd tl)
             (if (end-point? hd)
                 (cons (PointNum (+ i 1)) (end-points tl (+ i 1)))
                 (end-points tl (+ i 1)))]))
        (define end-pts : (Listof PointNum) (end-points pts 0))
        (define ends : (Listof BoardLoc)
          (if (player=? turn 'Black)
              (if (home? game) (append end-pts (list 'BlackOff)) end-pts)
              (if (home? game) (append end-pts (list 'WhiteOff)) end-pts)))
        (: helper : (Listof BoardLoc) (Listof BoardLoc) -> Boolean)
        (define (helper sts ens)
          (ormap (lambda ([st : BoardLoc])
                   (ormap (lambda ([en : BoardLoc])
                            (legal-move? game st en)) ens)) sts))}
       (helper starts ends))]))
(check-expect (available-moves? (Game initial-board 'Black (list 2 4))) #t)
(check-expect (available-moves? (Game
                                 (Board
                                  (list
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 1)
                                   (OccupiedPoint 'White 1)
                                   (OccupiedPoint 'White 7)
                                   'EmptyPoint
                                   (OccupiedPoint 'White 3)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 2)
                                   (OccupiedPoint 'White 1)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 2)
                                   (OccupiedPoint 'Black 5)
                                   (OccupiedPoint 'Black 3)
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 4)
                                   'EmptyPoint)
                                  0
                                  1
                                  0
                                  0)
                                 'White
                                 '(5 2))) #f)

(: game-over? : Game -> Boolean)
;; game-over?: return #t if the game is over, otherwise #f
;; parameter "game": the current game state
;; output: #t if the game is over, otherwise #f
(define (game-over? game)
  (match game
    [(Game (Board _ _ _ b-off w-off) _ _) (or (= b-off 15) (= w-off 15))]))
(check-expect (game-over? (Game initial-board 'Black (list 2 4))) #f)
(check-expect (game-over? (Game (Board (make-list 24 'EmptyPoint) 0 0 15 0)
                                'Black '())) #t)

(: winner : Game -> Player)
;; winner: return the player that has won the game
;; parameter "game": the current game state
;; output: the winner of the game; if the game is not over, an error is raised
(define (winner game)
  (if (game-over? game)
      (if (= (Board-black-off (Game-board game)) 15) 'Black 'White)
      (error "winner: game is not over")))
(check-expect (winner (Game (Board (make-list 24 'EmptyPoint) 0 0 15 0)
                                'Black '())) 'Black)
(check-expect (winner (Game (Board (make-list 24 'EmptyPoint) 0 0 0 15)
                                'White '())) 'White)

;; === universe
(: draw : World -> Image)
;; draw: draw the backgammon world
;; parameter "w": the World struct
;; output: an image of the backgammon world
(define (draw w)
  (match w
    [(World game (Style r s _ _ _ _ _ _ b-d w-d) w1 w2 b1 b2 cl _)
     (local
       {(define board : Image (draw-board (World-style w) (Game-board game)))
        (define font : Integer (* 4 r))
        (define fin : Image
          (if (game-over? game)
              (text (string-append (string-upcase
                                    (symbol->string (winner game))) " " "WINS!")
                    (if (byte? font) font 255) 'white) empty-image))
        (define w-dice : Image
          (beside (w-d r w1) (square (/ r 2) "solid" none) (w-d r w2)))
        (define b-dice : Image
          (beside (b-d r b1) (square (/ r 2) "solid" none) (b-d r b2)))
        (define w-dice-x : Real
          (if (game-over? game) (* 5.25 r) (+ (* 9 r) (* 3.5 s))))
        (define b-dice-x : Real
          (if (game-over? game)
              (- (image-width board) (* 5.25 r)) (+ (* 23 r) (* 10.5 s))))
        (define highlight : Image
          (rectangle (* 2 r) (* 10 r) "outline" 'white))
        (define x : Integer
          (match cl
            [(PointNum p)
             (cond
               [(< p 7) (+ (* (+ (* -2 p) 30) r) (* (- 14 p) s))]
               [(< p 13) (+ (* (+ (* -2 p) 28) r) (* (- 13 p) s))]
               [(< p 19) (+ (* (- (* 2 p) 22) r) (* (- p 12) s))]
               [else (+ (* (- (* 2 p) 20) r) (* (- p 11) s))])]
            ['BlackBar (+ (* 7 s) (* 16 r))]
            ['WhiteBar (+ (* 7 s) (* 16 r))]
            [_ 0]))
        (define y : Real
          (match cl
            [(PointNum p) (if (< p 13) (* 20.5 r) (* 5.5 r))]
            ['BlackBar (* 20.5 r)]
            ['WhiteBar (* 5.5 r)]
            [_ 0]))
        (define wrld : Image
          (overlay
           fin
           (place-image
            w-dice
            w-dice-x (/ (image-height board) 2)
            (place-image
             b-dice
             b-dice-x (/ (image-height board) 2) board))))}
       (if (and (> x 0) (> y 0)) (place-image highlight x y wrld) wrld))]))

(: react-to-mouse : World Integer Integer Mouse-Event -> World)
;; react-to-mouse: manifest appropriate reactions in the backgammon world,
;;   according to given click location(s)
;; parameter "w": the World struct
;; parameter "x": the x-coordinate of the click
;; parameter "y": the y-coordinate of the click
;; parameter "e": the click event type
;; output: the updated World struct, given the effect of the user click
(define (react-to-mouse w x y e)
  (if (game-over? (World-game w)) w
      (match* (w e)
        [((World (Game board turn _) style w1 w2 b1 b2 cl h) "button-down")
         (local
           {(define loc : ClickLoc (click-where style x y))
            (define game : Game (World-game w))}
           (match* (cl loc)
             [('Nowhere 'WhiteDice)
              (if (and (player=? turn 'Black) (not (available-moves? game)))
                  (local
                    {(define roll1 : Integer (roll 6))
                     (define roll2 : Integer (roll 6))
                     (define moves-2 : (Listof Integer)
                       (if (= roll1 roll2)
                           (make-list 4 roll1) (list roll1 roll2)))}
                    (World (Game board 'White moves-2)
                           style roll1 roll2 b1 b2 'Nowhere (cons game h))) w)]
             [('Nowhere 'BlackDice)
              (if (and (player=? turn 'White) (not (available-moves? game)))
                  (local
                    {(define roll1 : Integer (roll 6))
                     (define roll2 : Integer (roll 6))
                     (define moves-2 : (Listof Integer)
                       (if (= roll1 roll2)
                           (make-list 4 roll1) (list roll1 roll2)))}
                    (World (Game board 'Black moves-2)
                           style w1 w2 roll1 roll2 'Nowhere (cons game h))) w)]
             [('Nowhere 'WhiteBar)
              (if (and (player=? turn 'White) (> (Board-white-bar board) 0))
                  (World game style w1 w2 b1 b2 'WhiteBar h) w)]
             [('Nowhere 'BlackBar)
              (if (and (player=? turn 'Black) (> (Board-black-bar board) 0))
                  (World game style w1 w2 b1 b2 'BlackBar h) w)]
             [('Nowhere (PointNum p))
              (match (list-ref (Board-points board) (- p 1))
                [(OccupiedPoint c _)
                 (if (player=? c turn)
                     (World game style w1 w2 b1 b2 (PointNum p) h) w)]
                [_ w])]
             [('WhiteBar 'WhiteBar) (World game style w1 w2 b1 b2 'Nowhere h)]
             [('WhiteBar (PointNum p))
              (if (legal-move? game cl loc)
                  (World (apply-move game cl loc) style w1 w2 b1 b2 'Nowhere
                         (cons game h)) w)]
             [('BlackBar 'BlackBar) (World game style w1 w2 b1 b2 'Nowhere h)]
             [('BlackBar (PointNum p))
              (if (legal-move? game cl loc)
                  (World (apply-move game cl loc) style w1 w2 b1 b2 'Nowhere
                         (cons game h)) w)]
             [((PointNum p) 'WhiteOff)
              (if (legal-move? game cl loc)
                  (World (apply-move game cl loc) style w1 w2 b1 b2 'Nowhere
                         (cons game h)) w)]
             [((PointNum p) 'BlackOff)
              (if (legal-move? game cl loc)
                  (World (apply-move game cl loc) style w1 w2 b1 b2 'Nowhere
                         (cons game h)) w)]
             [((PointNum p) (PointNum p))
              (World game style w1 w2 b1 b2 'Nowhere h)]
             [((PointNum p) (PointNum q))
              (if (legal-move? game cl loc)
                  (World (apply-move game cl loc) style w1 w2 b1 b2 'Nowhere
                         (cons game h)) w)]
             [(_ _) w]))]
        [(_ _) w])))

(: key : World String -> World)
;; key: manifest appropriate reactions in the backgammon world, according to
;;   the given key press
;; parameter "w": the World struct
;; parameter "k": the key pressed
;; output: the updated World struct, given the effect of the user key press
(define (key w k)
  (match k
    ["u" (undo w)]
    ["s" (begin (save-game! w) w)]
    ["l" (load-game (World-style w))]
    [_ w]))

(: run : Style -> World)
;; run: run the backgammon game
;; parameter "style": board style, as specified by a Style struct
;; output: the interactive world program
(define (run style)
  (local
    {(: initial-roll : Integer -> (Listof Integer))
     (define (initial-roll max)
       (local
         {(define rll : (Listof Integer) (list (roll max) (roll max)))}
         (if (= (first rll) (second rll)) (initial-roll max) rll)))
     (define i-rll : (Listof Integer) (initial-roll 6))
     (define w1 : Integer (first i-rll))
     (define b1 : Integer (second i-rll))
     (define turn : Player (if (> w1 b1) 'White 'Black))}
    (big-bang (World (Game initial-board turn i-rll) style w1 0 b1 0 'Nowhere
                     '()) : World
      [to-draw draw]
      [on-mouse react-to-mouse]
      [on-key key])))

;; === undo
(: infer-dice : (Listof Game) -> (Listof Integer))
;; infer-dice: infer dice values that would be on display in the most recent
;;   game state, given a history of game states
;; parameter "h": a list of previous game states (game history)
;; output: the inferred dice values, in a list
(define (infer-dice h)
  (local
    {(: helper : Player (Listof Game) -> (Listof Integer))
     (define (helper p gs)
       (match gs
         ['() (error "infer-dice: out of scope")]
         [(cons (Game _ turn moves) '())
          (if (player=? turn 'Black)
              (if (player=? p 'White) (list (list-min moves) 0)
                  (list (list-max moves) 0))
              (if (player=? p 'White) (list (list-max moves) 0)
                  (list (list-min moves) 0)))]
         [(cons (Game _ turn moves) tl)
          (if (and (player=? turn p) (> (length moves) 1)) (sublist moves 0 1)
              (helper p tl))]))}
    (append (helper 'White h) (helper 'Black h))))
(check-expect (infer-dice (list (Game
                                 (Board
                                  (list
                                   (OccupiedPoint 'Black 1)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 1)
                                   (OccupiedPoint 'White 6)
                                   (OccupiedPoint 'Black 1)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   (OccupiedPoint 'White 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 3)
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 3))
                                  0
                                  0
                                  0
                                  0)
                                 'Black
                                 '(6 6 6 6))
                                (Game
                                 (Board
                                  (list
                                   (OccupiedPoint 'Black 1)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 1)
                                   (OccupiedPoint 'White 6)
                                   (OccupiedPoint 'Black 1)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   (OccupiedPoint 'White 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 3)
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 3))
                                  0
                                  0
                                  0
                                  0)
                                 'White
                                 '())
                                (Game
                                 (Board
                                  (list
                                   (OccupiedPoint 'Black 1)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 6)
                                   (OccupiedPoint 'Black 1)
                                   (OccupiedPoint 'White 1)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   (OccupiedPoint 'White 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 3)
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 3))
                                  0
                                  0
                                  0
                                  0)
                                 'White
                                 '(3))
                                (Game
                                 (Board
                                  (list
                                   (OccupiedPoint 'Black 1)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 6)
                                   (OccupiedPoint 'Black 1)
                                   (OccupiedPoint 'White 1)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   (OccupiedPoint 'White 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 3)
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 2))
                                  0
                                  1
                                  0
                                  0)
                                 'White
                                 '(1 3))
                                (Game
                                 (Board
                                  (list
                                   (OccupiedPoint 'Black 1)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 6)
                                   (OccupiedPoint 'Black 1)
                                   (OccupiedPoint 'White 1)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   (OccupiedPoint 'White 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 3)
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 2))
                                  0
                                  1
                                  0
                                  0)
                                 'Black
                                 '())
                                (Game
                                 (Board
                                  (list
                                   (OccupiedPoint 'Black 1)
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 1)
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 6)
                                   (OccupiedPoint 'White 1)
                                   (OccupiedPoint 'White 1)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   (OccupiedPoint 'White 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 3)
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 2))
                                  0
                                  0
                                  0
                                  0)
                                 'Black
                                 '(4))
                                (Game
                                 (Board
                                  (list
                                   (OccupiedPoint 'Black 2)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 6)
                                   (OccupiedPoint 'White 1)
                                   (OccupiedPoint 'White 1)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   (OccupiedPoint 'White 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 3)
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 2))
                                  0
                                  0
                                  0
                                  0)
                                 'Black
                                 '(2 4))
                                (Game
                                 (Board
                                  (list
                                   (OccupiedPoint 'Black 2)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 6)
                                   (OccupiedPoint 'White 1)
                                   (OccupiedPoint 'White 1)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   (OccupiedPoint 'White 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 3)
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 2))
                                  0
                                  0
                                  0
                                  0)
                                 'White
                                 '())
                                (Game
                                 (Board
                                  (list
                                   (OccupiedPoint 'Black 2)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 5)
                                   (OccupiedPoint 'White 1)
                                   (OccupiedPoint 'White 2)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   (OccupiedPoint 'White 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 3)
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 2))
                                  0
                                  0
                                  0
                                  0)
                                 'White
                                 '(2))
                                (Game
                                 (Board
                                  (list
                                   (OccupiedPoint 'Black 2)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 5)
                                   'EmptyPoint
                                   (OccupiedPoint 'White 3)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   (OccupiedPoint 'White 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 3)
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 2))
                                  0
                                  0
                                  0
                                  0)
                                 'White
                                 '(2 1)))) (list 1 3 6 6))
(check-expect (infer-dice (list (Game
                                 (Board
                                  (list
                                   (OccupiedPoint 'Black 2)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 5)
                                   'EmptyPoint
                                   (OccupiedPoint 'White 3)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   (OccupiedPoint 'White 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 3)
                                   'EmptyPoint
                                   (OccupiedPoint 'Black 5)
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   'EmptyPoint
                                   (OccupiedPoint 'White 2))
                                  0
                                  0
                                  0
                                  0)
                                 'White
                                 '(2 1)))) (list 2 0 1 0))

(: undo : World -> World)
;; undo: undo a move, unless the world is the starting world (in which case
;;   nothing changes)
;; parameter "w": the current World struct
;; output: the World struct one move back, unless the given world was the
;;   starting world (in which case the world remains unchanged)
(define (undo w)
  (match w
    [(World _ style _ _ _ _ 'Nowhere h)
     (match h
       ['() w]
       [(cons hd tl)
        (local
          {(define dice : (Listof Integer) (infer-dice h))
           (define w1 : Integer (first dice))
           (define w2 : Integer (second dice))
           (define b1 : Integer (third dice))
           (define b2 : Integer (fourth dice))}
          (World hd style w1 w2 b1 b2 'Nowhere tl))])]
    [_ w]))

;; === saving & loading a game
(: point->string : Point -> String)
;; point->string: convert a point into its string representation
;; parameter "pt": the point to be converted
;; output: the point's string representation
(define (point->string pt)
  (match pt
    ['EmptyPoint "_"]
    [(OccupiedPoint 'Black n) (string-append "B" (number->string n))]
    [(OccupiedPoint 'White n) (string-append "W" (number->string n))]))
(check-expect (point->string 'EmptyPoint) "_")
(check-expect (point->string (OccupiedPoint 'Black 5)) "B5")
(check-expect (point->string (OccupiedPoint 'White 2)) "W2")

(: string->point : String -> Point)
;; string->point: convert a string representation of a point into a point
;; parameter "s": the string representation
;; output: the point
(define (string->point s)
  (cond
    [(string-prefix? s "B")
     (OccupiedPoint 'Black (string->integer (substring s 1)))]
    [(string-prefix? s "W")
     (OccupiedPoint 'White (string->integer (substring s 1)))]
    [(string=? s "_") 'EmptyPoint]
    [else (error "string->point: string does not represent a point")]))
(check-expect (string->point "_") 'EmptyPoint)
(check-expect (string->point "B5") (OccupiedPoint 'Black 5))
(check-expect (string->point "W2") (OccupiedPoint 'White 2))

(: points->string : (Listof Point) -> String)
;; points->string: convert a list of points into its string representation
;; parameter "pts": the list of points to be converted
;; output: the point list's string representation
(define (points->string pts)
  (string-trim
   (foldr string-append ""
          (map (lambda ([pt : Point]) (string-append (point->string pt) " "))
               pts))))
(check-expect (points->string (Board-points initial-board))
              "B2 _ _ _ _ W5 _ W3 _ _ _ B5 W5 _ _ _ B3 _ B5 _ _ _ _ W2")

(: string->points : String -> (Listof Point))
;; string->point: convert a string representation of a list of points into a
;;   list of points
;; parameter "s": the string representation
;; output: the list of points
(define (string->points s) (map string->point (string-split s " ")))
(check-expect
 (string->points "B2 _ _ _ _ W5 _ W3 _ _ _ B5 W5 _ _ _ B3 _ B5 _ _ _ _ W2")
 (Board-points initial-board))

(: board->string : Board -> String)
;; board->string: convert a board into its string representation
;; parameter "brd": the board to be converted
;; output: the board's string representation
(define (board->string brd)
  (match brd
    [(Board pts b-bar w-bar b-off w-off)
     (string-append (points->string pts) "|" (number->string b-bar) "|"
                    (number->string w-bar) "|" (number->string b-off) "|"
                    (number->string w-off))]))
(check-expect (board->string initial-board)
              "B2 _ _ _ _ W5 _ W3 _ _ _ B5 W5 _ _ _ B3 _ B5 _ _ _ _ W2|0|0|0|0")

(: string->board : String -> Board)
;; string->board: convert a string representation of a board into a board
;; parameter "s": the string representation
;; output: the board
(define (string->board s)
  (local
    {(define l : (Listof String) (string-split s "|"))
     (define pts : (Listof Point) (string->points (first l)))
     (define is : (Listof Integer) (map string->integer (rest l)))}
    (Board pts (first is) (second is) (third is) (fourth is))))
(check-expect
 (string->board
  "B2 _ _ _ _ W5 _ W3 _ _ _ B5 W5 _ _ _ B3 _ B5 _ _ _ _ W2|0|0|0|0")
 initial-board)

(: game->string : Game -> String)
;; game->string: convert a game into its string representation
;; parameter "g": the game to be converted
;; output: the game's string representation
(define (game->string g)
  (match g
    [(Game brd trn mvs)
     (string-append
      (board->string brd) "@"
      (if (player=? trn 'Black) "B" "W") "@"
      (string-trim
       (foldr string-append ""
              (map (lambda ([m : Integer])
                     (string-append (number->string m) " ")) mvs))))]))
(check-expect
 (game->string (Game initial-board 'White (list 2 6)))
 "B2 _ _ _ _ W5 _ W3 _ _ _ B5 W5 _ _ _ B3 _ B5 _ _ _ _ W2|0|0|0|0@W@2 6")
(check-expect
 (game->string (Game initial-board 'White '()))
 "B2 _ _ _ _ W5 _ W3 _ _ _ B5 W5 _ _ _ B3 _ B5 _ _ _ _ W2|0|0|0|0@W@")

(: string->game : String -> Game)
;; string->game: convert a string representation of a game into a game
;; parameter "s": the string representation
;; output: the game
(define (string->game s)
  (local
    {(define l : (Listof String) (string-split s "@"))
     (define brd : Board (string->board (first l)))
     (define trn : Player (if (string=? (second l) "B") 'Black 'White))
     (define mvs : (Listof Integer)
       (if (> (length l) 2)
           (map string->integer (string-split (third l) " ")) '()))}
    (Game brd trn mvs)))
(check-expect
 (string->game
  "B2 _ _ _ _ W5 _ W3 _ _ _ B5 W5 _ _ _ B3 _ B5 _ _ _ _ W2|0|0|0|0@W@2 6")
 (Game initial-board 'White (list 2 6)))
(check-expect
 (string->game
  "B2 _ _ _ _ W5 _ W3 _ _ _ B5 W5 _ _ _ B3 _ B5 _ _ _ _ W2|0|0|0|0@W@")
 (Game initial-board 'White '()))

(: history->string : (Listof Game) -> String)
;; history->string: convert a history of games into its string representation
;; parameter "gs": the history of games to be converted
;; output: the history's string representation
(define (history->string gs)
  (local
    {(define l : (Listof String) (map game->string gs))
     (: ins : (Listof String) -> (Listof String))
     (define (ins ss)
       (match ss
         ['() '()]
         [(cons _ '()) ss]
         [(cons hd tl) (cons hd (cons "!" (ins tl)))]))}
    (foldr string-append "" (ins l))))

(: string->history : String -> (Listof Game))
;; string->history: convert a string representation of a history of games into a
;;   history of games
;; parameter "s": the string representation
;; output: the history of games
(define (string->history s)
  (local
    {(define l : (Listof String) (string-split s "!"))}
    (map string->game l)))

(: world->string : World -> String)
;; world->string: return a string of the history list of the current world, with
;;   the current game state prepended
;; parameter "w": the current world
;; output: the world's string representation
(define (world->string w)
  (match w [(World g _ _ _ _ _ _ h) (history->string (cons g h))]))

(: string->world : Style String -> World)
;; string->world: use the first game state in the game list as the current game
;;   state. use the rest of the list as game history. infer dice values from the
;;   game list. style according to given style.
(define (string->world style s)
  (local
    {(define l : (Listof Game) (string->history s))
     (define g : Game (first l))
     (define h : (Listof Game) (rest l))
     (define dice : (Listof Integer) (infer-dice l))
     (define w1 : Integer (first dice))
     (define w2 : Integer (second dice))
     (define b1 : Integer (third dice))
     (define b2 : Integer (fourth dice))}
    (World g style w1 w2 b1 b2 'Nowhere h)))

(: save-game! : World -> Void)
;; prompt the user for an output file location
;; then, save the game to that file
;; do nothing if the user cancels
(define (save-game! w)
  (local
    {(define path : (U Path False) (put-file))}
    (if (path? path)
        (begin
          (write-string (world->string w)
                        (open-output-file path))
          (void))
        (void))))

(: load-game : Style -> World)
;; prompt the user to choose a file
;; then load an in-progress game from that file
;; use the provided Style to make a new World
;; raise an error if the user cancels or if something goes wrong
(define (load-game s)
  (local
    {(define path : (U Path False) (get-file))}
    (if (path? path)
        (string->world s (port->string (open-input-file path)))
        (error "load-game: user cancelled"))))

(test)