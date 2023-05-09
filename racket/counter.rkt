#lang racket/base

(require racket/gui)

; Run with:
;   racket ./counter.rkt
;
; I have 3 problems with this implementation
;
;   1. I couldn't find a way to align items nicely (see "hacky" comment)
;   2. I couldn't find a way to vertically "center" the input box's text value
;   3. Even though I disabled the text field, it does not show as grayed out.

(define frame (new frame% [label "Counter Example"] [width 400] [height 300]))

(define counter 0)
(define v_panel (new vertical-panel% [parent frame]))
(define h_panel (new horizontal-panel% [parent v_panel] [stretchable-width #f]))
(define txt
  (new text-field%
       [parent h_panel]
       [label ""]
       [init-value (~v counter)]
       [enabled #f]
       [min-width 150]
       [min-height 32] ;hacky!
       ))

(new button%
     [parent h_panel]
     [label "Counter"]
     [callback
      (lambda (_button _event)
        (set! counter (add1 counter))
        (send txt set-value (~v counter)))])

(send frame show #t)

; How would I print a message on exit?
(println "I have initialized the UI!")
