(lambda (f y0 dt)
  (let ((y '*unassigned*) (dy '*unassigned*))
    (let ((a (integral (delay dy) y0 dt)) (b (stream-map f y)))
      (set! y a)
      (set! dy b))
    y))

;; The reason this new version would not work is because stream-map will evaluate the car in the stream like normal. However, the variable y which is passed as the stream
;; to stream-map is still unassigned, so an error will occur.

; ---

(lambda (f y0 dt)
  (let ((y '*unassigned*)
	(dy '*unassigned*))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))

;; The previous version will work because by the time stream-map runs, y will have it's first element in it's stream.
