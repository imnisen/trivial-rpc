;; server
(defvar *server* (make-server :host "127.0.0.1" :port 50879))

(expose *server* "sum" (lambda (a b) (reduce #'+ (list a b))))
(expose *server* "mul" (lambda (args) (reduce #'* args)))
(expose *server* "sub" (lambda (&rest args) (reduce #'- args)))

(server-listen *server* )



;; client

(defvar *client* (make-client))

(client-connect *client* :host "127.0.0.1" :port 50879)

(call *client* "sum" '(10 20))
(call *client* "mul" '((10 20 30)))
(call *client* "sub" '(10 20 30) :timeout 10)

(client-stop *client*)
;; add timeout
