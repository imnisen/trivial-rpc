;; loggic
;; (define-rpc-class server () ())

(defclass server () ())
(defmethod my-sum ((s server) a b)
  (+ a b))

;; server
(setf my-server (make-instance 'server))
(setf my-tps (make-instance 'transport-udp-server
                            :host "127.0.0.1"
                            :port 9555
                            :tag :server))

(start-rpc-server my-tps my-server)


;;;;;;;;
(defmacro make-server ()
  (let ((class-name (gensym)))
    `(progn
       (defclass ,class-name () ())
       (make-instance 'transport-udp-server
                      :tag :server
                      :class-def ,class-name))))

(defmacro expose (server rpc-method-name lambda-fun-form)
  `(defmethod (string-to-symbol ,rpc-method-name) ((self ,(class-def server)) ,@(second lambda-fun-form))
     ,@(third lambda-fun-form)))

(defun)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; server
(defvar *server* (jsonrpc:make-server))
(jsonrpc:expose *server* "sum" (lambda (args) (reduce #'+ args)))

(jsonrpc:server-listen *server* :port 50879 :mode :tcp)





;; client
(setf my-tpc (make-instance 'transport-udp-client
                            :host "127.0.0.1"
                            :port 9555
                            :tag :client))
(setf my-stub (make-instance 'stub :transport my-tpc))

(setf temp (call my-stub 'my-sum 4 3))

(funcall temp)








;; client
(defvar *client* (jsonrpc:make-client))
(jsonrpc:client-connect *client* :url "http://127.0.0.1:50879" :mode :tcp)
(jsonrpc:call *client* "sum" '(10 20))
                                        ;=> 30

;; Calling with :timeout option
(jsonrpc:call *client* "sum" '(10 20) :timeout 1.0)
                                        ;=> 30








;; Now we can test our asynchronous RPC framework based on threads, for the first we initialize all required objects:

;; CL-USER> (setf fs (make-instance 'file-server :filename "test.txt"))
;; #<a FILE-SERVER>

;; CL-USER> (setf tps (make-instance 'transport-mt-server))
;; #<a TRANSPORT-MT-SERVER>

;; CL-USER> (setf tpc (make-instance 'transport-mt-client :server tps))
;; #<a TRANSPORT-MT-CLIENT>

;; CL-USER> (setf stub (make-instance 'fs-stub :transport tpc))
;; #<a FS-STUB>

;; The usage is simple: start the server and use the stub object instead of the real object.
;; CL-USER> (start-rpc-server tps fs)
;; #<process "RPC/MT Server for #<a FILE-SERVER>">

;; CL-USER> (setf temp (read-file stub 4))
;; #<bytecompiled-closure #<bytecompiled-function 000000000236ba34>>

;; CL-USER> (funcall temp)
;; (4 . "text")


;; 


;; ;; server
;; (defvar *server* (jsonrpc:make-server))
;; (jsonrpc:expose *server* "sum" (lambda (args) (reduce #'+ args)))

;; (jsonrpc:server-listen *server* :port 50879 :mode :tcp)
;; ;; client
;; (defvar *client* (jsonrpc:make-client))
;; (jsonrpc:client-connect *client* :url "http://127.0.0.1:50879" :mode :tcp)
;; (jsonrpc:call *client* "sum" '(10 20))
;;                                         ;=> 30

;; ;; Calling with :timeout option
;; (jsonrpc:call *client* "sum" '(10 20) :timeout 1.0)
;;                                         ;=> 30
;; 
