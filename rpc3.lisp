(setf fs (make-instance 'file-server :filename "test.txt"))

(setf tps (make-instance 'transport-server-zeromq
                         :url "tcp://127.0.0.1:5557"
                         :tag :server))

(setf tpc (make-instance 'transport-client-zeromq
                         :url "tcp://127.0.0.1:5557"
                         :tag :client))

(setf stub (make-instance 'fs-stub :transport tpc))


(start-rpc-server tps fs)

(setf temp (read-file stub 4))

(funcall temp)


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