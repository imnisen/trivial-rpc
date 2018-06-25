(ql:quickload '(:usocket :bordeaux-threads :flexi-streams))


(defmacro p-r (x)
  (let ((g (gensym)))
    `(let ((,g ,x))
       (format t "~& ------- p-r begin --------- ~&")
       (format t "~& FORM: ~&")
       (format t "~& ~a: ~&" ',x)
       (format t "~& VALUE: ~&")
       (format t "~& ~a: ~&" ,g)
       (format t "~& ------- p-r end --------- ~&")
       ,g)))

(defvar data-max-length 65507)



(defclass stub ()
  ((transport :initarg :transport
              :initform (error "No transport given"))))

(defmethod call ((stub stub) method &rest args)
  (let ((transport (slot-value stub 'transport)))
    (send-packet transport (cons method args))
    (lambda () (recv-packet transport))))


;; (defmacro define-rpc-class (classname parents &rest classdef)
;;   `(prog1 (defclass ,classname ,parents ,@classdef)))


;; (defmacro define-rpc-method (method ((self class) &rest args)
;;                              &body body)
;;   (let ((argnames (loop for x in args
;;                         if (listp x) collect (car x) else
;;                         unless (find x lambda-list-keywords) collect x)))
;;     `(prog1 (defmethod ,method ((,self ,class) ,@args) ,@body))))





;; (define-rpc-class (file-server fs-stub) ()
;;   ((filename :initarg :filename
;;              :initform (error "No filename given"))))


;; (define-rpc-method read-file ((fs file-server fs-stub) read-length &key)
;;   (with-slots (filename) fs
;;     (with-open-file (fd filename :direction :input)
;;       (let ((buffer (make-array read-length
;;                                 :initial-element #\Nul
;;                                 :element-type 'character)))
;;         (cons (read-sequence buffer fd) buffer)))))



(defclass transport () ())
(defclass transport-client (transport) ())
(defclass transport-server (transport) ())

(defclass transport-serialized (transport) ())

;; (defmethod send-packet :around ((transport transport-serialized)
;;                                 packet
;;                                 &key)
;;   (format t "~a enter send-packet :around ~&" (tag transport))
;;   (format t "packet is : ~a ~&" packet)
;;   (call-next-method transport
;;                     (write-to-string packet
;;                                      :array t :base 10 :case :downcase :circle t
;;                                      :escape t :gensym t :length nil :level nil
;;                                      :lines nil :pretty nil :radix nil :readably nil))
;;   )

;; (defmethod recv-packet :around ((transport transport-serialized) &key)
;;   (format t "~a enter recv-packet :around ~&" (tag transport))
;;   (multiple-value-bind (msg host port) (call-next-method)
;;     (list (first (multiple-value-list (read-from-string msg))) host port))
;;   )




(defclass transport-udp (transport-serialized)
  ((host :reader :host
         :initarg :host
         :initform (error "No host given"))
   (port :reader :port
         :initarg :port
         :initform (error "No port given"))
   (socket :initform nil)
   (tag :initarg :tag
        :reader tag)))

(defclass transport-udp-server (transport-server transport-udp)
  (class-def :reader :class-def
             :initarg :class-def
             :initform (error "No class-def bind")))

(defclass transport-udp-client (transport-client transport-udp) ())



(defmethod initialize-instance :after ((transport transport-udp-server)
                                       &key)
  (with-slots (socket host port) transport
    (setf socket (usocket:socket-connect nil nil
                                         :protocol :datagram
                                         :local-host host
                                         :local-port port))))

(defmethod initialize-instance :after ((transport transport-udp-client)
                                       &key)
  (with-slots (socket host port) transport
    (setf socket (usocket:socket-connect "127.0.0.1" port
                                         :protocol :datagram))))

;;  (abort nil)
(defmethod transport-close ((transport transport-udp) &key)
  (with-slots (socket) transport
    (usocket:socket-close socket)))

;; ------------------------------------------------------------
;; server
;; ------------------------------------------------------------
(defmethod server-send-packet ((transport transport-udp-server) packet host port &key)
  (let ((buffer  (flexi-streams:string-to-octets packet :external-format :utf-8)))
    (format t "~a sending data ~&" (tag transport))
    (usocket:socket-send (slot-value transport 'socket) buffer (length buffer)
                         :host host
                         :port port))
  )

(defmethod server-recv-packet ((transport transport-udp-server) &key)
  (let ((msg))
    (format t "~a recving data ~&" (tag transport))
    (multiple-value-bind (return-buffer return-length remote-host remote-port)
        (usocket:socket-receive (slot-value transport 'socket) nil data-max-length)
      (setf msg (flexi-streams:octets-to-string (subseq return-buffer 0 return-length) :external-format :utf-8))
      (format t "Data: ~a ~&" msg)
      (list msg remote-host remote-port)
      )))


(defmethod server-send-packet :around ((transport transport-udp-server) packet host port  &key)
  (format t "~a enter send-packet :around ~&" (tag transport))
  (format t "packet is : ~a ~&" packet)
  (call-next-method transport
                    (write-to-string packet
                                     :array t :base 10 :case :downcase :circle t
                                     :escape t :gensym t :length nil :level nil
                                     :lines nil :pretty nil :radix nil :readably nil)
                    host port)
  )

(defmethod server-recv-packet :around ((transport transport-udp-server) &key)
  (format t "~a enter recv-packet :around ~&" (tag transport))
  (destructuring-bind (msg host port) (call-next-method)
    (list (first (multiple-value-list (read-from-string msg))) host port))
  )


;; ------------------------------------------------------------
;; client
;; ------------------------------------------------------------



(defmethod send-packet :around ((transport transport-serialized) packet &key)
  (format t "~a enter send-packet :around ~&" (tag transport))
  (format t "packet is : ~a ~&" packet)
  (call-next-method transport
                    (write-to-string packet
                                     :array t :base 10 :case :downcase :circle t
                                     :escape t :gensym t :length nil :level nil
                                     :lines nil :pretty nil :radix nil :readably nil))
  )

(defmethod send-packet ((transport transport-udp-client) packet &key)
  (let ((buffer (flexi-streams:string-to-octets packet :external-format :utf-8)))
    (format t "~a sending data ~&" (tag transport))
    (usocket:socket-send (slot-value transport 'socket) buffer (length buffer)))
  )



(defmethod recv-packet ((transport transport-udp-client) &key)
  (let ((msg))
    (format t "~a recving data ~&" (tag transport))
    (multiple-value-bind (return-buffer return-length)
        (usocket:socket-receive (slot-value transport 'socket) nil data-max-length)
      (setf msg (flexi-streams:octets-to-string (subseq return-buffer 0 return-length) :external-format :utf-8))
      (format t "Data: ~a ~&" msg)
      msg
      )))


(defmethod recv-packet :around ((transport transport-serialized) &key)
  (format t "~a enter recv-packet :around ~&" (tag transport))
  (first (multiple-value-list (read-from-string (call-next-method))))
  )


(defmethod run-rpc-server ((transport transport-udp-server) object &key)
  (loop for (request . addr) = (server-recv-packet transport) do
        (if (eql (car request) :shutdown)
            (return (send-packet transport nil ))
            (progn
              (format t "Loop once, trying to send packet from server~&")
              (server-send-packet transport
                                  (handler-case
                                      (apply (car request) object (cdr request))
                                    (condition (c) c))
                                  (first addr)
                                  (second addr)
                                  )))))

(defmethod start-rpc-server ((transport transport-udp-server) object &key)
  (bt:make-thread #'(lambda () (run-rpc-server transport object))
                  :name (format nil "RPC/UDP Server for ~S" object)))

;; (defmethod start-rpc-server ((transport transport-server-zeromq) object &key)
;;   (run-rpc-server transport object))



