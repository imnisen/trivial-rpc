;; (ql:quickload '(:pzmq :bordeaux-threads))

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

(defmethod send-packet :around ((transport transport-serialized)
                                packet &key)
  (format t "~a enter send-packet :around ~&" (tag transport))
  (format t "packet is : ~a ~&" packet)
  (call-next-method transport
                    (write-to-string packet
                                     :array t :base 10 :case :downcase :circle t
                                     :escape t :gensym t :length nil :level nil
                                     :lines nil :pretty nil :radix nil :readably nil)))

(defmethod recv-packet :around ((transport transport-serialized) &key)
  (format t "~a enter recv-packet :around ~&" (tag transport))
  (first (multiple-value-list (read-from-string (call-next-method)))))




(defclass transport-zeromq (transport-serialized)
  ((url :reader transport-zeromq-url
        :initarg :url
        :initform (error "No ZeroMQ URL given"))
   (context :initform (pzmq:ctx-new))
   (socket :initform nil)
   (tag :initarg :tag
        :reader tag)))

(defclass transport-client-zeromq (transport-client transport-zeromq) ())
(defclass transport-server-zeromq (transport-server transport-zeromq) ())


(defmethod initialize-instance :after ((transport transport-client-zeromq)
                                       &key)
  (with-slots (context socket url) transport
    (setf socket (pzmq:socket context :req))
    (pzmq:connect socket url)))

(defmethod initialize-instance :after ((transport transport-server-zeromq)
                                       &key)
  (with-slots (context socket url) transport
    (setf socket (pzmq:socket context :rep))
    (pzmq:bind socket url)))

;;  (abort nil)
(defmethod transport-close ((transport transport-zeromq) &key)
  (with-slots (context socket) transport
    (when socket (pzmq:close socket))
    (pzmq:ctx-term context)))


(defmethod send-packet ((transport transport-zeromq) packet &key)
  (format t "~a sending data ~&" (tag transport))
  (pzmq:send (slot-value transport 'socket) packet))

(defmethod recv-packet ((transport transport-zeromq) &key)
  (let ((msg))
    (format t "~a recving data ~&" (tag transport))
    (setf msg (pzmq:recv-string (slot-value transport 'socket)))
    (format t "~a have recved data: ~a ~&" (tag transport) msg)
    msg))



(defmethod run-rpc-server ((transport transport-server) object &key)
  (loop for request = (recv-packet transport) do
        (if (eql (car request) :shutdown)
            (return (send-packet transport nil))
            (progn
              (format t "Loop once, trying to send packet from server~&")
              (send-packet transport
                           (handler-case
                               (apply (car request) object (cdr request))
                             (condition (c) c)))))))

(defmethod start-rpc-server ((transport transport-server-zeromq) object &key)
  (bt:make-thread #'(lambda () (run-rpc-server transport object))
                  :name (format nil "RPC/Zeromq Server for ~S" object)))

;; (defmethod start-rpc-server ((transport transport-server-zeromq) object &key)
;;   (run-rpc-server transport object))



