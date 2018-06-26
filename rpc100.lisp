(ql:quickload '(:usocket :flexi-streams :log4cl :ironclad :bordeaux-threads))


(defvar data-max-length 65507)

(defclass transport-udp-server ()
  ((host :accessor host
         :initarg :host
         :initform (error "No host given"))
   (port :accessor port
         :initarg :port
         :initform (error "No port given"))
   ;; (socket :accessor :socket
   ;;         :initform nil)
   (router :accessor router
           :initform nil)))



(defun load-msg (str)
  (first (multiple-value-list (read-from-string str))))

(defun dump-msg (msg)
  (write-to-string msg
                   :array t :base 10 :case :downcase :circle t
                   :escape t :gensym t :length nil :level nil
                   :lines nil :pretty nil :radix nil :readably nil))



;;; server api
(defun make-server (&key host port)
  (make-instance 'transport-udp-server
                 :host host
                 :port port))

(defgeneric expose (server rpc-name lambda-fun)
  (:documentation ""))

(defmethod expose ((server transport-udp-server) rpc-name lambda-fun)
  (push (cons rpc-name lambda-fun) (router server)))

(defun server-listen (server)
  (log:info "Starting listen on address ~a ~a" (host server) (port server))
  (let ((socket (usocket:socket-connect nil nil
                                        :protocol :datagram
                                        :local-host (host server)
                                        :local-port (port server))))
    (unwind-protect
         (loop :for (msg . addr)
               := (multiple-value-bind (return-buffer return-length remote-host remote-port)
                      (usocket:socket-receive socket nil data-max-length)
                    (let ((msg (load-msg (flexi-streams:octets-to-string (subseq return-buffer 0 return-length) :external-format :utf-8))))
                      (log:info "Server receive msg: ~a from client ~a ~a " msg remote-host remote-port)
                      (list msg remote-host remote-port)))
               :do (progn

                     ;; send result back to client
                     (let* ((result (handler-case
                                        (progn
                                          ;; recved msg format : (msg-id rpc-name args-in-list)
                                          (apply (cdr (assoc (second msg) (router server) :test #'equalp)) (third msg)))
                                      (condition (c) c)))
                            (packet (dump-msg (append msg (list result)))) ;; construct the return msg: (msg-id rpc-name args-in-list result)
                            (buffer (flexi-streams:string-to-octets packet :external-format :utf-8)))
                       (log:info "Server reply msg: ~a to  client ~a ~a" packet (first addr) (second addr))
                       (usocket:socket-send socket buffer (length buffer)
                                            :host (first addr)
                                            :port (second addr))))
               )
      (usocket:socket-close socket))))




(defclass transport-udp-client ()
  ((host :accessor client-host
         :initarg :host
         :initform nil)
   (port :accessor client-port
         :initarg :port
         :initform nil)
   (outstanding :accessor client-outstanding
                :initform nil)
   (outstanding-lock :accessor client-outstanding-lock
                     :initform (bt:make-lock "outstanding-lock"))
   (socket :accessor client-socket
           :initform nil)
   (backgroud-thread :accessor client-background-thread
                     :initform nil)

   ))


;;; Client api
(defun make-client ()
  (make-instance 'transport-udp-client))


(defun generate-msg-id ()
  (ironclad:byte-array-to-hex-string (ironclad:digest-sequence
                                      :Sha1
                                      (crypto:random-data 32 (crypto:make-prng :fortuna)))))

(defmacro with-lock-held ((lock) &body body)
  "Simple wrapper to allow LispWorks and Bordeaux Threads to coexist."
  `(bt:with-lock-held (,lock) ,@body))

;; At separate thread
(defvar check-time-interval 0.2)
(defun call (client rpc-name args &key timeout)
  (let* ((msg-id (generate-msg-id))
         (msg (list msg-id rpc-name args))  ;; construct the send msg: (msg-id rpc-name args-in-list)
         (packet (dump-msg msg))
         (buffer (flexi-streams:string-to-octets packet :external-format :utf-8))
         (result nil)
         (end-time (and timeout (+ (get-internal-real-time) (* internal-time-units-per-second timeout)))))
    (log:info "Client send msg: ~a to server ~a ~a" msg (client-host client) (client-port client))
    (with-lock-held ((client-outstanding-lock client))
      (push (cons msg-id nil) (client-outstanding client)))
    (usocket:socket-send (client-socket client) buffer (length buffer))

    ;; fetch result from outstanding
    (loop :while (or (null end-time)
                     (< (get-internal-real-time) end-time))
          :do (progn
                (with-lock-held ((client-outstanding-lock client))
                  (when (cdr (assoc msg-id (client-outstanding client) :test #'equalp))
                    (setf result  (cdr (assoc msg-id (client-outstanding client) :test #'equalp)))
                    (log:info "Result is" result)
                    (return result)))
                (sleep check-time-interval)
                (log:info "once")))))


;; In backgroung thread
(defun handle-msg (client)
  (loop
    (multiple-value-bind (return-buffer return-length)
        (usocket:socket-receive (client-socket client) nil data-max-length)
      (let* ((msg (load-msg (flexi-streams:octets-to-string (subseq return-buffer 0 return-length) :external-format :utf-8))) ;; recved msg: (msg-id rpc-name args-in-list result)
             (msg-id (first msg))
             (result-value (fourth msg)))
        (with-lock-held ((client-outstanding-lock client))
          (if (assoc msg-id (client-outstanding client) :test #'equalp)
              (progn
                (log:info "Client get a msg" msg-id result-value)
                (setf (cdr (assoc msg-id (client-outstanding client) :test #'equalp))
                      result-value))
              (log:info "Unknown msg ~a" msg)))
        (log:info "Client receive msg: ~a from server" msg)))))


(defun client-connect (client &key host port)
  (log:info "Client start listen on address" host port)
  (let ((socket (usocket:socket-connect host port
                                        :protocol :datagram)))
    (setf (client-socket client) socket
          (client-host client) host
          (client-port client) client
          (client-background-thread client) (bt:make-thread (lambda () (handle-msg client))
                                                            :name "Background client"))
    ))


(defun client-stop (client)
  (bt:destroy-thread (client-background-thread client)) ;;very rude now
  (sleep 1)
  (usocket:socket-close (client-socket client))
  (log:info "client stop"))
