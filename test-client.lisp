(defun server ()
  (let* ((context (pzmq:ctx-new))
         (socket (pzmq:socket context :rep))
         (url "tcp://0.0.0.0:10000"))
    (format t "Binding ~&")
    (pzmq:bind socket url)

    (unwind-protect
         (progn
           (format t "Recving data ~&")
           (format t "Data:~a ~%" (pzmq:recv-string socket))
           ;; (sleep 1)
           (pzmq:send socket "world")
           (format t "Server End"))
      (progn
        (format t "Cleaning stuff~&")
        (pzmq:close socket)
        (pzmq:ctx-term context)))

    )
  )



(defun client ()
  (let* ((context (pzmq:ctx-new))
         (socket (pzmq:socket context :req))
         (url "tcp://0.0.0.0:10000"))
    (format t "Connecting ~&")
    (pzmq:connect socket url)

    (unwind-protect
         (progn
           (format t "Sending data ~&")
           (pzmq:send socket "Hello")
           (write-string "Receiving... ")
           (write-line (pzmq:recv-string socket))
           (format t "Client End"))
      (progn
        (format t "~%Cleaning stuff~&")
        (pzmq:close socket)
        (pzmq:ctx-term context)))

    )
  )

(ql:quickload :pzmq)


;; (defun hwserver (&optional (listen-address "tcp://*:5555"))
;;   "Translation of http://zguide.zeromq.org/c:hwserver updated for ZMQ 3. "
;;   (pzmq:with-context nil                ; use *default-context*
;;     (pzmq:with-socket responder :rep
;;       (pzmq:bind responder listen-address)
;;       (loop
;;         (write-string "Waiting for a request... ")
;;         (write-line (pzmq:recv-string responder))
;;         (sleep 1)
;;         (pzmq:send responder "World")))))

;; (defun hwclient (&optional (server-address "tcp://localhost:5555"))
;;   "Translation of http://zguide.zeromq.org/c:hwclient updated for ZMQ 3.  Includes some parameters in with-* macros to demonstrate syntax."
;;   (pzmq:with-context (ctx :max-sockets 10)
;;     (pzmq:with-socket (requester ctx) (:req :affinity 3 :linger 100)
;;       ;; linger is important in case of (keyboard) interrupt;
;;       ;; see http://api.zeromq.org/3-3:zmq-ctx-destroy
;;       (write-line "Connecting to hello world server...")
;;       (pzmq:connect requester server-address)
;;       (dotimes (i 10)
;;         (format t "Sending Hello ~d...~%" i)
;;         (pzmq:send requester "Hello")
;;         (write-string "Receiving... ")
;;         (write-line (pzmq:recv-string requester))))))
