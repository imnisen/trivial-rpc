;; Short guide to UDP/IP Client/Server programming in Common Lisp using usockets

;; The main reason for this guide is because there are very few examples that
;; explain how to get started with socket programming with Common Lisp that I
;; could understand.
;; After working on a short example on TCP, I found the
;; need for a UDP tutorial. So, here goes.

;; As usual, we will use quicklisp to load usocket.


(ql:quickload "usocket")


;; Now we need to create a server. As a protocol, UDP is connection-less, and
;; therefore there is no concept of binding and accepting a connection. Instead
;; we only do a socket-connect but pass a specific set of parameters to make
;; sure that we create an UDP socket that's waiting for data on a particular
;; port.

;; So, what were the problems I faced due to my mistakes?
;; Mistake 1 - Unlike TCP, you don't pass host and port to socket-connect.
;; If you do that, then you are indicating that you want to send a packet.
;; Instead, you pass nil but set :local-host and :local-port to the address
;; and port that you wnat to receive data on. This part took some time to
;; figure out, because the documentation didn't cover it. Instead reading
;; a bit of code from
;; https://code.google.com/p/blackthorn-engine-3d/source/browse/src/examples/usocket/usocket.lisp helped a lot.

;; Also, since UDP is connectionless, anyone can send data to it at any
;; time. So, we need to know which host/port did we get data from so
;; that we can respond on it. So we bind multiple values to socket-receive
;; and use those values to send back data to our peer "client".

(defun create-server (port buffer)
  (let* ((socket (usocket:socket-connect nil nil
                                         :protocol :datagram
                                         :element-type '(unsigned-byte 8)
                                         :local-host "127.0.0.1"
                                         :local-port port)))
    (unwind-protect
         (multiple-value-bind (buffer size client receive-port)
             (usocket:socket-receive socket buffer 8)
           (format t "~A~%" buffer)
           (usocket:socket-send socket (reverse buffer) size
                                :port receive-port
                                :host client))
      (usocket:socket-close socket))))



;; Now for the sender/receiver. This part is pretty easy. Create a socket,
;; send data on it and receive data back.

(defun create-client (port buffer)
  (let ((socket (usocket:socket-connect "127.0.0.1" port
                                        :protocol :datagram
                                        :element-type '(unsigned-byte 8))))
    (unwind-protect
         (progn
           (format t "Sending data~%")
           (replace buffer #(1 2 3 4 5 6 7 8))
           (format t "Receiving data~%")
           (usocket:socket-send socket buffer 8)
           (usocket:socket-receive socket buffer 8)
           (format t "~A~%" buffer))
      (usocket:socket-close socket))))



;; So, how do you run this? You need two REPLs - one for the server
;; and one for the client. Load this file in both REPLs. Create the
;; server in the first REPL.
;; (create-server 12321 (make-array 8 :element-type '(unsigned-byte 8)))
;; Now you are ready to run the client on the second REPL
;; (create-client 12321 (make-array 8 :element-type '(unsigned-byte 8)))
;; Voila! You should see a vector #(1 2 3 4 5 6 7 8) on the first REPL
;; and #(8 7 6 5 4 3 2 1) on the second REPL.

;; Also see
;; 1. Short Guide on TCP/IP
;; - https://gist.github.com/shortsightedsid/71cf34282dfae0dd2528
