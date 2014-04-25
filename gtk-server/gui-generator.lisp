;;;; Definizioni di alcune macro e metodi che servono per generare le gui
;;;; a partire da un codice Lisp
(in-package :gtk-server)

;;; Framework componenti
(defclass gui-component ()
  ((widget :accessor widget :initarg :widget :documentation "Widget string"))
  (:documentation "Generic gui component class. It defines a slot
'widget' to store the corrisponding gtk widget. If you want to create
a custom component with a class you must use put this class as your
superclass"))

(defmacro defcomponent (name lambda-list &body body)
  "Define a new component. This really defines only
a new function"
  `(defun ,name ,lambda-list ,@body))


;;; Creazione dei componenti
(defun create-component (tipo &rest argomenti)
  "Component creator function. Create the component of type 'tipo' using the constructor
function associated with the symbol"
  
  (cond
    ((fboundp tipo) (apply (symbol-function tipo) argomenti))
    (t (error "~a from ~a is not fboundp. No component found." tipo (symbol-package tipo)))))
			   

;;; Macro per la dichiarazione scritturata di un componente
(defmacro with-components (dichiarazioni &rest istruzioni)
  "Component creation macro. Example:
   (with-components ((bt-esci (button \"Esci\"))
                     (tv-testo (entry) :nodecl)
                     (lb-chiudi (label \"Chiudi\")))
             <do-something>)"
  (labels (
	   ;; Generazione delle variabili da dichiarare
	   (genera-dichiarazioni (comp)
	     (let ((nome (first comp)) (clausola (third comp)))
	       (if (string/= clausola :nodecl) (list nome) nil)))
	   
	   ;; Generazione della fase di creazione
	   (genera-creazione (comp)
	     (let ((nome (first comp)) (specifiche (second comp)))
		   `(setf ,nome (create-component
				 ',(first specifiche)
				 ,@(rest specifiche))))))
    
    (let ((da-dichiarare (mapcan #'genera-dichiarazioni dichiarazioni))
	  (codice-creazione (mapcar #'genera-creazione dichiarazioni)))
	  
	  `(let ,da-dichiarare 
	    ,@codice-creazione
	    ,@istruzioni))))

;;; Macro per la generazione dei pack
(defmacro with-pack-in (box &rest righe)
  "Example:

    (with-pack-in box-categoria
      ((:expand 0 :fill 0 :padding 2) lb-categoria)
      ((:expand 1 :fill 1 :padding 2) en-categoria))"
  
  (labels ((genera-istruzione-pack (r)
	     (let* ((child (second r))
		    (proprieta (first r))
		    (expand (or (getf proprieta :expand) 0))
		    (fill (or (getf proprieta :fill) 0))
		    (padding (or (getf proprieta :padding) 0))
		    (direzione (getf proprieta :direction))
		    (funzione-da-usare (if (string= direzione :end) 
					   'gtk-server:gtk_box_pack_end 
					   'gtk-server:gtk_box_pack_start)))

	       (list funzione-da-usare box child expand fill padding))))

    (cons 'progn (mapcar #'genera-istruzione-pack righe))))
      
	
;;; Macro per il ciclo degli eventi
(defmacro with-event-loop (nome &rest gestione)
  "Event loop macro. Example: 

  (WITH-EVENT-LOOP EVENTO 
    (BOTTONE (PRINT \"bottone\"))
    (TESTO (PRINT \"testo\")))"

  (labels ((genera-caso-controllo (elem)
	     (let ((controllo (first elem))
		   (codice (rest elem)))
	       `(when ,(cond
			((eq controllo t) t)
			(t `(gtk-server:eventp ,nome ,controllo)))
		  ,(cons 'progn codice)))))

  `(let (,nome)
    (loop 
     (setf ,nome (parse-integer (gtk-server:gtk "gtk_server_callback WAIT")))
     ,@(mapcar #'genera-caso-controllo gestione)))))
