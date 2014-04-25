(in-package :gtk-server)

;;{{{ Analizzatore Lessicale
(defun analizzatore (stringa)
  "Analizzatore lessicale alla ricerca di label e di campi
di testo"
  (let ((lunghezza (length stringa))
	(flag-avanti t)
	(indice-corrente 0)
	(colonna-inizio 0)
	(stato-corrente 0)
	(buffer "")
	(informazioni ()))

    (loop
     (setf flag-avanti t)
     (when (>= indice-corrente lunghezza)
       (return))
     (let ((carattere-corrente (char stringa indice-corrente)))
       (case stato-corrente
	 (0 
	  (cond 
	    ((char= carattere-corrente #\[) 
	     (setf buffer "")
	     (setf colonna-inizio indice-corrente)
	     (setf stato-corrente 1))
	    ((not (char= carattere-corrente #\Space))
	     (setf colonna-inizio indice-corrente)
	     (setf buffer (string carattere-corrente))
	     (setf stato-corrente 4))
	    (t 
	     (setf stato-corrente 0))))
	 (1 
	  (cond
	    ((not (char= carattere-corrente #\Space))
	     (setf buffer 
		   (concatenate 'string buffer (string carattere-corrente)))
	     (setf stato-corrente 2) )
	    (t
	     (error "Un nome di un campo di testo non puo' iniziare con uno spazio"))))
	 (2
	  (cond
	    ((char= carattere-corrente #\])
	     (setf stato-corrente 0)
	     (push (list 'entry buffer colonna-inizio) informazioni))
	    (t
	     (setf buffer 
		   (concatenate 'string buffer (string carattere-corrente)))
	     (setf stato-corrente 2))))
	 (4
	  (cond
	    ((char= carattere-corrente #\[)
	     (push (list 'label buffer colonna-inizio) informazioni)
	     (setf flag-avanti nil)
	     (setf stato-corrente 0))
	    (t
	     (setf buffer 
		   (concatenate 'string buffer (string carattere-corrente)))
	     (setf stato-corrente 4))))
	 (t
	  (error (format nil "Stato non conosciuto ~A" stato-corrente))))

       (when flag-avanti
	 (incf indice-corrente))))
    
    ;; Label rimasta appesa??
    (when (and (/= 0 (length buffer))
	       (= stato-corrente 4))
      (push (list 'label buffer colonna-inizio) informazioni))
      
    (nreverse informazioni)))
;;}}}
	     
(defun generatore-label (numero-riga specifica tabella)
  "Genera una label a partire dalla riga di specifica
output dell'analizzatore lessicale. La label viene aggiunta alla tabella"

  (let* ((nome (second specifica))
	 (colonna-inizio (third specifica))
	 (testo (string-trim " " nome))
	 (riga-inizio numero-riga)
	 (label (gtk_label_new testo)))
    (when (/= (length testo) 0)
      ;; Attacco la label alla tabella
      (gtk_label_set_use_markup label 1)
      (gtk_misc_set_alignment label 0 1)
      (gtk_table_attach_defaults tabella label
				 colonna-inizio
				 (+ colonna-inizio -1 (length nome))
				 riga-inizio (+ 1 riga-inizio)))))

(defun generatore-entry (numero-riga specifica tabella)
  "Genera un campo di testo a partire dalla riga di specifica
output dell'analizzatore lessicale. La entry viene aggiunta alla tabella"
  (let* ((nome (second specifica))
	 (colonna-inizio (third specifica))
	 (riga-inizio numero-riga)
	 (campo-testo (gtk_entry_new)))
    ;; Attacca il widget alla tabella
    (step
    (gtk_table_attach_defaults tabella campo-testo 
			       colonna-inizio
			       (+ 1 colonna-inizio (length nome))
			       riga-inizio (+ 1 riga-inizio)))
    ;; Memorizzo
    (list (intern (string-upcase (string-trim " -" nome)) 'keyword) campo-testo)))

(defun generatore-text (specifiche &key (colonne-conservare nil) (bordo 10))
  "Genera una gui secondo delle specifiche fornite
con una lista di stringhe. Ritorna una plist con i
componenti creati. Il componente principale viene
chiamato \"tabella\""

  (let* ((larghezza (reduce #'max (mapcar #'length specifiche)))
	 (altezza (length specifiche))
	 ;; Devo generare una tabella di larghezza x altezza
	 (tabella (gtk_table_new altezza larghezza 0))
	 (componenti)
	 (vbox)
	 (box)
	 (riga-corrente 0)
	 (colonne-usate))
    
    ;; Imposta tabella nella plist dei componenti
    (setf (getf componenti :tabella) tabella)

    ;; Lettura delle varie righe
    (dolist (riga specifiche)
      (let ((informazioni (analizzatore riga)))
	(dolist (elemento informazioni)
	  (let ((tipologia (first elemento)))

	    (cond
	      ((string= tipologia 'label)
	       (generatore-label riga-corrente elemento tabella))

	      ((string= tipologia 'entry)
	       (setf componenti (append componenti (generatore-entry riga-corrente elemento tabella)))))))

	;; Scansione delle colonne
	(dotimes (i (length riga))
	  (let ((contenuto (subseq riga i (1+ i))))
	    (unless (or (null contenuto) (string= " " contenuto))
	      (pushnew i colonne-usate))))

	;; Inserimento riga vuota
	(when (= 0 (length (string-trim " " riga)))
	  (let ((label (gtk_label_new " ")))
	    (gtk_table_attach_defaults tabella label 0 1 riga-corrente (1+ riga-corrente))))

	;; Incremento il numero di riga
	(incf riga-corrente)))

    ;; Implementazione colonne da conservare
    (dotimes (i (length colonne-conservare))
      (when (char/= (char colonne-conservare i) #\Space)
	(let ((label (gtk_label_new " ")))
	  (gtk_table_attach_defaults tabella label i (1+ i) riga-corrente (1+ riga-corrente)))))

    ;; Implementazione del bordo
    (setf vbox (gtk_vbox_new 0 2))
    (gtk_box_pack_start vbox tabella 1 1 bordo)
    (setf (getf componenti :vbox) vbox)

    (setf box (gtk_hbox_new 0 2))
    (gtk_box_pack_start box vbox 1 1 bordo)
    (setf (getf componenti :box) box)

    componenti))