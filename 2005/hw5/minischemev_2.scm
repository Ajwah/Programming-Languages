;;The main REP loop for the MiniScheme interpreter.  Execution of the interpreter 
;;starts by executing this function.  This interpreter allos you to read and
;;interpret a MiniScheme file!
;;How to read a file: normal REP execution remains the same.  If you have a file
;;"myfile" that contains minischeme sentences seperate by newlines, then you can 
;;execute "open myfile" at the minischeme prompt to read your file in line by line,
;;and evaluate your file's sentences.  Execution continues normally afterwards.
(define MiniScheme-v2
  (lambda ()
    (letrec
        ((evaluate-string (lambda (str) (read (open-input-string str))))
         (read-minischeme-file 
          (lambda (fh) 
            (let ((tmp (read-line fh)))
              (if (eof-object? tmp) 
                  'eof 
                  (begin 
                    (display (minischeme-eval (evaluate-string tmp) global-env))
                    (newline)
                    (read-minischeme-file fh))))))
         (loop 
          (lambda ()
            (display "$ ")
            (let ((input (read)))  ;;read is your simple parser
              (cond ((equal? input 'exit) ;; exit if "exit" is typed
                     (void))
                    ((equal? input 'open) ;; file open case?
                     (letrec ((file-name (symbol->string (read)))
                              (file-handle
                               (open-input-file file-name)))
                       (begin
                         (display "Successfully opened file: ")
                         (display file-name)
                         (newline)
                         (read-minischeme-file file-handle)
                         (loop))))
                    ;;do a normal REP from user input
                    (else
                     ;;display result of evaluating this sentence
                     (display (minischeme-eval input global-env))
                     (newline)
                     (loop)))))))
      (display "Welcome to MiniScheme v2!\n")
      (loop))))