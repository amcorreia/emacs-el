;;;
;;; Tempo template
;;;

(require 'tempo)
;; This is a way to hook tempo into cc-mode
(defvar c-tempo-tags nil
  "Tempo tags for C mode")
;(defvar c++-tempo-tags nil
;  "Tempo tags for C++ mode")
(defvar php-tempo-tags nil
  "Tempo tags for PHP mode")
(defvar emacs-tempo-tags nil
  "Tempo tags for Emacs Lisp mode")

;; C-Mode Templates and C++-Mode Templates (uses C-Mode Templates also)
(setq tempo-interactive t)

;; (add-hook 'c-mode-hook
;;           '(lambda ()
;;              (local-set-key [f9]  'tempo-complete-tag)
;;              (tempo-use-tag-list  'c-tempo-tags)))


(tempo-define-template "c-include1"                      ; name
                       '("#include <" r ".h>" > )        ; elements
                       "inc"                             ; &tag
                       "Insert a #include <> statement"  ; &documentation
                       'c-tempo-tags)                    ; &taglist
(tempo-define-template "c-include2"
                       '("#include "  r ".h" > n
                         )
                       "incp"
                       "Insert a #include \"\" statement"
                       'c-tempo-tags)

(tempo-define-template "c-ifdef"
                       '("ifdef " (p "ifdef-clause: " clause) > n> p n
                         "#else /* !(" (s clause) ") */" n> p n
                         "#endif /* " (s clause)" */" n>
                         )
                       "ifdef"
                       "Insert a #ifdef #else #endif statement"
                       'c-tempo-tags)

(tempo-define-template "c-ifndef"
                       '("ifndef " (p "ifndef-clause: " clause) > n
                         "#define " (s clause) n> p n
                         "#endif /* " (s clause)" */" n>
                         )
                       "ifndef"
                       "Insert a #ifndef #define #endif statement"
                       'c-tempo-tags)
;;;; C-Mode Templates
(tempo-define-template "c-if"
                       '(> "if (" (p "if-clause: " clause) ") {" > n>
                           > r n
                           "}" > n>
                           )
                       "if"
                       "Insert a C if statement"
                       'c-tempo-tags)

(tempo-define-template "c-else"
                       '(> "else" n>
                           "{" > n>
                           > r n
                           "}" > n>
                           )
                       "else"
                       "Insert a C else statement"
                       'c-tempo-tags)

(tempo-define-template "c-if-else"
                       '(> "if (" (p "if-clause: " clause) ") {"  n>
                           > r n
                           "} else {" > n
                           > r n
                           "}" > n>
                           )
                       "ifelse"
                       "Insert a C if else statement"
                       'c-tempo-tags)

(tempo-define-template "c-while"
                       '(> "while (" (p "while-clause: " clause) ") {" >  n>
                           > r n
                           "} /* while (" (s clause) ") */" > n>
                           )
                       "while"
                       "Insert a C while statement"
                       'c-tempo-tags)

(tempo-define-template "c-for"
                       '(> "for (" (p "for-clause: " clause) ") {" >  n>
                           > r n
                           "}" > n>
                           )
                       "for"
                       "Insert a C for statement"
                       'c-tempo-tags)

(tempo-define-template "c-for-i"
                       '(> "for (" (p "variable: " var) " = 0; " (s var)
                           " < "(p "upper bound: " ub)"; " (s var) "++) {" >  n>
                           > r n
                           "}" > n>
                           )
                       "fori"
                       "Insert a C for loop: for(x = 0; x < ..; x++)"
                       'c-tempo-tags)

(tempo-define-template "c-main"
                       '(> "int main (int argc, char *argv[])" >  n>
                           "{" > n>
                           > r n
                           > "return 0;" n>
                           > "}" > n>
                           )
                       "main"
                       "Insert a C main statement"
                       'c-tempo-tags)

(tempo-define-template "c-if-malloc"
                       '(> (p "variable: " var) " = ("
                           (p "type: " type) " *) malloc (sizeof(" (s type)
                           ") * " (p "nitems: " nitems) ");" n>
                           > "if (NULL == " (s var) ") {" n>
                           > "printf (\"%s[%d]: " r " Failed to malloc() "
                           (s var) " \", __FILE__, __LINE__);" n>
                           > "exit (1);" n>
                           "}" >
                           )
                       "ifmalloc"
                       "Insert a C if (malloc...) statement"
                       'c-tempo-tags)

(tempo-define-template "c-if-calloc"
                       '(> (p "variable: " var) " = ("
                           (p "type: " type) " *) calloc (sizeof(" (s type)
                           "), " (p "nitems: " nitems) ");" n>
                           > "if (NULL == " (s var) ") {" n>
                           > "error_exit (\"" (buffer-name) ": " r ": Failed to calloc() " (s var) " \");" n>
                           "}" >
                           )
                       "ifcalloc"
                       "Insert a C if (calloc...) statement"
                       'c-tempo-tags)

(tempo-define-template "c-switch"
                       '(> "switch (" (p "switch-condition: " clause) ")" n>
                           "{" >  n>
                           "case " (p "first value: ") ":" > n> p n
                           "break;" > n> p n
                           "default:" > n> p n
                           "break;" > n
                           "} /* switch (" (s clause) ") */"  > n>
                           )
                       "switch"
                       "Insert a C switch statement"
                       'c-tempo-tags)

(tempo-define-template "c-case"
                       '(n "case " (p "value: ") ":" > n> p n
                           "break;" > n> p
                           )
                       "case"
                       "Insert a C case statement"
                       'c-tempo-tags)

(tempo-define-template "c-printf"
                       '("printf (" r ");" > )
                       "pr"
                       "insert a printf skeleton"
                       'c-tempo-tags)

(tempo-define-template "debug"
                       '("printf (\DEBUG[%d]: \, __LINE__);  " > n)
                       "debug"
                       "Insert a debug printf"
                       'c-tempo-tags)

;;; Emacs lisp
(tempo-define-template "defvar"
                       '("(defvar " (p "defvar: " def-var) " "
                         (p "list of arguments: " arg-list) > n>
                         "\"" (p "documentation: " doc) > " \"" n>
                         r n> " )" > n>  )
                       "defvar"
                       "Insert a defvar statement"
                       'emacs-tempo-tags)

;;; PHP
(tempo-define-template "php-for-i"
                       '(> "for ($" (p "variable: $" var) "=0; $" (s var)
                           "<$" (p "upper bound: $" ub)"; $" (s var) "++)" n>
			   "{" >  n>
                           > r n
                           "}" > n>
                           )
                       "fori"
                       "Insert a FOR loop: for($x = 0; $x < ..; $x++)"
                       'php-tempo-tags)

(tempo-define-template "php-foreach"
		       '(> "foreach ($" (p "var: $" def-var) " as $"
			 (p "key: $" key) " => $" (p "value: $" value) ")" n>
			 "{" > n> r n> "}" >
			 )
			 "fore"
			 "Insert a FOREACH statement"
			 'php-tempo-tags)
; cakephp
(tempo-define-template "php-publicfunction"
		       '(> "public function " (p "Function name: " def-var) " ( " r " ) "
                           n> > "{" > n>  n> "}" >
			 )
			 "pf"
			 "Insert a PUBLIC FUNCTION () statement"
			 'php-tempo-tags)

;;; Local Variables: ***
;;; compile-command: "emacs --no-init-file --no-site-file -batch -f batch-byte-compile ~/.emacs.d/emacs-el/tempo-template.el" ***
;;; End: ***
