;;; yaml.el --- YAML parser for Elisp -*- lexical-binding: t -*-

;; Copyright © 2021 Zachary Romero <zkry@posteo.org>

;; Author: Zachary Romero <zkry@posteo.org>
;; Version: 0.5.1
;; Homepage: https://github.com/zkry/yaml.el
;; Package-Requires: ((emacs "25.1"))
;; Keywords: tools

;; yaml.el requires at least GNU Emacs 25.1

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; yaml.el contains the code for parsing YAML natively in Elisp with
;; no dependencies.  The main function to parse YAML provided is
;; `yaml-parse-string'.  `yaml-encode' is also provided to encode a
;; Lisp object to YAML.  The following are some examples of its usage:
;;
;; (yaml-parse-string "key1: value1\nkey2: value2")
;; (yaml-parse-string "key1: value1\nkey2: value2" :object-type 'alist)
;; (yaml-parse-string "numbers: [1, 2, 3]" :sequence-type 'list)
;;
;; (yaml-encode '((count . 3) (value . 10) (items ("ruby" "diamond"))))

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'cl-lib)

(defconst yaml-parser-version "0.5.1")

(defvar yaml--parse-debug nil
  "Turn on debugging messages when parsing YAML when non-nil.

This flag is intended for development purposes.")

(defconst yaml--tracing-ignore '("s-space"
                                 "s-tab"
                                 "s-white"
                                 "l-comment"
                                 "b-break"
                                 "b-line-feed"
                                 "b-carriage-return"
                                 "s-b-comment"
                                 "b-comment"
                                 "l-comment"
                                 "ns-char"
                                 "nb-char"
                                 "b-char"
                                 "c-printable"
                                 "b-as-space"))

(defvar yaml--parsing-input ""
  "The string content of the current item being processed.")
(defvar yaml--parsing-position 0
  "The position that the parser is currently looking at.")
(defvar yaml--states nil
  "Stack of parsing states.")

(defvar yaml--parsing-object-type nil)
(defvar yaml--parsing-object-key-type nil)
(defvar yaml--parsing-sequence-type nil)
(defvar yaml--parsing-null-object nil)
(defvar yaml--parsing-false-object nil)
(defvar yaml--parsing-store-position nil)
(defvar yaml--string-values nil)

(cl-defstruct (yaml--state (:constructor yaml--state-create)
                           (:copier nil))
  doc tt m name lvl beg end)

(defmacro yaml--parse (data &rest forms)
  "Parse DATA according to FORMS."
  (declare (indent defun))
  `(progn (setq yaml--parsing-input ,data)
          (setq yaml--parsing-position 0)
          (yaml--initialize-state)
          ,@forms))

(defun yaml--state-curr ()
  "Return the current state."
  (or (car yaml--states)
      (yaml--state-create
       :name nil :doc nil :lvl 0 :beg 0 :end 0 :m nil :tt nil)))

(defun yaml--state-set-m (val)
  "Set the current value of t to VAL."
  (let* ((states yaml--states))
    (while states
      (let* ((top-state (car states))
             (new-state (yaml--state-create :doc (yaml--state-doc top-state)
                                            :tt (yaml--state-tt top-state)
                                            :m val
                                            :name (yaml--state-name top-state)
                                            :lvl (yaml--state-lvl top-state)
                                            :beg (yaml--state-beg top-state)
                                            :end (yaml--state-end top-state))))
        (setcar states new-state))
      (setq states (cdr states)))))

(defun yaml--state-set-t (val)
  "Set the current value of t to VAL."
  (let* ((states yaml--states))
    (while states
      (let* ((top-state (car states))
             (new-state (yaml--state-create :doc (yaml--state-doc top-state)
                                            :tt val
                                            :m (yaml--state-m top-state)
                                            :name (yaml--state-name top-state)
                                            :lvl (yaml--state-lvl top-state)
                                            :beg (yaml--state-beg top-state)
                                            :end (yaml--state-end top-state))))
        (setcar states new-state))
      (setq states (cdr states)))))

(defun yaml--state-curr-doc ()
  "Return the doc property of current state."
  (yaml--state-doc (yaml--state-curr)))

(defun yaml--state-curr-t ()
  "Return the doc property of current state."
  (yaml--state-tt (yaml--state-curr)))

(defun yaml--state-curr-m ()
  "Return the doc property of current state."
  (or (yaml--state-m (yaml--state-curr)) 1))

(defun yaml--state-curr-end ()
  "Return the doc property of current state."
  (yaml--state-end (yaml--state-curr)))

(defun yaml--push-state (name)
  "Add a new state frame with NAME."
  (let* ((curr-state (yaml--state-curr))
         (new-state (yaml--state-create
                     :doc (yaml--state-curr-doc)
                     :tt (yaml--state-curr-t)
                     :m (yaml--state-curr-m)
                     :name name
                     :lvl (1+ (yaml--state-lvl curr-state))
                     :beg yaml--parsing-position
                     :end nil)))
    (push new-state yaml--states)))

(defun yaml--pop-state ()
  "Pop the current state."
  (let ((popped-state (car yaml--states)))
    (setq yaml--states (cdr yaml--states))
    (let ((top-state (car yaml--states)))
      (when top-state
        (setcar yaml--states
                (yaml--state-create :doc (yaml--state-doc top-state)
                                    :tt (yaml--state-tt top-state)
                                    :m (yaml--state-m top-state)
                                    :name (yaml--state-name top-state)
                                    :lvl (yaml--state-lvl top-state)
                                    :beg (yaml--state-beg popped-state)
                                    :end yaml--parsing-position))))))

(defun yaml--initialize-state ()
  "Initialize the yaml state for parsing."
  (setq yaml--states
        (list (yaml--state-create :doc nil
                                  :tt nil
                                  :m nil
                                  :name nil
                                  :lvl 0
                                  :beg nil
                                  :end nil))))

(defconst yaml--grammar-resolution-rules
  '(("ns-plain" . literal))
  "Alist determining how to resolve grammar rule.")

;;; Receiver Functions

(defvar yaml--document-start-version nil)
(defvar yaml--document-start-explicit nil)
(defvar yaml--document-end-explicit nil)
(defvar yaml--tag-map nil)
(defvar yaml--tag-handle nil)
(defvar yaml--document-end nil)

(defvar yaml--cache nil
  "Stack of data for temporary calculations.")
(defvar yaml--object-stack nil
  "Stack of objects currently being build.")
(defvar yaml--state-stack nil
  "The state that the YAML parser is with regards to incoming events.")
(defvar yaml--root nil)

(defvar yaml--anchor-mappings nil
  "Hashmap containing the anchor mappings of the current parsing run.")
(defvar yaml--resolve-aliases nil
  "Flag determining if the event processing should attempt to resolve aliases.")

(defun yaml--parse-block-header (header)
  "Parse the HEADER string returning chomping style and indent count."
  (let* ((pos 0)
         (chomp-indicator :clip)
         (indentation-indicator nil)
         (char (and (< pos (length header)) (aref header pos)))
         (process-char (lambda (char)
                         (when char
                           (cond
                            ((< ?0 char ?9)
                             (progn (setq indentation-indicator (- char ?0))))
                            ((equal char ?\-) (setq chomp-indicator :strip))
                            ((equal char ?\+) (setq chomp-indicator :keep)))
                           (setq pos (1+ pos))))))
    (when (or (eq char ?\|) (eq char ?\>))
      (setq pos (1+ pos))
      (setq char (and (< pos (length header)) (aref header pos))))
    (funcall process-char char)
    (let ((char (and (< pos (length header)) (aref header pos)))) ;
      (funcall process-char char)
      (list chomp-indicator indentation-indicator))))

(defun yaml--chomp-text (text-body chomp)
  "Change the ending newline of TEXT-BODY based on CHOMP."
  (cond ((eq :clip chomp)
         (concat (replace-regexp-in-string "\n*\\'" "" text-body) "\n"))
        ((eq :strip chomp)
         (replace-regexp-in-string "\n*\\'" "" text-body))
        ((eq :keep chomp)
         text-body)))

(defun yaml--process-folded-text (text)
  "Remvoe the header line for a folded match and return TEXT body formatted."
  (let* ((text (yaml--process-literal-text text))
         (done))
    (while (not done)
      (let ((replaced (replace-regexp-in-string "\\([^\n]\\)\n\\([^\n ]\\)"
                                                "\\1 \\2"
                                                text)))
        (when (equal replaced text)
          (setq done t))
        (setq text replaced)))
    (replace-regexp-in-string
     "\\(\\(?:^\\|\n\\)[^ \n][^\n]*\\)\n\\(\n+\\)\\([^\n ]\\)" "\\1\\2\\3"
     text)))

(defun yaml--process-literal-text (text)
  "Remove the header line for a folded match and return TEXT body formatted."
  (let ((n (get-text-property 0 'yaml-n text)))
    (remove-text-properties 0 (length text) '(yaml-n nil) text)
    (let* ((header-line (substring text 0 (string-match "\n" text)))
           (text-body (substring text (1+ (string-match "\n" text))))
           (parsed-header (yaml--parse-block-header header-line))
           (chomp (car parsed-header))
           (starting-spaces-ct
            (or (and (cadr parsed-header) (+ (or n 0) (cadr parsed-header)))
                (let ((_ (string-match "^\n*\\( *\\)" text-body)))
                  (length (match-string 1 text-body)))))
           (lines (split-string text-body "\n"))
           (striped-lines
            (seq-map (lambda (l)
                       (replace-regexp-in-string
                        (format "\\` \\{0,%d\\}" starting-spaces-ct) "" l))
                     lines))
           (text-body (string-join striped-lines "\n")))
      (yaml--chomp-text text-body chomp))))

;; TODO: Process tags and use them in this function.
(defun yaml--resolve-scalar-tag (scalar)
  "Convert a SCALAR string to it's corresponding object."
  (cond
   (yaml--string-values
    scalar)
   ;; tag:yaml.org,2002:null
   ((or (equal "null" scalar)
        (equal "Null" scalar)
        (equal "NULL" scalar)
        (equal "~" scalar))
    yaml--parsing-null-object)
   ;; tag:yaml.org,2002:bool
   ((or (equal "true" scalar)
        (equal "True" scalar)
        (equal "TRUE" scalar)) t)
   ((or (equal "false" scalar)
        (equal "False" scalar)
        (equal "FALSE" scalar))
    yaml--parsing-false-object)
   ;; tag:yaml.org,2002:int
   ((string-match "^0$\\|^-?[1-9][0-9]*$" scalar)
    (string-to-number scalar))
   ((string-match "^[-+]?[0-9]+$" scalar)
    (string-to-number scalar))
   ((string-match "^0o[0-7]+$" scalar)
    (string-to-number scalar 8))
   ((string-match "^0x[0-9a-fA-F]+$" scalar)
    (string-to-number scalar 16))
   ;; tag:yaml.org,2002:float
   ((string-match
     "^[-+]?\\(\\.[0-9]+\\|[0-9]+\\(\\.[0-9]*\\)?\\)\\([eE][-+]?[0-9]+\\)?$"
     scalar)
    (string-to-number scalar 10))
   ((string-match "^[-+]?\\(\\.inf\\|\\.Inf\\|\\.INF\\)$" scalar)
    1.0e+INF)
   ((string-match "^[-+]?\\(\\.nan\\|\\.NaN\\|\\.NAN\\)$" scalar)
    1.0e+INF)
   ((string-match "^0$\\|^-?[1-9]\\(\\.[0-9]*\\)?\\(e[-+][1-9][0-9]*\\)?$"
                  scalar)
    (string-to-number scalar))
   (t scalar)))

(defun yaml--hash-table-to-alist (hash-table)
  "Convert HASH-TABLE to a alist."
  (let ((alist nil))
    (maphash
     (lambda (k v)
       (setq alist (cons (cons k v) alist)))
     hash-table)
    alist))

(defun yaml--hash-table-to-plist (hash-table)
  "Convert HASH-TABLE to a plist."
  (let ((plist nil))
    (maphash
     (lambda (k v)
       (setq plist (cons k (cons v plist))))
     hash-table)
    plist))

(defun yaml--format-object (hash-table)
  "Convert HASH-TABLE to alist of plist if specified."
  (cond
   ((equal yaml--parsing-object-type 'hash-table)
    hash-table)
   ((equal yaml--parsing-object-type 'alist)
    (yaml--hash-table-to-alist hash-table))
   ((equal yaml--parsing-object-type 'plist)
    (yaml--hash-table-to-plist hash-table))
   (t hash-table)))

(defun yaml--format-list (l)
  "Convert L to array if specified."
  (cond
   ((equal yaml--parsing-sequence-type 'list)
    l)
   ((equal yaml--parsing-sequence-type 'array)
    (apply #'vector l))
   (t l)))

(defun yaml--stream-start-event ()
  "Create the data for a stream-start event."
  '(:stream-start))

(defun yaml--stream-end-event ()
  "Create the data for a stream-end event."
  '(:stream-end))

(defun yaml--mapping-start-event (_)
  "Process event indicating start of mapping."
  ;; NOTE: currently don't have a use for FLOW
  (push :mapping yaml--state-stack)
  (push (make-hash-table :test 'equal) yaml--object-stack))

(defun yaml--mapping-end-event ()
  "Process event indicating end of mapping."
  (pop yaml--state-stack)
  (let ((obj (pop yaml--object-stack)))
    (yaml--scalar-event nil obj))
  '(:mapping-end))

(defun yaml--sequence-start-event (_)
  "Process event indicating start of sequence according to FLOW."
  ;; NOTE: currently don't have a use for FLOW
  (push :sequence yaml--state-stack)
  (push nil yaml--object-stack)
  '(:sequence-start))

(defun yaml--sequence-end-event ()
  "Process event indicating end of sequence."
  (pop yaml--state-stack)
  (let ((obj (pop yaml--object-stack)))
    (yaml--scalar-event nil obj))
  '(:sequence-end))

(defun yaml--anchor-event (name)
  "Process event indicating an anchor has been defined with NAME."
  (push :anchor yaml--state-stack)
  (push `(:anchor ,name) yaml--object-stack))

(defun yaml--scalar-event (style value)
  "Process the completion of a scalar VALUE.

 Note that VALUE may be a complex object here.  STYLE is
 currently unused."
  (let ((top-state (car yaml--state-stack))
        (value* (cond
                 ((stringp value) (yaml--resolve-scalar-tag value))
                 ((listp value) (yaml--format-list value))
                 ((hash-table-p value) (yaml--format-object value))
                 ((vectorp value) value)
                 ((not value) nil))))
    (cond
     ((not top-state)
      (setq yaml--root value*))
     ((equal top-state :anchor)
      (let* ((anchor (pop yaml--object-stack))
             (name (nth 1 anchor)))
        (puthash name value yaml--anchor-mappings)
        (pop yaml--state-stack)
        (yaml--scalar-event nil value)))
     ((equal top-state :sequence)
      (let ((l (car yaml--object-stack)))
        (setcar yaml--object-stack (append l (list value*)))))
     ((equal top-state :mapping)
      (progn
        (push :mapping-value yaml--state-stack)
        (push value* yaml--cache)))
     ((equal top-state :mapping-value)
      (progn
        (let ((key (pop yaml--cache))
              (table (car yaml--object-stack)))
          (when (stringp key)
            (cond
             ((eql 'symbol yaml--parsing-object-key-type)
              (setq key (intern key)))
             ((eql 'keyword yaml--parsing-object-key-type)
              (setq key (intern (format ":%s" key))))))
          (puthash key value* table))
        (pop yaml--state-stack)))
     ((equal top-state :trail-comments)
      (pop yaml--state-stack)
      (let ((comment-text (pop yaml--object-stack)))
        (unless (stringp value*)
          (error "Trail-comments can't be nested under non-string"))
        (yaml--scalar-event
         style
         (replace-regexp-in-string (concat (regexp-quote comment-text) "\n*\\'")
                                   ""
                                   value*))))
     ((equal top-state nil))))
  '(:scalar))

(defun yaml--alias-event (name)
  "Process a node has been defined via alias NAME."
  (if yaml--resolve-aliases
      (let ((resolved (gethash name yaml--anchor-mappings)))
        (unless resolved (error "Undefined alias '%s'" name))
        (yaml--scalar-event nil resolved))
    (yaml--scalar-event nil (vector :alias name)))
  '(:alias))

(defun yaml--trail-comments-event (text)
  "Process trailing comments of TEXT which should be trimmed from parent."
  (push :trail-comments yaml--state-stack)
  (push text yaml--object-stack)
  '(:trail-comments))



(defun yaml--check-document-end ()
  "Return non-nil if at end of document."
  ;; NOTE: currently no need for this.  May be needed in the future.
  t)

(defun yaml--reverse-at-list ()
  "Reverse the list at the top of the object stack.

This is needed to get the correct order as lists are processed in
reverse order."
  (setcar yaml--object-stack (reverse (car yaml--object-stack))))

(defconst yaml--grammar-events-in
  `(("l-yaml-stream" . ,(lambda ()
                          (yaml--stream-start-event)
                          (setq yaml--document-start-version nil)
                          (setq yaml--document-start-explicit nil)
                          (setq yaml--tag-map (make-hash-table))))
    ("c-flow-mapping" . ,(lambda ()
                           (yaml--mapping-start-event t)))
    ("c-flow-sequence" . ,(lambda ()
                            (yaml--sequence-start-event nil)))
    ("l+block-mapping" . ,(lambda ()
                            (yaml--mapping-start-event nil)))
    ("l+block-sequence" . ,(lambda ()
                             (yaml--sequence-start-event nil)))
    ("ns-l-compact-mapping" . ,(lambda ()
                                 (yaml--mapping-start-event nil)))
    ("ns-l-compact-sequence" . ,(lambda ()
                                  (yaml--sequence-start-event nil)))
    ("ns-flow-pair" . ,(lambda ()
                         (yaml--mapping-start-event t)))
    ("ns-l-block-map-implicit-entry" . ,(lambda ()))
    ("ns-l-compact-mapping" . ,(lambda ()))
    ("c-l-block-seq-entry" . ,(lambda ())))
  "List of functions for matched rules that run on the entering of a rule.")

(defconst yaml--grammar-events-out
  `(("c-b-block-header" .
     ,(lambda (_text)
        nil))
    ("l-yaml-stream" .
     ,(lambda (_text)
        (yaml--check-document-end)
        (yaml--stream-end-event)))
    ("ns-yaml-version" .
     ,(lambda (text)
        (when yaml--document-start-version
          (throw 'error "Multiple %YAML directives not allowed."))
        (setq yaml--document-start-version text)))
    ("c-tag-handle" .
     ,(lambda (text)
        (setq yaml--tag-handle text)))
    ("ns-tag-prefix" .
     ,(lambda (text)
        (puthash yaml--tag-handle text yaml--tag-map)))
    ("c-directives-end" .
     ,(lambda (_text)
        (yaml--check-document-end)
        (setq yaml--document-start-explicit t)))
    ("c-document-end" .
     ,(lambda (_text)
        (when (not yaml--document-end)
          (setq yaml--document-end-explicit t))
        (yaml--check-document-end)))
    ("c-flow-mapping" .
     ,(lambda (_text)
        (yaml--mapping-end-event)))
    ("c-flow-sequence" .
     ,(lambda (_text)
        (yaml--sequence-end-event)))
    ("l+block-mapping" .
     ,(lambda (_text)
        (yaml--mapping-end-event)))
    ("l+block-sequence" .
     ,(lambda (_text)
        (yaml--reverse-at-list)
        (yaml--sequence-end-event)))
    ("ns-l-compact-mapping" .
     ,(lambda (_text)
        (yaml--mapping-end-event)))
    ("ns-l-compact-sequence" .
     ,(lambda (_text)
        (yaml--sequence-end-event)))
    ("ns-flow-pair" .
     ,(lambda (_text)
        (yaml--mapping-end-event)))
    ("ns-plain" .
     ,(lambda (text)
        (let* ((replaced (if (and (zerop (length yaml--state-stack))
                                  (string-match "\\(^\\|\n\\)\\.\\.\\.\\'" text))
                             ;; Hack to not send the document parse end.
                             ;; Will only occur with bare ns-plain at top level.
                             (replace-regexp-in-string "\\(^\\|\n\\)\\.\\.\\.\\'"
                                                       ""
                                                       text)
                           text))
               (replaced (replace-regexp-in-string
                          "\\(?:[ \t]*\r?\n[ \t]*\\)"
                          "\n"
                          replaced))
               (replaced (replace-regexp-in-string
                          "\\(\n\\)\\(\n*\\)"
                          (lambda (x)
                            (if (> (length x) 1)
                                (substring x 1)
                              " "))
                          replaced)))
          (yaml--scalar-event "plain" replaced))))
    ("c-single-quoted" .
     ,(lambda (text)
        (let* ((replaced (replace-regexp-in-string
                          "\\(?:[ \t]*\r?\n[ \t]*\\)"
                          "\n"
                          text))
               (replaced (replace-regexp-in-string
                          "\\(\n\\)\\(\n*\\)"
                          (lambda (x)
                            (if (> (length x) 1)
                                (substring x 1)
                              " "))
                          replaced))
               (replaced (if (not (equal "''" replaced))
                             (replace-regexp-in-string
                              "''"
                              (lambda (x)
                                (if (> (length x) 1)
                                    (substring x 1)
                                  "'"))
                              replaced)
                           replaced)))
          (yaml--scalar-event "single"
                              (substring replaced 1 (1- (length replaced)))))))
    ("c-double-quoted" .
     ,(lambda (text)
        (let* ((replaced (replace-regexp-in-string
                          "\\(?:[ \t]*\r?\n[ \t]*\\)"
                          "\n"
                          text))
               (replaced (replace-regexp-in-string
                          "\\(\n\\)\\(\n*\\)"
                          (lambda (x)
                            (if (> (length x) 1)
                                (substring x 1)
                              " "))
                          replaced))
               (replaced (replace-regexp-in-string "\\\\\\([\"\\/]\\)"
                                                   "\\1"
                                                   replaced))
               (replaced (replace-regexp-in-string "\\\\ " " " replaced))
               (replaced (replace-regexp-in-string "\\\\ " " " replaced))
               (replaced (replace-regexp-in-string "\\\\b" "\b" replaced))
               (replaced (replace-regexp-in-string "\\\\t" "\t" replaced))
               (replaced (replace-regexp-in-string "\\\\n" "\n" replaced))
               (replaced (replace-regexp-in-string "\\\\r" "\r" replaced))
               (replaced (replace-regexp-in-string "\\\\r" "\r" replaced))
               (replaced (replace-regexp-in-string
                          "\\\\x\\([0-9a-fA-F]\\{2\\}\\)"
                          (lambda (x)
                            (let ((char-pt (substring 2 x)))
                              (string (string-to-number char-pt 16))))
                          replaced))
               (replaced (replace-regexp-in-string
                          "\\\\x\\([0-9a-fA-F]\\{2\\}\\)"
                          (lambda (x)
                            (let ((char-pt (substring x 2)))
                              (string (string-to-number char-pt 16))))
                          replaced))
               (replaced (replace-regexp-in-string
                          "\\\\x\\([0-9a-fA-F]\\{4\\}\\)"
                          (lambda (x)
                            (let ((char-pt (substring x 2)))
                              (string (string-to-number char-pt 16))))
                          replaced))
               (replaced (replace-regexp-in-string
                          "\\\\x\\([0-9a-fA-F]\\{8\\}\\)"
                          (lambda (x)
                            (let ((char-pt (substring x 2)))
                              (string (string-to-number char-pt 16))))
                          replaced))
               (replaced (replace-regexp-in-string
                          "\\\\\\\\"
                          "\\"
                          replaced))
               (replaced (substring replaced 1 (1- (length replaced)))))
          (yaml--scalar-event "double" replaced))))
    ("c-l+literal" .
     ,(lambda (text)
        (when (equal (car yaml--state-stack) :trail-comments)
          (pop yaml--state-stack)
          (let ((comment-text (pop yaml--object-stack)))
            (setq text (replace-regexp-in-string
                        (concat (regexp-quote comment-text) "\n*\\'") "" text))))
        (let* ((processed-text (yaml--process-literal-text text)))
          (yaml--scalar-event "folded" processed-text))))
    ("c-l+folded" .
     ,(lambda (text)
        (when (equal (car yaml--state-stack) :trail-comments)
          (pop yaml--state-stack)
          (let ((comment-text (pop yaml--object-stack)))
            (setq text (replace-regexp-in-string
                        (concat (regexp-quote comment-text) "\n*\\'") "" text))))
        (let* ((processed-text (yaml--process-folded-text text)))
          (yaml--scalar-event "folded" processed-text))))
    ("e-scalar" .
     ,(lambda (_text)
        (yaml--scalar-event "plain" "null")))
    ("c-ns-anchor-property" .
     ,(lambda (text)
        (yaml--anchor-event (substring text 1))))
    ("c-ns-tag-property" .
     ,(lambda (_text)
        ;; TODO: Implement tags
        nil))
    ("l-trail-comments" .
     ,(lambda (text)
        (yaml--trail-comments-event text)))
    ("c-ns-alias-node" .
     ,(lambda (text)
        (yaml--alias-event (substring text 1)))))
  "List of functions for matched rules that run on the exiting of a rule.")

(defconst yaml--terminal-rules
  '( "l-nb-literal-text"
     "l-nb-diff-lines"
     "ns-plain"
     "c-single-quoted"
     "c-double-quoted")
  "List of rules that indicate at which the parse tree should stop.

This addition is a hack to prevent the parse tree from going too deep and thus
risk hitting the stack depth limit.  Each of these rules are recursive and
repeat for each character in a text.")

(defun yaml--walk-events (tree)
  "Event walker iterates over the parse TREE and signals events from the rules."
  (when (consp tree)
    (if (stringp (car tree))
        (let ((grammar-rule (car tree))
              (text (cadr tree))
              (children (cl-caddr tree)))
          (let ((in-fn (cdr (assoc grammar-rule yaml--grammar-events-in)))
                (out-fn (cdr (assoc grammar-rule yaml--grammar-events-out))))
            (when in-fn
              (funcall in-fn))
            (yaml--walk-events children)
            (when out-fn
              (funcall out-fn text))))
      (yaml--walk-events (car tree))
      (yaml--walk-events (cdr tree)))))

(defmacro yaml--frame (name rule)
  "Add a new state frame of NAME for RULE."
  (declare (indent defun))
  (let ((res-symbol (make-symbol "res")))
    `(let ((beg yaml--parsing-position)
           (_ (when (and yaml--parse-debug
                         (not (member ,name yaml--tracing-ignore)))
                (message "|%s>%s %40s '%s'"
                         (make-string (length yaml--states) ?-)
                         (make-string (- 70 (length yaml--states)) ?\s)
                         ,name
                         (replace-regexp-in-string
                          "\n"
                          "↓"
                          (yaml--slice yaml--parsing-position)))))
           (_ (yaml--push-state ,name))
           (,res-symbol ,rule))
       (when (and yaml--parse-debug
                  ,res-symbol
                  (not (member ,name yaml--tracing-ignore)))
         (message "<%s|%s %40s = '%s'"
                  (make-string (length yaml--states) ?-)
                  (make-string (- 70 (length yaml--states)) ?\s)
                  ,name
                  (replace-regexp-in-string
                   "\n"
                   "↓"
                   (substring yaml--parsing-input beg yaml--parsing-position))))
       (yaml--pop-state)
       (if (not ,res-symbol)
           nil
         (let ((res-type (cdr (assoc ,name yaml--grammar-resolution-rules)))
               (,res-symbol (if (member ,name yaml--terminal-rules)
                                ;; Ignore children if at-rule is
                                ;; indicated to be terminal.
                                t
                              ,res-symbol)))
           (cond
            ((or (assoc ,name yaml--grammar-events-in)
                 (assoc ,name yaml--grammar-events-out))
             (let ((str (substring yaml--parsing-input beg yaml--parsing-position)))
               (when yaml--parsing-store-position
                 (setq str (propertize str 'yaml-position
                                       (cons (1+ beg)
                                             (1+ yaml--parsing-position)))))
               (when (member ,name '("c-l+folded" "c-l+literal"))
                 (setq str (propertize str 'yaml-n (max 0 n))))
               (list ,name
                     (if yaml--parsing-store-position
                         (propertize str 'yaml-position (cons (1+ beg)
                                                              (1+ yaml--parsing-position)))
                       str)
                     ,res-symbol)))
            ((equal res-type 'list) (list ,name ,res-symbol))
            ((equal res-type 'literal)
             (substring yaml--parsing-input beg yaml--parsing-position))
            (t ,res-symbol)))))))

(defun yaml--end-of-stream ()
  "Return non-nil if the current position is after the end of the document."
  (>= yaml--parsing-position (length yaml--parsing-input)))

(defun yaml--char-at-pos (pos)
  "Return the character at POS."
  (aref yaml--parsing-input pos))

(defun yaml--slice (pos)
  "Return the character at POS."
  (substring yaml--parsing-input pos))

(defun yaml--at-char ()
  "Return the current character."
  (yaml--char-at-pos yaml--parsing-position))

(defun yaml--char-match (at &rest chars)
  "Return non-nil if AT match any of CHARS."
  (if (not chars)
      nil
    (or (equal at (car chars))
        (apply #'yaml--char-match (cons at (cdr chars))))))

(defun yaml--chr (c)
  "Try to match the character C."
  (if (or (yaml--end-of-stream) (not (equal (yaml--at-char) c)))
      nil
    (setq yaml--parsing-position (1+ yaml--parsing-position))
    t))

(defun yaml--chr-range (min max)
  "Return non-nil if the current character is between MIN and MAX."
  (if (or (yaml--end-of-stream) (not (<= min (yaml--at-char) max)))
      nil
    (setq yaml--parsing-position (1+ yaml--parsing-position))
    t))

(defmacro yaml--all (&rest forms)
  "Pass and return all forms if all of FORMS pass."
  (let ((start-pos-sym (make-symbol "*start-pos*"))
        (results-sym (make-symbol "*results*"))
        (break-sym (make-symbol "*break*")))
    `(let* ((,start-pos-sym yaml--parsing-position)
            (,results-sym '())
            (,break-sym nil))
       ;; Try all FORMS until one returns nil.
       (catch 'break
         ,@(mapcar (lambda (form)
                     `(let ((res ,form))
                        (unless res
                          (setq ,break-sym t))
                        (push res ,results-sym)
                        (when ,break-sym
                          (throw 'break t))))
                   forms))
       (when ,break-sym
         (setq yaml--parsing-position ,start-pos-sym))
       (unless ,break-sym
         (nreverse ,results-sym)))))

(defmacro yaml--any (&rest forms)
  "Pass if any of FORMS pass."
  (if (= 1 (length forms))
      (car forms)
    (let ((start-pos-sym (make-symbol "start"))
          (rules-sym (make-symbol "rules"))
          (res-sym (make-symbol "res")))
      `(let ((,start-pos-sym yaml--parsing-position)
             (,rules-sym ,(cons 'list
                                (seq-map (lambda (form) `(lambda () ,form))
                                         forms)))
             (,res-sym))
         (while (and (not ,res-sym) ,rules-sym)
           (setq ,res-sym (funcall (car ,rules-sym)))
           (unless ,res-sym
             (setq yaml--parsing-position ,start-pos-sym))
           (setq ,rules-sym (cdr ,rules-sym)))
         ,res-sym))))

(defmacro yaml--exclude (_)
  "Set the excluded characters according to RULE.

This is currently unimplemented."
  ;; NOTE: This is currently not implemented.
  't)

(defmacro yaml--max (_)
  "Automatically pass."
  t)

(defun yaml--empty ()
  "Return non-nil indicating that empty rule needs nothing to pass."
  't)

(defun yaml--sub (a b)
  "Return A minus B."
  (- a b))

(defun yaml--match ()
  "Return the content of the previous sibling completed."
  (let* ((states yaml--states)
         (res nil))
    (while (and states (not res))
      (let ((top-state (car states)))
        (if (yaml--state-end top-state)
            (let ((beg (yaml--state-beg top-state))
                  (end (yaml--state-end top-state)))
              (setq res (substring yaml--parsing-input beg end)))
          (setq states (cdr states)))))
    res))

(defun yaml--auto-detect (n)
  "Detect the indentation given N."
  (let* ((slice (yaml--slice yaml--parsing-position))
         (match (string-match
                 "^.*\n\\(\\(?: *\n\\)*\\)\\( *\\)"
                 slice)))
    (if (not match)
        1
      (let ((pre (match-string 1 slice))
            (m (- (length (match-string 2 slice)) n)))
        (if (< m 1)
            1
          (when (string-match (format "^.\\{%d\\}." m) pre)
            (error "Spaces found after indent in auto-detect (5LLU)"))
          m)))))

(defun yaml--auto-detect-indent (n)
  "Detect the indentation given N."
  (let* ((pos yaml--parsing-position)
         (in-seq (and
                  (> pos 0)
                  (yaml--char-match (yaml--char-at-pos (1- pos)) ?\- ?\? ?\:)))
         (slice (yaml--slice pos))
         (_ (string-match
             "^\\(\\(?: *\\(?:#.*\\)?\n\\)*\\)\\( *\\)"
             slice))
         (pre (match-string 1 slice))
         (m (length (match-string 2 slice))))
    (if (and in-seq (= (length pre) 0))
        (when (= n -1)
          (setq m (1+ m)))
      (setq m (- m n)))
    (when (< m 0)
      (setq m 0))
    m))

(defun yaml--the-end ()
  "Return non-nil if at the end of input (?)."
  (or (>= yaml--parsing-position (length yaml--parsing-input))
      (and (yaml--state-curr-doc)
           (yaml--start-of-line)
           (string-match
            "\\^g(?:---|\\.\\.\\.\\)\\([[:blank:]]\\|$\\)"
            (substring yaml--parsing-input yaml--parsing-position)))))

(defmacro yaml--ord (&rest forms)
  "Convert an ASCII number returned by FORMS to a number."
  `(let ((res (progn ,@forms)))
     (- (aref res 0) 48)))

(defmacro yaml--but (&rest forms)
  "Match the first in FORMS but none of the others."
  `(if (yaml--the-end)
       nil
     (let ((pos1 yaml--parsing-position))
       (if (not ,(car forms))
           nil
         (let ((pos2 yaml--parsing-position))
           (setq yaml--parsing-position pos1)
           (if (equal :error (catch 'break
                               ,@(mapcar
                                  (lambda (form)
                                    `(when ,form
                                       (setq yaml--parsing-position pos1)
                                       (throw 'break :error)))
                                  (cdr forms))))
               nil
             (setq yaml--parsing-position pos2)
             t))))))

(defmacro yaml--rep (min max &rest body)
  "Repeat BODY between MIN and MAX times."
  (declare (indent 2))
  `(if (and ,max (< ,max 0))
       nil
     (let* ((res-list '())
            (count 0)
            (pos yaml--parsing-position)
            (pos-start pos)
            (break nil))
       (while (and (not break) (or (not ,max) (< count ,max)))
         (let ((res (progn ,@body)))
           (if (or (not res) (= yaml--parsing-position pos))
               (setq break t)
             (setq res-list (cons res res-list))
             (setq count (1+ count))
             (setq pos yaml--parsing-position))))
       (if (and (>= count ,min)
                (or (not ,max) (<= count ,max)))
           (progn
             (setq yaml--parsing-position pos)
             (if (zerop count)
                 t
               res-list))
         (setq yaml--parsing-position pos-start)
         nil))))

(defun yaml--start-of-line ()
  "Return non-nil if start of line."
  (or (= yaml--parsing-position 0)
      (>= yaml--parsing-position (length yaml--parsing-input))
      (equal (yaml--char-at-pos (1- yaml--parsing-position)) ?\n)))

(defun yaml--top ()
  "Perform top level YAML parsing rule."
  (yaml--parse-l-yaml-stream))

(defmacro yaml--set (variable value)
  "Set the current state of VARIABLE to VALUE."
  (let ((res-sym (make-symbol "res")))
    `(let ((,res-sym ,value))
       (when ,res-sym
         (,(cond ((equal "m" (symbol-name variable)) 'yaml--state-set-m)
                 ((equal "t" (symbol-name variable)) 'yaml--state-set-t))
          ,res-sym)
         ,res-sym))))

(defmacro yaml--chk (type expr)
  "Check if EXPR is non-nil at the parsing position.

If TYPE is \"<=\" then check at the previous position.  If TYPE
is \"!\" ensure that EXPR is nil.  Otherwise, if TYPE is \"=\"
then check EXPR at the current position."
  (let ((start-symbol (make-symbol "start"))
        (ok-symbol (make-symbol "ok")))
    `(let ((,start-symbol yaml--parsing-position)
           (_ (when (equal ,type "<=")
                (setq yaml--parsing-position (1- yaml--parsing-position))))
           (,ok-symbol (and (>= yaml--parsing-position 0) ,expr)))
       (setq yaml--parsing-position ,start-symbol)
       (if (equal ,type "!")
           (not ,ok-symbol)
         ,ok-symbol))))

(defun yaml--initialize-parsing-state (args)
  "Initialize state required for parsing according to plist ARGS."
  (setq yaml--cache nil)
  (setq yaml--object-stack nil)
  (setq yaml--state-stack nil)
  (setq yaml--root nil)
  (setq yaml--anchor-mappings (make-hash-table :test 'equal))
  (setq yaml--resolve-aliases nil)
  (setq yaml--parsing-null-object
	    (if (plist-member args :null-object)
	        (plist-get args :null-object)
	      :null))
  (setq yaml--parsing-false-object
	    (if (plist-member args :false-object)
	        (plist-get args :false-object)
	      :false))
  (let ((object-type (plist-get args :object-type))
        (object-key-type (plist-get args :object-key-type))
        (sequence-type (plist-get args :sequence-type))
        (string-values (plist-get args :string-values)))
    (cond
     ((or (not object-type)
          (equal object-type 'hash-table))
      (setq yaml--parsing-object-type 'hash-table))
     ((equal 'alist object-type)
      (setq yaml--parsing-object-type 'alist))
     ((equal 'plist object-type)
      (setq yaml--parsing-object-type 'plist))
     (t (error "Invalid object-type.  Must be hash-table, alist, or plist")))
    (cond
     ((or (not object-key-type)
          (equal 'symbol object-key-type))
      (if (equal 'plist yaml--parsing-object-type)
          (setq yaml--parsing-object-key-type 'keyword)
        (setq yaml--parsing-object-key-type 'symbol)))
     ((equal 'string object-key-type)
      (setq yaml--parsing-object-key-type 'string))
     ((equal 'keyword object-key-type)
      (setq yaml--parsing-object-key-type 'keyword))
     (t (error "Invalid object-key-type.  Must be string, keyword, or symbol")))
    (cond
     ((or (not sequence-type)
          (equal sequence-type 'array))
      (setq yaml--parsing-sequence-type 'array))
     ((equal 'list sequence-type)
      (setq yaml--parsing-sequence-type 'list))
     (t (error "Invalid sequence-type.  sequence-type must be list or array")))
    (if string-values
        (setq yaml--string-values t)
      (setq yaml--string-values nil))))

(defun yaml-parse-string (string &rest args)
  "Parse the YAML value in STRING.  Keyword ARGS are as follows:

OBJECT-TYPE specifies the Lisp object to use for representing
key-value YAML mappings.  Possible values for OBJECT-TYPE are
the symbols `hash-table' (default), `alist', and `plist'.

OBJECT-KEY-TYPE specifies the Lisp type to use for keys in
key-value YAML mappings.  Possible values are the symbols
`string', `symbol', and `keyword'.  By default, this is `symbol';
if OBJECT-TYPE is `plist', the default is `keyword' (and `symbol'
becomes synonym for `keyword').

SEQUENCE-TYPE specifies the Lisp object to use for representing
YAML sequences.  Possible values for SEQUENCE-TYPE are the symbols
`list', and `array' (default).

NULL-OBJECT contains the object used to represent the null value.
It defaults to the symbol `:null'.

FALSE-OBJECT contains the object used to represent the false
value.  It defaults to the symbol `:false'."
  (yaml--initialize-parsing-state args)
  (let ((res (yaml--parse string
               (yaml--top))))
    (when (< yaml--parsing-position (length yaml--parsing-input))
      (error
       "Unable to parse YAML.  Parser finished before end of input %s/%s"
       yaml--parsing-position
       (length yaml--parsing-input)))
    (when yaml--parse-debug (message "Parsed data: %s" (pp-to-string res)))
    (yaml--walk-events res)
    (if (hash-table-empty-p yaml--anchor-mappings)
        yaml--root
      ;; Run event processing twice to resolve aliases.
      (let ((yaml--root nil)
            (yaml--resolve-aliases t))
        (yaml--walk-events res)
        yaml--root))))

(defun yaml-parse-tree (string)
  "Parse the YAML value in STRING and return its parse tree."
  (yaml--initialize-parsing-state nil)
  (let* ((yaml--parsing-store-position t)
         (res (yaml--parse string
                (yaml--top))))
    (when (< yaml--parsing-position (length yaml--parsing-input))
      (error
       "Unable to parse YAML.  Parser finished before end of input %s/%s"
       yaml--parsing-position
       (length yaml--parsing-input)))
    res))

(defun yaml-parse-string-with-pos (string)
  "Parse the YAML value in STRING, storing positions as text properties.

NOTE: This is an experimental feature and may experience API
changes in the future."
  (let ((yaml--parsing-store-position t))
    (yaml-parse-string string
                       :object-type 'alist
                       :object-key-type 'string
                       :string-values t)))

;;; YAML grammar functions for each state. Each function is a spec
;;; defined by the yaml-spec JSON file.

(defun yaml--parse-c-flow-sequence (n c)
  (yaml--frame "c-flow-sequence"
    (yaml--all
     (yaml--chr ?\[)
     (yaml--rep 0 1
       (yaml--parse-s-separate n c))
     (yaml--rep 0 1
       (yaml--parse-ns-s-flow-seq-entries
        n
        (yaml--parse-in-flow c)))
     (yaml--chr ?\]))))

(defun yaml--parse-c-indentation-indicator (m)
  (yaml--frame "c-indentation-indicator"
    (yaml--any (when (yaml--parse-ns-dec-digit)
                 (yaml--set m (yaml--ord (yaml--match))) t)
               (when (yaml--empty)
                 (let ((new-m (yaml--auto-detect m)))
                   (yaml--set m new-m))
                 t))))

(defun yaml--parse-ns-reserved-directive ()
  (yaml--frame "ns-reserved-directive"
    (yaml--all (yaml--parse-ns-directive-name)
               (yaml--rep 0 nil
                 (yaml--all
                  (yaml--parse-s-separate-in-line)
                  (yaml--parse-ns-directive-parameter))))))

(defun yaml--parse-ns-flow-map-implicit-entry (n c)
  (yaml--frame "ns-flow-map-implicit-entry"
    ;; NOTE: I ran into a bug with the order of these rules. It seems
    ;; sometimes ns-flow-map-yaml-key-entry succeeds with an empty
    ;; when the correct answer should be
    ;; c-ns-flow-map-json-key-entry.  Changing the order seemed to
    ;; have fix this but this seems like a bandage fix.
    (yaml--any
     (yaml--parse-c-ns-flow-map-json-key-entry n c)
     (yaml--parse-ns-flow-map-yaml-key-entry n c)
     (yaml--parse-c-ns-flow-map-empty-key-entry n c))))

(defun yaml--parse-ns-esc-double-quote ()
  (yaml--frame "ns-esc-double-quote"
    (yaml--chr ?\")))

(defun yaml--parse-c-mapping-start ()
  (yaml--frame "c-mapping-start"
    (yaml--chr ?\{)))

(defun yaml--parse-ns-flow-seq-entry (n c)
  (yaml--frame "ns-flow-seq-entry"
    (yaml--any (yaml--parse-ns-flow-pair n c)
               (yaml--parse-ns-flow-node n c))))

(defun yaml--parse-l-empty (n c)
  (yaml--frame "l-empty"
    (yaml--all (yaml--any (yaml--parse-s-line-prefix n c)
                          (yaml--parse-s-indent-lt n))
               (yaml--parse-b-as-line-feed))))

(defun yaml--parse-c-primary-tag-handle ()
  (yaml--frame "c-primary-tag-handle"
    (yaml--chr ?\!)))

(defun yaml--parse-ns-plain-safe-out ()
 (yaml--frame "ns-plain-safe-out"
   (yaml--parse-ns-char)))

(defun yaml--parse-c-ns-shorthand-tag ()
 (yaml--frame "c-ns-shorthand-tag"
   (yaml--all
    (yaml--parse-c-tag-handle)
    (yaml--rep 1 nil
      (yaml--parse-ns-tag-char)))))


(defun yaml--parse-nb-ns-single-in-line ()
  (yaml--frame "nb-ns-single-in-line"
    (yaml--rep 0 nil
      (yaml--all (yaml--rep 0 nil
                   (yaml--parse-s-white))
                 (yaml--parse-ns-single-char)))))

(defun yaml--parse-l-strip-empty (n)
  (yaml--frame "l-strip-empty"
    (yaml--all
     (yaml--rep 0 nil
       (yaml--all
        (yaml--parse-s-indent-le n)
        (yaml--parse-b-non-content)))
     (yaml--rep 0 1
       (yaml--parse-l-trail-comments n)))))

(defun yaml--parse-c-indicator ()
  (yaml--frame "c-indicator"
    (yaml--any (yaml--chr ?\-)
               (yaml--chr ?\?)
               (yaml--chr ?\:)
               (yaml--chr ?\,)
               (yaml--chr ?\[)
               (yaml--chr ?\])
               (yaml--chr ?\{)
               (yaml--chr ?\})
               (yaml--chr ?\#)
               (yaml--chr ?\&)
               (yaml--chr ?\*)
               (yaml--chr ?\!)
               (yaml--chr ?\|)
               (yaml--chr ?\>)
               (yaml--chr ?\')
               (yaml--chr ?\")
               (yaml--chr ?\%)
               (yaml--chr ?\@)
               (yaml--chr ?\`))))

(defun yaml--parse-c-l+literal (n)
  (yaml--frame "c-l+literal"
    (progn
      (yaml--all
       (yaml--chr ?\|)
       (yaml--parse-c-b-block-header n (yaml--state-curr-t))
       (yaml--parse-l-literal-content
        (max (+ n (yaml--state-curr-m)) 1)
        (yaml--state-curr-t))))))

(defun yaml--parse-c-single-quoted (n c)
  (yaml--frame "c-single-quoted"
    (yaml--all (yaml--chr ?\')
               (yaml--parse-nb-single-text n c)
               (yaml--chr ?\'))))

(defun yaml--parse-c-forbidden ()
  (yaml--frame "c-forbidden"
    (yaml--all (yaml--start-of-line)
               (yaml--any
                (yaml--parse-c-directives-end)
                (yaml--parse-c-document-end))
               (yaml--any
                (yaml--parse-b-char)
                (yaml--parse-s-white)
                (yaml--end-of-stream)))))

(defun yaml--parse-c-ns-alias-node ()
  (yaml--frame "c-ns-alias-node"
    (yaml--all (yaml--chr ?\*)
               (yaml--parse-ns-anchor-name))))

(defun yaml--parse-c-secondary-tag-handle ()
 (yaml--frame "c-secondary-tag-handle"
   (yaml--all (yaml--chr ?\!) (yaml--chr ?\!))))

(defun yaml--parse-ns-esc-next-line ()
 (yaml--frame "ns-esc-next-line" (yaml--chr ?N)))

(defun yaml--parse-l-nb-same-lines (n)
  (yaml--frame "l-nb-same-lines"
    (yaml--all
     (yaml--rep 0 nil
       (yaml--parse-l-empty n "block-in"))
     (yaml--any (yaml--parse-l-nb-folded-lines n)
                (yaml--parse-l-nb-spaced-lines n)))))

(defun yaml--parse-c-alias ()
 (yaml--frame "c-alias" (yaml--chr ?\*)))

(defun yaml--parse-ns-single-char ()
 (yaml--frame "ns-single-char"
   (yaml--but (yaml--parse-nb-single-char)
              (yaml--parse-s-white))))

(defun yaml--parse-c-l-block-map-implicit-value (n)
  (yaml--frame "c-l-block-map-implicit-value"
    (yaml--all (yaml--chr ?\:)
               (yaml--any
                (yaml--parse-s-l+block-node n "block-out")
                (yaml--all (yaml--parse-e-node)
                           (yaml--parse-s-l-comments))))))

(defun yaml--parse-ns-uri-char ()
  (yaml--frame "ns-uri-char"
    (yaml--any (yaml--all (yaml--chr ?\%)
                          (yaml--parse-ns-hex-digit)
                          (yaml--parse-ns-hex-digit))
               (yaml--parse-ns-word-char)
               (yaml--chr ?\#)
               (yaml--chr ?\;)
               (yaml--chr ?\/)
               (yaml--chr ?\?)
               (yaml--chr ?\:)
               (yaml--chr ?\@)
               (yaml--chr ?\&)
               (yaml--chr ?\=)
               (yaml--chr ?\+)
               (yaml--chr ?\$)
               (yaml--chr ?\,)
               (yaml--chr ?\_)
               (yaml--chr ?\.)
               (yaml--chr ?\!)
               (yaml--chr ?\~)
               (yaml--chr ?\*)
               (yaml--chr ?\')
               (yaml--chr ?\()
               (yaml--chr ?\))
               (yaml--chr ?\[)
               (yaml--chr ?\]))))

(defun yaml--parse-ns-esc-16-bit ()
  (yaml--frame "ns-esc-16-bit"
    (yaml--all (yaml--chr ?u)
               (yaml--rep 4 4
                 (yaml--parse-ns-hex-digit)))))

(defun yaml--parse-l-nb-spaced-lines (n)
  (yaml--frame "l-nb-spaced-lines"
    (yaml--all
     (yaml--parse-s-nb-spaced-text n)
     (yaml--rep 0 nil
       (yaml--all
        (yaml--parse-b-l-spaced n)
        (yaml--parse-s-nb-spaced-text n))))))

(defun yaml--parse-ns-plain (n c)
  (yaml--frame "ns-plain"
    (pcase c
      ("block-key" (yaml--parse-ns-plain-one-line c))
      ("flow-in" (yaml--parse-ns-plain-multi-line n c))
      ("flow-key" (yaml--parse-ns-plain-one-line c))
      ("flow-out" (yaml--parse-ns-plain-multi-line n c)))))

(defun yaml--parse-c-printable ()
  (yaml--frame "c-printable"
    (yaml--any (yaml--chr ?\x09)
               (yaml--chr ?\x0A)
               (yaml--chr ?\x0D)
               (yaml--chr-range ?\x20 ?\x7E)
               (yaml--chr ?\x85)
               (yaml--chr-range ?\xA0 ?\xD7FF)
               (yaml--chr-range ?\xE000 ?\xFFFD)
               (yaml--chr-range ?\x010000 ?\x10FFFF))))

(defun yaml--parse-c-mapping-value ()
  (yaml--frame "c-mapping-value" (yaml--chr ?\:)))

(defun yaml--parse-l-nb-literal-text (n)
  (yaml--frame "l-nb-literal-text"
    (yaml--all
     (yaml--rep 0 nil
       (yaml--parse-l-empty n "block-in"))
     (yaml--parse-s-indent n)
     (yaml--rep 1 nil
       (yaml--parse-nb-char)))))

(defun yaml--parse-ns-plain-char (c)
  (yaml--frame "ns-plain-char"
    (yaml--any
     (yaml--but
      (yaml--parse-ns-plain-safe c)
      (yaml--chr ?\:)
      (yaml--chr ?\#))
     (yaml--all
      (yaml--chk "<=" (yaml--parse-ns-char))
      (yaml--chr ?\#))
     (yaml--all
      (yaml--chr ?\:)
      (yaml--chk "=" (yaml--parse-ns-plain-safe c))))))

(defun yaml--parse-ns-anchor-char ()
 (yaml--frame "ns-anchor-char"
   (yaml--but (yaml--parse-ns-char)
              (yaml--parse-c-flow-indicator))))

(defun yaml--parse-s-l+block-scalar (n c)
  (yaml--frame "s-l+block-scalar"
    (yaml--all (yaml--parse-s-separate (1+ n) c)
               (yaml--rep 0 1
                 (yaml--all
                  (yaml--parse-c-ns-properties (1+ n) c)
                  (yaml--parse-s-separate (1+ n) c)))
               (yaml--any (yaml--parse-c-l+literal n)
                          (yaml--parse-c-l+folded n)))))

(defun yaml--parse-ns-plain-safe-in ()
 (yaml--frame "ns-plain-safe-in"
   (yaml--but (yaml--parse-ns-char)
              (yaml--parse-c-flow-indicator))))

(defun yaml--parse-nb-single-text (n c)
  (yaml--frame "nb-single-text"
    (pcase c
      ("block-key" (yaml--parse-nb-single-one-line))
      ("flow-in" (yaml--parse-nb-single-multi-line n))
      ("flow-key" (yaml--parse-nb-single-one-line))
      ("flow-out" (yaml--parse-nb-single-multi-line n)))))

(defun yaml--parse-s-indent-le (n)
  (yaml--frame "s-indent-le"
    (yaml--all (yaml--rep 0 nil
                 (yaml--parse-s-space))
               (<= (length (yaml--match)) n))))

(defun yaml--parse-ns-esc-carriage-return ()
  (yaml--frame "ns-esc-carriage-return"
    (yaml--chr ?r)))

(defun yaml--parse-l-chomped-empty (n tt)
  (yaml--frame "l-chomped-empty"
    (pcase tt
      ("clip" (yaml--parse-l-strip-empty n))
      ("keep" (yaml--parse-l-keep-empty n))
      ("strip" (yaml--parse-l-strip-empty n)))))

(defun yaml--parse-c-s-implicit-json-key (c)
  (yaml--frame "c-s-implicit-json-key"
    (yaml--all
     (yaml--max 1024)
     (yaml--parse-c-flow-json-node nil c)
     (yaml--rep 0 1
       (yaml--parse-s-separate-in-line)))))

(defun yaml--parse-b-as-space ()
 (yaml--frame "b-as-space"
   (yaml--parse-b-break)))

(defun yaml--parse-ns-s-flow-seq-entries (n c)
  (yaml--frame "ns-s-flow-seq-entries"
    (yaml--all
     (yaml--parse-ns-flow-seq-entry n c)
     (yaml--rep 0 1
       (yaml--parse-s-separate n c))
     (yaml--rep 0 1
       (yaml--all
        (yaml--chr ?\,)
        (yaml--rep 0 1
          (yaml--parse-s-separate n c))
        (yaml--rep 0 1
          (yaml--parse-ns-s-flow-seq-entries n c)))))))

(defun yaml--parse-l-block-map-explicit-value (n)
  (yaml--frame "l-block-map-explicit-value"
    (yaml--all
     (yaml--parse-s-indent n)
     (yaml--chr ?\:)
     (yaml--parse-s-l+block-indented n "block-out"))))

(defun yaml--parse-c-ns-flow-map-json-key-entry (n c)
  (yaml--frame "c-ns-flow-map-json-key-entry"
    (yaml--all
     (yaml--parse-c-flow-json-node n c)
     (yaml--any
      (yaml--all
       (yaml--rep 0 1
         (yaml--parse-s-separate n c))
       (yaml--parse-c-ns-flow-map-adjacent-value n c))
      (yaml--parse-e-node)))))

(defun yaml--parse-c-sequence-entry ()
  (yaml--frame "c-sequence-entry"
    (yaml--chr ?\-)))

(defun yaml--parse-l-bare-document ()
 (yaml--frame "l-bare-document"
   (yaml--all (yaml--exclude "c-forbidden")
              (yaml--parse-s-l+block-node -1 "block-in"))))

;; TODO: don't use the symbol t as a variable.
(defun yaml--parse-b-chomped-last (tt)
  (yaml--frame "b-chomped-last"
    (pcase tt
      ("clip"
       ;; TODO: Fix this
       (yaml--any (yaml--parse-b-as-line-feed)
                  (yaml--end-of-stream)))
      ("keep"
       (yaml--any (yaml--parse-b-as-line-feed)
                  (yaml--end-of-stream)))
      ("strip"
       (yaml--any (yaml--parse-b-non-content)
                  (yaml--end-of-stream))))))

(defun yaml--parse-l-trail-comments (n)
  (yaml--frame "l-trail-comments"
    (yaml--all (yaml--parse-s-indent-lt n)
               (yaml--parse-c-nb-comment-text)
               (yaml--parse-b-comment)
               (yaml--rep 0 nil
                 (yaml--parse-l-comment)))))

(defun yaml--parse-ns-flow-map-yaml-key-entry (n c)
  (yaml--frame "ns-flow-map-yaml-key-entry"
    (yaml--all
     (yaml--parse-ns-flow-yaml-node n c)
     (yaml--any
      (yaml--all
       (yaml--rep 0 1
         (yaml--parse-s-separate n c))
       (yaml--parse-c-ns-flow-map-separate-value n c))
      (yaml--parse-e-node)))))

(defun yaml--parse-s-indent (n)
  (yaml--frame "s-indent"
    (yaml--rep n n (yaml--parse-s-space))))

(defun yaml--parse-ns-esc-line-separator ()
 (yaml--frame "ns-esc-line-separator" (yaml--chr ?L)))

(defun yaml--parse-ns-flow-yaml-node (n c)
  (yaml--frame "ns-flow-yaml-node"
    (yaml--any
     (yaml--parse-c-ns-alias-node)
     (yaml--parse-ns-flow-yaml-content n c)
     (yaml--all
      (yaml--parse-c-ns-properties n c)
      (yaml--any
       (yaml--all
        (yaml--parse-s-separate n c)
        (yaml--parse-ns-flow-yaml-content n c))
       (yaml--parse-e-scalar))))))

(defun yaml--parse-ns-yaml-version ()
 (yaml--frame "ns-yaml-version"
   (yaml--all (yaml--rep 1 nil
                (yaml--parse-ns-dec-digit))
              (yaml--chr ?\.)
              (yaml--rep 1 nil
                (yaml--parse-ns-dec-digit)))))

(defun yaml--parse-c-folded ()
 (yaml--frame "c-folded" (yaml--chr ?\>)))

(defun yaml--parse-c-directives-end ()
 (yaml--frame "c-directives-end"
   (yaml--all (yaml--chr ?\-) (yaml--chr ?\-) (yaml--chr ?\-))))

(defun yaml--parse-s-double-break (n)
  (yaml--frame "s-double-break"
    (yaml--any (yaml--parse-s-double-escaped n)
               (yaml--parse-s-flow-folded n))))

(defun yaml--parse-s-nb-spaced-text (n)
  (yaml--frame "s-nb-spaced-text"
    (yaml--all (yaml--parse-s-indent n)
               (yaml--parse-s-white)
               (yaml--rep 0 nil
                 (yaml--parse-nb-char)))))

(defun yaml--parse-l-folded-content (n tt)
  (yaml--frame "l-folded-content"
    (yaml--all
     (yaml--rep 0 1
       (yaml--all (yaml--parse-l-nb-diff-lines n)
                  (yaml--parse-b-chomped-last tt)))
     (yaml--parse-l-chomped-empty n tt))))

(defun yaml--parse-nb-ns-plain-in-line (c)
  (yaml--frame "nb-ns-plain-in-line"
    (yaml--rep 0 nil
      (yaml--all
       (yaml--rep 0 nil
         (yaml--parse-s-white))
       (yaml--parse-ns-plain-char c)))))

(defun yaml--parse-nb-single-multi-line (n)
  (yaml--frame "nb-single-multi-line"
    (yaml--all
     (yaml--parse-nb-ns-single-in-line)
     (yaml--any
      (yaml--parse-s-single-next-line n)
      (yaml--rep 0 nil
        (yaml--parse-s-white))))))

(defun yaml--parse-l-document-suffix ()
 (yaml--frame "l-document-suffix"
   (yaml--all (yaml--parse-c-document-end)
              (yaml--parse-s-l-comments))))

(defun yaml--parse-c-sequence-start ()
 (yaml--frame "c-sequence-start"
   (yaml--chr ?\[)))

(defun yaml--parse-ns-l-block-map-entry (n)
 (yaml--frame "ns-l-block-map-entry"
   (yaml--any
    (yaml--parse-c-l-block-map-explicit-entry n)
    (yaml--parse-ns-l-block-map-implicit-entry n))))

(defun yaml--parse-ns-l-compact-mapping (n)
 (yaml--frame "ns-l-compact-mapping"
   (yaml--all
    (yaml--parse-ns-l-block-map-entry n)
    (yaml--rep 0 nil
      (yaml--all
       (yaml--parse-s-indent n)
       (yaml--parse-ns-l-block-map-entry n))))))

(defun yaml--parse-ns-esc-space ()
 (yaml--frame "ns-esc-space" (yaml--chr ?\x20)))
(defun yaml--parse-ns-esc-vertical-tab ()
 (yaml--frame "ns-esc-vertical-tab" (yaml--chr ?v)))

(defun yaml--parse-ns-s-implicit-yaml-key (c)
  (yaml--frame "ns-s-implicit-yaml-key"
    (yaml--all
     (yaml--max 1024)
     (yaml--parse-ns-flow-yaml-node nil c)
     (yaml--rep 0 1
       (yaml--parse-s-separate-in-line)))))

(defun yaml--parse-b-l-folded (n c)
  (yaml--frame "b-l-folded"
    (yaml--any (yaml--parse-b-l-trimmed n c)
               (yaml--parse-b-as-space))))

(defun yaml--parse-s-l+block-collection (n c)
  (yaml--frame "s-l+block-collection"
    (yaml--all
     (yaml--rep 0 1
       (yaml--all
        (yaml--parse-s-separate (1+ n) c)
        (yaml--parse-c-ns-properties (1+ n) c)))
     (yaml--parse-s-l-comments)
     (yaml--any
      (yaml--parse-l+block-sequence
       (yaml--parse-seq-spaces n c))
      (yaml--parse-l+block-mapping n)))))

(defun yaml--parse-c-quoted-quote ()
  (yaml--frame "c-quoted-quote" (yaml--all (yaml--chr ?\') (yaml--chr ?\'))))

(defun yaml--parse-l+block-sequence (n)
 (yaml--frame "l+block-sequence"
   ;; NOTE: deviated from the spec example here by making new-m at least 1.
   ;; The wording and examples lead me to believe this is how it's done.
   ;; ie /* For some fixed auto-detected m > 0 */
   (let ((new-m (max (yaml--auto-detect-indent n) 1)))
     (yaml--all
      (yaml--set m new-m)
      (yaml--rep 1 nil
        (yaml--all
         (yaml--parse-s-indent (+ n new-m))
         (yaml--parse-c-l-block-seq-entry (+ n new-m))))))))

(defun yaml--parse-c-double-quote ()
 (yaml--frame "c-double-quote"
   (yaml--chr ?\")))

(defun yaml--parse-ns-esc-backspace ()
 (yaml--frame "ns-esc-backspace"
   (yaml--chr ?b)))

(defun yaml--parse-c-flow-json-content (n c)
  (yaml--frame "c-flow-json-content"
    (yaml--any (yaml--parse-c-flow-sequence n c)
               (yaml--parse-c-flow-mapping n c)
               (yaml--parse-c-single-quoted n c)
               (yaml--parse-c-double-quoted n c))))

(defun yaml--parse-c-mapping-end ()
 (yaml--frame "c-mapping-end" (yaml--chr ?\})))

(defun yaml--parse-nb-single-char ()
 (yaml--frame "nb-single-char"
   (yaml--any (yaml--parse-c-quoted-quote)
              (yaml--but (yaml--parse-nb-json)
                         (yaml--chr ?\')))))

(defun yaml--parse-ns-flow-node (n c)
  (yaml--frame "ns-flow-node"
    (yaml--any
     (yaml--parse-c-ns-alias-node)
     (yaml--parse-ns-flow-content n c)
     (yaml--all
      (yaml--parse-c-ns-properties n c)
      (yaml--any
       (yaml--all (yaml--parse-s-separate n c)
                  (yaml--parse-ns-flow-content n c))
       (yaml--parse-e-scalar))))))

(defun yaml--parse-c-non-specific-tag ()
 (yaml--frame "c-non-specific-tag" (yaml--chr ?\!)))

(defun yaml--parse-l-directive-document ()
 (yaml--frame "l-directive-document"
   (yaml--all (yaml--rep 1 nil
                (yaml--parse-l-directive))
              (yaml--parse-l-explicit-document))))

(defun yaml--parse-c-l-block-map-explicit-entry (n)
  (yaml--frame "c-l-block-map-explicit-entry"
    (yaml--all
     (yaml--parse-c-l-block-map-explicit-key n)
     (yaml--any (yaml--parse-l-block-map-explicit-value n)
                (yaml--parse-e-node)))))

(defun yaml--parse-e-node ()
 (yaml--frame "e-node"
   (yaml--parse-e-scalar)))

(defun yaml--parse-seq-spaces (n c)
  (yaml--frame "seq-spaces"
    (pcase c
      ("block-in" n)
      ("block-out" (yaml--sub n 1)))))

(defun yaml--parse-l-yaml-stream ()
 (yaml--frame "l-yaml-stream"
   (yaml--all
    (yaml--rep 0 nil
      (yaml--parse-l-document-prefix))
    (yaml--rep 0 1
      (yaml--parse-l-any-document))
    (yaml--rep 0 nil
      (yaml--any
       (yaml--all
        (yaml--rep 1 nil
          (yaml--parse-l-document-suffix))
        (yaml--rep 0 nil
          (yaml--parse-l-document-prefix))
        (yaml--rep 0 1
          (yaml--parse-l-any-document)))
       (yaml--all
        (yaml--rep 0 nil
          (yaml--parse-l-document-prefix))
        (yaml--rep 0 1
          (yaml--parse-l-explicit-document))))))))

(defun yaml--parse-nb-double-one-line ()
 (yaml--frame "nb-double-one-line"
   (yaml--rep 0 nil
     (yaml--parse-nb-double-char))))

(defun yaml--parse-s-l-comments ()
 (yaml--frame "s-l-comments"
   (yaml--all (yaml--any
               (yaml--parse-s-b-comment)
               (yaml--start-of-line))
              (yaml--rep 0 nil
                (yaml--parse-l-comment)))))

(defun yaml--parse-nb-char ()
 (yaml--frame "nb-char"
   (yaml--but (yaml--parse-c-printable)
              (yaml--parse-b-char)
              (yaml--parse-c-byte-order-mark))))

(defun yaml--parse-ns-plain-first (c)
  (yaml--frame "ns-plain-first"
    (yaml--any
     (yaml--but (yaml--parse-ns-char)
                (yaml--parse-c-indicator))
     (yaml--all
      (yaml--any (yaml--chr ?\?)
                 (yaml--chr ?\:)
                 (yaml--chr ?\-))
      (yaml--chk "=" (yaml--parse-ns-plain-safe c))))))

(defun yaml--parse-c-ns-esc-char ()
  (yaml--frame "c-ns-esc-char"
    (yaml--all
     (yaml--chr ?\\)
     (yaml--any (yaml--parse-ns-esc-null)
                (yaml--parse-ns-esc-bell)
                (yaml--parse-ns-esc-backspace)
                (yaml--parse-ns-esc-horizontal-tab)
                (yaml--parse-ns-esc-line-feed)
                (yaml--parse-ns-esc-vertical-tab)
                (yaml--parse-ns-esc-form-feed)
                (yaml--parse-ns-esc-carriage-return)
                (yaml--parse-ns-esc-escape)
                (yaml--parse-ns-esc-space)
                (yaml--parse-ns-esc-double-quote)
                (yaml--parse-ns-esc-slash)
                (yaml--parse-ns-esc-backslash)
                (yaml--parse-ns-esc-next-line)
                (yaml--parse-ns-esc-non-breaking-space)
                (yaml--parse-ns-esc-line-separator)
                (yaml--parse-ns-esc-paragraph-separator)
                (yaml--parse-ns-esc-8-bit)
                (yaml--parse-ns-esc-16-bit)
                (yaml--parse-ns-esc-32-bit)))))

(defun yaml--parse-ns-flow-map-entry (n c)
  (yaml--frame "ns-flow-map-entry"
    (yaml--any
     (yaml--all (yaml--chr ?\?)
                (yaml--parse-s-separate n c)
                (yaml--parse-ns-flow-map-explicit-entry n c))
     (yaml--parse-ns-flow-map-implicit-entry n c))))

(defun yaml--parse-l-explicit-document ()
 (yaml--frame "l-explicit-document"
   (yaml--all
    (yaml--parse-c-directives-end)
    (yaml--any (yaml--parse-l-bare-document)
               (yaml--all (yaml--parse-e-node)
                          (yaml--parse-s-l-comments))))))

(defun yaml--parse-s-white ()
 (yaml--frame "s-white"
   (yaml--any (yaml--parse-s-space)
              (yaml--parse-s-tab))))

(defun yaml--parse-l-keep-empty (n)
  (yaml--frame "l-keep-empty"
    (yaml--all
     (yaml--rep 0 nil
       (yaml--parse-l-empty n "block-in"))
     (yaml--rep 0 1
       (yaml--parse-l-trail-comments n)))))

(defun yaml--parse-ns-tag-prefix ()
 (yaml--frame "ns-tag-prefix"
   (yaml--any (yaml--parse-c-ns-local-tag-prefix)
              (yaml--parse-ns-global-tag-prefix))))

(defun yaml--parse-c-l+folded (n)
  (yaml--frame "c-l+folded"
    (yaml--all
     (yaml--chr ?\>)
     (yaml--parse-c-b-block-header n (yaml--state-curr-t))
     (yaml--parse-l-folded-content
      (max (+ n (yaml--state-curr-m)) 1)
      (yaml--state-curr-t)))))

(defun yaml--parse-ns-directive-name ()
 (yaml--frame "ns-directive-name"
   (yaml--rep 1 nil (yaml--parse-ns-char))))

(defun yaml--parse-b-char ()
 (yaml--frame "b-char"
   (yaml--any (yaml--parse-b-line-feed)
              (yaml--parse-b-carriage-return))))

(defun yaml--parse-ns-plain-multi-line (n c)
  (yaml--frame "ns-plain-multi-line"
    (yaml--all
     (yaml--parse-ns-plain-one-line c)
     (yaml--rep 0 nil
       (yaml--parse-s-ns-plain-next-line n c)))))

(defun yaml--parse-ns-char ()
 (yaml--frame "ns-char"
   (yaml--but (yaml--parse-nb-char)
              (yaml--parse-s-white))))

(defun yaml--parse-s-space ()
 (yaml--frame "s-space" (yaml--chr ?\x20)))

(defun yaml--parse-c-l-block-seq-entry (n)
  (yaml--frame "c-l-block-seq-entry"
    (yaml--all (yaml--chr ?\-)
               (yaml--chk "!" (yaml--parse-ns-char))
               (yaml--parse-s-l+block-indented n "block-in"))))

(defun yaml--parse-c-ns-properties (n c)
  (yaml--frame "c-ns-properties"
    (yaml--any
     (yaml--all
      (yaml--parse-c-ns-tag-property)
      (yaml--rep 0 1
        (yaml--all
         (yaml--parse-s-separate n c)
         (yaml--parse-c-ns-anchor-property))))
     (yaml--all
      (yaml--parse-c-ns-anchor-property)
      (yaml--rep 0 1
        (yaml--all
         (yaml--parse-s-separate n c)
         (yaml--parse-c-ns-tag-property)))))))

(defun yaml--parse-ns-directive-parameter ()
 (yaml--frame "ns-directive-parameter"
   (yaml--rep 1 nil (yaml--parse-ns-char))))

;; FIXME: is TT supposed to be used here?
(defun yaml--parse-c-chomping-indicator (_tt)
 (yaml--frame "c-chomping-indicator"
   (yaml--any (when (yaml--chr ?\-) (yaml--set t "strip") t)
              (when (yaml--chr ?\+) (yaml--set t "keep") t)
              (when (yaml--empty) (yaml--set t "clip") t))))

(defun yaml--parse-ns-global-tag-prefix ()
 (yaml--frame "ns-global-tag-prefix"
   (yaml--all
    (yaml--parse-ns-tag-char)
    (yaml--rep 0 nil
      (yaml--parse-ns-uri-char)))))

(defun yaml--parse-c-ns-flow-pair-json-key-entry (n c)
  (yaml--frame "c-ns-flow-pair-json-key-entry"
    (yaml--all
     (yaml--parse-c-s-implicit-json-key "flow-key")
     (yaml--parse-c-ns-flow-map-adjacent-value n c))))

(defun yaml--parse-l-literal-content (n tt)
  (yaml--frame "l-literal-content"
    (yaml--all
     (yaml--rep 0 1
       (yaml--all (yaml--parse-l-nb-literal-text n)
                  (yaml--rep 0 nil
                    (yaml--parse-b-nb-literal-next n))
                  (yaml--parse-b-chomped-last tt)))
     (yaml--parse-l-chomped-empty n tt))))

(defun yaml--parse-c-document-end ()
 (yaml--frame "c-document-end"
   (yaml--all (yaml--chr ?\.)
              (yaml--chr ?\.)
              (yaml--chr ?\.))))

(defun yaml--parse-nb-double-text (n c)
  (yaml--frame "nb-double-text"
    (pcase c
      ("block-key" (yaml--parse-nb-double-one-line))
      ("flow-in" (yaml--parse-nb-double-multi-line n))
      ("flow-key" (yaml--parse-nb-double-one-line))
      ("flow-out" (yaml--parse-nb-double-multi-line n)))))

(defun yaml--parse-s-b-comment ()
 (yaml--frame "s-b-comment"
   (yaml--all
    (yaml--rep 0 1
      (yaml--all
       (yaml--parse-s-separate-in-line)
       (yaml--rep 0 1
         (yaml--parse-c-nb-comment-text))))
    (yaml--parse-b-comment))))

(defun yaml--parse-s-block-line-prefix (n)
  (yaml--frame "s-block-line-prefix"
    (yaml--parse-s-indent n)))

(defun yaml--parse-c-tag-handle ()
  (yaml--frame "c-tag-handle"
    (yaml--any (yaml--parse-c-named-tag-handle)
               (yaml--parse-c-secondary-tag-handle)
               (yaml--parse-c-primary-tag-handle))))

(defun yaml--parse-ns-plain-one-line (c)
  (yaml--frame "ns-plain-one-line"
    (yaml--all (yaml--parse-ns-plain-first c)
               (yaml--parse-nb-ns-plain-in-line c))))

(defun yaml--parse-nb-json ()
 (yaml--frame "nb-json"
   (yaml--any (yaml--chr ?\x09)
              (yaml--chr-range ?\x20 ?\x10FFFF))))

(defun yaml--parse-s-ns-plain-next-line (n c)
  (yaml--frame "s-ns-plain-next-line"
    (yaml--all (yaml--parse-s-flow-folded n)
               (yaml--parse-ns-plain-char c)
               (yaml--parse-nb-ns-plain-in-line c))))

(defun yaml--parse-c-reserved ()
 (yaml--frame "c-reserved"
   (yaml--any (yaml--chr ?\@) (yaml--chr ?\`))))

(defun yaml--parse-b-l-trimmed (n c)
  (yaml--frame "b-l-trimmed"
    (yaml--all
     (yaml--parse-b-non-content)
     (yaml--rep 1 nil
       (yaml--parse-l-empty n c)))))

(defun yaml--parse-l-document-prefix ()
 (yaml--frame "l-document-prefix"
   (yaml--all
    (yaml--rep 0 1
      (yaml--parse-c-byte-order-mark))
    (yaml--rep 0 nil
      (yaml--parse-l-comment)))))

(defun yaml--parse-c-byte-order-mark ()
 (yaml--frame "c-byte-order-mark" (yaml--chr ?\xFEFF)))

(defun yaml--parse-c-anchor ()
 (yaml--frame "c-anchor" (yaml--chr ?\&)))

(defun yaml--parse-s-double-escaped (n)
  (yaml--frame "s-double-escaped"
    (yaml--all
     (yaml--rep 0 nil
       (yaml--parse-s-white))
     (yaml--chr ?\\)
     (yaml--parse-b-non-content)
     (yaml--rep 0 nil
       (yaml--parse-l-empty n "flow-in"))
     (yaml--parse-s-flow-line-prefix n))))

(defun yaml--parse-ns-esc-32-bit ()
 (yaml--frame "ns-esc-32-bit"
   (yaml--all
    (yaml--chr ?U)
    (yaml--rep 8 8 (yaml--parse-ns-hex-digit)))))


(defun yaml--parse-b-non-content ()
 (yaml--frame "b-non-content" (yaml--parse-b-break)))

(defun yaml--parse-ns-tag-char ()
 (yaml--frame "ns-tag-char"
   (yaml--but (yaml--parse-ns-uri-char)
              (yaml--chr ?\!)
              (yaml--parse-c-flow-indicator))))

(defun yaml--parse-b-carriage-return ()
 (yaml--frame "b-carriage-return" (yaml--chr ?\x0D)))

(defun yaml--parse-s-double-next-line (n)
  (yaml--frame "s-double-next-line"
    (yaml--all
     (yaml--parse-s-double-break n)
     (yaml--rep 0 1
       (yaml--all
        (yaml--parse-ns-double-char)
        (yaml--parse-nb-ns-double-in-line)
        (yaml--any
         (yaml--parse-s-double-next-line n)
         (yaml--rep 0 nil
           (yaml--parse-s-white))))))))

(defun yaml--parse-ns-esc-non-breaking-space ()
 (yaml--frame "ns-esc-non-breaking-space" (yaml--chr ?\_)))

(defun yaml--parse-l-nb-diff-lines (n)
  (yaml--frame "l-nb-diff-lines"
    (yaml--all
     (yaml--parse-l-nb-same-lines n)
     (yaml--rep 0 nil
       (yaml--all (yaml--parse-b-as-line-feed)
                  (yaml--parse-l-nb-same-lines n))))))

(defun yaml--parse-s-flow-folded (n)
  (yaml--frame "s-flow-folded"
    (yaml--all
     (yaml--rep 0 1
       (yaml--parse-s-separate-in-line))
     (yaml--parse-b-l-folded n "flow-in")
     (yaml--parse-s-flow-line-prefix n))))

(defun yaml--parse-ns-flow-map-explicit-entry (n c)
  (yaml--frame "ns-flow-map-explicit-entry"
    (yaml--any
     (yaml--parse-ns-flow-map-implicit-entry n c)
     (yaml--all
      (yaml--parse-e-node)
      (yaml--parse-e-node)))))

(defun yaml--parse-ns-l-block-map-implicit-entry (n)
 (yaml--frame "ns-l-block-map-implicit-entry"
   (yaml--all
    (yaml--any (yaml--parse-ns-s-block-map-implicit-key)
               (yaml--parse-e-node))
    (yaml--parse-c-l-block-map-implicit-value n))))

(defun yaml--parse-l-nb-folded-lines (n)
  (yaml--frame "l-nb-folded-lines"
    (yaml--all
     (yaml--parse-s-nb-folded-text n)
     (yaml--rep 0 nil
       (yaml--all (yaml--parse-b-l-folded n "block-in")
                  (yaml--parse-s-nb-folded-text n))))))

(defun yaml--parse-c-l-block-map-explicit-key (n)
  (yaml--frame "c-l-block-map-explicit-key"
    (yaml--all
     (yaml--chr ?\?)
     (yaml--parse-s-l+block-indented n "block-out"))))

(defun yaml--parse-s-separate (n c)
  (yaml--frame "s-separate"
    (pcase c
      ("block-in" (yaml--parse-s-separate-lines n))
      ("block-key" (yaml--parse-s-separate-in-line))
      ("block-out" (yaml--parse-s-separate-lines n))
      ("flow-in" (yaml--parse-s-separate-lines n))
      ("flow-key" (yaml--parse-s-separate-in-line))
      ("flow-out" (yaml--parse-s-separate-lines n)))))

(defun yaml--parse-ns-flow-pair-entry (n c)
  (yaml--frame "ns-flow-pair-entry"
    (yaml--any
     (yaml--parse-ns-flow-pair-yaml-key-entry n c)
     (yaml--parse-c-ns-flow-map-empty-key-entry n c)
     (yaml--parse-c-ns-flow-pair-json-key-entry n c))))

(defun yaml--parse-c-flow-indicator ()
 (yaml--frame "c-flow-indicator"
   (yaml--any (yaml--chr ?\,)
              (yaml--chr ?\[)
              (yaml--chr ?\])
              (yaml--chr ?\{)
              (yaml--chr ?\}))))

(defun yaml--parse-ns-flow-pair-yaml-key-entry (n c)
  (yaml--frame "ns-flow-pair-yaml-key-entry"
    (yaml--all
     (yaml--parse-ns-s-implicit-yaml-key "flow-key")
     (yaml--parse-c-ns-flow-map-separate-value n c))))

(defun yaml--parse-e-scalar ()
 (yaml--frame "e-scalar" (yaml--empty)))

(defun yaml--parse-s-indent-lt (n)
  (yaml--frame "s-indent-lt"
    (yaml--all
     (yaml--rep 0 nil
       (yaml--parse-s-space))
     (< (length (yaml--match)) n))))

(defun yaml--parse-nb-single-one-line ()
 (yaml--frame "nb-single-one-line"
   (yaml--rep 0 nil
     (yaml--parse-nb-single-char))))

(defun yaml--parse-c-collect-entry ()
 (yaml--frame "c-collect-entry" (yaml--chr ?\,)))

(defun yaml--parse-ns-l-compact-sequence (n)
  (yaml--frame "ns-l-compact-sequence"
    (yaml--all
     (yaml--parse-c-l-block-seq-entry n)
     (yaml--rep 0 nil
       (yaml--all
        (yaml--parse-s-indent n)
        (yaml--parse-c-l-block-seq-entry n))))))

(defun yaml--parse-c-comment ()
 (yaml--frame "c-comment" (yaml--chr ?\#)))

(defun yaml--parse-s-line-prefix (n c)
  (yaml--frame "s-line-prefix"
    (pcase c
      ("block-in" (yaml--parse-s-block-line-prefix n))
      ("block-out" (yaml--parse-s-block-line-prefix n))
      ("flow-in" (yaml--parse-s-flow-line-prefix n))
      ("flow-out" (yaml--parse-s-flow-line-prefix n)))))

(defun yaml--parse-s-tab ()
 (yaml--frame "s-tab" (yaml--chr ?\x09)))

(defun yaml--parse-c-directive ()
 (yaml--frame "c-directive" (yaml--chr ?\%)))

(defun yaml--parse-ns-flow-pair (n c)
  (yaml--frame "ns-flow-pair"
    (yaml--any
     (yaml--all (yaml--chr ?\?)
                (yaml--parse-s-separate n c)
                (yaml--parse-ns-flow-map-explicit-entry n c))
     (yaml--parse-ns-flow-pair-entry n c))))

(defun yaml--parse-s-l+block-indented (n c)
 (yaml--frame "s-l+block-indented"
   (let ((m (yaml--auto-detect-indent n)))
     (yaml--any
      (yaml--all
       (yaml--parse-s-indent m)
       (yaml--any
        (yaml--parse-ns-l-compact-sequence (+ n (1+ m)))
        (yaml--parse-ns-l-compact-mapping (+ n (1+ m)))))
      (yaml--parse-s-l+block-node n c)
      (yaml--all (yaml--parse-e-node)
                 (yaml--parse-s-l-comments))))))

(defun yaml--parse-c-single-quote ()
 (yaml--frame "c-single-quote" (yaml--chr ?\')))

(defun yaml--parse-s-flow-line-prefix (n)
  (yaml--frame "s-flow-line-prefix"
    (yaml--all
     (yaml--parse-s-indent n)
     (yaml--rep 0 1
       (yaml--parse-s-separate-in-line)))))

(defun yaml--parse-nb-double-char ()
 (yaml--frame "nb-double-char"
   (yaml--any
    (yaml--parse-c-ns-esc-char)
    (yaml--but (yaml--parse-nb-json)
               (yaml--chr ?\\)
               (yaml--chr ?\")))))

(defun yaml--parse-l-comment ()
 (yaml--frame "l-comment"
   (yaml--all
    (yaml--parse-s-separate-in-line)
    (yaml--rep 0 1
      (yaml--parse-c-nb-comment-text))
    (yaml--parse-b-comment))))

(defun yaml--parse-ns-hex-digit ()
 (yaml--frame "ns-hex-digit"
   (yaml--any
    (yaml--parse-ns-dec-digit)
    (yaml--chr-range ?\x41 ?\x46)
    (yaml--chr-range ?\x61 ?\x66))))

(defun yaml--parse-s-l+flow-in-block (n)
  (yaml--frame "s-l+flow-in-block"
    (yaml--all
     (yaml--parse-s-separate (+ n 1) "flow-out")
     (yaml--parse-ns-flow-node (+ n 1) "flow-out")
     (yaml--parse-s-l-comments))))

(defun yaml--parse-c-flow-json-node (n c)
  (yaml--frame "c-flow-json-node"
    (yaml--all
     (yaml--rep 0 1
       (yaml--all
        (yaml--parse-c-ns-properties n c)
        (yaml--parse-s-separate n c)))
     (yaml--parse-c-flow-json-content n c))))

(defun yaml--parse-c-b-block-header (m tt)
  (yaml--frame "c-b-block-header"
    (yaml--all
     (yaml--any
      (and (not (string-match "\\`[-+][0-9]"
                              (yaml--slice yaml--parsing-position)))
           ;; hack to not match this case if there is a number.
           (yaml--all
            (yaml--parse-c-indentation-indicator m)
            (yaml--parse-c-chomping-indicator tt)))
      (yaml--all
       (yaml--parse-c-chomping-indicator tt)
       (yaml--parse-c-indentation-indicator m)))
     (yaml--parse-s-b-comment))))

(defun yaml--parse-ns-esc-8-bit ()
 (yaml--frame "ns-esc-8-bit"
   (yaml--all (yaml--chr ?\x)
              (yaml--rep 2 2
                (yaml--parse-ns-hex-digit)))))

(defun yaml--parse-ns-anchor-name ()
 (yaml--frame "ns-anchor-name"
   (yaml--rep 1 nil
     (yaml--parse-ns-anchor-char))))

(defun yaml--parse-ns-esc-slash ()
 (yaml--frame "ns-esc-slash" (yaml--chr ?\/)))

(defun yaml--parse-s-nb-folded-text (n)
  (yaml--frame "s-nb-folded-text"
    (yaml--all (yaml--parse-s-indent n)
               (yaml--parse-ns-char)
               (yaml--rep 0 nil
                 (yaml--parse-nb-char)))))

(defun yaml--parse-ns-word-char ()
 (yaml--frame "ns-word-char"
   (yaml--any (yaml--parse-ns-dec-digit)
              (yaml--parse-ns-ascii-letter)
              (yaml--chr ?\-))))

(defun yaml--parse-ns-esc-form-feed ()
 (yaml--frame "ns-esc-form-feed" (yaml--chr ?f)))

(defun yaml--parse-ns-s-block-map-implicit-key ()
 (yaml--frame "ns-s-block-map-implicit-key"
   (yaml--any
    (yaml--parse-c-s-implicit-json-key "block-key")
    (yaml--parse-ns-s-implicit-yaml-key "block-key"))))

(defun yaml--parse-ns-esc-null ()
  (yaml--frame "ns-esc-null" (yaml--chr ?\0)))

(defun yaml--parse-c-ns-tag-property ()
 (yaml--frame "c-ns-tag-property"
   (yaml--any (yaml--parse-c-verbatim-tag)
              (yaml--parse-c-ns-shorthand-tag)
              (yaml--parse-c-non-specific-tag))))

(defun yaml--parse-c-ns-local-tag-prefix ()
 (yaml--frame "c-ns-local-tag-prefix"
   (yaml--all
    (yaml--chr ?\!)
    (yaml--rep 0 nil
      (yaml--parse-ns-uri-char)))))

(defun yaml--parse-ns-tag-directive ()
 (yaml--frame "ns-tag-directive"
   (yaml--all (yaml--chr ?T) (yaml--chr ?A) (yaml--chr ?G)
              (yaml--parse-s-separate-in-line)
              (yaml--parse-c-tag-handle)
              (yaml--parse-s-separate-in-line)
              (yaml--parse-ns-tag-prefix))))

(defun yaml--parse-c-flow-mapping (n c)
  (yaml--frame "c-flow-mapping"
    (yaml--all
     (yaml--chr ?\{)
     (yaml--rep 0 1
       (yaml--parse-s-separate n c))
     (yaml--rep 0 1
       (yaml--parse-ns-s-flow-map-entries n (yaml--parse-in-flow c)))
     (yaml--chr ?\}))))

(defun yaml--parse-ns-double-char ()
 (yaml--frame "ns-double-char"
   (yaml--but (yaml--parse-nb-double-char)
              (yaml--parse-s-white))))

(defun yaml--parse-ns-ascii-letter ()
 (yaml--frame "ns-ascii-letter"
   (yaml--any (yaml--chr-range ?\x41 ?\x5A)
              (yaml--chr-range ?\x61 ?\x7A))))

(defun yaml--parse-b-break ()
 (yaml--frame "b-break"
   (yaml--any (yaml--all (yaml--parse-b-carriage-return)
                         (yaml--parse-b-line-feed))
              (yaml--parse-b-carriage-return)
              (yaml--parse-b-line-feed))))

(defun yaml--parse-nb-ns-double-in-line ()
 (yaml--frame "nb-ns-double-in-line"
   (yaml--rep 0 nil
     (yaml--all
      (yaml--rep 0 nil
        (yaml--parse-s-white))
      (yaml--parse-ns-double-char)))))

(defun yaml--parse-s-l+block-node (n c)
 (yaml--frame "s-l+block-node"
   (yaml--any
    (yaml--parse-s-l+block-in-block n c)
    (yaml--parse-s-l+flow-in-block n))))

(defun yaml--parse-ns-esc-bell ()
 (yaml--frame "ns-esc-bell" (yaml--chr ?a)))

(defun yaml--parse-c-named-tag-handle ()
 (yaml--frame "c-named-tag-handle"
   (yaml--all
    (yaml--chr ?\!)
    (yaml--rep 1 nil (yaml--parse-ns-word-char))
    (yaml--chr ?\!))))

(defun yaml--parse-s-separate-lines (n)
  (yaml--frame "s-separate-lines"
    (yaml--any (yaml--all (yaml--parse-s-l-comments)
                          (yaml--parse-s-flow-line-prefix n))
               (yaml--parse-s-separate-in-line))))

(defun yaml--parse-l-directive ()
 (yaml--frame "l-directive"
   (yaml--all (yaml--chr ?\%)
              (yaml--any (yaml--parse-ns-yaml-directive)
                         (yaml--parse-ns-tag-directive)
                         (yaml--parse-ns-reserved-directive))
              (yaml--parse-s-l-comments))))

(defun yaml--parse-ns-esc-escape ()
 (yaml--frame "ns-esc-escape" (yaml--chr ?e)))

(defun yaml--parse-b-nb-literal-next (n)
  (yaml--frame "b-nb-literal-next"
    (yaml--all (yaml--parse-b-as-line-feed)
               (yaml--parse-l-nb-literal-text n))))

(defun yaml--parse-ns-s-flow-map-entries (n c)
  (yaml--frame "ns-s-flow-map-entries"
    (yaml--all
     (yaml--parse-ns-flow-map-entry n c)
     (yaml--rep 0 1 (yaml--parse-s-separate n c))
     (yaml--rep 0 1
       (yaml--all
        (yaml--chr ?\,)
        (yaml--rep 0 1
          (yaml--parse-s-separate n c))
        (yaml--rep 0 1
          (yaml--parse-ns-s-flow-map-entries n c)))))))

(defun yaml--parse-c-nb-comment-text ()
 (yaml--frame "c-nb-comment-text"
   (yaml--all
    (yaml--chr ?\#)
    (yaml--rep 0 nil (yaml--parse-nb-char)))))

(defun yaml--parse-ns-dec-digit ()
 (yaml--frame "ns-dec-digit"
   (yaml--chr-range ?\x30 ?\x39)))

(defun yaml--parse-ns-yaml-directive ()
 (yaml--frame "ns-yaml-directive"
   (yaml--all (yaml--chr ?Y) (yaml--chr ?A) (yaml--chr ?M) (yaml--chr ?L)
              (yaml--parse-s-separate-in-line)
              (yaml--parse-ns-yaml-version))))

(defun yaml--parse-c-mapping-key ()
 (yaml--frame "c-mapping-key" (yaml--chr ?\?)))

(defun yaml--parse-b-as-line-feed ()
 (yaml--frame "b-as-line-feed"
   (yaml--parse-b-break)))

(defun yaml--parse-s-l+block-in-block (n c)
  (yaml--frame "s-l+block-in-block"
    (yaml--any
     (yaml--parse-s-l+block-scalar n c)
     (yaml--parse-s-l+block-collection n c))))

(defun yaml--parse-ns-esc-paragraph-separator ()
 (yaml--frame "ns-esc-paragraph-separator" (yaml--chr ?P)))

(defun yaml--parse-c-double-quoted (n c)
  (yaml--frame "c-double-quoted"
    (yaml--all (yaml--chr ?\")
               (yaml--parse-nb-double-text n c)
               (yaml--chr ?\"))))

(defun yaml--parse-b-line-feed ()
 (yaml--frame "b-line-feed" (yaml--chr ?\x0A)))

(defun yaml--parse-ns-esc-horizontal-tab ()
 (yaml--frame "ns-esc-horizontal-tab"
   (yaml--any (yaml--chr ?t) (yaml--chr ?\x09))))

(defun yaml--parse-c-ns-flow-map-empty-key-entry (n c)
  (yaml--frame "c-ns-flow-map-empty-key-entry"
    (yaml--all
     (yaml--parse-e-node)
     (yaml--parse-c-ns-flow-map-separate-value n c))))

(defun yaml--parse-l-any-document ()
 (yaml--frame "l-any-document"
   (yaml--any (yaml--parse-l-directive-document)
              (yaml--parse-l-explicit-document)
              (yaml--parse-l-bare-document))))

(defun yaml--parse-c-tag ()
  (yaml--frame "c-tag" (yaml--chr ?\!)))

(defun yaml--parse-c-escape ()
  (yaml--frame "c-escape" (yaml--chr ?\\)))

(defun yaml--parse-c-sequence-end ()
  (yaml--frame "c-sequence-end" (yaml--chr ?\])))

(defun yaml--parse-l+block-mapping (n)
 (yaml--frame "l+block-mapping"
   (let ((new-m (yaml--auto-detect-indent n)))
     (if (= 0 new-m)
         nil ;; For some fixed auto-detected m > 0 ;; Is this right???
       (yaml--all
        (yaml--set m new-m)
        (yaml--rep 1 nil
          (yaml--all
           (yaml--parse-s-indent (+ n new-m))
           (yaml--parse-ns-l-block-map-entry (+ n new-m)))))))))

(defun yaml--parse-c-ns-flow-map-adjacent-value (n c)
  (yaml--frame "c-ns-flow-map-adjacent-value"
    (yaml--all
     (yaml--chr ?\:)
     (yaml--any
      (yaml--all
       (yaml--rep 0 1
         (yaml--parse-s-separate n c))
       (yaml--parse-ns-flow-node n c))
      (yaml--parse-e-node)))))

(defun yaml--parse-s-single-next-line (n)
  (yaml--frame "s-single-next-line"
    (yaml--all
     (yaml--parse-s-flow-folded n)
     (yaml--rep 0 1
       (yaml--all
        (yaml--parse-ns-single-char)
        (yaml--parse-nb-ns-single-in-line)
        (yaml--any
         (yaml--parse-s-single-next-line n)
         (yaml--rep 0 nil
           (yaml--parse-s-white))))))))

(defun yaml--parse-s-separate-in-line ()
 (yaml--frame "s-separate-in-line"
   (yaml--any (yaml--rep 1 nil
                (yaml--parse-s-white))
              (yaml--start-of-line))))

(defun yaml--parse-b-comment ()
 (yaml--frame "b-comment"
   (yaml--any (yaml--parse-b-non-content)
              (yaml--end-of-stream))))

(defun yaml--parse-ns-esc-backslash ()
 (yaml--frame "ns-esc-backslash" (yaml--chr ?\\)))

(defun yaml--parse-c-ns-anchor-property ()
 (yaml--frame "c-ns-anchor-property"
   (yaml--all (yaml--chr ?\&)
              (yaml--parse-ns-anchor-name))))

(defun yaml--parse-ns-plain-safe (c)
  (yaml--frame "ns-plain-safe"
    (pcase c
      ("block-key" (yaml--parse-ns-plain-safe-out))
      ("flow-in" (yaml--parse-ns-plain-safe-in))
      ("flow-key" (yaml--parse-ns-plain-safe-in))
      ("flow-out" (yaml--parse-ns-plain-safe-out)))))

(defun yaml--parse-ns-flow-content (n c)
  (yaml--frame "ns-flow-content"
    (yaml--any (yaml--parse-ns-flow-yaml-content n c)
               (yaml--parse-c-flow-json-content n c))))

(defun yaml--parse-c-ns-flow-map-separate-value (n c)
  (yaml--frame "c-ns-flow-map-separate-value"
    (yaml--all
     (yaml--chr ?\:)
     (yaml--chk "!" (yaml--parse-ns-plain-safe c))
     (yaml--any (yaml--all (yaml--parse-s-separate n c)
                           (yaml--parse-ns-flow-node n c))
                (yaml--parse-e-node)))))

(defun yaml--parse-in-flow (c)
  (yaml--frame "in-flow"
    (pcase c
      ("block-key" "flow-key")
      ("flow-in" "flow-in")
      ("flow-key" "flow-key")
      ("flow-out" "flow-in"))))

(defun yaml--parse-c-verbatim-tag ()
 (yaml--frame "c-verbatim-tag"
   (yaml--all
    (yaml--chr ?\!)
    (yaml--chr ?\<)
    (yaml--rep 1 nil (yaml--parse-ns-uri-char))
    (yaml--chr ?\>))))

(defun yaml--parse-c-literal ()
 (yaml--frame "c-literal" (yaml--chr ?\|)))

(defun yaml--parse-ns-esc-line-feed ()
 (yaml--frame "ns-esc-line-feed" (yaml--chr ?n)))

(defun yaml--parse-nb-double-multi-line (n)
  (yaml--frame "nb-double-multi-line"
    (yaml--all
     (yaml--parse-nb-ns-double-in-line)
     (yaml--any (yaml--parse-s-double-next-line n)
                (yaml--rep 0 nil
                  (yaml--parse-s-white))))))

(defun yaml--parse-b-l-spaced (n)
  (yaml--frame "b-l-spaced"
    (yaml--all
     (yaml--parse-b-as-line-feed)
     (yaml--rep 0 nil
       (yaml--parse-l-empty n "block-in")))))

(defun yaml--parse-ns-flow-yaml-content (n c)
  (yaml--frame "ns-flow-yaml-content"
    (yaml--parse-ns-plain n c)))

;;; Encoding

(defun yaml-encode (object)
  "Encode OBJECT to a YAML string."
  (with-temp-buffer
    (yaml--encode-object object 0)
    (buffer-string)))

(defun yaml--encode-object (object indent &optional auto-indent)
  "Encode a Lisp OBJECT to YAML.

INDENT indicates how deeply nested the object will be displayed
in the YAML.  If AUTO-INDENT is non-nil, then emit the object
without first inserting a newline."
  (cond
   ((yaml--scalarp object) (yaml--encode-scalar object))
   ((hash-table-p object) (yaml--encode-hash-table object indent auto-indent))
   ((listp object) (yaml--encode-list object indent auto-indent))
   ((arrayp object) (yaml--encode-array object indent auto-indent))
   (t (error "Unknown object %s" object))))

(defun yaml--scalarp (object)
  "Return non-nil if OBJECT correlates to a YAML scalar."
  (or (numberp object)
      (symbolp object)
      (stringp object)
      (not object)))

(defun yaml--encode-escape-string (s)
  "Escape yaml special characters in string S."
  (let* ((s (replace-regexp-in-string "\\\\" "\\\\" s))
         (s (replace-regexp-in-string "\n" "\\\\n" s))
         (s (replace-regexp-in-string "\t" "\\\\t" s))
         (s (replace-regexp-in-string "\r" "\\\\r" s))
         (s (replace-regexp-in-string "\"" "\\\\\"" s)))
    s))

(defun yaml--encode-array (a indent &optional auto-indent)
  "Encode array A to a string in the context of being INDENT deep.

If AUTO-INDENT is non-nil, start the list on the current line,
auto-detecting the indentation.  Functionality defers to
`yaml--encode-list'."
  (yaml--encode-list (seq-map #'identity a)
                     indent
                     auto-indent))


(defun yaml--encode-scalar (s)
  "Encode scalar S to buffer."
  (cond
   ((not s) (insert "null"))
   ((eql t s) (insert "true"))
   ((symbolp s)
    (cond
     ((eql s :null) (insert "null"))
     ((eql s :false) (insert "false"))
     (t (insert (symbol-name s)))))
   ((numberp s) (insert (number-to-string s)))
   ((stringp s)
    (if (string-match "\\`[-_a-zA-Z0-9]+\\'" s)
        (insert s)
      (insert "\"" (yaml--encode-escape-string s) "\"")))))

(defun yaml--alist-to-hash-table (l)
  "Return hash representation of L if it is an alist, nil otherwise."
  (when (and (listp l)
             (seq-every-p (lambda (x) (and (consp x) (atom (car x)))) l))
    (let ((h (make-hash-table)))
      (seq-map (lambda (cpair)
                 (let* ((k (car cpair))
                        (v (alist-get k l)))
                   (puthash k v h)))
               l)
      h)))

(defun yaml--encode-list (l indent &optional auto-indent)
  "Encode list L to a string in the context of being INDENT deep.

If AUTO-INDENT is non-nil, start the list on the current line,
auto-detecting the indentation"
  (let ((ht (yaml--alist-to-hash-table l)))
    (cond (ht
           (yaml--encode-hash-table ht indent auto-indent))
          ((zerop (length l))
           (insert "[]"))
          ((seq-every-p #'yaml--scalarp l)
           (insert "[")
           (yaml--encode-object (car l) 0)
           (seq-do (lambda (object)
                     (insert ", ")
                     (yaml--encode-object object 0))
                   (cdr l))
           (insert "]"))
          (t
           (when (zerop indent)
             (setq indent 2))
           (let* ((first t)
                  (indent-string (make-string (- indent 2) ?\s)))
             (seq-do
              (lambda (object)
                (if (not first)
                    (insert "\n" indent-string "- ")
                  (if auto-indent
                      (let ((curr-indent (yaml--encode-auto-detect-indent)))
                        (insert (make-string (- indent curr-indent) ?\s)  "- "))
                    (insert "\n" indent-string "- "))
                  (setq first nil))
                (if (or (hash-table-p object)
                        (yaml--alist-to-hash-table object))
                    (yaml--encode-object object indent t)
                  (yaml--encode-object object (+ indent 2) nil)))
              l))))))

(defun yaml--encode-auto-detect-indent ()
  "Return the amount of indentation at current place in encoding."
  (length (thing-at-point 'line)))

(defun yaml--encode-hash-table (m indent &optional auto-indent)
  "Encode hash table M to a string in the context of being INDENT deep.

If AUTO-INDENT is non-nil, auto-detect the indent on the current
line and insert accordingly."
  (cond ((zerop (hash-table-size m))
         (insert "{}"))
        (t
         (let ((first t)
               (indent-string (make-string indent ?\s)))
           (maphash
            (lambda (k v)
              (if (not first)
                  (insert "\n" indent-string)
                (if auto-indent
                    (let ((curr-indent (yaml--encode-auto-detect-indent)))
                      (when (> curr-indent indent)
                        (setq indent (+ curr-indent 1)))
                      (insert (make-string (- indent curr-indent) ?\s)))
                  (insert "\n" indent-string))
                (setq first nil))
              (yaml--encode-object k indent nil)
              (insert ": ")
              (yaml--encode-object v (+ indent 2)))
            m)))))

(provide 'yaml)

;;; yaml.el ends here
