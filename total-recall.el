;;; total-recall.el --- Spaced repetition system -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Pierre-Henry FRÖHRING
;; Author: Pierre-Henry FRÖHRING <contact@phfrohring.com>
;; Maintainer: Pierre-Henry FRÖHRING <contact@phfrohring.com>
;; Homepage: https://github.com/phf-1/total-recall
;; Package-Version: 0.10
;; Package-Requires: ((emacs "29.4"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;; `total-recall.el` is a spaced repetition system for Emacs that helps users review and
;; retain knowledge stored in Org Mode files. It searches for definitions and exercises
;; in Org files under a configurable root directory, schedules reviews based on past
;; performance, presents exercises via a user interface, and persists results in a
;; SQLite database.
;; 
;; * Goals
;; 
;; - Enable users to create and review learning content (definitions and exercises) in
;;   Org Mode files.
;; - Implement spaced repetition to optimize retention by scheduling reviews at
;;   increasing intervals based on success.
;; - Provide a modular, extensible architecture using an actor model for managing system
;;   components.
;; - Offer a customizable UI and configuration options for integration into diverse
;;   Emacs workflows.
;; 
;; * User Workflows
;; 
;; 1. *Setup*:
;;    - Users configure ~total-recall-root-dir~ for the search directory,
;;      ~total-recall-database~ for the SQLite database path, and keybindings (e.g.,
;;      ~total-recall-key-skip~).
;;    - Org files are created with headings marked by ~:TYPE:~ (matching
;;      ~total-recall-def-type~ or ~total-recall-ex-type~) and ~:ID:~ (UUIDs).
;;    - Exercises have subheadings for questions and answers; definitions have content.
;; 
;; 2. *Running Total Recall*:
;;    - Invoke ~M-x total-recall~ to start the system.
;;    - The system searches for Org files, parses exercises, and selects those due for
;;      review based on prior ratings.
;;    - Exercises are shown in a dedicated frame (~UI~), where users press keys to reveal
;;      answers, mark success/failure/skip, or quit.
;;    - Results are saved to the database, and a report is displayed in a buffer
;;      (~total-recall-io-buffer-name~).
;; 
;; 3. *Review Process*:
;;    - For each exercise, users view the question, choose to reveal the answer, and
;;      rate their performance.
;;    - The ~Planner~ schedules future reviews: successful reviews double the interval
;;      (e.g., 1, 2, 4 days), while failures reset the schedule.
;;    - Definitions are treated as exercises with a fixed question ("* Definition?") and
;;      content as the answer.
;; 
;; * Example Org File Structure
;; 
;; #+begin_example
;; ,* Topic
;; :PROPERTIES:
;; :TYPE: f590edb9-5fa3-4a07-8f3d-f513950d5663
;; :ID:   123e4567-e89b-12d3-a456-426614174000
;; :END:
;; Definition content...
;; 
;; ,* Exercise
;; :PROPERTIES:
;; :TYPE: b0d53cd4-ad89-4333-9ef1-4d9e0995a4d8
;; :ID:   789abcde-f012-3456-7890-abcdef123456
;; :END:
;; 
;; ,** Question
;; What is the capital of France?
;; 
;; ,** Answer
;; The capital of France is Paris.
;; #+end_example
;; 
;; * Components and Interactions
;; 
;; The system comprises several actors, each handling a specific responsibility:
;; - *Searcher*: Uses Ripgrep to find Org files containing definitions or exercises,
;;   identified by UUIDs in ~:ID:~ and ~:TYPE:~ properties.
;; - *Parser*: Extracts definitions and exercises from Org files in depth-first order,
;;   converting them into actors (~Definition~ and ~Exercise~).
;; - *Planner*: Selects exercises due for review based on a spaced repetition algorithm,
;;   using ratings stored in the database.
;; - *DB*: Manages persistence of review results (ratings) in a SQLite database, storing
;;   success, failure, or skip outcomes with timestamps.
;; - *Clock*: Provides time-related functionality, including current time and review
;;   scheduling logic.
;; - *UI*: Displays exercises to the user, collects input (success, failure, skip, quit),
;;   and shows reports.
;; - *Report*: Aggregates execution logs for user feedback.
;; - *IO*: Handles output to a buffer and minibuffer for reports and notifications.
;; - *TotalRecall*: Orchestrates the workflow, coordinating other actors to search, parse,
;;   schedule, display, and save results.
;; 
;; The workflow begins with ~total-recall~, which initializes a ~TotalRecall~ actor. This
;; actor:
;; 1. Uses ~Searcher~ to locate relevant Org files.
;; 2. Employs ~Parser~ to extract exercises and definitions.
;; 3. Filters exercises with ~Planner~ based on review schedules stored in ~DB~.
;; 4. Presents exercises via ~UI~, collecting user ratings.
;; 5. Saves ratings to ~DB~ and generates a ~Report~ for output via ~IO~.
;; 
;; * Extensibility
;; 
;; The actor model allows new components (e.g., alternative UIs or scheduling
;; algorithms) to be added by defining new actors and messages. Users can customize
;; keybindings, database paths, and UI dimensions via ~defcustom~ variables.
;; 
;; This system integrates seamlessly with Emacs, leveraging Org Mode for content
;; management and SQLite for persistence, providing a robust tool for knowledge
;; retention.
;;
;;; Code:

;; Dependencies

(unless (sqlite-available-p)
  (error "Emacs must be compiled with built-in support for SQLite databases"))
(require 'cl-generic)
(require 'org)
(require 'time-date)
(require 'parse-time)
(require 'org-element)
(require 'cl-lib)

;; Configuration

(defgroup total-recall nil
  "Customization options for Total Recall.
This package provides `total-recall' for spaced repetition in Emacs."
  :group 'convenience
  :prefix "total-recall-")

(defcustom total-recall-root-dir (expand-file-name "~")
  "Specifies the root directory for Total Recall file searches.
This is a string representing the directory path where Org Mode files
are searched."
  :type 'string
  :group 'total-recall)

(defcustom total-recall-database (file-name-concat (expand-file-name user-emacs-directory) "total-recall-test.sqlite3")
  "Specifies the path to the Total Recall SQLite database.
This is a string representing the file path for storing review data."
  :type 'string
  :group 'total-recall)

(defcustom total-recall-ripgrep-cmd "rg"
  "Specifies the name or path of the Ripgrep executable.
This is a string used to locate the Ripgrep command for file searching."
  :type 'string
  :group 'total-recall)

(defcustom total-recall-io-buffer-name "*TotalRecall*"
  "Specifies the name of the Total Recall output buffer.
This is a string used for the buffer where reports are written."
  :type 'string
  :group 'total-recall)

(defcustom total-recall-def-type "f590edb9-5fa3-4a07-8f3d-f513950d5663"
  "Specifies the UUID for identifying definition headings in Org files.
This is a string used to mark headings as definitions in Total Recall."
  :type 'string
  :group 'total-recall)

(defcustom total-recall-ex-type "b0d53cd4-ad89-4333-9ef1-4d9e0995a4d8"
  "Specifies the UUID for identifying exercise headings in Org files.
This is a string used to mark headings as exercises in Total Recall."
  :type 'string
  :group 'total-recall)

(defcustom total-recall-window-width 160
  "Specifies the width of the Total Recall UI frame in characters.
This is an integer defining the frame width for the UI."
  :type 'integer
  :group 'total-recall)

(defcustom total-recall-window-height 90
  "Specifies the height of the Total Recall UI frame in characters.
This is an integer defining the frame height for the UI."
  :type 'integer
  :group 'total-recall)

(defcustom total-recall-key-skip ?k
  "Specifies the key to skip an exercise in the Total Recall UI.
This is a character used to skip the current exercise."
  :type 'character
  :group 'total-recall)

(defcustom total-recall-key-quit ?q
  "Specifies the key to quit the Total Recall session.
This is a character used to exit the UI session."
  :type 'character
  :group 'total-recall)

(defcustom total-recall-key-success ?s
  "Specifies the key to mark an exercise as successful in the Total Recall UI.
This is a character used to record a successful review."
  :type 'character
  :group 'total-recall)

(defcustom total-recall-key-failure ?f
  "Specifies the key to mark an exercise as failed in the Total Recall UI.
This is a character used to record a failed review."
  :type 'character
  :group 'total-recall)

(defcustom total-recall-key-reveal ?r
  "Specifies the key to reveal the answer in the Total Recall UI.
This is a character used to show the exercise answer."
  :type 'character
  :group 'total-recall)

;; Utils

(defun total-recall--truncate-str (str)
  "Truncates STR to 25 characters, replacing newlines with spaces.
Returns the truncated string with an ellipsis if necessary."
  (truncate-string-to-width
   (replace-regexp-in-string "\n" " " (string-trim str))
   25
   0
   nil
   "…"))

(defun total-recall--not-implemented-error ()
  "Signals an error indicating the function is not implemented.
Throws an error with the message \"NotImplemented\"."
  (error "NotImplemented"))

(defun total-recall--not-implemented-warning ()
  "Displays a warning indicating the function is not implemented.
Shows a message \"WARNING: NotImplemented\" in the echo area."
  (message "WARNING: NotImplemented"))

(defun total-recall--string-uuid-p (str)
  "Check if STR is a valid UUID string.
Returns t if STR matches the UUID format, nil otherwise."
  (and (stringp str)
       (string-match-p
        "^[0-9a-fA-F]\\{8\\}-[0-9a-fA-F]\\{4\\}-[0-9a-fA-F]\\{4\\}-[0-9a-fA-F]\\{4\\}-[0-9a-fA-F]\\{12\\}$"
        str)))

(defun total-recall--timestamp-leq (t1 t2)
  "Check if timestamp T1 is less than or equal to T2.
Returns t if T1 is less than or equal to T2, nil otherwise."
  (or (time-less-p t1 t2)
      (equal t1 t2)))

(defconst total-recall--day (* 24 60 60)
  "Number of seconds in a day.")

(defun total-recall--find-last-index (lst pred)
  "Find the last index in LST where PRED return non-nil.
LST is a list, and PRED is a function taking a list element.
Returns the index of the last matching element or nil if none."
  (let ((index -1)
        (last-index nil))
    (dolist (item lst)
      (setq index (1+ index))
      (when (funcall pred item)
        (setq last-index index)))
    last-index))

(defun total-recall--org-element-lineage-map (fun datum &optional types with-self first-match)
  "Apply FUN to each ancestor of DATUM, from closest to farthest.
DATUM is an Org element or object.
TYPES, if non-nil, is a list of symbols to restrict ancestors.
WITH-SELF, if non-nil, includes DATUM if it matches TYPES.
FIRST-MATCH, if non-nil, stops at the first non-nil result from FUN.
Returns a list of non-nil results in reverse order or the first match."
  (let ((lineage (if with-self
                     (cons datum (org-element-lineage datum))
                   (org-element-lineage datum)))
        results)
    (catch 'first-match
      (dolist (element lineage)
        (when (or (not types)
                  (memq (org-element-type element) types))
          (let ((result (funcall fun element)))
            (when result
              (if first-match
                  (throw 'first-match result)
                (push result results)))))))
    (if first-match
        nil  ; If we reach here with first-match, no match was found
      (nreverse results))))

;; Actor

(defmacro total-recall--Actor (init name)
  "Define an actor named NAME with initialization function INIT.
INIT is a function that takes DATA and returns a memory hash table.
NAME is a symbol naming the actor function, which processes messages."
  `(defun ,name (data)
     (let* ((memory (funcall ,init data))
            (self (lambda (msg)
                    (let* ((rcv (gethash 'rcv memory))
                           (stack (puthash 'stack (funcall rcv msg) memory)))
                      (while (not (null stack))
                        (puthash 'stack (cdr stack) memory)
                        (funcall (gethash 'tx memory) memory (car stack))
                        (setq stack (gethash 'stack memory)))
                      (gethash 'out memory)))))
       (puthash 'self self memory)
       self)))

(defun total-recall--send (actor msg)
  "Send MSG to ACTOR and return the result.
ACTOR is a function created by `total-recall--Actor'.
MSG is the message to process."
  (funcall actor msg))

(defun total-recall--Actor-memory (rcv tx)
  "Create a memory hash table for an actor with RCV and TX functions.
RCV is a function that processes incoming messages.
TX is a function that handles transactions.
Returns the initialized memory hash table."
  (let ((memory (make-hash-table :test 'eq)))
    (puthash 'rcv rcv memory)
    (puthash 'tx tx memory)
    (puthash 'stack '() memory)
    (puthash 'self t memory)
    (puthash 'out nil memory)
    memory))

(defmacro total-recall--message (name)
  "Define a message function for NAME to send to an actor.
NAME is a symbol used to create a function `total-recall--NAME'.
The function sends a message to an actor with optional arguments."
  `(defun ,(intern (concat "total-recall--" (symbol-name name))) (actor &rest args)
     (total-recall--send actor
                         (pcase args
                           ('() ',name)
                           (_ (cons ',name args))))))

(total-recall--message add)
(total-recall--message answer)
(total-recall--message buffer)
(total-recall--message buffer-name)
(total-recall--message date)
(total-recall--message file)
(total-recall--message files)
(total-recall--message id)
(total-recall--message minibuffer)
(total-recall--message now)
(total-recall--message parse)
(total-recall--message path)
(total-recall--message question)
(total-recall--message ratings)
(total-recall--message read)
(total-recall--message save)
(total-recall--message select)
(total-recall--message show-exercise)
(total-recall--message show-report)
(total-recall--message start)
(total-recall--message stop)
(total-recall--message string)
(total-recall--message struct)
(total-recall--message tick)
(total-recall--message tick2)
(total-recall--message value)

;; Clock

(total-recall--Actor
 #'total-recall--Clock-init
 total-recall--Clock)

(defun total-recall--Clock-init (time)
  "Initialize a clock actor with TIME.
TIME is a natural number representing the initial clock time.
Returns a memory hash table for the clock actor."
  (let ((memory (total-recall--Actor-memory
                 #'total-recall--Clock-rcv
                 #'total-recall--Clock-tx)))
    (puthash 'time time memory)
    memory))

(defun total-recall--Clock-rcv (msg)
  "Process incoming MSG for the clock actor.
MSG is a symbol or list representing a clock command.
Returns a list of instructions to be executed."
  (pcase msg
    ('read '(read))
    ('tick '(tick))
    ('tick2 '(tick tick))
    ('now '(now))
    (_ (error "Unexpected message: msg = %s" msg))))

(defun total-recall--Clock-tx (memory inst)
  "Handle transaction INST for the clock actor using MEMORY.
MEMORY is the actor’s memory hash table.
INST is a symbol representing a clock instruction.
Updates MEMORY based on INST."
  (let ((time (gethash 'time memory)))

    (pcase inst
      ('read
       (puthash 'out time memory))

      ('now
       (puthash 'out (time-convert (current-time) 'list) memory))

      ('tick
       (puthash 'time (+ time 1) memory)
       (puthash 'out (gethash 'self memory) memory))

      (_ (error "Unexpected instruction: inst = %s" inst)))))

;; Report

(total-recall--Actor
 #'total-recall--Report-init
 total-recall--Report)

(defun total-recall--Report-init (_data)
  "Initialize a report actor with DATA.
DATA is ignored in this implementation.
Returns a memory hash table for the report actor."
  (let ((memory (total-recall--Actor-memory
                 #'total-recall--Report-rcv
                 #'total-recall--Report-tx)))
    (puthash 'lines '() memory)
    memory))

(defun total-recall--Report-rcv (msg)
  "Process incoming MSG for the report actor.
MSG is a list or symbol, such as `(add LINE)` or `string`.
Returns a list containing the instruction to execute."
  (pcase msg
    (`(add ,_line)
     `(,msg))

    ('string
     `(,msg))

    (_ (error "Unexpected message: msg = %s" msg))))

(defun total-recall--Report-tx (memory inst)
  "Handle transaction INST for the report actor using MEMORY.
MEMORY is the actor’s memory hash table.
INST is a list or symbol, such as `(add LINE)` or `string`.
Updates MEMORY based on INST."
  (let ((self (gethash 'self memory))
        (lines (gethash 'lines memory)))
    (pcase inst
      (`(add ,line)
       (puthash 'lines (cons line lines) memory)
       (puthash 'out self memory))

      ('string
       (puthash 'out (string-join (reverse lines) "\n") memory))

      (_ (error "Unexpected instruction: inst = %s" inst)))))

;; Searcher

(total-recall--Actor
 #'total-recall--Searcher-init
 total-recall--Searcher)

(defun total-recall--Searcher-init (data)
  "Initialize a searcher actor with DATA.
DATA is a list of (ROOT DEF-ID EX-ID), where ROOT is a directory path,
DEF-ID and EX-ID are strings identifying definitions and exercises.
Returns a memory hash table for the searcher actor."
  (let ((memory (total-recall--Actor-memory
                 #'total-recall--Searcher-rcv
                 #'total-recall--Searcher-tx)))
    (pcase data
      (`(,root ,def-id ,ex-id)

       (unless (file-directory-p (puthash 'root root memory))
         (error "Root is not a directory: root = %s" root))

       (let ((ripgrep total-recall-ripgrep-cmd))
         (unless (stringp (puthash 'ripgrep (executable-find ripgrep) memory))
           (error "Ripgrep not found in PATH: ripgrep = %s" ripgrep)))

       (unless (stringp (puthash 'def-id def-id memory))
         (error "Def-id is not a string: def-id = %s" def-id))

       (unless (stringp (puthash 'ex-id ex-id memory))
         (error "Ex-id is not a string: ex-id = %s" ex-id))

       (puthash
        'cmd
        (format "%s -g '*.org' -i --no-heading -n --color=never -m 1 '%s' %s"
                (gethash 'ripgrep memory)
                (format "%s|%s" (gethash 'def-id memory) (gethash 'ex-id memory))
                (gethash 'root memory))
        memory)

       memory)
      (_ (error "Unexpected data: data = %s" data)))))

(defun total-recall--Searcher-rcv (msg)
  "Process incoming MSG for the searcher actor.
MSG is the symbol `files` to request file paths.
Returns a list containing the `files` instruction."
  (pcase msg
    ('files
     '(files))
    (_ (error "Unexpected message: msg = %s" msg))))

(defun total-recall--Searcher-tx (memory inst)
  "Handle transaction INST for the searcher actor using MEMORY.
MEMORY is the actor’s memory hash table.
INST is the symbol `files` to search for files.
Updates MEMORY with the list of found file paths."
  (let ((cmd (gethash 'cmd memory)))
    (pcase inst
      ('files
       (let (matches)
         (with-temp-buffer
           (call-process-shell-command cmd nil `(,(current-buffer) nil) nil)
           (goto-char (point-min))
           (while (not (eobp))
             (let* ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
                    (match (split-string line ":")))
               (push (car match) matches))
             (forward-line 1)))
         (puthash 'out (delete-dups matches) memory)))
      (_ (error "Unexpected instruction: inst = %s" inst)))))

;; Exercise

(total-recall--Actor
 #'total-recall--Exercise-init
 total-recall--Exercise)

(defun total-recall--Exercise-init (data)
  "Initialize an exercise actor with DATA.
DATA is a list of (FILE ID PATH QUESTION ANSWER), where FILE is a path,
ID is a UUID string, PATH, QUESTION, and ANSWER are strings.
Returns a memory hash table for the exercise actor."
  (let ((memory (total-recall--Actor-memory
                 #'total-recall--Exercise-rcv
                 #'total-recall--Exercise-tx)))
    (pcase data
      (`(,file ,id ,path ,question ,answer)
       (puthash 'file file memory)
       (puthash 'id id memory)
       (puthash 'path path memory)
       (puthash 'question question memory)
       (puthash 'answer answer memory)
       memory)
      (_
       (error "Unexpected data: data = %s" data)))))

(defun total-recall--Exercise-rcv (msg)
  "Process incoming MSG for the exercise actor.
MSG is a symbol like `file`, `id`, `path`, `question`, `answer`, or `string`.
Returns a list containing the corresponding instruction."
  (pcase msg
    ('file
     '(file))

    ('id
     '(id))

    ('path
     '(path))

    ('question
     '(question))

    ('answer
     '(answer))

    ('string
     '(string))

    (_ (error "Unexpected message: msg = %s" msg))))

(defun total-recall--Exercise-tx (memory inst)
  "Handle transaction INST for the exercise actor using MEMORY.
MEMORY is the actor’s memory hash table.
INST is a symbol like `file`, `id`, `path`, `question`, `answer`, or `string`.
Updates MEMORY with the requested data."
  (let ((file (gethash 'file memory))
        (id (gethash 'id memory))
        (path (gethash 'path memory))
        (question (gethash 'question memory))
        (answer (gethash 'answer memory)))

    (pcase inst
      ('file
       (puthash 'out file memory))

      ('id
       (puthash 'out id memory))

      ('path
       (puthash 'out path memory))

      ('question
       (puthash 'out question memory))

      ('answer
       (puthash 'out answer memory))

      ('string
       (puthash 'out (string-join `("Exercise(" ,id ,path ,(total-recall--truncate-str question) ,(total-recall--truncate-str answer) ")") " ") memory))

      (_ (error "Unexpected instruction: inst = %s" inst)))))

;; Definition

(total-recall--Actor #'total-recall--Definition-init total-recall--Definition)

(defun total-recall--Definition-init (data)
  "Initialize a definition actor with DATA.
DATA is a list of (FILE ID PATH CONTENT), where FILE is a path,
ID is a UUID string, PATH and CONTENT are strings.
Returns a memory hash table for the definition actor."
  (let ((memory (total-recall--Actor-memory
                 #'total-recall--Definition-rcv
                 #'total-recall--Definition-tx)))
    (pcase data
      (`(,file ,id ,path ,content)
       (puthash 'file file memory)
       (puthash 'id id memory)
       (puthash 'path path memory)
       (puthash 'content content memory)
       memory)
      (_
       (error "Unexpected data: data = %s" data)))))

(defun total-recall--Definition-rcv (msg)
  "Process incoming MSG for the definition actor.
MSG is a symbol like `file`, `id`, `path`, `content`, `question`,
`answer`, or `string`.  Returns a list containing the corresponding
instruction."
  (pcase msg
    ('file
     `(file))

    ('id
     `(id))

    ('path
     `(path))

    ('content
     `(content))

    ('question
     `(question))

    ('answer
     `(content))

    ('string
     '(string))

    (_ (error "Unexpected message: msg = %s" msg))))

(defun total-recall--Definition-tx (memory inst)
  "Handle transaction INST for the definition actor using MEMORY.
MEMORY is the actor’s memory hash table.
INST is a symbol like `file`, `id`, `path`, `content`, `question`, or `string`.
Updates MEMORY with the requested data."
  (let ((file (gethash 'file memory))
        (id (gethash 'id memory))
        (path (gethash 'path memory))
        (content (gethash 'content memory)))

    (pcase inst
      ('file
       (puthash 'out file memory))

      ('id
       (puthash 'out id memory))

      ('path
       (puthash 'out path memory))

      ('content
       (puthash 'out content memory))

      ('question
       (puthash 'out "* Definition?" memory))

      ('string
       (puthash 'out (string-join `("Definition(" ,id ,path ,(total-recall--truncate-str content) ")") " ") memory))

      (_ (error "Unexpected instruction: inst = %s" inst)))))

;; Parser

(total-recall--Actor
 #'total-recall--Parser-init
 total-recall--Parser)

(defun total-recall--Parser-init (data)
  "Initialize a parser actor with DATA.
DATA is a list of (DEF-ID EX-ID), where DEF-ID and EX-ID are strings
identifying definition and exercise headings.
Returns a memory hash table for the parser actor."
  (let ((memory (total-recall--Actor-memory
                 #'total-recall--Parser-rcv
                 #'total-recall--Parser-tx)))
    (pcase data
      (`(,def-id ,ex-id)
       (puthash 'def-id def-id memory)
       (puthash 'ex-id ex-id memory)
       memory)
      (_
       (error "Unexpected data: data = %s" data)))))

(defun total-recall--Parser-rcv (msg)
  "Process incoming MSG for the parser actor.
MSG is a list like `(parse FILE)` where FILE is a file path.
Returns a list containing the parse instruction."
  (pcase msg
    (`(parse ,_file) `(,msg))
    (_ (error "Unexpected message: msg = %s" msg))))

(defun total-recall--Parser-tx (memory inst)
  "Handle transaction INST for the parser actor using MEMORY.
MEMORY is the actor’s memory hash table.
INST is a list like `(parse FILE)` where FILE is a file path.
Updates MEMORY with the parsed elements."
  (pcase inst
    (`(parse ,file)
     (puthash
      'out
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (org-fold-show-all)
        (let ((org-element-use-cache nil))
          (total-recall--node-depth-first
           (org-element-parse-buffer 'greater-element)
           (lambda (node) (total-recall--node-to-element file node)))))
      memory))
    (_ (error "Unexpected instruction: inst = %s" inst))))

(defun total-recall--node-depth-first (node func)
  "Traverse NODE depth-first and apply FUNC to each node.
NODE is an Org element, and FUNC is a function taking a node.
Returns a list of non-error results from FUNC."
  (let ((head
         (mapcan
          (lambda (node) (total-recall--node-depth-first node func))
          (org-element-contents node)))
        (last (funcall func node)))
    (pcase last
      (:err head)
      (_ (append head (list last))))))

(defun total-recall--node-to-element (file node)
  "Convert NODE to an exercise or definition element from FILE.
FILE is the path to the Org file, and NODE is an Org element.
Returns an exercise or definition actor, or `:err` if not applicable."
  (let ((exercise-result (total-recall--node-to-exercise file node)))
    (if (eq exercise-result :err)
        (total-recall--node-to-definition file node)
      exercise-result)))

(defun total-recall--node-to-exercise (file node)
  "Convert NODE to an exercise actor from FILE.
FILE is the path to the Org file, and NODE is an Org element.
Returns an exercise actor or `:err` if NODE is not an exercise."
  (let (should-be-exercise id list-headline question answer)

    (setq should-be-exercise
          (and (eq (org-element-type node) 'headline)
               (string= (org-element-property :TYPE node) total-recall-ex-type)))

    (if should-be-exercise
        (progn
          (setq id (org-element-property :ID node))
          (unless (stringp id) (error "Exercise has no ID property"))
          (setq list-headline
                (seq-filter
                 (lambda (child) (eq (org-element-type child) 'headline))
                 (org-element-contents node)))
          (pcase (length list-headline)
            (0 (error "Exercise has no question nor answer: id = %s" id))
            (1 (error "Exercise has no answer: id = %s" id))
            (_
             (setq question (total-recall--node-to-string (car list-headline)))
             (setq answer (total-recall--node-to-string (cadr list-headline)))))

          (total-recall--Exercise
           (list
            file
            id
            (total-recall--node-subject node)
            question
            answer)))
      :err)))

(defun total-recall--node-to-definition (file node)
  "Convert NODE to a definition actor from FILE.
FILE is the path to the Org file, and NODE is an Org element.
Returns a definition actor or `:err` if NODE is not a definition."
  (let (should-be-definition id subject content)

    (setq should-be-definition
          (and (eq (org-element-type node) 'headline)
               (string= (org-element-property :TYPE node) total-recall-def-type)))

    (if should-be-definition
        (progn
          (setq id (org-element-property :ID node))
          (setq subject (total-recall--node-subject node))
          (unless (stringp id) (error "Definition has no ID property: file = %s" file))
          (setq content (total-recall--node-to-string node))
          (total-recall--Definition
           (list
            file
            id
            subject
            content)))
      :err)))

(defun total-recall--node-to-string (node)
  "Convert NODE to a string with headline leveled to level 1.
NODE is an Org element.
Returns the trimmed string representation."
  (replace-regexp-in-string
   "\\`\\*+" "*"
   (string-trim
    (buffer-substring-no-properties
     (org-element-property :begin node)
     (org-element-property :end node)))))

(defun total-recall--node-subject (node)
  "Extract the subject of NODE as a path-like string.
NODE is an Org headline element.
Returns a string like A/B/C, where C is NODE’s title and A, B are ancestors."
  (string-join
   (reverse
    (total-recall--org-element-lineage-map
     (lambda (parent) (org-element-property :raw-value parent))
     node
     '(headline)
     t))
   " / "))

;; Rating

(total-recall--Actor #'total-recall--Rating-init total-recall--Rating)

(defun total-recall--Rating-init (data)
  "Initialize a rating actor with DATA.
DATA is a list of (DATE ID VALUE), where DATE is a timestamp,
ID is a UUID string, and VALUE is a symbol.
Returns a memory hash table for the rating actor."
  (let ((memory (total-recall--Actor-memory
                 #'total-recall--Rating-rcv
                 #'total-recall--Rating-tx)))
    (pcase data
      (`(,date ,id ,value)
       (puthash 'date date memory)
       (puthash 'id id memory)
       (puthash 'value value memory)))

    memory))

(defun total-recall--Rating-rcv (msg)
  "Process incoming MSG for the rating actor.
MSG is a symbol like `struct`, `date`, or `value`.
Returns a list containing the corresponding instruction."
  (pcase msg
    ('struct '(struct))
    ('date '(date))
    ('value '(value))
    (_ (error "Unexpected message: msg = %s" msg))))

(defun total-recall--Rating-tx (memory inst)
  "Handle transaction INST for the rating actor using MEMORY.
MEMORY is the actor’s memory hash table.
INST is a symbol like `struct`, `date`, or `value`.
Updates MEMORY with the requested data."
  (let ((date (gethash 'date memory))
        (id (gethash 'id memory))
        (value (gethash 'value memory)))
    (pcase inst
      ('struct
       (puthash 'out `(,date ,id ,value) memory))
      ('date
       (puthash 'out date memory))
      ('value
       (puthash 'out value memory))
      (_ (error "Unexpected instruction: inst = %s" inst)))))

(defun total-recall--Rating-eq (r1 r2)
  "Check if rating actors R1 and R2 are equal.
R1 and R2 are rating actors.
Returns t if their structures are equal, nil otherwise."
  (equal (total-recall--struct r1)
         (total-recall--struct r2)))

;; DB

(total-recall--Actor #'total-recall--DB-init total-recall--DB)

(defun total-recall--DB-init (db-path)
  "Initialize a database actor with DB-PATH.
DB-PATH is a string or nil for an in-memory SQLite database.
Returns a memory hash table for the database actor."
  (let ((memory (total-recall--Actor-memory
                 #'total-recall--DB-rcv
                 #'total-recall--DB-tx))
        (sqlite nil))

    (unless (sqlite-available-p)
      (error "Emacs must be compiled with built-in support for SQLite databases"))

    (setq sqlite (sqlite-open db-path))

    (unless (sqlite-select sqlite "SELECT name FROM sqlite_master WHERE type='table' AND name='exercise_log'")
      (sqlite-execute sqlite
                      "CREATE TABLE exercise_log (
                       type TEXT NOT NULL,
                       id TEXT NOT NULL,
                       time TEXT NOT NULL)"))
    (puthash 'sqlite sqlite memory)
    memory))

(defun total-recall--DB-rcv (msg)
  "Process incoming MSG for the database actor.
MSG is a list like `(save RATING)`, `(ratings ID)`, or `stop`.
Returns a list containing the corresponding instruction."
  (pcase msg
    (`(save ,_rating)
     `(,msg))

    (`(ratings ,_id)
     `(,msg))

    ('stop
     `(,msg))

    (_ (error "Unexpected message: msg = %s" msg))))

(defun total-recall--DB-tx (memory inst)
  "Handle transaction INST for the database actor using MEMORY.
MEMORY is the actor’s memory hash table.
INST is a list or symbol like `(save RATING)`, `(ratings ID)`, or `stop`.
Updates MEMORY based on INST."
  (let ((self (gethash 'self memory))
        (sqlite (gethash 'sqlite memory)))
    (pcase inst
      (`(save ,rating)
       (pcase (total-recall--struct rating)
         (`(,date ,id ,value)
          (let ((row nil))
            (setq row
                  (list
                   (if (memq value '(success failure skip))
                       (symbol-name value)
                     (error "Unexpected value: value = %s" value))

                   (if (total-recall--string-uuid-p id)
                       id
                     (error "ID is not a UUID string: id = %s" id))

                   (format-time-string "%FT%TZ" (time-convert date 'list) t)))

            (sqlite-execute
             sqlite
             "INSERT INTO exercise_log (type, id, time) VALUES (?, ?, ?)"
             row)))

         (struct (error "Unexpected struct: struct = %s" struct)))
       (puthash 'out self memory))

      (`(ratings ,id)
       (unless (total-recall--string-uuid-p id)
         (error "ID is not a UUID string: id = %s" id))

       (let (rows ratings)
         (setq rows
               (sqlite-select
                sqlite
                "SELECT type, id, time FROM exercise_log WHERE id = ? ORDER BY time ASC"
                (list id)))

         (setq ratings
               (mapcar
                (lambda (row)
                  (pcase row
                    (`(,type ,id ,time)
                     (total-recall--Rating
                      `(,(parse-iso8601-time-string time)
                        ,(if (total-recall--string-uuid-p id) id
                           (error "ID is not a UUID string: id = %s" id))
                        ,(if (member type '("success" "failure" "skip")) (intern type)
                           (error "Unexpected type: id = %s, type = %s" type id)))))
                    (_ (error "Unexpected row: row = %s" row))))
                rows))

         (puthash 'out ratings memory)))

      ('stop
       (sqlite-close sqlite)
       (puthash 'out self memory))

      (_ (error "Unexpected instruction: inst = %s" inst)))))

;; Planner

(total-recall--Actor #'total-recall--Planner-init total-recall--Planner)

(defun total-recall--Planner-init (data)
  "Initialize a planner actor with DATA.
DATA is a list of (DB CLOCK), where DB is a database actor and
CLOCK is a clock actor.
Returns a memory hash table for the planner actor."
  (let ((memory (total-recall--Actor-memory
                 #'total-recall--Planner-rcv
                 #'total-recall--Planner-tx)))
    (pcase data
      (`(,db ,clock)
       (puthash 'db db memory)
       (puthash 'clock clock memory)
       memory)
      (_ (error "Unexpected data: data = %s" data)))))

(defun total-recall--Planner-rcv (msg)
  "Process incoming MSG for the planner actor.
MSG is a list like `(select EXERCISES)` where EXERCISES is a list.
Returns a list containing the select instruction."
  (pcase msg
    (`(select ,_exercises)
     `(,msg))
    (_ (error "Unexpected message: msg = %s" msg))))

(defun total-recall--Planner-tx (memory inst)
  "Handle transaction INST for the planner actor using MEMORY.
MEMORY is the actor’s memory hash table.
INST is a list like `(select EXERCISES)` or `(is_scheduled EX)`.
Updates MEMORY with the filtered exercises or scheduling decision."
  (let ((db (gethash 'db memory))
        (clock (gethash 'clock memory)))
    (pcase inst
      (`(select ,exercises)
       (puthash
        'out
        (seq-filter (lambda (ex) (total-recall--Planner-tx memory `(is_scheduled ,ex)) (gethash 'out memory)) exercises)
        memory))

      (`(is_scheduled ,ex)
       (let (today ratings last-failure-idx successes delta_t last-rating cutoff decision)
         (setq today (total-recall--now clock))
         (setq ratings (total-recall--ratings db (total-recall--id ex)))
         (setq successes
               (pcase ratings
                 ('nil '())
                 (_
                  (setq last-failure-idx
                        (total-recall--find-last-index
                         ratings
                         (lambda (rating) (eq (total-recall--value rating) 'failure))))

                  (seq-filter (lambda (rating) (eq (total-recall--value rating) 'success))
                              (pcase last-failure-idx
                                ('nil ratings)
                                ((pred (eq (- (length ratings) 1)) '()))
                                (_ (nthcdr (+ last-failure-idx 1) ratings)))))))
         (setq cutoff
               (pcase successes
                 ('nil today)
                 (_
                  (setq delta_t (* (expt 2 (- (length successes) 1)) total-recall--day))
                  (setq last-rating (car (last successes)))
                  (time-add (total-recall--date last-rating) delta_t))))
         (setq decision (total-recall--timestamp-leq cutoff today))
         (puthash 'out decision memory)))

      (_ (error "Unexpected instruction: inst = %s" inst)))))

;; UI

(total-recall--Actor #'total-recall--UI-init total-recall--UI)

(defun total-recall--UI-init (data)
  "Initialize a UI actor with DATA.
DATA is a list of (NAME WIDTH HEIGHT CLOCK), where NAME is a buffer name,
WIDTH and HEIGHT are integers, and CLOCK is a clock actor.
Returns a memory hash table for the UI actor."
  (let ((memory (total-recall--Actor-memory #'total-recall--UI-rcv #'total-recall--UI-tx)))
    (pcase data
      (`(,name ,width ,height ,clock)
       (puthash 'buffer (get-buffer-create name) memory)
       (with-current-buffer (gethash 'buffer memory) (setq buffer-read-only t))
       (puthash 'name (buffer-name (gethash 'buffer memory)) memory)
       (puthash 'width width memory)
       (puthash 'height height memory)
       (puthash 'frame (make-frame `((width . ,width) (height . ,height))) memory)
       (puthash 'clock clock memory)
       memory)
      (_ (error "Unexpected data: data = %s" data)))))

(defun total-recall--UI-rcv (msg)
  "Process incoming MSG for the UI actor.
MSG is a list like `(show-exercise EXERCISE)`, `(show-report REPORT)`,
or `stop`.  Returns a list containing the corresponding instruction."
  (pcase msg
    (`(show-exercise ,_exercise)
     `(,msg))

    (`(show-report ,_report)
     `(,msg))

    ('stop
     '(kill))

    (_ (error "Unexpected message: msg = %s" msg))))

(defun total-recall--UI-tx (memory inst)
  "Handle transaction INST for the UI actor using MEMORY.
MEMORY is the actor’s memory hash table.
INST is a list or symbol for UI operations like `show-exercise` or `stop`.
Updates MEMORY with the result of the operation."
  (let ((self (gethash 'self memory))
        (clock (gethash 'clock memory))
        (frame (gethash 'frame memory))
        (buffer (gethash 'buffer memory)))

    (pcase inst
      (`(show-exercise ,exercise)
       (total-recall--UI-tx memory 'show-frame)
       (total-recall--UI-tx memory 'clear)
       (let (meta)
         (setq meta (format "┌────
│ file: %s
│ link: %s
│ path: %s
└────
"
                            (total-recall--file exercise)
                            (format "[[ref:%s]]" (total-recall--id exercise))
                            (total-recall--path exercise)))
         (total-recall--UI-tx memory `(show-content ,meta)))
       (total-recall--UI-tx memory `(show-content ,(total-recall--question exercise)))
       (total-recall--UI-tx memory `(ask ((,total-recall-key-quit . "Quit")
                                          (,total-recall-key-skip . "Skip")
                                          (,total-recall-key-reveal . "Reveal"))))
       (pcase (gethash 'out memory)
         ((pred (equal total-recall-key-quit)) (total-recall--UI-tx memory 'stop))
         ((pred (equal total-recall-key-skip)) (total-recall--UI-tx memory `(skip ,exercise)))
         ((pred (equal total-recall-key-reveal))
          (total-recall--UI-tx memory `(show-content ,(total-recall--answer exercise)))
          (total-recall--UI-tx memory `(ask ((,total-recall-key-success . "Success")
                                             (,total-recall-key-failure . "Failure")
                                             (,total-recall-key-skip . "Skip")
                                             (,total-recall-key-quit . "Quit"))))
          (pcase (gethash 'out memory)
            ((pred (equal total-recall-key-success)) (total-recall--UI-tx memory `(success ,exercise)))
            ((pred (equal total-recall-key-failure)) (total-recall--UI-tx memory `(failure ,exercise)))
            ((pred (equal total-recall-key-skip)) (total-recall--UI-tx memory `(skip ,exercise)))
            ((pred (equal total-recall-key-quit)) (total-recall--UI-tx memory 'stop))))))

      ('show-frame
       (select-frame-set-input-focus frame)
       (switch-to-buffer buffer)
       (puthash 'out self memory))

      ('clear
       (with-current-buffer buffer
         (setq buffer-read-only nil)
         (erase-buffer)
         (unless (derived-mode-p 'org-mode) (org-mode))
         (insert "* Total Recall *\n\n")
         (goto-char (point-min))
         (setq buffer-read-only t))
       (puthash 'out self memory))

      (`(show-report ,report)
       (total-recall--UI-tx memory 'clear)
       (total-recall--UI-tx memory 'show-frame)
       (total-recall--UI-tx memory `(show-content ,(total-recall--string report)))
       (puthash 'out self memory))

      (`(show-content ,content)
       (total-recall--UI-tx memory 'show-frame)
       (with-current-buffer buffer
         (setq buffer-read-only nil)
         (save-excursion
           (goto-char (point-max))
           (insert (string-join (list (string-trim content) "\n\n"))))
         (setq buffer-read-only t))
       (puthash 'out self memory))

      ('kill
       (when (buffer-live-p buffer) (kill-buffer buffer))
       (when (frame-live-p frame) (delete-frame frame))
       (puthash 'out self memory))

      ('stop
       (puthash 'out 'stop memory))

      (`(ask ,options)
       (total-recall--UI-tx memory 'show-frame)
       (let (strs str key)
         (setq strs
               (mapcar
                (lambda (opt)
                  (pcase opt
                    (`(,char . ,name)
                     (format "%s (%s)" name (string char)))
                    (_
                     (error "Unexpected option: option = %s" opt))))
                options))
         (setq str (string-join strs ", "))
         (setq key (read-char-choice str (mapcar #'car options)))
         (puthash 'out key memory)))

      (`(skip ,exercise)
       (puthash
        'out
        `(rating ,(total-recall--Rating (list (total-recall--now clock) (total-recall--id exercise) 'skip)))
        memory))

      (`(success ,exercise)
       (puthash
        'out
        `(rating ,(total-recall--Rating (list (total-recall--now clock) (total-recall--id exercise) 'success)))
        memory))

      (`(failure ,exercise)
       (puthash
        'out
        `(rating ,(total-recall--Rating (list (total-recall--now clock) (total-recall--id exercise) 'failure)))
        memory))

      (_ (error "Unexpected instruction: inst = %s" inst)))))

;; IO

(total-recall--Actor
 #'total-recall--IO-init
 total-recall--IO)

(defun total-recall--IO-init (name)
  "Initialize an IO actor with NAME.
NAME is a string for the output buffer name.
Returns a memory hash table for the IO actor."
  (let ((memory (total-recall--Actor-memory
                 #'total-recall--IO-rcv
                 #'total-recall--IO-tx)))
    (puthash 'buffer (get-buffer-create name) memory)
    (puthash 'name (buffer-name (gethash 'buffer memory)) memory)
    memory))

(defun total-recall--IO-rcv (msg)
  "Process incoming MSG for the IO actor.
MSG is a list like `(minibuffer STRING)`, `(buffer STRING)`, or `buffer-name`.
Returns a list containing the corresponding instruction."
  (pcase msg
    (`(minibuffer ,_string)
     `(,msg))

    (`(buffer ,_string)
     `(,msg))

    ('buffer-name
     `(,msg))

    (_ (error "Unexpected message: msg = %s" msg))))

(defun total-recall--IO-tx (memory inst)
  "Handle transaction INST for the IO actor using MEMORY.
MEMORY is the actor’s memory hash table.
INST is a list or symbol like `(minibuffer STRING)` or `buffer-name`.
Updates MEMORY with the result of the operation."
  (let ((self (gethash 'self memory))
        (buffer (gethash 'buffer memory))
        (name (gethash 'name memory)))

    (pcase inst
      (`(minibuffer ,string)
       (message "%s" (string-trim string))
       (puthash 'out self memory))

      (`(buffer ,string)
       (with-current-buffer buffer
         (insert (string-join (list string "\n"))))
       (puthash 'out self memory))

      ('buffer-name
       (puthash 'out name memory))

      (_ (error "Unexpected instruction: inst = %s" inst)))))

;; TotalRecall

(total-recall--Actor #'total-recall--TotalRecall-init total-recall--TotalRecall)

(defun total-recall--TotalRecall-init (_data)
  "Initialize a TotalRecall actor with DATA.
DATA is ignored in this implementation.
Returns a memory hash table with initialized sub-actors."
  (let ((memory (total-recall--Actor-memory
                 #'total-recall--TotalRecall-rcv
                 #'total-recall--TotalRecall-tx)))
    (puthash 'clock (total-recall--Clock t) memory)

    (puthash 'db-path total-recall-database memory)
    (puthash 'db (total-recall--DB (gethash 'db-path memory)) memory)

    (puthash 'root total-recall-root-dir memory)
    (puthash 'def-type total-recall-def-type memory)
    (puthash 'ex-type total-recall-ex-type memory)
    (puthash 'searcher (total-recall--Searcher (list (gethash 'root memory) (gethash 'def-type memory) (gethash 'ex-type memory))) memory)

    (puthash 'parser (total-recall--Parser (list (gethash 'def-type memory) (gethash 'ex-type memory))) memory)

    (puthash 'planner (total-recall--Planner (list (gethash 'db memory) (gethash 'clock memory))) memory)

    (puthash 'ui (total-recall--UI (list "*TotalRecall UI*" total-recall-window-width total-recall-window-height (gethash 'clock memory))) memory)

    (puthash 'nbr-files 0 memory)

    (puthash 'nbr-exercises 0 memory)

    (puthash 'files '() memory)

    (puthash 'exercises '() memory)

    memory))

(defun total-recall--TotalRecall-rcv (msg)
  "Process incoming MSG for the TotalRecall actor.
MSG is a symbol like `start` or `stop`.
Returns a list containing the corresponding instruction."
  (pcase msg
    ('start '(start))
    ('stop '(stop))
    (_ (error "Unexpected message: msg = %s" msg))))

(defun total-recall--TotalRecall-tx (memory inst)
  "Handle transaction INST for the TotalRecall actor using MEMORY.
MEMORY is the actor’s memory hash table.
INST is a symbol or list for operations like `start` or `process-file`.
Updates MEMORY with the result of the operation."
  (let ((self (gethash 'self memory))
        (root (gethash 'root memory))
        (db-path (gethash 'db-path memory))
        (searcher (gethash 'searcher memory))
        (parser (gethash 'parser memory))
        (db (gethash 'db memory))
        (planner (gethash 'planner memory))
        (ui (gethash 'ui memory))
        (nbr-files (gethash 'nbr-files memory))
        (nbr-exercises (gethash 'nbr-exercises memory))
        (report (gethash 'report memory))
        (files (gethash 'files memory))
        (exercises (gethash 'exercises memory)))

    (pcase inst
      ('start
       (let ((report (puthash 'report (total-recall--Report t) memory)))

         (total-recall--add report "TotalRecall started.")
         (total-recall--add report (format "Definitions and exercises under %s will be reviewed." root))
         (total-recall--add report (format "Review results will be saved in %s." db-path))
         (puthash 'files (total-recall--files searcher) memory)
         (total-recall--add report (format "%s files have been found." (length (gethash 'files memory))))
         (total-recall--TotalRecall-tx memory 'process-files)
         (total-recall--add report (format "%s files have been reviewed." (gethash 'nbr-files memory)))
         (total-recall--add report (format "%s exercises have been reviewed." (gethash 'nbr-exercises memory)))
         (puthash 'out report memory)))

      ('process-files
       (pcase files
         ('()
          (puthash 'out self memory))
         (`(,file . ,files)
          (puthash 'files files memory)
          (total-recall--TotalRecall-tx memory `(process-file ,file))
          (total-recall--TotalRecall-tx memory 'process-files))))

      (`(process-file ,file)
       (total-recall--add report (format "file = %s" file))
       (puthash 'exercises (total-recall--select planner (total-recall--parse parser file)) memory)
       (total-recall--add report (format "%s exercises have been found." (length (gethash 'exercises memory))))
       (total-recall--TotalRecall-tx memory 'process-exercises)
       (puthash 'nbr-files (+ nbr-files 1) memory))

      ('process-exercises
       (pcase exercises
         ('()
          (puthash 'out self memory))
         (`(,exercise . ,exercises)
          (puthash 'exercises exercises memory)
          (total-recall--TotalRecall-tx memory `(process-exercise ,exercise))
          (total-recall--TotalRecall-tx memory 'process-exercises))))

      (`(process-exercise ,exercise)
       (total-recall--add report (format "exercise = %s %s" (total-recall--id exercise) (total-recall--path exercise)))
       (pcase (total-recall--show-exercise ui exercise)
         ('stop
          (puthash 'files '() memory)
          (puthash 'exercises '() memory)
          (puthash 'out self memory))
         (`(rating ,rating)
          (total-recall--save db rating)
          (puthash 'nbr-exercises (+ nbr-exercises 1) memory)
          (puthash 'out self memory))))

      ('stop
       (total-recall--stop ui)
       (total-recall--stop db)
       (puthash 'out self memory))

      (_ (error "Unexpected instruction: inst = %s" inst)))))

;; total-recall

;;;###autoload
(defun total-recall ()
  "Run the Total Recall spaced repetition application.
Initiates a TotalRecall actor, processes data, and displays the report."
  (interactive)
  (let* ((tr (total-recall--TotalRecall t))
         (report (total-recall--start tr))
         (io (total-recall--IO total-recall-io-buffer-name)))
    (total-recall--stop tr)
    (total-recall--buffer io (total-recall--string report))
    (total-recall--minibuffer io (format "Total-recall execution finished. Report written to %s" (total-recall--buffer-name io)))))

(provide 'total-recall)

;;; total-recall.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-docstring-max-column: 80
;; require-final-newline: t
;; sentence-end-double-space: nil
;; indent-tabs-mode: nil
;; End:
