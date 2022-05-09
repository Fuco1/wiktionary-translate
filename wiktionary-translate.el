;;; wiktionary-translate.el --- Translate words using wiktionary

;; Copyright (C) 2013 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Keywords: convenience
;; Version: 0.1
;; Created: 7th November 2013
;; Package-requires: ((dash "2.3.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Translate words using wiktionary. Right now, only italian is
;; supported.

;; It's very much a work in progress, please heed that warning.

;;; Code:

(require 'dash)

;;;_. Start
(defconst wd-base-query
  "http://en.wiktionary.org/w/api.php?action=query&format=json&prop=revisions&rvslots=*&rvprop=content&titles="
  "Append the word to this query to retrieve the data.")

(defun wd-interactive-translate-spec ()
  "Interactive spec for interactive translation functions."
  (list (if current-prefix-arg
            (read-from-minibuffer "Word: " (or (word-at-point) ""))
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning)
                                              (region-end))
            (or (word-at-point)
                (read-from-minibuffer "Word: "))))))

(defvar wd-buffer-word nil
  "Word translated in current *Wiktionary* buffer.")

(define-derived-mode wd-mode special-mode "Wiktionary"
  "Wiktionary translation mode.

Display translation of word."
  (make-local-variable 'wd-buffer-word)
  (font-lock-mode 1)
  (set (make-local-variable 'revert-buffer-function)
       (function wd-revert-buffer)))

(defun wd-revert-buffer (&optional _ _ _)
  (wd-show-translation wd-buffer-word))

;;;_. UI

;; TODO: refactor these two methods into something nicer

;;;###autoload
(defun wd-show-translation (word)
  (interactive (wd-interactive-translate-spec))
  (-if-let (meaning (wd-translate-word word))
      (pop-to-buffer
       (with-current-buffer (get-buffer-create "*Wiktionary*")
         (if (fboundp 'read-only-mode)
             (read-only-mode -1)
           (setq buffer-read-only nil))
         (erase-buffer)
         (insert meaning)
         (wd-mode)
         (setq wd-buffer-word word)
         (goto-char (point-min))
         (current-buffer)))
    (message "Failed to find a translation")))

(defun wd-show-raw-translation (word)
  (interactive (wd-interactive-translate-spec))
  (-if-let (raw-data (wd-get-raw-page-data word))
      (pop-to-buffer
       (with-current-buffer (get-buffer-create "*Wiktionary*")
         (if (fboundp 'read-only-mode)
             (read-only-mode -1)
           (setq buffer-read-only nil))
         (erase-buffer)
         (insert raw-data)
         (special-mode)
         (goto-char (point-min))
         (current-buffer)))
    (message "Failed to find a translation")))

;;;_. helpers
(defun wd-get-page-text (buffer)
  "Get the text from the wiktionary dump."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((data (json-read)))
      (cdr (nth 3 (cadr (car (elt (cdr (nth 4 (cadr (cadr (cadr data))))) 0))))))))

(defun wd-query-for-word (word)
  (let ((data (url-retrieve-synchronously (concat wd-base-query word))))
    (with-current-buffer data
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (delete-region (point-min) (point)))
    data))

(defun wd-translate-word (word)
  (-when-let (meaning (wd-process-word word))
    (concat "Translations of " word "\n"
            "================" (make-string (length word) ?=)
            "\n\n"
            (wd-sanitize-translation (wd-dewikify-markup meaning)))))

(defun wd-get-raw-page-data (word)
  (let* ((word (replace-regexp-in-string "́" "" word)))
    (-when-let* ((raw-json-buffer (wd-query-for-word word))
                 (text (wd-get-page-text raw-json-buffer)))
      text)))

(defun wd-process-word (word &optional language extra)
  (-if-let (text (wd-get-raw-page-data word))
      (with-temp-buffer
        (insert text)
        (if language
            (cond
             ((equal language "Italian")
              (wd-process-italian-word word extra))
             ((equal language "German")
              (wd-process-german-word word extra))
             ((equal language "Russian")
              (wd-process-russian-word word extra)))
          (concat
           (--if-let (wd-process-italian-word word extra) (concat "Italian\n-------\n" it) "")
           (--if-let (wd-process-german-word word extra) (concat "German\n------\n" it) "")
           (--if-let (wd-process-russian-word word extra) (concat "Russian\n-------\n" it) ""))))
    ;; try to run the decapitalized version of the word
    (if (not (equal (downcase word) word))
        (wd-process-word (downcase word))
      (message "Failed to retrieve the page")
      nil)))

(defun wd-extract-language (language &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (goto-char (point-min))
    (when (search-forward (concat "==" language "==") nil t)
      (let ((start (line-beginning-position)))
        (if (re-search-forward "^==[^=].?*[^=]==$" nil t)
            (buffer-substring-no-properties start (line-beginning-position))
          (buffer-substring-no-properties start (point-max)))))))

(defun wd-dewikify-markup (text)
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (save-excursion (replace-regexp "\\[\\[\\(.*?\\)\\]\\]" "\\1"))
    (save-excursion (while (re-search-forward "{{context|\\(.*?\\)|lang=.*?}}" nil t)
                      (let ((start (match-beginning 0)))
                        (replace-match "(\\1)")
                        (replace-string "|" ", " nil start (point)))))
    (save-excursion (while (re-search-forward "{{syn|.*?|\\(.*?\\)}}" nil t)
                      (let ((start (match-beginning 0)))
                        (replace-match "Synonyms: \\1")
                        (replace-string "|" ", " nil start (point)))))
    (save-excursion (replace-regexp "{{qualifier|\\(.*?\\)}}" "(Qualifier: \\1)"))
    (save-excursion (replace-regexp "{{gloss|\\(.*?\\)}}" "(Gloss: \\1)"))
    (save-excursion
      (while (re-search-forward "{{uxi|.*?|\\(.*?\\)|\\(.*?\\)}}" nil t)
        (let ((start (match-beginning 0)))
          (replace-match "\\1 ― \\2"))))
    (save-excursion
      (while (re-search-forward "{{ux|.*?|\\(.*?\\)|" nil t)
        (let ((indent (current-indentation)))
          (replace-match "\\1\n")
          (catch 'done
            (while (re-search-forward "\\(.*?\\)\\(|\\|}}\\)" nil t)
              (let ((ms (match-string 2)))
                (replace-match (format "%s\\1%s"
                                       (make-string (+ indent 4) ? )
                                       (if (equal "}}" ms) "" "\n")))
                (when (equal "}}" ms)
                  (throw 'done t))))))))
    (save-excursion
      (while (re-search-forward "<br/>" nil t)
        (let ((indent (current-indentation)))
          (save-excursion
            (back-to-indentation)
            (when (looking-at-p "- ")
              (setq indent (+ indent 2))))
          (replace-match (format "\n%s" (make-string indent ? ))))))
    (save-excursion
      (while (re-search-forward "'''\\(.*?\\)'''" nil t)
        (let ((start (match-beginning 0))
              (len (length (match-string 1))))
          (replace-match "\\1")
          (put-text-property start (+ start len) 'font-lock-face '(bold font-lock-variable-name-face)))))
    (save-excursion
      (while (re-search-forward "{{lb|.*?|\\(.*?\\)}}" nil t)
        (let ((start (match-beginning 0))
              (len (length (match-string 1))))
          (replace-match "(\\1)")
          (put-text-property start (+ start len 2) 'font-lock-face 'shadow))))
    (buffer-string)))

(defun wd-sanitize-translation (text)
  ;; "feminine of ... verb" happens when there is an adjective same as
  ;; verb form in some finite form.
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (save-excursion
      (while (re-search-forward "feminine of .*?verb:" nil t)
        (beginning-of-line)
        (let (kill-ring) (kill-paragraph 1))
        (delete-blank-lines)))
    (buffer-string)))

(defun wd-process-meanings ()
  (when (search-forward "#" nil t)
    (beginning-of-line)
    (let ((text "") (i 1) done)
      (while (not done)
        ;; process a sub-header
        (if (looking-at "#\\([:*]+\\)")
            (let ((depth (length (match-string 0))))
              (forward-char depth)
              (setq text (concat
                          text
                          (make-string (* 2 (1- depth)) ?\ )
                          "- "
                          (string-trim (buffer-substring-no-properties (point) (line-end-position))) "\n"))
              (forward-line 1)
              (setq done (not (looking-at "#"))))
          (forward-char 2)
          (setq text (concat text (int-to-string i) ". " (buffer-substring-no-properties (point) (line-end-position)) "\n"))
          (forward-line 1)
          (setq done (not (looking-at "#")))
          (setq i (1+ i))))
      text)))

(defun wd-process-generic-verb-inflection (language)
  "Process `inflection of' macro for LANGUAGE verb.

This is a macro used across languages on English wiktionary."
  (let ((what (save-excursion
                (buffer-substring-no-properties
                 (progn
                   (re-search-forward "[123]" nil t)
                   (backward-char 1)
                   (point))
                 (progn
                   (search-forward "}}" nil t)
                   (backward-char 2)
                   (point))))))
    (wd-process-word (buffer-substring-no-properties start (1- (search-forward "|" nil t)))
                     language
                     (list :verb (concat what " of ")))))

(defun wd-process-generic-noun-inflection (language)
  "Process `inflection of' macro for LANGUAGE noun.

This is a macro used across languages on English wiktionary."
  (let ((what (save-excursion
                (buffer-substring-no-properties
                 (progn
                   (re-search-forward "||" nil t)
                   (point))
                 (progn
                   (search-forward "}}" nil t)
                   (backward-char 2)
                   (point))))))
    (wd-process-word (buffer-substring-no-properties start (1- (search-forward "|" nil t)))
                     language
                     (list :nominal (concat what " of ")))))

(defmacro wd-cond (start-var &rest clauses)
  "A helper macro to simplify the search for various subsections.

START-VAR is a symbol to which start position after the search is saved.

CLAUSES is a list of the form ((search-form) (what-to-do)),
similar to `cond'."
  (declare (indent 1)
           (debug (sexp &rest [(sexp body)])))
  `(let (,start-var)
     (cond
      ,@(mapcar
         (lambda (clause)
           (if (eq (car clause) t)
               `(t ,@(cdr clause))
             `((setq ,start-var (save-excursion ,(append (car clause) (list nil t))))
               (goto-char ,start-var)
               ,@(cdr clause))))
         clauses))))

;;;_. Italian

(defun wd-process-italian-word (word extra)
  "Process Italian word."
  (let ((ita (wd-extract-language "Italian"))
        (extra-verb (or (plist-get extra :verb) ""))
        (extra-nominal (or (plist-get extra :nominal) "")))
    (when ita
      (concat (--if-let (wd-process-italian-verb ita) (concat extra-verb "[" word "] " it) "")
              (--if-let (wd-process-italian-noun ita) (concat extra-nominal "[" word "] " it) "")
              (--if-let (wd-process-italian-adjective ita) (concat extra-nominal "[" word "] " it) "")
              (wd-process-italian-invar ita)))))

;;;_ , Verbs
(defun wd-process-italian-verb (text)
  (with-temp-buffer
    (insert text)
    (wd--process-italian-verb)))

(defun wd--process-italian-verb ()
  (goto-char (point-min))
  (when (re-search-forward (regexp-opt '("===Verb==="
                                         "===Verb form===")) nil t)
    (save-restriction
      ;;(--when-let (re-search-forward "^===[^=]" nil t) (narrow-to-region (point) it))
      (forward-line 1)
      ;; first test if the page contains "conjugation" info
      (wd-cond start
        ;; # {{conjugation of|togliere||1|s|pres|ind|lang=it}} ''tolgo''
        ((search-forward "conjugation of|")
         (let ((what (save-excursion
                       (buffer-substring-no-properties
                        (progn
                          (re-search-forward "[123]" nil t)
                          (backward-char 1)
                          (point))
                        (progn
                          (search-forward "|lang" nil t)
                          (backward-char 5)
                          (point))))))
           (wd-process-word (buffer-substring-no-properties start (1- (search-forward "|" nil t)))
                            "Italian"
                            (list :verb (concat what " of ")))))
        ;; # {{inflection of|it|cantare||1|s|pres|indc}}
        ((search-forward "inflection of|it|")
         (wd-process-generic-verb-inflection "Italian"))
        ;; # {{form of|first-, second- and third-person singular subjunctive present tense|essere|lang=it}}
        ((search-forward "{{form of|")
         (let ((what (buffer-substring-no-properties
                      (point)
                      (1- (search-forward "|")))))
           (wd-process-word (buffer-substring-no-properties
                             (point)
                             (1- (search-forward "|")))
                            "Italian"
                            (list :verb (concat what " of ")))))
        ;; # ''first-, second-person singular subjunctive imperfect of [[amare]]''
        ((re-search-forward (regexp-opt '("-person singular"
                                          "-person plural")))
         (let ((what (buffer-substring-no-properties
                      (+ (save-excursion (search-backward "#" nil t)) 2)
                      (- (save-excursion (search-forward " of " nil t)) 3))))
           (when (search-forward " of [[" nil t)
             (let ((s (point)))
               (when (search-forward "]]" nil t)
                 (forward-char -2)
                 (wd-process-word (buffer-substring-no-properties s (point))
                                  "Italian"
                                  (list :verb (concat what " of "))))))))
        ;; past participle {{past participle of|subordinare|lang=it}}
        ((search-forward "past participle of|")
         (let ((parent (buffer-substring-no-properties start (1- (search-forward "|" nil t)))))
           (wd-process-word parent "Italian" (list :verb "past participle of "))))
        ;; # {{gerund of|lasciare|lang=it}}
        ((search-forward "gerund of|")
         (let ((parent (buffer-substring-no-properties start (1- (search-forward "|" nil t)))))
           (wd-process-word parent "Italian" (list :verb "gerund of "))))
        ;; # [[past participle]] of [[andare]]
        ((search-forward "[[past participle]] of")
         (let ((parent (buffer-substring-no-properties (search-forward "[[") (- (search-forward "]]") 2))))
           (wd-process-word parent "Italian" (list :verb "past participle of "))))
        ;; feminine form of pp {{feminine of|subordinare|lang=it}}
        ((search-forward "feminine of|")
         (let ((parent (buffer-substring-no-properties start (1- (search-forward "|" nil t)))))
           (wd-process-word parent "Italian" (list :verb "feminine form of "))))
        ;; normal definition
        (t (concat "verb:\n" (wd-process-meanings) "\n"))))))

;;;_ , Nominals
(defun wd-process-italian-noun (text)
  (with-temp-buffer
    (insert text)
    (wd--process-italian-nominal (regexp-opt '("===Noun==="
                                               "===Noun form==="))
                                 "noun")))

(defun wd-process-italian-adjective (text)
  (with-temp-buffer
    (insert text)
    (wd--process-italian-nominal (regexp-opt '("===Adjective==="
                                               "===Adjective form==="))
                                 "adjective")))

(defun wd--process-italian-nominal (category-regexp pos-name)
  (goto-char (point-min))
  (when (re-search-forward category-regexp nil t)
    (forward-line 1)
    (wd-cond start
      ;; plural form # {{plural of|cavallo|lang=it}}
      ((search-forward "plural of|")
       (wd-process-word (buffer-substring-no-properties start (1- (search-forward "|" nil t)))
                        "Italian"
                        (list :nominal "plural of ")))
      ;; plural form 2 # Plural form of [[compito]]
      ((search-forward "Plural form of [[")
       (wd-process-word (buffer-substring-no-properties
                         start
                         (- (search-forward "]]" nil t) 2))
                        "Italian"
                        (list :nominal "plural of ")))
      ;; feminine form of # {{feminine of|compito|lang=it}}
      ((search-forward "feminine of|")
       (wd-process-word (buffer-substring-no-properties start (1- (search-forward "|" nil t))) "Italian" (list :nominal "feminine of ") ))
      ;; normal definition
      (t
       ;; get the gender
       (let ((gender (save-excursion
                       (when (search-forward "{{it-noun|" nil t)
                         (replace-regexp-in-string
                          "|" ","
                          (buffer-substring-no-properties
                           (point)
                           (- (search-forward "}}") 2)))))))
         (concat pos-name (if gender (concat "[" gender "]") "") ":\n" (wd-process-meanings) "\n"))))))

;;;_ , Invariable: conjunctions, prepositions, adverbs
(defun wd-process-italian-invar (text)
  (with-temp-buffer
    (insert text)
    (or (wd--process-italian-invar) "")))

(defun wd--process-italian-invar ()
  (let ((re ""))
    (goto-char (point-min))
    (when (search-forward "===Adverb===" nil t)
      (setq re (concat "Adverb:\n" (wd-process-meanings) "\n" re)))
    (goto-char (point-min))
    (when (search-forward "===Preposition===" nil t)
      (setq re (concat "Preposition:\n" (wd-process-meanings) "\n" re)))
    (goto-char (point-min))
    (when (search-forward "===Interjection===" nil t)
      (setq re (concat "Interjection:\n" (wd-process-meanings) "\n" re)))
    (goto-char (point-min))
    (when (search-forward "===Conjunction===" nil t)
      (setq re (concat "Conjunction:\n" (wd-process-meanings) "\n" re)))
    re))

;;;_. German

(defun wd-process-german-word (word extra)
  "Process German word."
  (let ((ger (wd-extract-language "German"))
        (extra-verb (or (plist-get extra :verb) ""))
        (extra-nominal (or (plist-get extra :nominal) "")))
    (when ger
      (concat (--if-let (wd-process-german-verb ger) (concat extra-verb "[" word "] " it) "")
              (--if-let (wd-process-german-noun ger) (concat extra-nominal "[" word "] " it) "")
              (--if-let (wd-process-german-adjective ger) (concat extra-nominal "[" word "] " it) "")
              (wd-process-german-invar ger)))))

;;;_ , Verbs

(defun wd-process-german-verb (text)
  (with-temp-buffer
    (insert text)
    (wd--process-german-verb)))

(defun wd--process-german-verb ()
  (goto-char (point-min))
  (when (re-search-forward (regexp-opt '("===Verb==="
                                         "===Verb form===")) nil t)
    (save-restriction
      ;;(--when-let (re-search-forward "^===[^=]" nil t) (narrow-to-region (point) it))
      (forward-line 1)
      ;; first test if the page contains "conjugation" info
      (wd-cond start
        ;; # {{de-verb form of|fahren|1|s|g}}
        ((search-forward "de-verb form of|")
         (let ((what (save-excursion
                       (buffer-substring-no-properties
                        (progn
                          (re-search-forward "|" nil t)
                          (point))
                        (progn
                          (search-forward "}}" nil t)
                          (backward-char 2)
                          (point))))))
           (wd-process-word (buffer-substring-no-properties start (1- (search-forward "|" nil t)))
                            "German"
                            (list :verb (concat what " of ")))))
        ;; # {{first-person singular of|führen|lang=de}}
        ((re-search-forward (regexp-opt '("-person singular of|"
                                          "-person plural of |")))
         (let ((what (buffer-substring-no-properties
                      (+ (save-excursion (search-backward "{{" nil t)) 2)
                      (- (save-excursion (search-forward " of|" nil t)) 4))))
           (wd-process-word (buffer-substring-no-properties
                             (point)
                             (1- (search-forward "|")))
                            "German"
                            (list :verb (concat what " of ")))))
        ;; past participle {{past participle of|essen|lang=de}}
        ((search-forward "past participle of|")
         (let ((parent (buffer-substring-no-properties start (1- (search-forward "|" nil t)))))
           (wd-process-word parent "German" (list :verb "past participle of "))))
        ;; # past participle of ''[[rufen]]''
        ((search-forward "past participle of")
         (let ((parent (buffer-substring-no-properties (search-forward "[[") (- (search-forward "]]") 2))))
           (wd-process-word parent "German" (list :verb "past participle of "))))
        ;; normal definition
        (t (concat "verb:\n" (wd-process-meanings) "\n"))))))

;;;_ , Nouns

(defun wd-process-german-noun (text)
  (with-temp-buffer
    (insert text)
    (wd--process-german-noun)))

(defun wd--process-german-noun ()
  (goto-char (point-min))
  (when (search-forward "===Noun===" nil t)
    (forward-line 1)
    (wd-cond start
      ;; plural form # {{plural of|Fall|lang=de}}
      ((search-forward "plural of|")
       (wd-process-word (buffer-substring-no-properties start (1- (search-forward "|" nil t)))
                        "German"
                        (list :nominal "plural of ")))
      ;; plural form 2 # Plural form of [[compito]] // not attested yet :D
      ((search-forward "Plural form of [[")
       (wd-process-word (buffer-substring-no-properties
                         start
                         (- (search-forward "]]" nil t) 2))
                        "German"
                        (list :nominal "plural of ")))
      ;; feminine form of # {{feminine of|lang=de|Lehrer}}
      ((search-forward "feminine of|lang=de|")
       (let ((info (save-excursion
                     (search-backward "===Noun===")
                     (when (search-forward "{{de-noun|" nil t)
                       (replace-regexp-in-string
                        "|" ","
                        (buffer-substring-no-properties
                         (point)
                         (- (search-forward "}}") 2)))))))
         (wd-process-word (buffer-substring-no-properties start (1- (search-forward "|" nil t))) "German" (list :nominal (concat "(" info "), feminine of ")))))
      ;; feminine form of # {{feminine of|Lehrer|lang=de}}
      ((search-forward "feminine of|")
       (wd-process-word (buffer-substring-no-properties start (1- (search-forward "|" nil t))) "German" (list :nominal "feminine of ")))
      ;; normal definition
      (t
       ;; get the gender {{de-noun|m|Falls|gen2=Falles|Fälle}} // {{de-noun|f|Lehrerin|Lehrerinnen|m=Lehrer}}
       (let ((gender (save-excursion
                       (when (search-forward "{{de-noun|" nil t)
                         (replace-regexp-in-string
                          "|" ","
                          (buffer-substring-no-properties
                           (point)
                           (- (search-forward "}}") 2)))))))
         (concat "noun" (if gender (concat "[" gender "]") "") ":\n" (wd-process-meanings) "\n"))))))

;;;_ , Adjectives

(defun wd-process-german-adjective (text)
  (with-temp-buffer
    (insert text)
    (wd--process-german-adjective)))

(defun wd--process-german-adjective ()
  (goto-char (point-min))
  (when (search-forward "===Adjective===" nil t)
    (forward-line 1)
    (wd-cond start
      ;; # {{inflected form of|voll|lang=de}}
      ((search-forward "inflected form of|")
       (wd-process-word (buffer-substring-no-properties start (1- (search-forward "|" nil t)))
                        "German"
                        (list :nominal "inflected form of ")))
      ;; # {{comparative of|reich|lang=de}}
      ((search-forward "comparative of|")
       (wd-process-word (buffer-substring-no-properties start (1- (search-forward "|" nil t)))
                        "German"
                        (list :nominal "comparative of ")))
      ;; # {{superlative of|jung|lang=de}}
      ((search-forward "superlative of|")
       (wd-process-word (buffer-substring-no-properties start (1- (search-forward "|" nil t)))
                        "German"
                        (list :nominal "superlative of ")))
      ;; # {{de-form-adj|s|f|n|voll}}
      ((search-forward "de-form-adj|")
       (search-forward "|")
       (search-forward "|")
       (search-forward "|")
       (wd-process-word (buffer-substring-no-properties (point) (- (search-forward "}}" nil t) 2))
                        "German"
                        (list :nominal "inflected form of ")))
      ;; normal definition
      (t
       ;; get the gender {{de-noun|m|Falls|gen2=Falles|Fälle}} // {{de-noun|f|Lehrerin|Lehrerinnen|m=Lehrer}}
       (let ((info (save-excursion
                     (when (search-forward "{{de-adj|" nil t)
                       (replace-regexp-in-string
                        "|" ","
                        (buffer-substring-no-properties
                         (point)
                         (- (search-forward "}}") 2)))))))
         (concat "adjective" (if info (concat "[" info "]") "") ":\n" (wd-process-meanings) "\n"))))))

;;;_ , Invariables
(defun wd-process-german-invar (text)
  (with-temp-buffer
    (insert text)
    (or (wd--process-german-invar) "")))

(defun wd--process-german-invar ()
  (let ((re ""))
    (goto-char (point-min))
    (when (search-forward "===Adverb===" nil t)
      (setq re (concat "Adverb:\n" (wd-process-meanings) "\n" re)))
    (goto-char (point-min))
    (when (search-forward "===Preposition===" nil t)
      (setq re (concat "Preposition:\n" (wd-process-meanings) "\n" re)))
    (goto-char (point-min))
    (when (search-forward "===Interjection===" nil t)
      (setq re (concat "Interjection:\n" (wd-process-meanings) "\n" re)))
    (goto-char (point-min))
    (when (search-forward "===Conjunction===" nil t)
      (setq re (concat "Conjunction:\n" (wd-process-meanings) "\n" re)))
    re))

;;;_. End

;;;_. Russian
(defun wd-process-russian-word (word extra)
  "Process Russian word."
  (let ((ger (wd-extract-language "Russian"))
        (extra-verb (or (plist-get extra :verb) ""))
        (extra-nominal (or (plist-get extra :nominal) "")))
    (when ger
      (concat (--if-let (wd-process-russian-verb ger) (concat extra-verb "[" word "] " it) "")
              (--if-let (wd-process-russian-noun ger) (concat extra-nominal "[" word "] " it) "")
              (--if-let (wd-process-russian-adjective ger) (concat extra-nominal "[" word "] " it) "")
              (wd-process-russian-invar ger)))))

(defun wd-process-russian-verb (text)
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (when (re-search-forward (regexp-opt '("===Verb==="
                                           "===Verb form===")) nil t)
      (save-restriction
        (forward-line 1)
        ;; first test if the page contains "conjugation" info
        (wd-cond start
          ;; # {{ru-verb|обсужда́ть|impf|pf=обсуди́ть}}
          ((search-forward "ru-verb|")
           (-let (((aspect other-aspect other-verb)
                   (save-excursion
                     (re-search-forward "|\\(.*?\\)|\\(.*?\\)=\\(.*?\\)}}" nil t)
                     (list (match-string 1) (match-string 2) (match-string 3)))))
             (format "verb (%s, %s: %s):\n%s\n"
                     aspect
                     other-aspect
                     other-verb
                     (wd-process-meanings))))
          ;; # {{inflection of|ru|обсужда́ть||1|s|pres|ind|impfv}}
          ((search-forward "inflection of|ru|")
           (wd-process-generic-verb-inflection "Russian"))
          ;; normal definition
          (t (concat "verb:\n" (wd-process-meanings) "\n")))))))

(defun wd-process-russian-noun (text)
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (when (re-search-forward (regexp-opt '("===Noun==="
                                           "===Noun form===")) nil t)
      (forward-line 1)
      (wd-cond start
        ;; {{ru-noun+|пе́ленг}}
        ((re-search-forward "ru-noun\\+?|")
         (let ((what
                (save-excursion
                  (re-search-forward "|\\(.*?\\)}}" nil t)
                  (match-string 1))))
           (format "noun%s:\n%s\n"
                   (if what (format " (%s)" what) "")
                   (wd-process-meanings))))
        ;; # {{inflection of|ru|пробле́ма||gen|p}}
        ((search-forward "inflection of|ru|")
         (wd-process-generic-noun-inflection "Russian"))
        ;; normal definition
        (t (concat "noun:\n" (wd-process-meanings) "\n"))))))

(defun wd-process-russian-adjective (text) nil)

(defun wd-process-russian-invar (text) nil)

(provide 'wiktionary-translate)

;; Local Variables:
;;   mode: emacs-lisp
;;   eval: (font-lock-add-keywords nil `((,(concat "(" (regexp-opt '("wd-cond") t) "\\_>") 1 'font-lock-keyword-face)))
;; End:

;;; wiktionary-translate.el ends here
