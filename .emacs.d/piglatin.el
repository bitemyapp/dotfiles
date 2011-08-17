;;
;; Licensed to the Apache Software Foundation (ASF) under one
;; or more contributor license agreements. See the NOTICE file
;; distributed with this work for additional information
;; regarding copyright ownership. The ASF licenses this file
;; to you under the Apache License, Version 2.0 (the
;; "License"); you may not use this file except in compliance
;; with the License. You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing,
;; software distributed under the License is distributed on an
;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;; KIND, either express or implied. See the License for the
;; specific language governing permissions and limitations
;; under the License.
;;

(require 'font-lock)
(eval-when-compile
  (require 'regexp-opt))

(defvar piglatin-mode-hook nil)
(add-to-list 'auto-mode-alist '("\\.pig\\'" . piglatin-mode))

(defvar piglatin-indent-level 2
  "Defines 2 spaces for indentation")

;; syntax coloring
(defconst piglatin-font-lock-keywords
  (list
   '("\\(#\\|\-\-\\).*$" . font-lock-comment-face)
  '("\\<\\(LOAD\\|FILTER\\|FOREACH\\|GENERATE\\|AND\\|OR\\|ANY\\|ALL\\|ARRANGE\\|AS\\|ASC\\|BY\\|cache\\|cat\\|cd\|COGROUP\\|copyFromLocal\\|copyToLocal\\|cp\\|cross\\|define\\|desc\\|describe\\|diff\\|distinct\\|du\\|dump\\|eval\\|exec\\|explain\\|flatten\\|generate\\|group\\|help\\|if\\|illustrate\\|inner\\|input\\|into\\|is\\|join\\|kill\\|limit\\|ls\\|mkdir\\|mv\\|not\\|null\\|or\\|order\\|outer\\|output\\|parallel\\|pig\\|pwd\\|quit\\|register\\|rm\\|rmf\\|run\\|sample\\|set\\|ship\\|size\\|split\\|stderr\\|stdin\\|stdout\\|store\\|stream\\|through\\|union\\|using\\)\\>" . font-lock-keyword-face)
   '("\\<\\(\\w+\\)\\s-*(" (1 font-lock-function-name-face))  ;; functions
   '("\\<\\(\\w+\\)\\>\\s-*=" (1 font-lock-variable-name-face))
   '("'.+'" . font-lock-string-face)
   '("\\<\\(bytearray\\|chararray\\|double\\|int\\|float\\|long\\|tuple\\|bag\\|map\\)\\>" . font-lock-type-face)
   )
  "PigLatin Keywords")

;; C/C++ comments; also allowing underscore in words
(defvar piglatin-mode-syntax-table
  (let ((piglatin-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" piglatin-mode-syntax-table)
    (modify-syntax-entry ?/ ". 1456" piglatin-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" piglatin-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" piglatin-mode-syntax-table)
    piglatin-mode-syntax-table)
  "Syntax table for piglatin-mode")

(defun piglatin-mode ()
  "Mode for editing PigLatin files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table piglatin-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(piglatin-font-lock-keywords nil t))
  (setq major-mode 'piglatin-mode)
  (setq mode-name "PigLatin")
  (run-hooks 'piglatin-mode-hook)
;;  (set (make-local-variable 'indent-line-function) 'piglatin-indent-line)
  )
(provide 'piglatin-mode)