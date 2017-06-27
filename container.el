;;; container --- Simple libary to avoid code side-effects. -*- lexical-binding: t -*-

;; Copyright (C) 2017 Free Software Foundation

;; Author: Etienne Prud’homme <etienne@etienne.cc>
;; Maintainer: Etienne Prud’homme <etienne@etienne.cc>
;; Version: 0.0.1
;; Created: 2017-06-20
;; Last-Updated: Mon Jun 26 22:16:05 (EDT) 2017 by etienne
;; Keywords:  emacs-lisp, side-effects, container
;; URL: http://github.com/notetienne/emacs-lisp-container
;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you ca redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or any
;; later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Source code
;;
;; container’s code can be found here:
;;    http://github.com/notetiene/emacs-lisp-container

;;; Style note
;;
;; This codes uses the Emacs style of:
;;
;;    container--private-function

;;; Code:

(require 'cl-lib)

(defgroup container nil
  "Emacs Lisp container to avoid side-effects of unknown code."
  :group 'lisp)

(defcustom container-buffer-name "*container-context*"
  "Name of the buffer used by `container'."
  :type 'string
  :group 'container)

(defcustom container-ignore-error nil
  "Ignore errors when executing a `container'."
  :type 'boolean
  :group 'container)

(defvar container-last-value nil
  "Last value returned by a `container'.")

(defmacro container--window (&rest body)
  "Private.

Evaluate BODY and discard window modifications."
  (declare (indent 0))
  `(save-window-excursion
     ,@body))

(defmacro container--set (&rest body)
  "Private.

Remove side-effects from the set functions in BODY.

This function advises the `setq', `set' and `set-default'
functions to make modification of variables only buffer-local."
  (declare (indent 0))
  `(flet ((old-set (symbol value)
                   (set symbol value)))
     (cl-macrolet ((setq (symbol value)
                         `(old-set (make-local-variable ',symbol) ,value))
                   (set (symbol value)
                        `(old-set (make-local-variable ,symbol) ,value))
                   (set-default (symbol value)
                                `(set (make-local-variable ,symbol) ,value)))
       ,@body)))

(defmacro container--load (&rest body)
  "Private.

Evaluate BODY and while recording file loading.

The loaded features will be unloaded when BODY is executed."
  (declare (indent 0))
  `(let ((loaded-items '((:require)
                         (:load)
                         (:load-theme)))
         (ignore-error container-ignore-error))
     (cl-flet ((old-require (feature filename noerror)
                            (require feature filename nil)))
       (cl-macrolet ((require (feature &optional filename noerror)
                              `(unless (featurep ,feature)
                                 (let ((current-item))
                                   (condition-case err
                                       (prog1
                                           (old-require ,feature filename nil)
                                         (setq current-item (alist-get :require loaded-items))
                                         (setf (alist-get :require loaded-items) (cons ,feature current-item)))
                                     (error (unless ignore-error
                                              (signal (car err) (cddr err)))))))))
         (prog1
             (progn
               ,@body)
           (let ((load-type))
             (dolist (list loaded-items)
               (setq load-type (pop list))
               (dolist (item list)
                 (pcase load-type
                   (:require (unload-feature item)))))))))))

(defmacro container--containers (&rest body)
  "Private.

Wrap BODY in the containers."
  (declare (indent 0))
  `(container--window
     (container--load
       (container--set
         (container--eval
           ,@body)))))

(defun container--eval-container (form &optional lexical)
  "Private.

Evaluate FORM in a `container--containers'.  This is to override
the default behavior of the `eval' function.  If LEXICAL is t,
evaluate using lexical scoping."
  (eval `(container--containers ,form) lexical))

(defmacro container--eval (&rest body)
  "Private.

Remove side-effects from the `eval' function in BODY.

This function advises the `eval' function to make modification of
variables/functions only buffer-local."
  (declare (indent 0))
  `(flet ((old-eval (form &optional lexical)
                    (eval form lexical)))
     (cl-macrolet ((eval (form &optional lexical)
                         `(old-eval
                           (container--eval-container ',form ,lexical))))
       ,@body)))

(defun container--get-buffer (file)
  "Private.

Get the buffer for FILE.

If a buffer for FILE exists, return a new indirect buffer.
Otherwise, create a new buffer openning FILE."
  (save-excursion
    (let ((buffer-name (generate-new-buffer-name container-buffer-name)))
      (or (and
           (stringp file)
           (or (and
                (get-file-buffer file)
                (make-indirect-buffer (get-file-buffer file)
                                      buffer-name))
               (find-file-noselect file)))
          (generate-new-buffer buffer-name)))))

(defmacro container--evaluator (&optional file &rest body)
  "Private.

Evaluate a form in a container.

FILE is an optional file to set the container context.
BODY should be forms to exececute in the container."
  (declare (indent 1))
  `(container--containers
     (let* ((buffer (container--get-buffer ,file)))
       (prog1
           (with-current-buffer buffer
             ,@body)
         (kill-buffer buffer)))))

(defmacro container--last-value (&rest body)
  "Private.

Change the `container-last-value' value to what is returned by
BODY."
  (declare (indent 0))
  `(setq container-last-value
         ,@body))

(defmacro container (&rest body)
  "Evalute BODY in a container.

The evalution context is a newly created buffer."
  (declare (indent 0))
  `(container--last-value
     (container--evaluator nil
       ,@body)))

(defmacro container-in-context (&optional file &rest body)
  "Evaluate a form in a container.

FILE is an optional file to set the container context.
BODY should be forms to exececute in the container.

The value returned by BODY changes `container-last-value'."
  (declare (indent 0))
  `(container--last-value
     (container--evaluator ,file
       ,@body)))

(provide 'container)
;;; container.el ends here
