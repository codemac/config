;;; pgg-def.el --- functions/macros for defining PGG functions

;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Created: 1999/11/02
;; Keywords: PGP, OpenPGP, GnuPG

;; This file is part of SEMI (Secure Emacs MIME Interface).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'custom)

(defgroup pgg ()
  "Glue for the various PGP implementations."
  :group 'mime)

(defcustom pgg-default-scheme 'gpg
  "Default PGP scheme."
  :group 'pgg
  :type '(choice (const :tag "GnuPG" gpg)
		 (const :tag "PGP 5" pgp5)
		 (const :tag "PGP" pgp)))

(defcustom pgg-default-user-id (user-login-name)
  "User ID of your default identity."
  :group 'pgg
  :type 'string)

(defcustom pgg-default-keyserver-address "wwwkeys.pgp.net"
  "Host name of keyserver."
  :group 'pgg
  :type 'string)

(defcustom pgg-encrypt-for-me nil
  "If t, encrypt all outgoing messages with user's public key."
  :group 'pgg
  :type 'boolean)

(defcustom pgg-cache-passphrase t
  "If t, cache passphrase."
  :group 'pgg
  :type 'boolean)

(defvar pgg-messages-coding-system nil
  "Coding system used when reading from a PGP external process.")

(defvar pgg-messages-locale nil
  "Locale set before running a PGP external process.")

(defvar pgg-status-buffer " *PGG status*")
(defvar pgg-errors-buffer " *PGG errors*")
(defvar pgg-output-buffer " *PGG output*")

(defvar pgg-echo-buffer "*PGG-echo*")

(defvar pgg-scheme nil
  "Current scheme of PGP implementation.")

(defmacro pgg-truncate-key-identifier (key)
  `(if (> (length ,key) 8) (substring ,key 8) ,key))

(provide 'pgg-def)

;;; pgg-def.el ends here
