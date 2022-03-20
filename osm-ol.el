;;; osm-ol.el --- Org links for `osm-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Org links for `osm-mode'.

;;; Code:

(require 'ol)

;; Only load osm on demand
(autoload 'osm--goto "osm")
(autoload 'osm-search "osm")
(declare-function osm--org-link-data "osm")

(org-link-set-parameters
 "osm"
 :follow #'osm-ol-open
 :store #'osm-ol-store)

(defun osm-ol-open (link _)
  "Open osm LINK."
  (save-match-data
    (cond
     ((string-match
       "\\`\\(?:\\([^:]+\\):\\)?\\([0-9.-]+\\),\\([0-9.-]+\\),\\([0-9]+\\)\\'" link)
      (osm--goto
       (string-to-number (match-string 2 link))
       (string-to-number (match-string 3 link))
       (string-to-number (match-string 4 link))
       (and (match-end 1) (intern (match-string 1 link)))
       'osm-link
       "Org Link"))
     (t (osm-search link)))))

(defun osm-ol-store ()
  "Store osm link."
  (when (derived-mode-p 'osm-mode)
    (pcase-let ((`(,lat ,lon ,zoom ,server ,desc) (osm--org-link-data)))
      (org-link-store-props
       :type "osm"
       :description desc
       :link (format
              "osm:%s%.6f,%.6f,%s"
              (if server (format "%s:" server) "")
              lat lon zoom)))))

(provide 'osm-ol)
;;; osm-ol.el ends here
