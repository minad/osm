;;; ol-osm.el --- Org links for `osm-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2022 Daniel Mendler

;; This file is not part of GNU Emacs.

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
(autoload 'osm--setup "osm")
(declare-function osm--link-data "osm")

(org-link-set-parameters
 "osm"
 :follow #'ol-osm-open
 :store #'ol-osm-store)

(defun ol-osm-open (link _)
  "Open osm LINK."
  (save-match-data
    (unless (string-match
             "\\`\\(?:\\([^:]+\\):\\)?\\([^,]+\\),\\([^,]+\\),\\([^,]+\\)\\'"
             link)
      (error "Invalid osm link"))
    (osm--setup
     :at (list (string-to-number (match-string 2 link))
               (string-to-number (match-string 3 link))
               (string-to-number (match-string 4 link)))
     :server (and (match-end 1) (intern (match-string 1 link))))))

(defun ol-osm-store ()
  "Store osm link."
  (when (derived-mode-p 'osm-mode)
    (pcase-let ((`(,lat ,lon ,zoom ,server ,desc) (osm--link-data)))
      (org-link-store-props
       :type "osm"
       :description desc
       :link (format
              "osm:%s%s,%s,%s"
              (if server (format "%s:" server) "")
              lat lon zoom)))))

(provide 'ol-osm)
;;; ol-osm.el ends here
