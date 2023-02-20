;;; osm-ol.el --- Org links for `osm-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Free Software Foundation, Inc.

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org links for `osm-mode'.

;;; Code:

(require 'ol)

(declare-function osm "ext:osm")
(declare-function osm--org-link-props "ext:osm")
(org-link-set-parameters
 "geo"
 :follow (lambda (link _) (osm (concat "geo:" link)))
 :store (lambda ()
          (when (derived-mode-p 'osm-mode)
            (apply #'org-link-store-props (osm--org-link-props)))))

(provide 'osm-ol)
;;; osm-ol.el ends here
