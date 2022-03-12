;;; osm.el --- OpenStreetMap viewer -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2022
;; Version: 0.4
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/minad/osm

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

;; OpenStreetMap viewer

;;; Code:

(require 'bookmark)
(require 'dom)
(eval-when-compile (require 'cl-lib))

(defgroup osm nil
  "OpenStreetMap viewer."
  :group 'web
  :prefix "osm-")

(defcustom osm-curl-options
  "--fail --location --silent"
  "Additional Curl command line options."
  :type 'string)

(defcustom osm-server-defaults
  '(:min-zoom 2 :max-zoom 19 :max-connections 2 :subdomains ("a" "b" "c"))
  "Default server properties."
  :type 'plist)

(defcustom osm-server-list
  '((default
     :name "Mapnik"
     :description "Standard Mapnik map provided by OpenStreetMap"
     :url "https://%s.tile.openstreetmap.org/%z/%x/%y.png")
    (de
     :name "Mapnik(de)"
     :description "Localized Mapnik map provided by OpenStreetMap Germany"
     :url "https://%s.tile.openstreetmap.de/%z/%x/%y.png")
    (fr
     :name "Mapnik(fr)"
     :description "Localized Mapnik map by OpenStreetMap France"
     :url "https://%s.tile.openstreetmap.fr/osmfr/%z/%x/%y.png")
    (humanitarian
     :name "Humanitarian"
     :description "Humanitarian map provided by OpenStreetMap France"
     :url "https://%s.tile.openstreetmap.fr/hot/%z/%x/%y.png")
    (cyclosm
     :name "CyclOSM"
     :description "Bicycle-oriented map provided by OpenStreetMap France"
     :url "https://%s.tile.openstreetmap.fr/cyclosm/%z/%x/%y.png")
    (openriverboatmap
     :name "OpenRiverBoatMap"
     :description "Waterways map provided by OpenStreetMap France"
     :url "https://%s.tile.openstreetmap.fr/openriverboatmap/%z/%x/%y.png")
    (opentopomap
     :name "OpenTopoMap"
     :description "Topographical map provided by OpenTopoMap"
     :url "https://%s.tile.opentopomap.org/%z/%x/%y.png")
    (opvn
     :name "ÖPNV" :max-zoom 18
     :description "Base layer with public transport information"
     :url "http://%s.tile.memomaps.de/tilegen/%z/%x/%y.png")
    (stamen-watercolor
     :name "Stamen Watercolor"
     :description "Artistic map in watercolor style provided by Stamen"
     :url "https://stamen-tiles-%s.a.ssl.fastly.net/watercolor/%z/%x/%y.jpg")
    (stamen-terrain
     :name "Stamen Terrain" :max-zoom 18
     :description "Map with hill shading provided by Stamen"
     :url "https://stamen-tiles-%s.a.ssl.fastly.net/terrain/%z/%x/%y.png")
    (stamen-toner-dark
     :name "Stamen Toner Dark"
     :description "Artistic map in toner style provided by Stamen"
     :url "https://stamen-tiles-%s.a.ssl.fastly.net/toner/%z/%x/%y.png")
    (stamen-toner-light
     :name "Stamen Toner Lite"
     :description "Artistic map in toner style provided by Stamen"
     :url "https://stamen-tiles-%s.a.ssl.fastly.net/toner-lite/%z/%x/%y.png"))
  "List of tile servers."
  :type '(alist :key-type symbol :value-type plist))

(defcustom osm-pin-colors
  '((osm-selected-bookmark "#e20" "#600")
    (osm-bookmark "#f80" "#820")
    (osm-center "#08f" "#028")
    (osm-home "#80f" "#208")
    (osm-poi "#88f" "#228")
    (osm-org-link "#7a9" "#254"))
  "Colors of pins."
  :type '(alist :key-type symbol :value-type (list string string)))

(defcustom osm-track-style
  "stroke:#00A;stroke-width:10;stroke-linejoin:round;stroke-linecap:round;opacity:0.4;"
  "SVG style used to draw tracks."
  :type'string)

(defcustom osm-home
  (let ((lat (bound-and-true-p calendar-latitude))
        (lon (bound-and-true-p calendar-longitude)))
    (if (and lat lon)
        (list lat lon 12)
      (list 0 0 3)))
  "Home coordinates, latitude, longitude and zoom level."
  :type '(list number number number))

(defcustom osm-large-step 256
  "Scroll step in pixel."
  :type 'integer)

(defcustom osm-tile-border nil
  "Display tile borders.
Useful for debugging, set to value `debug'."
  :type '(choice boolean (const debug)))

(defcustom osm-small-step 16
  "Scroll step in pixel."
  :type 'integer)

(defcustom osm-server 'default
  "Tile server name."
  :type 'symbol)

(defcustom osm-tile-directory
  (expand-file-name "var/osm/" user-emacs-directory)
  "Tile cache directory."
  :type 'string)

(defcustom osm-max-age 14
  "Maximum tile age in days.
Should be at least 7 days according to the server usage policies."
  :type '(choice (const nil) integer))

(defcustom osm-max-tiles 256
  "Size of tile memory cache."
  :type '(choice (const nil) integer))

(defvar osm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [osm-home] #'ignore)
    (define-key map [osm-org-link] #'ignore)
    (define-key map [osm-center] #'ignore)
    (define-key map [osm-selected-bookmark] #'ignore)
    (define-key map [osm-bookmark mouse-1] #'osm-bookmark-select-click)
    (define-key map [osm-bookmark mouse-2] #'osm-bookmark-select-click)
    (define-key map [osm-bookmark mouse-3] #'osm-bookmark-select-click)
    (define-key map [osm-poi mouse-1] #'osm-poi-click)
    (define-key map [osm-poi mouse-2] #'osm-poi-click)
    (define-key map [osm-poi mouse-3] #'osm-poi-click)
    (define-key map [home] #'osm-home)
    (define-key map "+" #'osm-zoom-in)
    (define-key map "-" #'osm-zoom-out)
    (define-key map " " #'osm-zoom-in)
    (define-key map (kbd "S-SPC") #'osm-zoom-out)
    (define-key map [mouse-1] #'osm-center-click)
    (define-key map [mouse-2] #'osm-org-link-click)
    (define-key map [mouse-3] #'osm-bookmark-set-click)
    (define-key map [down-mouse-1] #'osm-mouse-drag)
    (define-key map [down-mouse-2] #'osm-mouse-drag)
    (define-key map [down-mouse-3] #'osm-mouse-drag)
    (define-key map [up] #'osm-up)
    (define-key map [down] #'osm-down)
    (define-key map [left] #'osm-left)
    (define-key map [right] #'osm-right)
    (define-key map [C-up] #'osm-up-up)
    (define-key map [C-down] #'osm-down-down)
    (define-key map [C-left] #'osm-left-left)
    (define-key map [C-right] #'osm-right-right)
    (define-key map [M-up] #'osm-up-up)
    (define-key map [M-down] #'osm-down-down)
    (define-key map [M-left] #'osm-left-left)
    (define-key map [M-right] #'osm-right-right)
    (define-key map "n" #'osm-bookmark-rename)
    (define-key map "d" #'osm-bookmark-delete)
    (define-key map "\d" #'osm-bookmark-delete)
    (define-key map "c" #'clone-buffer)
    (define-key map "h" #'osm-home)
    (define-key map "t" #'osm-goto)
    (define-key map "s" #'osm-search)
    (define-key map "v" #'osm-server)
    (define-key map "l" 'org-store-link)
    (define-key map "b" #'osm-bookmark-set)
    (define-key map "j" #'osm-bookmark-jump)
    (define-key map "x" #'osm-gpx-show)
    (define-key map "X" #'osm-gpx-hide)
    (define-key map [remap scroll-down-command] #'osm-down)
    (define-key map [remap scroll-up-command] #'osm-up)
    (define-key map "<" nil)
    (define-key map ">" nil)
    map)
  "Keymap used by `osm-mode'.")

(defconst osm--placeholder
  '(:type svg :width 256 :height 256
    :data "<svg width='256' height='256' version='1.1' xmlns='http://www.w3.org/2000/svg'>
  <defs>
    <pattern id='grid' width='16' height='16'  patternUnits='userSpaceOnUse'>
      <path d='m 0 0 l 0 16 16 0' fill='none' stroke='#888888'/>
    </pattern>
  </defs>
  <rect width='256' height='256' fill='url(#grid)'/>
</svg>")
  "Placeholder image for tiles.")

(defvar osm--search-history nil
  "Minibuffer search history used by `osm-search'.")

(defvar osm--purge-directory 0
  "Last time the tile cache was cleaned.")

(defvar osm--tile-cache nil
  "Global tile memory cache.")

(defvar osm--tile-cookie 0
  "Tile cache cookie.")

(defvar osm--gpx-files nil
  "Global list of loaded tracks.")

(defvar-local osm--subdomain-index 0
  "Subdomain index to query the servers in a round-robin fashion.")

(defvar-local osm--download-queue nil
  "Download queue of tiles.")

(defvar-local osm--download-active nil
  "Active download jobs.")

(defvar-local osm--wx 0
  "Half window width in pixel.")

(defvar-local osm--wy 0
  "Half window height in pixel.")

(defvar-local osm--nx 0
  "Number of tiles in x direction.")

(defvar-local osm--ny 0
  "Number of tiles in y direction.")

(defvar-local osm--zoom nil
  "Zoom level of the map.")

(defvar-local osm--x nil
  "Y coordinate on the map in pixel.")

(defvar-local osm--y nil
  "X coordinate on the map in pixel.")

(defvar-local osm--overlay-table nil
  "Overlay hash table.")

(defvar-local osm--transient-pin nil
  "Transient pin.")

(defun osm--boundingbox-to-zoom (lat1 lat2 lon1 lon2)
  "Compute zoom level from boundingbox LAT1 to LAT2 and LON1 to LON2."
  (let ((w (/ (frame-pixel-width) 256))
        (h (/ (frame-pixel-height) 256)))
    (max (osm--server-property :min-zoom)
         (min
          (osm--server-property :max-zoom)
          (min (logb (/ w (abs (- (osm--lon-to-normalized-x lon1) (osm--lon-to-normalized-x lon2)))))
               (logb (/ h (abs (- (osm--lat-to-normalized-y lat1) (osm--lat-to-normalized-y lat2))))))))))

(defun osm--lon-to-normalized-x (lon)
  "Convert LON to normalized x coordinate."
  (/ (+ lon 180.0) 360.0))

(defun osm--lat-to-normalized-y (lat)
  "Convert LAT to normalized y coordinate."
  (setq lat (* lat (/ float-pi 180.0)))
  (- 0.5 (/ (log (+ (tan lat) (/ 1 (cos lat)))) float-pi 2)))

(defun osm--x-to-lon (x zoom)
  "Return longitude in degrees for X/ZOOM."
  (- (/ (* x 360.0) 256.0 (expt 2.0 zoom)) 180.0))

(defun osm--y-to-lat (y zoom)
  "Return latitude in degrees for Y/ZOOM."
  (setq y (* float-pi (- 1 (* 2 (/ y 256.0 (expt 2.0 zoom))))))
  (/ (* 180 (atan (/ (- (exp y) (exp (- y))) 2))) float-pi))

(defun osm--lon ()
  "Return longitude in degrees."
  (osm--x-to-lon osm--x osm--zoom))

(defun osm--lat ()
  "Return latitude in degrees."
  (osm--y-to-lat osm--y osm--zoom))

(defun osm--lon-to-x (lon zoom)
  "Convert LON/ZOOM to x coordinate in pixel."
  (floor (* 256 (expt 2.0 zoom) (osm--lon-to-normalized-x lon))))

(defun osm--lat-to-y (lat zoom)
  "Convert LAT/ZOOM to y coordinate in pixel."
  (floor (* 256 (expt 2.0 zoom) (osm--lat-to-normalized-y lat))))

(defun osm--server-property (prop)
  "Return server property PROP."
  (or (plist-get (alist-get osm-server osm-server-list) prop)
      (plist-get osm-server-defaults prop)))

(defun osm--tile-url (x y zoom)
  "Return tile url for coordinate X, Y and ZOOM."
  (let* ((url (osm--server-property :url))
         (sub (osm--server-property :subdomains))
         (count (length sub)))
    (prog1
        (format-spec url `((?z . ,zoom) (?x . ,x) (?y . ,y)
                           (?s . ,(nth (mod osm--subdomain-index count) sub))))
      (setq osm--subdomain-index (mod (1+ osm--subdomain-index) count)))))

(defun osm--tile-file (x y zoom)
  "Return tile file name for coordinate X, Y and ZOOM."
  (expand-file-name
   (format "%s%s/%d-%d-%d.%s"
           osm-tile-directory
           (symbol-name osm-server)
           zoom x y
           (file-name-extension
            (url-file-nondirectory
             (osm--server-property :url))))))

(defun osm--enqueue-download (x y)
  "Enqueue tile X/Y for download."
  (when (let ((n (expt 2 osm--zoom))) (and (>= x 0) (>= y 0) (< x n) (< y n)))
    (let ((job `(,x ,y . ,osm--zoom)))
      (unless (or (member job osm--download-queue) (member job osm--download-active))
        (setq osm--download-queue (nconc osm--download-queue (list job)))))))

(defun osm--download ()
  "Download next tile in queue."
  (when-let (job (and (< (length osm--download-active)
                         (* (length (osm--server-property :subdomains))
                            (osm--server-property :max-connections)))
                      (pop osm--download-queue)))
    (push job osm--download-active)
    (pcase-let* ((`(,x ,y . ,zoom) job)
                 (buffer (current-buffer))
                 (dst (osm--tile-file x y zoom))
                 (tmp (concat dst ".tmp"))
                 (dir (file-name-directory tmp)))
      (unless (file-exists-p dir)
        (make-directory dir t))
      (make-process
       :name (format "osm %s %s %s" x y zoom)
       :connection-type 'pipe
       :noquery t
       :command
       `("curl" ,@(split-string osm-curl-options) "--output" ,tmp ,(osm--tile-url x y zoom))
       :filter #'ignore
       :sentinel
       (lambda (_proc status)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (and (string-match-p "finished" status)
                        (eq osm--zoom zoom))
               (ignore-errors (rename-file tmp dst t))
               (osm--display-tile x y (osm--get-tile x y)))
             (delete-file tmp)
             (force-mode-line-update)
             (setq osm--download-active (delq job osm--download-active))
             (osm--download)))))
      (osm--download))))

(defun osm-mouse-drag (event)
  "Handle drag EVENT."
  (interactive "@e")
  (pcase-let ((`(,sx . ,sy) (posn-x-y (event-start event)))
              (win (selected-window))
              (map (make-sparse-keymap)))
    (cl-incf sx osm--x)
    (cl-incf sy osm--y)
    (define-key map [mouse-movement]
      (lambda (event)
        (interactive "e")
        (setq event (event-start event))
        (when (eq win (posn-window event))
          (define-key map [mouse-1] #'ignore)
          (define-key map [mouse-2] #'ignore)
          (define-key map [mouse-3] #'ignore)
          (define-key map [drag-mouse-1] #'ignore)
          (define-key map [drag-mouse-2] #'ignore)
          (define-key map [drag-mouse-3] #'ignore)
          (pcase-let ((`(,ex . ,ey) (posn-x-y event)))
            (setq osm--x (- sx ex)
                  osm--y (- sy ey))
            (osm--update)))))
    (setq track-mouse 'dragging)
    (set-transient-map map
                       (lambda () (eq (car-safe last-input-event) 'mouse-movement))
                       (lambda () (setq track-mouse nil)))))

(defun osm--zoom-in-wheel (_n)
  "Zoom in with the mouse wheel."
  (pcase-let ((`(,x . ,y) (posn-x-y (event-start last-input-event))))
    (when (< osm--zoom (osm--server-property :max-zoom))
      (cl-incf osm--x (/ (- x osm--wx) 2))
      (cl-incf osm--y (/ (- y osm--wy) 2))
      (osm-zoom-in))))

(defun osm--zoom-out-wheel (_n)
  "Zoom out with the mouse wheel."
  (pcase-let ((`(,x . ,y) (posn-x-y (event-start last-input-event))))
    (when (> osm--zoom (osm--server-property :min-zoom))
      (cl-decf osm--x (- x osm--wx))
      (cl-decf osm--y (- y osm--wy))
      (osm-zoom-out))))

(defun osm-center-click (event)
  "Center to the location of the click EVENT."
  (interactive "e")
  (pcase-let ((`(,x . ,y) (posn-x-y (event-start event))))
    (when (< osm--zoom (osm--server-property :max-zoom))
      (cl-incf osm--x (- x osm--wx))
      (cl-incf osm--y (- y osm--wy))
      (osm--put-transient-pin 'osm-center osm--x osm--y "Center")
      (osm--update))))

(defun osm-bookmark-set-click (event)
  "Create bookmark at position of click EVENT."
  (interactive "@e")
  (pcase-let ((`(,x . ,y) (posn-x-y (event-start event))))
    (osm--put-transient-pin 'osm-selected-bookmark
                            (+ osm--x (- x osm--wx))
                            (+ osm--y (- y osm--wy))
                            "New bookmark")
    (osm-bookmark-set)))

(defun osm-org-link-click (event)
  "Store link at position of click EVENT."
  (interactive "@e")
  (pcase-let ((`(,x . ,y) (posn-x-y (event-start event))))
    (osm--put-transient-pin 'osm-org-link
                            (+ osm--x (- x osm--wx))
                            (+ osm--y (- y osm--wy))
                            "New Org Link")
    (call-interactively 'org-store-link)))

(defun osm--pin-at (type x y)
  "Get pin of TYPE at X/Y."
  (let ((x (+ osm--x (- x osm--wx)))
        (y (+ osm--y (- y osm--wy)))
        (min most-positive-fixnum)
        found)
    (dolist (pin (car (osm--get-overlays (/ x 256) (/ y 256))))
      (pcase-let ((`(,p ,q ,id . ,_) pin))
        (when (eq type id)
          (let ((d (+ (* (- p x) (- p x)) (* (- q y) (- q y)))))
            (when (and (>= q y) (< q (+ y 50)) (>= p (- x 20)) (< p (+ x 20)) (< d min))
              (setq min d found pin))))))
    found))

(defun osm-bookmark-select-click (event)
  "Select bookmark at position of click EVENT."
  (interactive "@e")
  (pcase-let* ((`(,x . ,y) (posn-x-y (event-start event))))
    (when-let (pin (osm--pin-at 'osm-bookmark x y))
      (message "%s" (cdddr pin))
      (osm--put-transient-pin 'osm-selected-bookmark
                              (car pin) (cadr pin)
                              (cdddr pin))
      (osm--update))))

(defun osm-poi-click (event)
  "Select point of interest at position of click EVENT."
  (interactive "@e")
  (pcase-let* ((`(,x . ,y) (posn-x-y (event-start event))))
    (when-let (pin (osm--pin-at 'osm-poi x y))
      (message "%s" (cdddr pin))
      (osm--goto
       (list
        (osm--y-to-lat (cadr pin) osm--zoom)
        (osm--x-to-lon (car pin) osm--zoom)
        osm--zoom)
       nil))))

(defun osm-zoom-in (&optional n)
  "Zoom N times into the map."
  (interactive "p")
  (osm--barf-unless-osm)
  (setq n (or n 1))
  (cl-loop for i from n above 0
           if (< osm--zoom (osm--server-property :max-zoom)) do
           (setq osm--zoom (1+ osm--zoom)
                 osm--x (* osm--x 2)
                 osm--y (* osm--y 2)))
  (cl-loop for i from n below 0
           if (> osm--zoom (osm--server-property :min-zoom)) do
           (setq osm--zoom (1- osm--zoom)
                 osm--x (/ osm--x 2)
                 osm--y (/ osm--y 2)))
  (osm--update))

(defun osm-zoom-out (&optional n)
  "Zoom N times out of the map."
  (interactive "p")
  (osm-zoom-in (- (or n 1))))

(defun osm--move (dx dy step)
  "Move by DX/DY with STEP size."
  (osm--barf-unless-osm)
  (setq osm--x (min (* 256 (1- (expt 2 osm--zoom)))
                    (max 0 (+ osm--x (* dx step))))
        osm--y (min (* 256 (1- (expt 2 osm--zoom)))
                    (max 0 (+ osm--y (* dy step)))))
  (osm--update))

(defun osm-right (&optional n)
  "Move N small stepz to the right."
  (interactive "p")
  (osm--move (or n 1) 0 osm-small-step))

(defun osm-down (&optional n)
  "Move N small stepz down."
  (interactive "p")
  (osm--move 0 (or n 1) osm-small-step))

(defun osm-up (&optional n)
  "Move N small stepz up."
  (interactive "p")
  (osm-down (- (or n 1))))

(defun osm-left (&optional n)
  "Move N small stepz to the left."
  (interactive "p")
  (osm-right (- (or n 1))))

(defun osm-right-right (&optional n)
  "Move N large stepz to the right."
  (interactive "p")
  (osm--move (or n 1) 0 osm-large-step))

(defun osm-down-down (&optional n)
  "Move N large stepz down."
  (interactive "p")
  (osm--move 0 (or n 1) osm-large-step))

(defun osm-up-up (&optional n)
  "Move N large stepz up."
  (interactive "p")
  (osm-down-down (- (or n 1))))

(defun osm-left-left (&optional n)
  "Move N large stepz to the left."
  (interactive "p")
  (osm-right-right (- (or n 1))))

(defun osm--purge-directory ()
  "Clean tile directory."
  (when (and (integerp osm-max-age)
             (> (- (float-time) osm--purge-directory) (* 60 60 24)))
    (setq osm--purge-directory (float-time))
    (run-with-idle-timer
     30 nil
     (lambda ()
       (dolist (file
                (ignore-errors
                  (directory-files-recursively
                   osm-tile-directory
                   "\\.\\(?:png\\|jpe?g\\)\\(?:\\.tmp\\)?\\'" nil)))
         (when (> (float-time
                   (time-since
                    (file-attribute-modification-time
                     (file-attributes file))))
                  (* 60 60 24 osm-max-age))
           (delete-file file)))))))

(define-derived-mode osm-mode special-mode "Osm"
  "OpenStreetMap viewer mode."
  :interactive nil
  (dolist (type '(svg jpeg png))
    (unless (image-type-available-p type)
      (warn "osm: Support for %s images is missing" type)))
  (setq-local osm-server osm-server
              line-spacing nil
              cursor-type nil
              cursor-in-non-selected-windows nil
              left-fringe-width 1
              right-fringe-width 1
              left-margin-width 0
              right-margin-width 0
              truncate-lines t
              show-trailing-whitespace nil
              display-line-numbers nil
              buffer-read-only t
              fringe-indicator-alist '((truncation . nil))
              revert-buffer-function #'osm--revert
              mode-line-process '(:eval (osm--download-queue-info))
              mouse-wheel-progressive-speed nil
              mwheel-scroll-up-function #'osm--zoom-out-wheel
              mwheel-scroll-down-function #'osm--zoom-in-wheel
              mwheel-scroll-left-function #'osm--zoom-out-wheel
              mwheel-scroll-right-function #'osm--zoom-in-wheel
              bookmark-make-record-function #'osm--make-bookmark)
  (add-hook 'change-major-mode-hook #'osm--barf-change-mode nil 'local)
  (add-hook 'write-contents-functions #'osm--barf-write nil 'local)
  (add-hook 'window-size-change-functions #'osm--resize nil 'local))

(defun osm--barf-write ()
  "Barf for write operation."
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (set-visited-file-name nil)
  (error "Writing the buffer to a file is not supported"))

(defun osm--barf-change-mode ()
  "Barf for change mode operation."
  (error "Changing the major mode is not supported"))

(defun osm--barf-unless-osm ()
  "Barf if not an `osm-mode' buffer."
  (unless (eq major-mode #'osm-mode)
    (error "Not an osm-mode buffer")))

(defun osm--pin-inside-p (x y p q)
  "Return non-nil if pin P/Q is inside tile X/Y."
  (setq x (* x 256) y (* y 256))
  (and (>= p (- x 32)) (< p (+ x 256 32))
       (>= q y) (< q (+ y 256 64))))

(defun osm--put-pin (pins id x y help)
  "Put pin at X/Y with HELP and ID in PINS hash table."
  (let ((x0 (/ x 256)) (y0 (/ y 256))
        (pin `(,x, y ,id . ,help)))
    (push pin (gethash (cons x0 y0) pins))
    (cl-loop
     for i from -1 to 1 do
     (cl-loop
      for j from -1 to 0 do
      (let ((x1 (/ (+ x (* 32 i)) 256))
            (y1 (/ (+ y (* 64 j)) 256)))
        (unless (and (= x0 x1) (= y0 y1))
          (push pin (gethash (cons x1 y1) pins))))))))

(defun osm--compute-pins ()
  "Compute pin hash table."
  (let ((pins (make-hash-table :test #'equal)))
    (osm--put-pin pins 'osm-home
                  (osm--lon-to-x (cadr osm-home) osm--zoom)
                  (osm--lat-to-y (car osm-home) osm--zoom)
                  "Home")
    (bookmark-maybe-load-default-file)
    (dolist (bm bookmark-alist)
      (when (eq (bookmark-prop-get bm 'handler) #'osm-bookmark-jump)
        (let ((coord (bookmark-prop-get bm 'coordinates)))
          (osm--put-pin pins 'osm-bookmark
                        (osm--lon-to-x (cadr coord) osm--zoom)
                        (osm--lat-to-y (car coord) osm--zoom)
                        (car bm)))))
    (dolist (file osm--gpx-files)
      (dolist (pt (cddr file))
        (osm--put-pin pins 'osm-poi
                      (osm--lon-to-x (cddr pt) osm--zoom)
                      (osm--lat-to-y (cadr pt) osm--zoom)
                      (car pt))))
    pins))

;; TODO The Bresenham algorithm used here to add the line segments
;; to the tiles has the issue that lines which go along a tile
;; border may be drawn only partially. We can fix this by starting
;; Bresenham at (x0±line width, y0±line width).
(defun osm--compute-tracks ()
  "Compute track hash table."
  (let ((tracks (make-hash-table :test #'equal)))
    (dolist (file osm--gpx-files)
      (dolist (seg (cadr file))
        (let ((p0 (cons (osm--lon-to-x (cdar seg) osm--zoom)
                        (osm--lat-to-y (caar seg) osm--zoom))))
          (dolist (pt (cdr seg))
            (let* ((px1 (osm--lon-to-x (cdr pt) osm--zoom))
                   (py1 (osm--lat-to-y (car pt) osm--zoom))
                   (pdx (- px1 (car p0)))
                   (pdy (- py1 (cdr p0))))
              ;; Ignore point if too close to last point
              (unless (< (+ (* pdx pdx) (* pdy pdy)) 50)
                (let* ((p1 (cons px1 py1))
                       (seg (cons p0 p1))
                       (x0 (/ (car p0) 256))
                       (y0 (/ (cdr p0) 256))
                       (x1 (/ px1 256))
                       (y1 (/ py1 256))
                       (sx (if (< x0 x1) 1 -1))
                       (sy (if (< y0 y1) 1 -1))
                       (dx (* sx (- x1 x0)))
                       (dy (* sy (- y0 y1)))
                       (err (+ dx dy)))
                  ;; Bresenham
                  (while
                      (let ((ey (> (* err 2) dy))
                            (ex (< (* err 2) dx)))
                        (push seg (gethash (cons x0 y0) tracks))
                        (unless (and (= x0 x1) (= y0 y1))
                          (when (and ey ex)
                            (push seg (gethash (cons x0 (+ y0 sy)) tracks))
                            (push seg (gethash (cons (+ x0 sx) y0) tracks)))
                          (when ey
                            (cl-incf err dy)
                            (cl-incf x0 sx))
                          (when ex
                            (cl-incf err dx)
                            (cl-incf y0 sy))
                          t)))
                  (setq p0 p1))))))))
    tracks))

(defun osm--get-overlays (x y)
  "Compute overlays and return the overlays in tile X/Y."
  (unless (eq (car osm--overlay-table) osm--zoom)
    ;; TODO: Do not compute overlays for the entire map, only for a reasonable viewport around the
    ;; current center, maybe 10x the window size. Otherwise the spatial hash map for the tracks can
    ;; get very large if a line segment spans many tiles.
    (setq osm--overlay-table (list osm--zoom (osm--compute-pins) (osm--compute-tracks))))
  (let ((pins (gethash (cons x y) (cadr osm--overlay-table)))
        (tracks (gethash (cons x y) (caddr osm--overlay-table))))
    (and (or pins tracks) (cons pins tracks))))

(autoload 'svg--image-data "svg")
(defun osm--draw-tile (x y tpin)
  "Make tile at X/Y from FILE.
TPIN is an optional transient pin."
  (let ((file (osm--tile-file x y osm--zoom))
        overlays)
    (when (file-exists-p file)
      (if (or (setq overlays (osm--get-overlays x y)) (eq osm-tile-border t) tpin)
          (let* ((areas nil)
                 (x0 (* 256 x))
                 (y0 (* 256 y))
                 (svg-pin
                  (lambda (pin)
                    (pcase-let* ((`(,p ,q ,id . ,help) pin)
                                 (`(,_ ,bg ,fg) (assq id osm-pin-colors)))
                      (setq p (- p x0) q (- q y0))
                      (push `((poly . [,p ,q ,(- p 20) ,(- q 40) ,p ,(- q 50) ,(+ p 20) ,(- q 40) ])
                              ,id (help-echo ,(truncate-string-to-width help 20 0 nil t) pointer hand))
                            areas)
                      ;; https://commons.wikimedia.org/wiki/File:Simpleicons_Places_map-marker-1.svg
                      (format "
<g fill='%s' stroke='%s' stroke-width='9' transform='translate(%s %s) scale(0.09) translate(-256 -512)'>
<path d='M256 0C167.641 0 96 71.625 96 160c0 24.75 5.625 48.219 15.672
69.125C112.234 230.313 256 512 256 512l142.594-279.375
C409.719 210.844 416 186.156 416 160C416 71.625 344.375
0 256 0z M256 256c-53.016 0-96-43-96-96s42.984-96 96-96
c53 0 96 43 96 96S309 256 256 256z'/>
</g>" bg fg p q))))
                 (svg-text
                  (concat "<svg width='256' height='256' version='1.1'
xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink'>
<image xlink:href='"
                          (if (> emacs-major-version 27)
                              (file-name-nondirectory file)
                            ;; NOTE: On Emacs 27, :base-uri and embedding by file
                            ;; path is not supported. Use the less efficient base64 encoding.
                            (svg--image-data
                             file
                             (if (member (file-name-extension file) '("jpg" "jpeg"))
                                 "image/jpeg" "image/png")
                             nil))
                          "' height='256' width='256'/>"
                          (when-let (track (cdr overlays))
                            (format
                             "<path style='%s' d='%s'/>"
                             osm-track-style
                             (let (last)
                               (mapconcat
                                (pcase-lambda (`(,beg . ,end))
                                  (prog1
                                      (if (equal beg last)
                                          (format "L%s %s" (- (car end) x0) (- (cdr end) y0))
                                        (format "M%s %sL%s %s"
                                                (- (car beg) x0) (- (cdr beg) y0)
                                                (- (car end) x0) (- (cdr end) y0)))
                                    (setq last end)))
                                track ""))))
                          (pcase-exhaustive osm-tile-border
                            ('nil nil)
                            ('debug "<path d='M 1 1 L 1 255 255 255 255 1 Z' stroke='#000' stroke-width='2' fill='none'/>")
                            ('t "<path d='M 0 0 L 0 256 256 256' stroke='#000' fill='none'/>"))
                          (mapconcat svg-pin (car overlays) "")
                          (and tpin (funcall svg-pin tpin))
                          "</svg>")))
            (list 'image :width 256 :height 256 :type 'svg :base-uri file :data svg-text :map areas))
        (list 'image :width 256 :height 256 :file file :type
              (if (member (file-name-extension file) '("jpg" "jpeg"))
                  'jpeg 'png))))))

(defun osm--get-tile (x y)
  "Get tile at X/Y."
  (if (pcase osm--transient-pin
        (`(,p ,q . ,_) (osm--pin-inside-p x y p q)))
      (osm--draw-tile x y osm--transient-pin)
    (let* ((key `(,osm-server ,osm--zoom ,x . ,y))
           (tile (and osm--tile-cache (gethash key osm--tile-cache))))
      (if tile
          (progn (setcar tile osm--tile-cookie) (cdr tile))
        (setq tile (osm--draw-tile x y nil))
        (when tile
          (when osm-max-tiles
            (unless osm--tile-cache
              (setq osm--tile-cache (make-hash-table :test #'equal :size osm-max-tiles)))
            (puthash key (cons osm--tile-cookie tile) osm--tile-cache))
          tile)))))

(defun osm--display-tile (x y tile)
  "Display TILE at X/Y."
  (let ((i (- x (/ (- osm--x osm--wx) 256)))
        (j (- y (/ (- osm--y osm--wy) 256))))
    (when (and (>= i 0) (< i osm--nx) (>= j 0) (< j osm--ny))
      (let* ((mx (if (= 0 i) (mod (- osm--x osm--wx) 256) 0))
             (my (if (= 0 j) (mod (- osm--y osm--wy) 256) 0))
             (pos (+ (point-min) (* j (1+ osm--nx)) i)))
        (unless tile
          (setq tile (cons 'image osm--placeholder)))
        (with-silent-modifications
          (put-text-property
           pos (1+ pos) 'display
           (if (or (/= 0 mx) (/= 0 my))
               `((slice ,mx ,my ,(- 256 mx) ,(- 256 my)) ,tile)
             tile)))))))

;;;###autoload
(defun osm-home ()
  "Go to home coordinates."
  (interactive)
  (osm--goto osm-home nil))

(defun osm--download-queue-info ()
  "Return queue info string."
  (let ((n (length osm--download-queue)))
    (if (> n 0)
        (format "[%s/%s]" (length osm--download-active) n))))

(defun osm--revert (&rest _)
  "Revert osm buffers."
  (dolist (buf (buffer-list))
    (when (eq (buffer-local-value 'major-mode buf) #'osm-mode)
      (with-current-buffer buf
        (setq osm--tile-cache nil osm--overlay-table nil)
        (osm--update)))))

(defun osm--resize (&rest _)
  "Resize buffer."
  (when (eq major-mode #'osm-mode)
    (osm--update)))

(defun osm--header-button (text action)
  "Format header line button with TEXT and ACTION."
  (propertize text
              'keymap (let ((map (make-sparse-keymap)))
                        (define-key map [header-line mouse-1]
                          (lambda ()
                            (interactive "@")
                            (call-interactively action)))
                        map)
              'face '(:box (:line-width -2 :style released-button))
              'mouse-face '(:box (:line-width -2 :style pressed-button))))

(defun osm--update-header ()
  "Update header line."
  (let* ((meter-per-pixel (/ (* 156543.03 (cos (/ (osm--lat) (/ 180.0 float-pi)))) (expt 2 osm--zoom)))
         (server (osm--server-property :name))
         (meter 1) (idx 0)
         (factor '(2 2.5 2)))
    (while (and (< idx 20) (< (/ (* meter (nth (mod idx 3) factor)) meter-per-pixel) 150))
      (setq meter (round (* meter (nth (mod idx 3) factor))))
      (cl-incf idx))
    (setq-local
     header-line-format
     (concat
      (format #(" %7.2f°" 0 6 (face bold)) (osm--lat))
      (format #(" %7.2f°" 0 6 (face bold)) (osm--lon))
      (propertize " " 'display '(space :align-to (- center 10)))
      (format "%3s " (if (>= meter 1000) (/ meter 1000) meter))
      (if (>= meter 1000) "km " "m ")
      (propertize " " 'face '(:inverse-video t)
                  'display '(space :width (3)))
      (propertize " " 'face '(:strike-through t)
                  'display `(space :width (,(floor (/ meter meter-per-pixel)))))
      (propertize " " 'face '(:inverse-video t)
                  'display '(space :width (3)))
      (propertize " " 'display `(space :align-to (- right ,(+ 13 (length server)) (10))))
      (format " Z%-2d " osm--zoom)
      (osm--header-button " + " #'osm-zoom-in)
      (propertize " " 'display '(space :width (1)))
      (osm--header-button " - " #'osm-zoom-out)
      (propertize " " 'display '(space :width (1)))
      (osm--header-button (format " %s " server) #'osm-server)))))

(defun osm--update ()
  "Update map display."
  (osm--barf-unless-osm)
  (rename-buffer (osm--buffer-name) 'unique)
  (osm--update-sizes)
  (osm--update-header)
  (osm--update-buffer)
  (osm--process-download-queue)
  (osm--purge-tile-cache)
  (osm--purge-directory))

(defun osm--update-sizes ()
  "Update window sizes."
  (let* ((windows (or (get-buffer-window-list) (list (frame-root-window))))
         (win-width (cl-loop for w in windows maximize (window-pixel-width w)))
         (win-height (cl-loop for w in windows maximize (window-pixel-height w))))
    (setq osm--wx (/ win-width 2)
          osm--wy (/ win-height 2)
          osm--nx (1+ (ceiling win-width 256))
          osm--ny (1+ (ceiling win-height 256)))))

(defun osm--update-buffer ()
  "Update buffer display."
  (with-silent-modifications
    (erase-buffer)
    (dotimes (_j osm--ny)
      (insert (make-string osm--nx ?\s) "\n"))
    (goto-char (point-min))
    (dotimes (j osm--ny)
      (dotimes (i osm--nx)
        (let* ((x (+ i (/ (- osm--x osm--wx) 256)))
               (y (+ j (/ (- osm--y osm--wy) 256)))
               (tile (osm--get-tile x y)))
          (osm--display-tile x y tile)
          (unless tile (osm--enqueue-download x y)))))))

(defun osm--process-download-queue ()
  "Process the download queue."
  (setq osm--download-queue
        (sort
         (cl-loop for job in osm--download-queue
                  for (x y . zoom) = job
                  for i = (- x (/ (- osm--x osm--wx) 256))
                  for j = (- y (/ (- osm--y osm--wy) 256))
                  if (and (= zoom osm--zoom)
                          (>= i 0) (< i osm--nx)
                          (>= j 0) (< j osm--ny))
                  collect job)
         (pcase-lambda (`(,x1 ,y1 . ,_z1) `(,x2 ,y2 . ,_z2))
           (setq x1 (- x1 (/ osm--x 256)) y1 (- y1 (/ osm--y 256))
                 x2 (- x2 (/ osm--x 256)) y2 (- y2 (/ osm--y 256)))
           (< (+ (* x1 x1) (* y1 y1)) (+ (* x2 x2) (* y2 y2))))))
  (osm--download))

(defun osm--purge-tile-cache ()
  "Purge old tiles from the tile cache."
  (cl-incf osm--tile-cookie)
  (when (and osm--tile-cache (> (hash-table-count osm--tile-cache) osm-max-tiles))
    (let (items)
      (maphash (lambda (k v) (push (cons (car v) k) items)) osm--tile-cache)
      (setq items (sort items #'car-less-than-car))
      (dotimes (_ (- (hash-table-count osm--tile-cache) osm-max-tiles))
        (remhash (cdr (pop items)) osm--tile-cache)))))

(defun osm--make-bookmark (&optional name lat lon)
  "Make osm bookmark record with NAME at LAT/LON."
  (setq bookmark-current-bookmark nil) ;; Reset bookmark to use new name
  `(,(or name (osm--bookmark-name))
    (coordinates ,(or lat (osm--lat)) ,(or lon (osm--lon)) ,osm--zoom)
    (server . ,osm-server)
    (handler . ,#'osm-bookmark-jump)))

(defun osm--org-link-data ()
  "Return Org link data."
  (pcase-let ((`(,lat ,lon ,name) (osm--location-data 'osm-org-link "Org link")))
    (setq name (string-remove-prefix "osm: " (osm--bookmark-name name)))
    (list lat lon osm--zoom
          (and (not (eq osm-server (default-value 'osm-server))) osm-server)
          (if (eq osm-server (default-value 'osm-server))
              (string-remove-suffix (concat " " (osm--server-property :name)) name)
            name))))

(defun osm--buffer-name ()
  "Return buffer name."
  (format "*osm: %.2f° %.2f° Z%s %s*"
          (osm--lat) (osm--lon) osm--zoom
          (osm--server-property :name)))

(defun osm--bookmark-name (&optional loc)
  "Return bookmark name with optional LOC name."
  (format "osm: %s%.2f° %.2f° Z%s %s"
          (if loc (concat loc ", ") "")
          (osm--lat) (osm--lon) osm--zoom
          (osm--server-property :name)))

(defun osm--goto (at server)
  "Go to AT, change SERVER."
  ;; Server not found
  (when (and server (not (assq server osm-server-list))) (setq server nil))
  (with-current-buffer
      (or
       (and (eq major-mode #'osm-mode) (current-buffer))
       (pcase-let* ((`(,def-lat ,def-lon ,def-zoom) (or at osm-home))
                    (def-x (osm--lon-to-x def-lon def-zoom))
                    (def-y (osm--lat-to-y def-lat def-zoom))
                    (def-server (or server osm-server)))
         ;; Search for existing buffer
         (cl-loop
          for buf in (buffer-list) thereis
          (and (eq (buffer-local-value 'major-mode buf) #'osm-mode)
               (eq (buffer-local-value 'osm-server buf) def-server)
               (eq (buffer-local-value 'osm--zoom buf) def-zoom)
               (eq (buffer-local-value 'osm--x buf) def-x)
               (eq (buffer-local-value 'osm--y buf) def-y)
               buf)))
       (generate-new-buffer "*osm*"))
    (unless (eq major-mode #'osm-mode)
      (osm-mode))
    (when (and server (not (eq osm-server server)))
      (setq osm-server server
            osm--download-active nil
            osm--download-queue nil))
    (when (or (not (and osm--x osm--y)) at)
      (setq at (or at osm-home)
            osm--zoom (nth 2 at)
            osm--x (osm--lon-to-x (nth 1 at) osm--zoom)
            osm--y (osm--lat-to-y (nth 0 at) osm--zoom))
      (osm--put-transient-pin 'osm-center osm--x osm--y "Center"))
    (prog1 (pop-to-buffer (current-buffer))
      (osm--update))))

(defun osm--put-transient-pin (id x y help)
  "Set transient pin at X/Y with ID and HELP."
  (let ((buffer (current-buffer))
        (sym (make-symbol "osm--remove-transient-pin")))
    (fset sym (lambda ()
                (with-current-buffer buffer
                  ;; Handle bookmark deletion and renaming
                  (pcase this-command
                    ((or 'undefined 'ignore)
                     nil)
                    ((and (guard (eq id 'osm-selected-bookmark))
                          cmd (or 'osm-bookmark-delete 'osm-bookmark-rename))
                     (remove-hook 'pre-command-hook sym)
                     (setq osm--transient-pin nil
                           this-command
                           (lambda ()
                             (interactive)
                             (funcall cmd help))))
                    (_
                     (remove-hook 'pre-command-hook sym)
                     (when osm--transient-pin
                       (setq osm--transient-pin nil)
                       (osm--update)))))))
    (add-hook 'pre-command-hook sym)
    (setq osm--transient-pin `(,x ,y ,id . ,help))))

;;;###autoload
(defun osm-goto (lat lon zoom)
  "Go to LAT/LON/ZOOM."
  (interactive
   (pcase-let ((`(,lat ,lon ,zoom)
                (mapcar #'string-to-number
                        (split-string (read-string "Lat Lon (Zoom): ") nil t))))
     (setq zoom (or zoom 11))
     (unless (and (numberp lat) (numberp lon) (numberp zoom))
       (error "Invalid coordinate"))
     (list lat lon zoom)))
  (osm--goto (list lat lon zoom) nil))

;;;###autoload
(defun osm-bookmark-jump (bm)
  "Jump to osm bookmark BM."
  (interactive (list (osm--bookmark-read)))
  (set-buffer (osm--goto (bookmark-prop-get bm 'coordinates)
                         (bookmark-prop-get bm 'server))))

;;;###autoload
(defun osm-bookmark-delete (bm)
  "Delete osm bookmark BM."
  (interactive (list (osm--bookmark-read)))
  (bookmark-delete bm)
  (osm--revert))

;;;###autoload
(defun osm-bookmark-rename (old-name)
  "Rename osm bookmark OLD-NAME."
  (interactive (list (car (osm--bookmark-read))))
  (unwind-protect
      (bookmark-rename
       old-name
       (read-from-minibuffer
        "New name: " old-name nil nil
        'bookmark-history old-name))
    (osm--revert)))

(defun osm--bookmark-read ()
  "Read bookmark name."
  (bookmark-maybe-load-default-file)
  (or (assoc
       (completing-read
        "Bookmark: "
        (or (cl-loop for bm in bookmark-alist
                     if (eq (bookmark-prop-get bm 'handler) #'osm-bookmark-jump)
                     collect (car bm))
            (error "No bookmarks found"))
        nil t nil 'bookmark-history)
       bookmark-alist)
      (error "No bookmark selected")))

(defun osm-bookmark-set ()
  "Create osm bookmark."
  (interactive)
  (osm--barf-unless-osm)
  (unwind-protect
      (pcase-let* ((`(,lat ,lon ,desc) (osm--location-data 'osm-selected-bookmark "Bookmark"))
                   (def (osm--bookmark-name desc))
                   (name (read-from-minibuffer "Bookmark name: " def nil nil 'bookmark-history def))
                   (bookmark-make-record-function
                    (lambda () (osm--make-bookmark name lat lon))))
        (bookmark-set name)
        (message "Stored bookmark: %s" name))
    (osm--revert)))

(defun osm--location-data (id help)
  "Fetch location info for ID with HELP."
  (unless osm--transient-pin
    (osm--put-transient-pin id osm--x osm--y help))
  (let ((lat (osm--y-to-lat (cadr osm--transient-pin) osm--zoom))
        (lon (osm--x-to-lon (car osm--transient-pin) osm--zoom)))
    (message "%s: Fetching name of %.2f %.2f..." help lat lon)
    ;; Redisplay before slow fetching
    (osm--update)
    (redisplay)
    (list lat lon
          (ignore-errors
            (alist-get
             'display_name
             (json-parse-string
              (shell-command-to-string
               (concat
                "curl " osm-curl-options " "
                (shell-quote-argument
                 (format "https://nominatim.openstreetmap.org/reverse?format=json&zoom=%s&lat=%s&lon=%s"
                         (min 18 (max 3 osm--zoom)) lat lon))))
              :array-type 'list
              :object-type 'alist))))))

;;;###autoload
(defun osm-search ()
  "Search for location and display the map."
  (interactive)
  ;; TODO add search bounded to current viewbox, bounded=1, viewbox=x1,y1,x2,y2
  (let* ((search (completing-read
                  "Location: "
                  (osm--sorted-table osm--search-history)
                  nil nil nil 'osm--search-history))
         (json (json-parse-string
                (shell-command-to-string
                 (concat
                  "curl " osm-curl-options " "
                  (shell-quote-argument
                   (concat "https://nominatim.openstreetmap.org/search?format=json&q="
                           (url-encode-url search)))))
                :array-type 'list
                :object-type 'alist))
         (results (mapcar
                   (lambda (x)
                     `(,(format "%s (%s° %s°)"
                                (alist-get 'display_name x)
                                (alist-get 'lat x)
                                (alist-get 'lon x))
                       ,(string-to-number (alist-get 'lat x))
                       ,(string-to-number (alist-get 'lon x))
                       ,@(mapcar #'string-to-number (alist-get 'boundingbox x))))
                   (or json (error "No results"))))
         (selected (or (cdr (assoc
                             (completing-read
                              (format "Matches for '%s': " search)
                              (osm--sorted-table results)
                              nil t)
                             results))
                       (error "No selection"))))
    (osm-goto (car selected) (cadr selected)
              (apply #'osm--boundingbox-to-zoom (cddr selected)))))

(defun osm--sorted-table (coll)
  "Sorted completion table from COLL."
  (lambda (str pred action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity))
      (complete-with-action action coll str pred))))

;;;###autoload
(defun osm-gpx-show (file)
  "Show the tracks of gpx FILE in an `osm-mode' buffer."
  (interactive "fGPX file: ")
  (let ((dom (with-temp-buffer
               (insert-file-contents file)
               (libxml-parse-xml-region (point-min) (point-max))))
        (min-lat 90) (max-lat -90) (min-lon 180) (max-lon -180))
    (setf (alist-get (abbreviate-file-name file) osm--gpx-files nil nil #'equal)
          (cons
           (cl-loop
            for trk in (dom-children dom)
            if (eq (dom-tag trk) 'trk) nconc
            (cl-loop
             for seg in (dom-children trk)
             if (eq (dom-tag seg) 'trkseg) collect
             (cl-loop
              for pt in (dom-children seg)
              if (eq (dom-tag pt) 'trkpt) collect
              (let ((lat (string-to-number (dom-attr pt 'lat)))
                    (lon (string-to-number (dom-attr pt 'lon))))
                (setq min-lat (min lat min-lat)
                      max-lat (max lat max-lat)
                      min-lon (min lon min-lon)
                      max-lon (max lon max-lon))
                (cons lat lon)))))
           (cl-loop
            for pt in (dom-children dom)
            if (eq (dom-tag pt) 'wpt) collect
            (let ((lat (string-to-number (dom-attr pt 'lat)))
                  (lon (string-to-number (dom-attr pt 'lon))))
              (setq min-lat (min lat min-lat)
                    max-lat (max lat max-lat)
                    min-lon (min lon min-lon)
                    max-lon (max lon max-lon))
              `(,(dom-text (dom-child-by-tag pt 'name)) ,lat . ,lon)))))
    (osm--revert)
    (osm-goto (/ (+ min-lat max-lat) 2) (/ (+ min-lon max-lon) 2)
              (osm--boundingbox-to-zoom min-lat max-lat min-lon max-lon))))

(defun osm-gpx-hide (file)
  "Show the tracks of gpx FILE in an `osm-mode' buffer."
  (interactive (list (completing-read "GPX file: "
                                      (or osm--gpx-files
                                          (error "No GPX files shown"))
                                      nil t nil 'file-name-history)))
  (osm--barf-unless-osm)
  (setq osm--gpx-files (assoc-delete-all file osm--gpx-files))
  (osm--revert))

;;;###autoload
(defun osm-server (server)
  "Select tile SERVER."
  (interactive
   (let* ((max-name (cl-loop for (_ . x) in osm-server-list
                             maximize (length (plist-get x :name))))
          (fmt (concat
                (propertize (format "%%-%ds " max-name)
                            'face 'font-lock-comment-face)
                " %s"))
          (servers
           (mapcar
            (lambda (x)
              (cons
               (format fmt
                       (plist-get (cdr x) :name)
                       (or (plist-get (cdr x) :description) ""))
               (car x)))
            osm-server-list))
          (selected (completing-read
                     "Server: " servers nil t nil nil
                     (format fmt
                             (osm--server-property :name)
                             (or (osm--server-property :description) "")))))
     (list (or (cdr (assoc selected servers))
               (error "No server selected")))))
  (osm--goto nil server))

(dolist (sym (list #'osm-up #'osm-down #'osm-left #'osm-right
                   #'osm-up-up #'osm-down-down #'osm-left-left #'osm-right-right
                   #'osm-zoom-out #'osm-zoom-in #'osm-bookmark-set #'osm-gpx-hide))
  (put sym 'command-modes '(osm-mode)))
(dolist (sym (list #'osm-mouse-drag #'osm-center-click #'osm-org-link-click
                   #'osm-poi-click #'osm-bookmark-set-click #'osm-bookmark-select-click))
  (put sym 'completion-predicate #'ignore))

(provide 'osm)
;;; osm.el ends here
