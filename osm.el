;;; osm.el --- OpenStreetMap viewer -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2022
;; Version: 0.6
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

;; Osm.el is a tile-based map viewer, with a responsive moveable and
;; zoomable display. The map can be controlled with the keyboard or with
;; the mouse. The viewer fetches the map tiles in parallel from tile
;; servers via the `curl' program. The package comes with a list of
;; multiple preconfigured tile servers. You can bookmark your favorite
;; locations using regular Emacs bookmarks or create links from Org
;; files to locations. Furthermore the package provides commands to
;; search for locations by name and to open and display GPX tracks.

;;; Code:

(require 'bookmark)
(require 'dom)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defgroup osm nil
  "OpenStreetMap viewer."
  :group 'web
  :prefix "osm-")

(defcustom osm-curl-options
  "--disable --fail --location --silent --max-time 30"
  "Curl command line options."
  :type 'string)

(defcustom osm-server-defaults
  '(:min-zoom 2
    :max-zoom 19
    :download-batch 4
    :max-connections 2
    :subdomains ("a" "b" "c"))
  "Default server properties."
  :type 'plist)

(defcustom osm-server-list
  '((default
     :name "Mapnik"
     :description "Standard Mapnik map provided by OpenStreetMap"
     :url "https://%s.tile.openstreetmap.org/%z/%x/%y.png"
     :group "Standard"
     :copyright ("Map data © {OpenStreetMap|https://www.openstreetmap.org/copyright} contributors"
                 "Map style: © {OpenStreetMap Standard|https://www.openstreetmap.org/copyright}"))
    (de
     :name "Mapnik(de)"
     :description "Localized Mapnik map provided by OpenStreetMap Germany"
     :url "https://%s.tile.openstreetmap.de/%z/%x/%y.png"
     :group "Standard"
     :copyright ("Map data: © {OpenStreetMap|https://www.openstreetmap.org/copyright} contributors"
                 "Map style: © {OpenStreetMap Deutschland|https://www.openstreetmap.de/germanstyle.html}"))
    (fr
     :name "Mapnik(fr)"
     :description "Localized Mapnik map by OpenStreetMap France"
     :url "https://%s.tile.openstreetmap.fr/osmfr/%z/%x/%y.png"
     :group "Standard"
     :copyright ("Map data: © {OpenStreetMap|https://www.openstreetmap.org/copyright} contributors"
                 "Map style: © {OpenStreetMap France|https://www.openstreetmap.fr/mentions-legales/}"))
    (humanitarian
     :name "Humanitarian"
     :description "Humanitarian map provided by OpenStreetMap France"
     :url "https://%s.tile.openstreetmap.fr/hot/%z/%x/%y.png"
     :group "Special Purpose"
     :copyright ("Map data: © {OpenStreetMap|https://www.openstreetmap.org/copyright} contributors"
                 "Map style: © {Humanitarian OpenStreetMap Team|https://www.hotosm.org/updates/2013-09-29_a_new_window_on_openstreetmap_data}"))
    (cyclosm
     :name "CyclOSM"
     :description "Bicycle-oriented map provided by OpenStreetMap France"
     :url "https://%s.tile.openstreetmap.fr/cyclosm/%z/%x/%y.png"
     :group "Transportation"
     :copyright ("Map data: © {OpenStreetMap|https://www.openstreetmap.org/copyright} contributors"
                 "Map style: © {CyclOSM|https://www.cyclosm.org/} contributors"))
    (openriverboatmap
     :name "OpenRiverBoatMap"
     :description "Waterways map provided by OpenStreetMap France"
     :url "https://%s.tile.openstreetmap.fr/openriverboatmap/%z/%x/%y.png"
     :group "Transportation"
     :copyright ("Map data: © {OpenStreetMap|https://www.openstreetmap.org/copyright} contributors"
                 "Map style: © {OpenRiverBoatMap|https://github.com/tilery/OpenRiverboatMap}"))
    (opentopomap
     :name "OpenTopoMap"
     :description "Topographical map provided by OpenTopoMap"
     :url "https://%s.tile.opentopomap.org/%z/%x/%y.png"
     :group "Topographical"
     :copyright ("Map data: © {OpenStreetMap|https://www.openstreetmap.org/copyright} contributors"
                 "Map style: © {OpenTopoMap|https://www.opentopomap.org} ({CC-BY-SA|https://creativecommons.org/licenses/by-sa/3.0/})"
                 "Elevation data: {SRTM|https://www2.jpl.nasa.gov/srtm/}"))
    (opvn
     :name "ÖPNV" :max-zoom 18
     :description "Base layer with public transport information"
     :url "http://%s.tile.memomaps.de/tilegen/%z/%x/%y.png"
     :group "Transportation"
     :copyright ("Map data: © {OpenStreetMap|https://www.openstreetmap.org/copyright} contributors"
                 "Map style: © {ÖPNVKarte|https://www.öpnvkarte.de}"))
    (stamen-watercolor
     :name "Stamen Watercolor"
     :description "Artistic map in watercolor style provided by Stamen"
     :url "https://stamen-tiles-%s.a.ssl.fastly.net/watercolor/%z/%x/%y.jpg"
     :group "Artistic"
     :copyright ("Map data: © {OpenStreetMap|https://www.openstreetmap.org/copyright} contributors"
                 "Map style: © {Stamen Design|http://maps.stamen.com/} ({CC-BY|https://creativecommons.org/licenses/by/3.0/})"))
    (stamen-terrain
     :name "Stamen Terrain" :max-zoom 18
     :description "Map with hill shading provided by Stamen"
     :url "https://stamen-tiles-%s.a.ssl.fastly.net/terrain/%z/%x/%y.png"
     :group "Artistic"
     :copyright ("Map data: © {OpenStreetMap|https://www.openstreetmap.org/copyright} contributors"
                 "Map style: © {Stamen Design|http://maps.stamen.com/} ({CC-BY|https://creativecommons.org/licenses/by/3.0/})"))
    (stamen-toner-dark
     :name "Stamen Toner Dark"
     :description "Artistic map in toner style provided by Stamen"
     :url "https://stamen-tiles-%s.a.ssl.fastly.net/toner/%z/%x/%y.png"
     :group "Artistic"
     :copyright ("Map data: © {OpenStreetMap|https://www.openstreetmap.org/copyright} contributors"
                 "Map style: © {Stamen Design|http://maps.stamen.com/} ({CC-BY|https://creativecommons.org/licenses/by/3.0/})"))
    (stamen-toner-light
     :name "Stamen Toner Lite"
     :description "Artistic map in toner style provided by Stamen"
     :url "https://stamen-tiles-%s.a.ssl.fastly.net/toner-lite/%z/%x/%y.png"
     :group "Artistic"
     :copyright ("Map data: © {OpenStreetMap|https://www.openstreetmap.org/copyright} contributors"
                 "Map style: © {Stamen Design|http://maps.stamen.com/} ({CC-BY|https://creativecommons.org/licenses/by/3.0/})")))
  "List of tile servers."
  :type '(alist :key-type symbol :value-type plist))

(defcustom osm-copyright t
  "Display the copyright information above the map."
  :type 'boolean)

(defcustom osm-pin-colors
  '((osm-selected-bookmark "#e20" "#600")
    (osm-selected-poi "#e20" "#600")
    (osm-bookmark "#f80" "#820")
    (osm-transient "#08f" "#028")
    (osm-link "#f6f" "#808")
    (osm-poi "#88f" "#228")
    (osm-home "#80f" "#208"))
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
  :type '(list :tag "Coordinates"
               (number :tag "Latitude  ")
               (number :tag "Longitude ")
               (number :tag "Zoom      ")))

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

(defun osm--menu-item (menu)
  "Generate menu item from MENU."
  `(menu-item
    ""
    nil :filter
    ,(lambda (&optional _)
       (select-window
        (posn-window
         (event-start last-input-event)))
       (easy-menu-filter-return (if (functionp menu)
                                    (funcall menu)
                                  menu)))))

(defvar osm--menu
  '(["Home" osm-home t]
    ["Center" osm-center t]
    ["Go to" osm-goto t]
    ["Search" osm-search t]
    ["Server" osm-server t]
    "--"
    ["Org Link" org-store-link t]
    ["Elisp Link" osm-elisp-link t]
    ("Bookmark"
     ["Set" osm-bookmark-set t]
     ["Jump" osm-bookmark-jump t]
     ["Rename" osm-bookmark-rename t]
     ["Delete" osm-bookmark-delete t])
    "--"
    ["Show GPX" osm-gpx-show t]
    ["Hide GPX" osm-gpx-hide t]
    "--"
    ["Clone" clone-buffer t]
    ["Revert" revert-buffer t]
    ["Customize" (customize-group 'osm) t])
  "Menu for `osm-mode.")

(defvar osm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [menu-bar osm--menu] (osm--menu-item osm--menu))
    (define-key map [osm-home] #'ignore)
    (define-key map [osm-link] #'ignore)
    (define-key map [osm-transient] #'ignore)
    (define-key map [osm-selected-bookmark] #'ignore)
    (define-key map [osm-selected-poi] #'ignore)
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
    (define-key map [mouse-1] #'osm-transient-click)
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
    (define-key map "c" #'osm-center)
    (define-key map "o" #'clone-buffer)
    (define-key map "h" #'osm-home)
    (define-key map "t" #'osm-goto)
    (define-key map "s" #'osm-search)
    (define-key map "v" #'osm-server)
    (define-key map "e" #'osm-elisp-link)
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

(defvar-local osm--download-processes 0
  "Number of active download processes.")

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

(defvar-local osm--lat nil
  "Latitude coordinate.")

(defvar-local osm--lon nil
  "Longitude coordinate.")

(defvar-local osm--overlay-table nil
  "Overlay hash table.")

(defvar-local osm--transient-pin nil
  "Transient pin.")

(defun osm--server-menu ()
  "Generate server menu."
  (let (menu last-group)
    (dolist (server osm-server-list)
      (let* ((plist (cdr server))
             (group (plist-get plist :group)))
        (unless (equal last-group group)
          (push (format "─── %s ───" group) menu)
          (setq last-group group))
        (push
         `[,(plist-get plist :name)
           (osm-server ',(car server))
           :style toggle
           :selected (eq osm-server ',(car server))]
         menu)))
    (nreverse menu)))

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

(defun osm--lon-to-x (lon zoom)
  "Convert LON/ZOOM to x coordinate in pixel."
  (floor (* 256 (expt 2.0 zoom) (osm--lon-to-normalized-x lon))))

(defun osm--lat-to-y (lat zoom)
  "Convert LAT/ZOOM to y coordinate in pixel."
  (floor (* 256 (expt 2.0 zoom) (osm--lat-to-normalized-y lat))))

(defsubst osm--x ()
  "Return longitude in pixel of map center."
  (osm--lon-to-x osm--lon osm--zoom))

(defsubst osm--y ()
  "Return latitude in pixel of map center."
  (osm--lat-to-y osm--lat osm--zoom))

(defsubst osm--x0 ()
  "Return longitude in pixel of top left corner."
  (- (osm--x) osm--wx))

(defsubst osm--y0 ()
  "Return latitude in pixel of top left corner."
  (- (osm--y) osm--wy))

(defun osm--server-property (prop &optional server)
  "Return server property PROP for SERVER."
  (or (plist-get (alist-get (or server osm-server) osm-server-list) prop)
      (plist-get osm-server-defaults prop)))

(defun osm--tile-url (x y zoom)
  "Return tile url for coordinate X, Y and ZOOM."
  (let ((url (osm--server-property :url))
        (sub (osm--server-property :subdomains)))
    (format-spec url `((?z . ,zoom) (?x . ,x) (?y . ,y)
                       (?s . ,(nth (mod osm--subdomain-index (length sub)) sub))))))

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

(defun osm--download-filter (output)
  "Filter function for the download process which receives OUTPUT."
  (while (string-match
          "\\`\\([0-9]+\\) \\(.*?/\\([^/]+\\)/\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)\\..+\\)\n"
          output)
    (let ((status (match-string 1 output))
          (file (match-string 2 output))
          (server (intern-soft (match-string 3 output)))
          (zoom (string-to-number (match-string 4 output)))
          (x (string-to-number (match-string 5 output)))
          (y (string-to-number (match-string 6 output))))
      (setq output (substring output (match-end 0)))
      (when (equal status "200")
        (ignore-errors (rename-file file (string-remove-suffix ".tmp" file) t))
        (when (and (= osm--zoom zoom) (eq osm-server server))
          (osm--display-tile x y (osm--get-tile x y))))
      (setq osm--download-active (delete `(,x ,y . ,zoom) osm--download-active))
      (delete-file file)))
  output)

(defun osm--download-command ()
  "Build download command."
  (let* ((count 0)
         (batch (osm--server-property :download-batch))
         (subs (length (osm--server-property :subdomains)))
         (parallel (* subs (osm--server-property :max-connections)))
         args jobs job)
    (while (and (< count batch)
                (setq job (nth (* count parallel) osm--download-queue)))
      (pcase-let ((`(,x ,y . ,zoom) job))
        (setq args `(,(osm--tile-url x y zoom)
                     ,(concat (osm--tile-file x y zoom) ".tmp")
                     "--output"
                     ,@args))
        (push job jobs)
        (cl-incf count)))
    (dolist (job jobs)
      (push job osm--download-active)
      (setq osm--download-queue (delq job osm--download-queue)))
    (setq osm--subdomain-index (mod (1+ osm--subdomain-index) subs))
    (cons `("curl" "--write-out" "%{http_code} %{filename_effective}\n"
            ,@(split-string-and-unquote osm-curl-options) ,@(nreverse args))
          jobs)))

(defun osm--download ()
  "Download next tiles from the queue."
  (when (and (< osm--download-processes
                (* (length (osm--server-property :subdomains))
                   (osm--server-property :max-connections)))
             osm--download-queue)
    (pcase-let ((dir (concat osm-tile-directory (symbol-name osm-server)))
                (`(,command . ,jobs) (osm--download-command))
                (buffer (current-buffer))
                (output ""))
      (unless (file-exists-p dir)
        (make-directory dir t))
      (cl-incf osm--download-processes)
      (make-process
       :name "*osm curl*"
       :connection-type 'pipe
       :noquery t
       :command command
       :filter
       (lambda (_proc out)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setq output (osm--download-filter (concat output out)))
             (force-mode-line-update))))
       :sentinel
       (lambda (&rest _)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (dolist (job jobs)
               (setq osm--download-active (delq job osm--download-active)))
             (cl-decf osm--download-processes)
             (osm--download)
             (force-mode-line-update)))))
      (osm--download))))

(defun osm-mouse-drag (event)
  "Handle drag EVENT."
  (interactive "@e")
  (pcase-let ((`(,sx . ,sy) (posn-x-y (event-start event)))
              (win (selected-window))
              (map (make-sparse-keymap)))
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
            (osm--move (- sx ex) (- sy ey))
            (setq sx ex sy ey)
            (osm--update)))))
    (setq track-mouse 'dragging)
    (set-transient-map map
                       (lambda () (eq (car-safe last-input-event) 'mouse-movement))
                       (lambda () (setq track-mouse nil)))))

(defun osm--zoom-in-wheel (_n)
  "Zoom in with the mouse wheel."
  (pcase-let ((`(,x . ,y) (posn-x-y (event-start last-input-event))))
    (when (< osm--zoom (osm--server-property :max-zoom))
      (osm--move (/ (- x osm--wx) 2) (/ (- y osm--wy) 2))
      (osm-zoom-in))))

(defun osm--zoom-out-wheel (_n)
  "Zoom out with the mouse wheel."
  (pcase-let ((`(,x . ,y) (posn-x-y (event-start last-input-event))))
    (when (> osm--zoom (osm--server-property :min-zoom))
      (osm--move (- osm--wx x) (- osm--wy y))
      (osm-zoom-out))))

(defun osm-center ()
  "Center to location of transient pin."
  (interactive)
  (osm--barf-unless-osm)
  (when osm--transient-pin
    (setq osm--lat (car osm--transient-pin)
          osm--lon (cadr osm--transient-pin))
    (message "%s" (cdddr osm--transient-pin))
    (osm--update)))

(defun osm-transient-click (event)
  "Put a transient pin at location of the click EVENT."
  (interactive "@e")
  (osm--put-transient-pin-event event)
  (osm--update))

(defun osm-bookmark-set-click (event)
  "Create bookmark at position of click EVENT."
  (interactive "@e")
  (osm--put-transient-pin-event event 'osm-selected-bookmark "New Bookmark")
  (osm-bookmark-set))

(defun osm-org-link-click (event)
  "Store link at position of click EVENT."
  (interactive "@e")
  (osm--put-transient-pin-event event 'osm-link "New Org Link")
  (call-interactively 'org-store-link))

(defun osm--pin-at (type x y)
  "Get pin of TYPE at X/Y."
  (let ((x (+ (osm--x0) x))
        (y (+ (osm--y0) y))
        (min most-positive-fixnum)
        found)
    (dolist (pin (car (osm--get-overlays (/ x 256) (/ y 256))))
      (pcase-let ((`(,p ,q ,_lat ,_lon ,id . ,_) pin))
        (when (eq type id)
          (let ((d (+ (* (- p x) (- p x)) (* (- q y) (- q y)))))
            (when (and (>= q y) (< q (+ y 50)) (>= p (- x 20)) (< p (+ x 20)) (< d min))
              (setq min d found pin))))))
    (cddr found)))

(defun osm-bookmark-select-click (event)
  "Select bookmark at position of click EVENT."
  (interactive "@e")
  (pcase-let* ((`(,x . ,y) (posn-x-y (event-start event))))
    (when-let (pin (osm--pin-at 'osm-bookmark x y))
      (osm--put-transient-pin 'osm-selected-bookmark (car pin) (cadr pin) (cdddr pin))
      (osm--update))))

(defun osm-poi-click (event)
  "Select point of interest at position of click EVENT."
  (interactive "@e")
  (pcase-let* ((`(,x . ,y) (posn-x-y (event-start event))))
    (when-let (pin (osm--pin-at 'osm-poi x y))
      (osm--put-transient-pin 'osm-selected-poi (car pin) (cadr pin) (cdddr pin))
      (osm--update))))

(defun osm-zoom-in (&optional n)
  "Zoom N times into the map."
  (interactive "p")
  (osm--barf-unless-osm)
  (setq osm--zoom (max (osm--server-property :min-zoom)
                       (min (osm--server-property :max-zoom)
                            (+ osm--zoom (or n 1)))))
  (osm--update))

(defun osm-zoom-out (&optional n)
  "Zoom N times out of the map."
  (interactive "p")
  (osm-zoom-in (- (or n 1))))

(defun osm--move (dx dy)
  "Move by DX/DY."
  (osm--barf-unless-osm)
  (setq osm--lon (osm--x-to-lon (+ (osm--x) dx) osm--zoom)
        osm--lat (osm--y-to-lat (+ (osm--y) dy) osm--zoom)))

(defun osm-right (&optional n)
  "Move N small stepz to the right."
  (interactive "p")
  (osm--move (* (or n 1) osm-small-step) 0)
  (osm--update))

(defun osm-down (&optional n)
  "Move N small stepz down."
  (interactive "p")
  (osm--move 0 (* (or n 1) osm-small-step))
  (osm--update))

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
  (osm--move (* (or n 1) osm-large-step) 0)
  (osm--update))

(defun osm-down-down (&optional n)
  "Move N large stepz down."
  (interactive "p")
  (osm--move 0 (* (or n 1) osm-large-step))
  (osm--update))

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
  (unless (libxml-available-p)
    (warn "osm: libxml is not available"))
  ;; json-available-p is not available on Emacs 27
  ;; (unless (json-available-p)
  ;;   (warn "osm: libjansson is not available"))
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
              bookmark-make-record-function #'osm--bookmark-record-default)
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

(defun osm--pin-inside-p (x y lat lon)
  "Return non-nil if pin at LAT/LON is inside tile X/Y."
  (let ((p (osm--lon-to-x lon osm--zoom))
        (q (osm--lat-to-y lat osm--zoom)))
    (setq x (* x 256) y (* y 256))
    (and (>= p (- x 32)) (< p (+ x 256 32))
         (>= q y) (< q (+ y 256 64)))))

(defun osm--put-pin (pins id lat lon name)
  "Put pin at X/Y with NAME and ID in PINS hash table."
  (let* ((x (osm--lon-to-x lon osm--zoom))
         (y (osm--lat-to-y lat osm--zoom))
         (x0 (/ x 256))
         (y0 (/ y 256))
         (pin `(,x ,y ,lat ,lon ,id . ,name)))
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
    (osm--put-pin pins 'osm-home (car osm-home) (cadr osm-home) "Home")
    (bookmark-maybe-load-default-file)
    (dolist (bm bookmark-alist)
      (when (eq (bookmark-prop-get bm 'handler) #'osm-bookmark-jump)
        (let ((coord (bookmark-prop-get bm 'coordinates)))
          (osm--put-pin pins 'osm-bookmark (car coord) (cadr coord) (car bm)))))
    (dolist (file osm--gpx-files)
      (dolist (pt (cddr file))
        (osm--put-pin pins 'osm-poi (cadr pt) (cddr pt) (car pt))))
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
                    (pcase-let* ((`(,p ,q ,_lat ,_lon ,id . ,name) pin)
                                 (`(,_ ,bg ,fg) (assq id osm-pin-colors)))
                      (setq p (- p x0) q (- q y0))
                      (push `((poly . [,p ,q ,(- p 20) ,(- q 40) ,p ,(- q 50) ,(+ p 20) ,(- q 40) ])
                              ,id (help-echo ,(truncate-string-to-width name 40 0 nil t) pointer hand))
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
  (pcase osm--transient-pin
    ((and `(,lat ,lon . ,_)
          (guard (osm--pin-inside-p x y lat lon)))
     (osm--draw-tile x y `(,(osm--lon-to-x lon osm--zoom)
                           ,(osm--lat-to-y lat osm--zoom)
                           ,@osm--transient-pin)))
    (_
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
           tile))))))

(defun osm--display-tile (x y tile)
  "Display TILE at X/Y."
  (let ((i (- x (/ (osm--x0) 256)))
        (j (- y (/ (osm--y0) 256))))
    (when (and (>= i 0) (< i osm--nx) (>= j 0) (< j osm--ny))
      (let* ((mx (if (= 0 i) (mod (osm--x0) 256) 0))
             (my (if (= 0 j) (mod (osm--y0) 256) 0))
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
  (osm--goto (nth 0 osm-home) (nth 1 osm-home) (nth 2 osm-home) nil 'osm-home "Home"))

(defun osm--download-queue-info ()
  "Return queue info string."
  (if (> osm--download-processes 0)
      (format "[%s/%s]" osm--download-processes
              (+ (length osm--download-active)
                 (length osm--download-queue)))))

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
                          (if (commandp action)
                              (lambda ()
                                (interactive "@")
                                (call-interactively action))
                            (osm--menu-item action)))
                        map)
              'face '(:box (:line-width -2 :style released-button))
              'mouse-face '(:box (:line-width -2 :style pressed-button))))

(defun osm--update-header ()
  "Update header line."
  (let* ((meter-per-pixel (/ (* 156543.03 (cos (/ osm--lat (/ 180.0 float-pi)))) (expt 2 osm--zoom)))
         (server (osm--server-property :name))
         (meter 1) (idx 0)
         (factor '(2 2.5 2)))
    (while (and (< idx 20) (< (/ (* meter (nth (mod idx 3) factor)) meter-per-pixel) 150))
      (setq meter (round (* meter (nth (mod idx 3) factor))))
      (cl-incf idx))
    (setq-local
     header-line-format
     (concat
      (format #(" %7.2f°" 0 6 (face bold)) osm--lat)
      (format #(" %7.2f°" 0 6 (face bold)) osm--lon)
      (propertize " " 'display '(space :align-to (- center 10)))
      (format "%3s " (if (>= meter 1000) (/ meter 1000) meter))
      (if (>= meter 1000) "km " "m ")
      (propertize " " 'face '(:inverse-video t)
                  'display '(space :width (3)))
      (propertize " " 'face '(:strike-through t)
                  'display `(space :width (,(floor (/ meter meter-per-pixel)))))
      (propertize " " 'face '(:inverse-video t)
                  'display '(space :width (3)))
      (propertize " " 'display `(space :align-to
                                       (- right ,(+ 5 3 3 2 (length server) 3) (,(+ 4 1 4 1 4 1 4)))))
      (format " Z%-2d " osm--zoom)
      (osm--header-button " + " #'osm-zoom-in)
      (propertize " " 'display '(space :width (1)))
      (osm--header-button " - " #'osm-zoom-out)
      (propertize " " 'display '(space :width (1)))
      (osm--header-button (format " %s " server) #'osm--server-menu)
      (propertize " " 'display '(space :width (1)))
      (osm--header-button " ☰ " osm--menu)))))

(defun osm--update ()
  "Update map display."
  (osm--barf-unless-osm)
  (osm--rename-buffer)
  (osm--update-sizes)
  (osm--update-header)
  (osm--update-buffer)
  (osm--update-copyright)
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

(defun osm--copyright-link (text url)
  "Format link with TEXT to URL."
  (propertize text
              'face 'button
              'mouse-face 'highlight
              'help-echo
              (format "Go to %s" url)
              'keymap
              (let ((map (make-sparse-keymap)))
                (define-key map [tab-line mouse-1]
                  (lambda ()
                    (interactive)
                    (browse-url url)))
                map)))

(defun osm--update-copyright ()
  "Update copyright info."
  (let ((copyright (and osm-copyright (osm--server-property :copyright))))
    (if (not copyright)
        (when (eq 'osm-copyright (car-safe tab-line-format))
          (kill-local-variable 'tab-line-format))
      (setq copyright (replace-regexp-in-string
                       "{\\(.*?\\)|\\(.*?\\)}"
                       (lambda (str)
                         (osm--copyright-link
                          (match-string 1 str)
                          (match-string 2 str)))
                       (concat
                        " "
                        (if (listp copyright)
                            (string-join copyright " | ")
                          copyright)
                        (propertize " "
                                    'display
                                    '(space :align-to right)))))
      (add-face-text-property
       0 (length copyright)
       '(:inherit (header-line variable-pitch) :height 0.75)
       t copyright)
      (setq-local tab-line-format (list 'osm-copyright copyright)))))

(defun osm--update-buffer ()
  "Update buffer display."
  (with-silent-modifications
    (erase-buffer)
    (dotimes (_j osm--ny)
      (insert (make-string osm--nx ?\s) "\n"))
    (goto-char (point-min))
    (let ((tx (/ (osm--x0) 256))
          (ty (/ (osm--y0) 256)))
      (dotimes (j osm--ny)
        (dotimes (i osm--nx)
          (let* ((x (+ i tx))
                 (y (+ j ty))
                 (tile (osm--get-tile x y)))
            (osm--display-tile x y tile)
            (unless tile (osm--enqueue-download x y))))))))

(defun osm--process-download-queue ()
  "Process the download queue."
  (setq osm--download-queue
        (sort
         (cl-loop with tx = (/ (osm--x0) 256)
                  with ty = (/ (osm--y0) 256)
                  for job in osm--download-queue
                  for (x y . zoom) = job
                  if (and (= zoom osm--zoom)
                          (>= x tx) (< x (+ tx osm--nx))
                          (>= y ty) (< y (+ ty osm--ny)))
                  collect job)
         (let ((tx (/ (osm--x) 256))
               (ty (/ (osm--y) 256)))
           (pcase-lambda (`(,x1 ,y1 . ,_z1) `(,x2 ,y2 . ,_z2))
             (setq x1 (- x1 tx) y1 (- y1 ty) x2 (- x2 tx) y2 (- y2 ty))
             (< (+ (* x1 x1) (* y1 y1)) (+ (* x2 x2) (* y2 y2)))))))
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

(defun osm--bookmark-record-default ()
  "Make osm bookmark record."
  (osm--bookmark-record (osm--bookmark-name osm--lat osm--lon nil)
                        osm--lat osm--lon nil))

(defun osm--bookmark-record (name lat lon loc)
  "Make osm bookmark record with NAME and LOC description at LAT/LON."
  (setq bookmark-current-bookmark nil) ;; Reset bookmark to use new name
  `(,name
    (location . ,(osm--location-name lat lon loc 6))
    (coordinates ,lat ,lon ,osm--zoom)
    (server . ,osm-server)
    (handler . ,#'osm-bookmark-jump)))

(defun osm--org-link-data ()
  "Return Org link data."
  (pcase-let* ((`(,lat ,lon ,loc) (osm--fetch-location-data 'osm-link "New Org Link"))
               (name (osm--location-name lat lon loc 2)))
    (list lat lon osm--zoom
          (and (not (eq osm-server (default-value 'osm-server))) osm-server)
          (if (eq osm-server (default-value 'osm-server))
              (string-remove-suffix (concat " " (osm--server-property :name)) name)
            name))))

(defun osm--rename-buffer ()
  "Rename current buffer."
  (setq list-buffers-directory (osm--location-name osm--lat osm--lon nil 6))
  (rename-buffer
   (format "*osm: %s*" (osm--location-name osm--lat osm--lon nil 2))
   'unique))

(defun osm--location-name (lat lon loc prec)
  "Format location string LAT/LON with optional LOC description.
The coordinates are formatted with precision PREC."
  (format (format "%%s%%.%df° %%.%df° Z%%s %%s" prec prec)
          (if loc (concat loc ", ") "")
          lat lon osm--zoom (osm--server-property :name)))

(defun osm--bookmark-name (lat lon loc)
  "Return bookmark name for LAT/LON/LOC."
  (concat "osm: " (osm--location-name lat lon loc 2)))

(defun osm--goto (lat lon zoom server id name)
  "Go to LAT/LON/ZOOM, change SERVER.
Optionally place transient pin with ID and NAME."
  ;; Server not found
  (when (and server (not (assq server osm-server-list))) (setq server nil))
  (with-current-buffer
      (or
       (and (eq major-mode #'osm-mode) (current-buffer))
       (let ((def-server (or server osm-server))
             (def-lat (or lat (nth 0 osm-home)))
             (def-lon (or lon (nth 1 osm-home)))
             (def-zoom (or zoom (nth 2 osm-home))))
         ;; Search for existing buffer
         (cl-loop
          for buf in (buffer-list) thereis
          (and (eq (buffer-local-value 'major-mode buf) #'osm-mode)
               (eq (buffer-local-value 'osm-server buf) def-server)
               (eq (buffer-local-value 'osm--zoom buf) def-zoom)
               (eq (buffer-local-value 'osm--lat buf) def-lat)
               (eq (buffer-local-value 'osm--lon buf) def-lon)
               buf)))
       (generate-new-buffer "*osm*"))
    (unless (eq major-mode #'osm-mode)
      (osm-mode))
    (when (and server (not (eq osm-server server)))
      (setq osm-server server
            osm--download-active nil
            osm--download-queue nil
            osm--download-processes 0))
    (when (or (not (and osm--lon osm--lat)) lat)
      (setq osm--lat (or lat (nth 0 osm-home))
            osm--lon (or lon (nth 1 osm-home))
            osm--zoom (or zoom (nth 2 osm-home)))
      (when id
        (osm--put-transient-pin id osm--lat osm--lon name)))
    (prog1 (pop-to-buffer (current-buffer))
      (osm--update))))

(defun osm--put-transient-pin (id lat lon name)
  "Set transient pin at LAT/LON with ID and NAME."
  (setq osm--transient-pin
        `(,lat ,lon ,(or id 'osm-transient)
               . ,(or name (format "Location %.6f° %.6f°" lat lon))))
  (message "%s" (cdddr osm--transient-pin)))

(defun osm--put-transient-pin-event (event &optional id name)
  "Set transient pin with ID and NAME at location of EVENT."
  (pcase-let ((`(,x . ,y) (posn-x-y (event-start event))))
    (osm--put-transient-pin id
                            (osm--y-to-lat (+ (osm--y0) y) osm--zoom)
                            (osm--x-to-lon (+ (osm--x0) x) osm--zoom)
                            name)))

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
  (osm--goto lat lon zoom nil 'osm-transient nil)
  nil)

;;;###autoload
(defmacro osm (&rest link)
  "Go to LINK."
  (pcase link
    (`(,lat ,lon ,zoom . ,server)
     (setq server (car server))
     (unless (and server (symbolp server)) (setq server nil)) ;; Ignore comment
     `(progn
        (osm--goto ,lat ,lon ,zoom ',server 'osm-link "Elisp Link")
        '(osm ,lat ,lon ,zoom ,@(and server (list server)))))
    ((and `(,search) (guard (stringp search)))
     `(progn
        (osm-search ,search)
        '(osm ,search)))
    (_ (error "Invalid osm link"))))

;;;###autoload
(defun osm-bookmark-jump (bm)
  "Jump to osm bookmark BM."
  (interactive (list (osm--bookmark-read)))
  (let ((coords (bookmark-prop-get bm 'coordinates)))
    (set-buffer (osm--goto (nth 0 coords) (nth 1 coords) (nth 2 coords)
                           (bookmark-prop-get bm 'server)
                           'osm-selected-bookmark (car bm)))))

;;;###autoload
(defun osm-bookmark-delete (bm)
  "Delete osm bookmark BM."
  (interactive (list (osm--bookmark-read)))
  (bookmark-delete bm)
  (setq osm--transient-pin nil)
  (osm--revert))

;;;###autoload
(defun osm-bookmark-rename (old-name)
  "Rename osm bookmark OLD-NAME."
  (interactive (list (car (osm--bookmark-read))))
  (let ((new-name (read-from-minibuffer "New name: " old-name nil nil
                                        'bookmark-history old-name)))
    (when osm--transient-pin (setf (cdddr osm--transient-pin) new-name))
    (bookmark-rename old-name new-name)
    (osm--revert)))

(defun osm--bookmark-read ()
  "Read bookmark name."
  (bookmark-maybe-load-default-file)
  (or (assoc
       (if (eq (caddr osm--transient-pin) 'osm-selected-bookmark)
           (cdddr osm--transient-pin)
         (completing-read
          "Bookmark: "
          (or (cl-loop for bm in bookmark-alist
                       if (eq (bookmark-prop-get bm 'handler) #'osm-bookmark-jump)
                       collect (car bm))
              (error "No bookmarks found"))
          nil t nil 'bookmark-history))
       bookmark-alist)
      (error "No bookmark selected")))

(defun osm-bookmark-set ()
  "Create osm bookmark."
  (interactive)
  (osm--barf-unless-osm)
  (unwind-protect
      (pcase-let* ((`(,lat ,lon ,loc) (osm--fetch-location-data 'osm-selected-bookmark "New Bookmark"))
                   (def (osm--bookmark-name lat lon loc))
                   (name (read-from-minibuffer "Bookmark name: " def nil nil 'bookmark-history def))
                   (bookmark-make-record-function
                    (lambda () (osm--bookmark-record name lat lon loc))))
        (bookmark-set name)
        (message "Stored bookmark: %s" name))
    (osm--revert)))

(defun osm--fetch-location-data (id name)
  "Fetch location info for ID with NAME."
  (let ((lat (or (car osm--transient-pin) osm--lat))
        (lon (or (cadr osm--transient-pin) osm--lon)))
    (osm--put-transient-pin id lat lon name)
    (message "%s: Fetching name of %.6f %.6f..." name lat lon)
    ;; Redisplay before slow fetching
    (osm--update)
    (redisplay)
    (list lat lon
          (ignore-errors
            (alist-get
             'display_name
             (osm--fetch-json
              (format "https://nominatim.openstreetmap.org/reverse?format=json&zoom=%s&lat=%s&lon=%s"
                      (min 18 (max 3 osm--zoom)) lat lon)))))))

(defun osm--fetch-json (url)
  "Get json from URL."
  (json-parse-string
   (let ((default-process-coding-system '(utf-8-unix . utf-8-unix)))
     (shell-command-to-string
      (concat
       "curl " osm-curl-options " "
       (shell-quote-argument url))))
   :array-type 'list
   :object-type 'alist))

;;;###autoload
(defun osm-search (search &optional lucky)
  "Search for SEARCH and display the map.
If the prefix argument LUCKY is non-nil take the first result and jump there."
  (interactive
   (list (completing-read "Location: "
                          (osm--sorted-table osm--search-history)
                          nil nil nil 'osm--search-history)
         current-prefix-arg))
  ;; TODO add search bounded to current viewbox, bounded=1, viewbox=x1,y1,x2,y2
  (let* ((results (mapcar
                   (lambda (x)
                     (let ((lat (string-to-number (alist-get 'lat x)))
                           (lon (string-to-number (alist-get 'lon x))))
                       `(,(format "%s (%.6f° %.6f°)"
                                  (alist-get 'display_name x)
                                  lat lon)
                         ,lat ,lon
                         ,@(mapcar #'string-to-number (alist-get 'boundingbox x)))))
                   (or
                    (osm--fetch-json
                     (concat "https://nominatim.openstreetmap.org/search?format=json&q="
                             (url-encode-url search)))
                    (error "No results"))))
         (selected (or
                    (and (or lucky (not (cdr results))) (car results))
                    (assoc
                     (completing-read
                      (format "Matches for '%s': " search)
                      (osm--sorted-table results)
                      nil t)
                     results)
                    (error "No selection"))))
    (osm--goto (cadr selected) (caddr selected)
               (apply #'osm--boundingbox-to-zoom (cdddr selected))
               nil 'osm-transient (car selected))))

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
    (osm--goto (/ (+ min-lat max-lat) 2) (/ (+ min-lon max-lon) 2)
               (osm--boundingbox-to-zoom min-lat max-lat min-lon max-lon)
               nil nil nil)))

(defun osm-gpx-hide (file)
  "Show the tracks of gpx FILE in an `osm-mode' buffer."
  (interactive (list (completing-read "GPX file: "
                                      (or osm--gpx-files
                                          (error "No GPX files shown"))
                                      nil t nil 'file-name-history)))
  (osm--barf-unless-osm)
  (setq osm--gpx-files (assoc-delete-all file osm--gpx-files))
  (osm--revert))

(defun osm--server-annotation (cand)
  "Annotation for server CAND."
  (when-let* ((copyright (osm--server-property :copyright (get-text-property 0 'osm--server cand)))
              (str
               (replace-regexp-in-string
                "{\\(.*?\\)|.*?}"
                (lambda (str) (match-string 1 str))
                (if (listp copyright) (string-join copyright " | ") copyright))))
    (concat (propertize " " 'display ` (space :align-to (- right ,(length str) 2)))
            " "
            str)))

(defun osm--server-group (cand transform)
  "Group function for server CAND with candidate TRANSFORM."
  (if transform
      cand
    (osm--server-property :group (get-text-property 0 'osm--server cand))))

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
              (propertize
               (format fmt
                       (plist-get (cdr x) :name)
                       (or (plist-get (cdr x) :description) ""))
               'osm--server (car x)))
            osm-server-list))
          (selected (completing-read
                     "Server: "
                     (lambda (str pred action)
                       (if (eq action 'metadata)
                           `(metadata
                             (annotation-function
                              . ,(and osm-copyright #'osm--server-annotation))
                             (group-function . ,#'osm--server-group))
                         (complete-with-action action servers str pred)))
                     nil t nil nil
                     (format fmt
                             (osm--server-property :name)
                             (or (osm--server-property :description) "")))))
     (list
      (get-text-property 0 'osm--server
                         (or (car (member selected servers))
                             (error "No server selected"))))))
  (osm--goto nil nil nil server nil nil))

(defun osm-elisp-link ()
  "Store coordinates as an Elisp link in the kill ring."
  (interactive)
  (osm--barf-unless-osm)
  (pcase-let* ((`(,lat ,lon ,loc) (osm--fetch-location-data 'osm-link "New Elisp Link"))
               (link (format "(osm %.6f %.6f %s%s%s)"
                             lat lon osm--zoom
                             (if (eq osm-server (default-value 'osm-server))
                                 ""
                               (format " %s" osm-server))
                             (if loc (format " %S" loc) ""))))
    (kill-new link)
    (message "Stored in the kill ring: %s" link)))

(dolist (sym (list #'osm-center #'osm-up #'osm-down #'osm-left #'osm-right
                   #'osm-up-up #'osm-down-down #'osm-left-left #'osm-right-right
                   #'osm-zoom-out #'osm-zoom-in #'osm-bookmark-set #'osm-gpx-hide
                   #'osm-elisp-link))
  (put sym 'command-modes '(osm-mode)))
(dolist (sym (list #'osm-mouse-drag #'osm-transient-click #'osm-org-link-click
                   #'osm-poi-click #'osm-bookmark-set-click #'osm-bookmark-select-click))
  (put sym 'completion-predicate #'ignore))

(provide 'osm)
;;; osm.el ends here
