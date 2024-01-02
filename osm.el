;;; osm.el --- OpenStreetMap viewer -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2022
;; Version: 1.2
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.4"))
;; Homepage: https://github.com/minad/osm
;; Keywords: network, multimedia, hypermedia, mouse

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

;; Osm.el is a tile-based map viewer, with a responsive movable and
;; zoomable display.  The map can be controlled with the keyboard or with
;; the mouse.  The viewer fetches the map tiles in parallel from tile
;; servers via the `curl' program.  The package comes with a list of
;; multiple preconfigured tile servers.  You can bookmark your favorite
;; locations using regular Emacs bookmarks or create links from Org files
;; to locations.  Furthermore the package provides commands to measure
;; distances, search for locations by name and to open and display GPX
;; tracks.

;; osm.el requires Emacs 27 and depends on the external `curl' program.
;; Emacs must be built with libxml, libjansson, librsvg, libjpeg and
;; libpng support.

;;; Code:

(require 'compat)
(require 'bookmark)
(require 'dom)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defgroup osm nil
  "OpenStreetMap viewer."
  :link '(info-link :tag "Info Manual" "(osm)")
  :link '(url-link :tag "Homepage" "https://github.com/minad/osm")
  :link '(emacs-library-link :tag "Library Source" "osm.el")
  :group 'web
  :prefix "osm-")

(defcustom osm-curl-options
  "--disable --fail --location --silent --max-time 30"
  "Curl command line options."
  :type 'string)

(defcustom osm-search-language "en"
  "Language used for search results.
Use RFC 1766 abbreviations, e.g.: `en' for English, `de' for German.
A comma-separated specifies descending order of preference.  See also
`url-mime-language-string'."
  :type 'string)

(defcustom osm-search-server
  "https://nominatim.openstreetmap.org"
  "Server used to search for location names.
The server must offer the nominatim.org API."
  :type 'string)

(defcustom osm-server-defaults
  '(:min-zoom 2
    :max-zoom 19
    :download-batch 4
    :max-connections 2
    :subdomains ("a" "b" "c"))
  "Default server properties.
See also `osm-server-list'."
  :type 'plist)

(defcustom osm-server-list
  '((default
     :name "Carto"
     :description "Standard Carto map provided by OpenStreetMap"
     :url "https://%s.tile.openstreetmap.org/%z/%x/%y.png"
     :group "Standard"
     :copyright ("Map data © {OpenStreetMap|https://www.openstreetmap.org/copyright} contributors"
                 "Map style © {OpenStreetMap Standard|https://www.openstreetmap.org/copyright}"))
    (de
     :name "Carto(de)"
     :description "Localized Carto map provided by OpenStreetMap Germany"
     :url "https://%s.tile.openstreetmap.de/%z/%x/%y.png"
     :group "Standard"
     :copyright ("Map data © {OpenStreetMap|https://www.openstreetmap.org/copyright} contributors"
                 "Map style © {OpenStreetMap Deutschland|https://www.openstreetmap.de/germanstyle.html}"))
    (fr
     :name "Carto(fr)"
     :description "Localized Carto map by OpenStreetMap France"
     :url "https://%s.tile.openstreetmap.fr/osmfr/%z/%x/%y.png"
     :group "Standard"
     :copyright ("Map data © {OpenStreetMap|https://www.openstreetmap.org/copyright} contributors"
                 "Map style © {OpenStreetMap France|https://www.openstreetmap.fr/mentions-legales/}"))
    (humanitarian
     :name "Humanitarian"
     :description "Humanitarian map provided by OpenStreetMap France"
     :url "https://%s.tile.openstreetmap.fr/hot/%z/%x/%y.png"
     :group "Special Purpose"
     :copyright ("Map data © {OpenStreetMap|https://www.openstreetmap.org/copyright} contributors"
                 "Map style © {Humanitarian OpenStreetMap Team|https://www.hotosm.org/updates/2013-09-29_a_new_window_on_openstreetmap_data}"))
    (cyclosm
     :name "CyclOSM"
     :description "Bicycle-oriented map provided by OpenStreetMap France"
     :url "https://%s.tile-cyclosm.openstreetmap.fr/cyclosm/%z/%x/%y.png"
     :group "Transportation"
     :copyright ("Map data © {OpenStreetMap|https://www.openstreetmap.org/copyright} contributors"
                 "Map style © {CyclOSM|https://www.cyclosm.org/} contributors"))
    (openriverboatmap
     :name "OpenRiverBoatMap"
     :description "Waterways map provided by OpenStreetMap France"
     :url "https://%s.tile.openstreetmap.fr/openriverboatmap/%z/%x/%y.png"
     :group "Transportation"
     :copyright ("Map data © {OpenStreetMap|https://www.openstreetmap.org/copyright} contributors"
                 "Map style © {OpenRiverBoatMap|https://github.com/tilery/OpenRiverboatMap}"))
    (opentopomap
     :name "OpenTopoMap"
     :description "Topographical map provided by OpenTopoMap"
     :url "https://%s.tile.opentopomap.org/%z/%x/%y.png"
     :group "Topographical"
     :copyright ("Map data © {OpenStreetMap|https://www.openstreetmap.org/copyright} contributors"
                 "Map style © {OpenTopoMap|https://www.opentopomap.org} ({CC-BY-SA|https://creativecommons.org/licenses/by-sa/3.0/})"
                 "Elevation data: {SRTM|https://www2.jpl.nasa.gov/srtm/}"))
    (opvn
     :name "ÖPNV" :max-zoom 18
     :description "Base layer with public transport information"
     :url "http://%s.tile.memomaps.de/tilegen/%z/%x/%y.png"
     :group "Transportation"
     :copyright ("Map data © {OpenStreetMap|https://www.openstreetmap.org/copyright} contributors"
                 "Map style © {ÖPNVKarte|https://www.öpnvkarte.de}")))
  "List of tile servers.

Allowed keys:
  :name            Server name
  :description     Server description
  :copyright       Copyright information
  :group           Name of server groups for related servers
  :url             Url with placeholders
  :min-zoom        Minimum zoom level
  :max-zoom        Maximum zoom level
  :download-batch  Number of tiles downloaded via a single connection
  :max-connections Maximum number of parallel connections
  :subdomains      Subdomains used for the %s placeholder

See also `osm-server-defaults' for default values used for a
server if the property is missing.

The :url of each server should specify %x, %y, %z and %s placeholders
for the map coordinates.  It can optionally use an %s placeholder
for the subdomain and a %k placeholder for an apikey.  The apikey
will be retrieved via `auth-source-search' with the :host set to
the domain name and the :user to the string \"apikey\"."
  :type '(alist :key-type symbol :value-type plist))

(defcustom osm-copyright t
  "Display the copyright information above the map."
  :type 'boolean)

(defcustom osm-pin-colors
  '((osm-selected . "#e20")
    (osm-bookmark . "#f80")
    (osm-poi . "#88f")
    (osm-home . "#80f")
    (osm-track . "#00a"))
  "Colors of pins."
  :type '(alist :key-type symbol :value-type string))

(defcustom osm-track-style
  "stroke:#00a;stroke-width:5;stroke-linejoin:round;stroke-linecap:round;opacity:0.4;"
  "SVG style used to draw tracks."
  :type 'string)

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
  :type 'natnum)

(defcustom osm-tile-border nil
  "Set to t to display thin tile borders.
For debugging set the value to `debug', such that a border is
shown around SVG tiles."
  :type '(choice boolean (const debug)))

(defcustom osm-small-step 16
  "Scroll step in pixel."
  :type 'natnum)

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
  :type '(choice (const nil) natnum))

(defcustom osm-max-tiles 256
  "Number of tiles to keep in the memory cache."
  :type '(choice (const nil) natnum))

(defun osm--menu-item (menu)
  "Generate menu item from MENU."
  `(menu-item
    "" nil :filter
    ,(lambda (&optional _)
       (select-window
        (posn-window
         (event-start last-input-event)))
       (if (functionp menu)
           (funcall menu)
         menu))))

(defvar-keymap osm-prefix-map
  :doc "Global prefix map of OSM entry points."
  "h" #'osm-home
  "s" #'osm-search
  "v" #'osm-server
  "t" #'osm-goto
  "j" #'osm-jump
  "x" #'osm-gpx-show
  "X" #'osm-gpx-hide)

;;;###autoload (autoload 'osm-prefix-map "osm" nil t 'keymap)
(defalias 'osm-prefix-map osm-prefix-map)

(defvar-keymap osm-mode-map
  :doc "Keymap used by `osm-mode'."
  :parent (make-composed-keymap osm-prefix-map special-mode-map)
  "<osm-selected>" #'osm-mouse-select
  "<osm-bookmark> <mouse-1>" #'osm-mouse-select
  "<osm-bookmark> <mouse-2>" #'osm-mouse-select
  "<osm-bookmark> <mouse-3>" #'osm-mouse-select
  "<osm-home> <mouse-1>" #'osm-mouse-select
  "<osm-home> <mouse-2>" #'osm-mouse-select
  "<osm-home> <mouse-3>" #'osm-mouse-select
  "<osm-poi> <mouse-1>" #'osm-mouse-select
  "<osm-poi> <mouse-2>" #'osm-mouse-select
  "<osm-poi> <mouse-3>" #'osm-mouse-select
  "<osm-track> <mouse-1>" #'osm-mouse-select
  "<osm-track> <mouse-2>" #'osm-mouse-select
  "<osm-track> <mouse-3>" #'osm-mouse-select
  "<home>" #'osm-home
  "+" #'osm-zoom-in
  "-" #'osm-zoom-out
  "SPC" #'osm-zoom-in
  "S-SPC" #'osm-zoom-out
  "<mouse-1>" #'osm-mouse-pin
  "<mouse-2>" 'org-store-link
  "<mouse-3>" #'osm-bookmark-set
  "S-<down-mouse-1>" #'ignore
  "S-<mouse-1>" #'osm-mouse-track
  "<down-mouse-1>" #'osm-mouse-drag
  "<down-mouse-2>" #'osm-mouse-drag
  "<down-mouse-3>" #'osm-mouse-drag
  "<drag-mouse-1>" #'ignore
  "<drag-mouse-2>" #'ignore
  "<drag-mouse-3>" #'ignore
  "<up>" #'osm-up
  "<down>" #'osm-down
  "<left>" #'osm-left
  "<right>" #'osm-right
  "C-<up>" #'osm-up-up
  "C-<down>" #'osm-down-down
  "C-<left>" #'osm-left-left
  "C-<right>" #'osm-right-right
  "M-<up>" #'osm-up-up
  "M-<down>" #'osm-down-down
  "M-<left>" #'osm-left-left
  "M-<right>" #'osm-right-right
  "S-<up>" #'osm-up-up
  "S-<down>" #'osm-down-down
  "S-<left>" #'osm-left-left
  "S-<right>" #'osm-right-right
  "n" #'osm-rename
  "d" #'osm-delete
  "DEL" #'osm-delete
  "<deletechar>" #'osm-delete
  "c" #'osm-center
  "o" #'clone-buffer
  "u" #'osm-save-url
  "l" 'org-store-link
  "b" #'osm-bookmark-set
  "X" #'osm-gpx-hide
  "<remap> <scroll-down-command>" #'osm-down
  "<remap> <scroll-up-command>" #'osm-up
  "<" nil
  ">" nil)

(easy-menu-define osm-mode-menu osm-mode-map
  "Menu for `osm-mode'."
  '("OSM"
    ["Go home" osm-home]
    ["Center" osm-center]
    ["Go to coordinates" osm-goto]
    ["Jump to pin" osm-jump]
    ["Search by name" osm-search]
    ["Change tile server" osm-server]
    "--"
    ["Org Link" org-store-link]
    ["Geo Url" osm-save-url]
    ["Elisp Link" (osm-save-url t)]
    ("Bookmark"
     ["Set" osm-bookmark-set]
     ["Jump" osm-bookmark-jump]
     ["Rename" osm-bookmark-rename]
     ["Delete" osm-bookmark-delete])
    "--"
    ["Show GPX file" osm-gpx-show]
    ["Hide GPX file" osm-gpx-hide]
    "--"
    ["Clone buffer" clone-buffer]
    ["Revert buffer" revert-buffer]
    "--"
    ["Manual" (info "(osm)")]
    ["Customize" (customize-group 'osm)]))

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
  "Minibuffer history used by command `osm-search'.")

(defvar osm--jump-history nil
  "Minibuffer history used by command `osm-jump'.")

(defvar osm--server-history nil
  "Minibuffer history used by command `osm-server'.")

(defvar osm--purge-directory 0
  "Last time the tile cache was cleaned.")

(defvar osm--tile-cache nil
  "Global tile memory cache.")

(defvar osm--tile-age 0
  "Tile age, incremented on every update.")

(defvar osm--gpx-files nil
  "Global list of loaded tracks.")

(defvar osm--track nil
  "List of track coordinates.")

(defvar osm--download-processes nil
  "Globally active download processes.")

(defvar osm--download-active nil
  "Globally active download jobs.")

(defvar osm--download-subdomain nil
  "Subdomain indices to query the servers in a round-robin fashion.")

(defvar-local osm--download-queue nil
  "Buffer-local tile download queue.")

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

(defvar-local osm--overlays nil
  "Overlay hash table.
Local per buffer since the overlays depend on the zoom level.")

(defvar-local osm--pin nil
  "Currently selected pin.")

(defmacro osm--each (&rest body)
  "Execute BODY in each `osm-mode' buffer."
  (cl-with-gensyms (buf)
    `(dolist (,buf (buffer-list))
       (when (eq (buffer-local-value 'major-mode ,buf) #'osm-mode)
         (with-current-buffer ,buf
           ,@body)))))

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
    (easy-menu-create-menu "Server" (nreverse menu))))

(defsubst osm--lon-to-normalized-x (lon)
  "Convert LON to normalized x coordinate."
  (/ (+ lon 180.0) 360.0))

(defsubst osm--lat-to-normalized-y (lat)
  "Convert LAT to normalized y coordinate."
  (setq lat (* lat (/ float-pi 180.0)))
  (- 0.5 (/ (log (+ (tan lat) (/ 1.0 (cos lat)))) float-pi 2)))

(defun osm--boundingbox-to-zoom (lat1 lat2 lon1 lon2)
  "Compute zoom level from boundingbox LAT1 to LAT2 and LON1 to LON2."
  (let ((w (/ (frame-pixel-width) 256))
        (h (/ (frame-pixel-height) 256)))
    (max (osm--server-property :min-zoom)
         (min
          (osm--server-property :max-zoom)
          (min (logb (/ w (abs (- (osm--lon-to-normalized-x lon1) (osm--lon-to-normalized-x lon2)))))
               (logb (/ h (abs (- (osm--lat-to-normalized-y lat1) (osm--lat-to-normalized-y lat2))))))))))

(defun osm--x-to-lon (x zoom)
  "Return longitude in degrees for X/ZOOM."
  (- (/ (* x 360.0) 256.0 (expt 2.0 zoom)) 180.0))

(defun osm--y-to-lat (y zoom)
  "Return latitude in degrees for Y/ZOOM."
  (setq y (* float-pi (- 1 (* 2 (/ y 256.0 (expt 2.0 zoom))))))
  (/ (* 180 (atan (/ (- (exp y) (exp (- y))) 2))) float-pi))

(defsubst osm--lon-to-x (lon zoom)
  "Convert LON/ZOOM to x coordinate in pixel."
  (floor (* 256 (expt 2.0 zoom) (osm--lon-to-normalized-x lon))))

(defsubst osm--lat-to-y (lat zoom)
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
        (sub (osm--server-property :subdomains))
        (key (osm--server-property :key)))
    (when (and (string-search "%k" url) (not key))
      (require 'auth-source)
      (declare-function auth-source-search "auth-source")
      (let ((host (string-join
                   (last (split-string (cadr (split-string url "/" t)) "\\.") 2)
                   ".")))
        (setq key (plist-get
                   (car (auth-source-search :require '(:user :host :secret)
                                            :host host
                                            :user "apikey"))
                   :secret))
        (unless key
          (warn "No auth source secret found for apikey@%s" host)
          (setq key ""))
        (setf (plist-get (alist-get osm-server osm-server-list) :key) key)))
    (format-spec
     url `((?z . ,zoom) (?x . ,x) (?y . ,y)
           (?k . ,(if (functionp key) (funcall key) key))
           (?s . ,(nth (mod (alist-get osm-server osm--download-subdomain 0)
                            (length sub))
                       sub))))))

(defun osm--tile-file (x y zoom)
  "Return tile file name for coordinate X, Y and ZOOM."
  (file-name-concat
   (expand-file-name osm-tile-directory)
   (symbol-name osm-server)
   (format "%d-%d-%d.%s"
           zoom x y
           (file-name-extension
            (url-file-nondirectory
             (osm--server-property :url))))))

(defun osm--enqueue-download (x y)
  "Enqueue tile X/Y for download."
  (when (let ((n (expt 2 osm--zoom))) (and (>= x 0) (>= y 0) (< x n) (< y n)))
    (let ((job (list osm-server osm--zoom x y)))
      (unless (or (member job osm--download-queue) (member job osm--download-active))
        (setq osm--download-queue (nconc osm--download-queue (list job)))))))

(defun osm--download-filter (output)
  "Filter function for the download process which receives OUTPUT."
  (while (string-match
          "\\`\\([0-9]+\\) \\(.*?/\\([^/]+\\)/\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)\\.[^\r\n]+\\)\r?\n"
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
        (osm--each
          (when (and (= osm--zoom zoom) (eq osm-server server))
            (osm--display-tile x y (osm--get-tile x y)))))
      (cl-callf2 delete (list server zoom x y) osm--download-active)
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
      (pcase-let ((`(,_server ,zoom ,x ,y) job))
        (setq args `(,(osm--tile-url x y zoom)
                     ,(concat (osm--tile-file x y zoom) ".tmp")
                     "--output"
                     ,@args))
        (push job jobs)
        (push job osm--download-active)
        (cl-incf count)))
    (osm--each
      (dolist (job jobs)
        (cl-callf2 delq job osm--download-queue)))
    (cl-callf (lambda (s) (mod (1+ s) subs))
        (alist-get osm-server osm--download-subdomain 0))
    (cons `("curl" "--write-out" "%{http_code} %{filename_effective}\n"
            ,@(split-string-and-unquote osm-curl-options) ,@(nreverse args))
          jobs)))

(defun osm--download ()
  "Download next tiles from the queue."
  (when (and (< (length (alist-get osm-server osm--download-processes))
                (* (length (osm--server-property :subdomains))
                   (osm--server-property :max-connections)))
             osm--download-queue)
    (pcase-let ((`(,command . ,jobs) (osm--download-command))
                (dir (file-name-concat (expand-file-name osm-tile-directory)
                                       (symbol-name osm-server)))
                (server osm-server))
      (make-directory dir t)
      (push
       (make-process
        :name "*osm curl*"
        :connection-type 'pipe
        :noquery t
        :command command
        :filter
        (let ((output ""))
          (lambda (_proc out)
            (setq output (osm--download-filter (concat output out)))
            (force-mode-line-update t)))
        :sentinel
        (lambda (proc _status)
          (dolist (job jobs)
            (cl-callf2 delq job osm--download-active))
          (cl-callf2 delq proc (alist-get server osm--download-processes nil t))
          (force-mode-line-update t)
          (osm--download)))
       (alist-get server osm--download-processes))
       (force-mode-line-update t)
       (osm--download))))

(defun osm-mouse-drag (event)
  "Handle drag EVENT."
  (interactive "@e")
  (pcase-let* ((`(,sx . ,sy) (posn-x-y (event-start event)))
               (win (selected-window))
               (map (define-keymap
                      "<mouse-movement>"
                      (lambda (event)
                        (interactive "e")
                        (setq event (event-start event))
                        (when (eq win (posn-window event))
                          (pcase-let ((`(,ex . ,ey) (posn-x-y event)))
                            (osm--move (- sx ex) (- sy ey))
                            (setq sx ex sy ey)
                            (osm--update)))))))
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
  "Center to location of selected pin."
  (interactive)
  (osm--barf-unless-osm)
  (pcase osm--pin
    (`(,lat ,lon ,_id ,name)
     (setq osm--lat lat osm--lon lon)
     (message "%s" name)
     (osm--update))))

(defun osm--haversine (lat1 lon1 lat2 lon2)
  "Compute distance between LAT1/LON1 and LAT2/LON2 in km."
  ;; https://en.wikipedia.org/wiki/Haversine_formula
  (let* ((rad (/ float-pi 180))
         (y (sin (* 0.5 rad (- lat2 lat1))))
         (x (sin (* 0.5 rad (- lon2 lon1))))
         (h (+ (* x x) (* (cos (* rad lat1)) (cos (* rad lat2)) y y))))
    (* 2 6371 (atan (sqrt h) (sqrt (- 1 h))))))

(defun osm-mouse-track (event)
  "Set track pin at location of the click EVENT."
  (interactive "@e")
  (pcase osm--pin
    ((and (guard (not osm--track)) `(,lat ,lon ,_id ,_name))
     (push (list lat lon "WP1") osm--track)))
  (osm--set-pin-event event 'osm-track
                      (format "WP%s" (1+ (length osm--track))) 'quiet)
  (pcase-let ((`(,lat ,lon ,_id ,name) osm--pin))
    (push (list lat lon name) osm--track))
  (osm--revert)
  (osm--track-length))

(defun osm--track-length ()
  "Echo track length."
  (when (cdr osm--track)
    (pcase-let* ((len1 0)
                 (len2 0)
                 (p osm--track)
                 (`(,sel-lat ,sel-lon ,_ ,sel-name) osm--pin))
      (while (and (cdr p) (not (and (equal (caar p) sel-lat)
                                    (equal (cadar p) sel-lon))))
        (cl-incf len2 (osm--haversine (caar p) (cadar p)
                                      (caadr p) (cadadr p)))
        (pop p))
      (while (cdr p)
        (cl-incf len1 (osm--haversine (caar p) (cadar p)
                                      (caadr p) (cadadr p)))
        (pop p))
      (message "%s way points, length %.2fkm, %s"
               (length osm--track) (+ len1 len2)
               (if (or (= len1 0) (= len2 0))
                   sel-name
                 (format "%.2fkm → %s → %.2fkm"
                         len1 sel-name len2))))))

(defun osm--pin-at (event &optional type)
  "Get pin of TYPE at EVENT."
  (let* ((xy (posn-x-y (event-start event)))
         (x (+ (osm--x0) (car xy)))
         (y (+ (osm--y0) (cdr xy)))
         (min most-positive-fixnum)
         found)
    (dolist (pin (car (osm--get-overlays (/ x 256) (/ y 256))))
      (pcase-let ((`(,p ,q ,_lat ,_lon ,id ,_name) pin))
        (when (or (not type) (eq type id))
          (let ((d (+ (* (- p x) (- p x)) (* (- q y) (- q y)))))
            (when (and (>= q y) (< q (+ y 50)) (>= p (- x 20)) (< p (+ x 20)) (< d min))
              (setq min d found pin))))))
    (cddr found)))

(defun osm-mouse-pin (event)
  "Create location pin at the click EVENT."
  (interactive "@e")
  (osm--set-pin-event event)
  (osm--update))

(defun osm-mouse-select (event)
  "Select pin at position of click EVENT."
  (interactive "@e")
  (pcase (osm--pin-at event)
    (`(,lat ,lon ,id ,name)
     (osm--set-pin id lat lon name (eq id 'osm-track))
     (when (eq id 'osm-track) (osm--track-length))
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
       (dolist (dir (directory-files osm-tile-directory t "\\`[^.]+\\'" t))
         (dolist (file (directory-files
                        dir t "\\.\\(?:png\\|jpe?g\\)\\(?:\\.tmp\\)?\\'" t))
           (when (> (float-time (time-since
                                 (file-attribute-modification-time
                                  (file-attributes file))))
                    (* 60 60 24 osm-max-age))
             (delete-file file)))
         (when (directory-empty-p dir)
           (ignore-errors (delete-directory dir))))))))

(defun osm--check-libraries ()
  "Check that Emacs is compiled with the necessary libraries."
  (let (req)
    (unless (display-graphic-p)
      (push "graphical display" req))
    (dolist (type '(svg jpeg png))
      (unless (image-type-available-p type)
        (push (format "%s support" type) req)))
    (unless (libxml-available-p)
      (push "libxml" req))
    ;; json-available-p is not available on Emacs 27
    (unless (ignore-errors (equal [] (json-parse-string "[]")))
      (push "libjansson" req))
    (when req
      (error "Osm: Please compile Emacs with the required libraries, %s needed to proceed"
             (string-join req ", ")))))

(define-derived-mode osm-mode special-mode "Osm"
  "OpenStreetMap viewer mode."
  :interactive nil :abbrev-table nil :syntax-table nil
  (osm--check-libraries)
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
              mode-line-position nil
              eldoc-documentation-functions nil
              mouse-wheel-progressive-speed nil
              mwheel-scroll-up-function #'osm--zoom-out-wheel
              mwheel-scroll-down-function #'osm--zoom-in-wheel
              mwheel-scroll-left-function #'osm--zoom-out-wheel
              mwheel-scroll-right-function #'osm--zoom-in-wheel
              bookmark-make-record-function #'osm--bookmark-record-default)
  (when (boundp 'mwheel-coalesce-scroll-events)
    (setq-local mwheel-coalesce-scroll-events t))
  (when (boundp 'pixel-scroll-precision-mode)
    (setq-local pixel-scroll-precision-mode nil))
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
    (error "Not an `osm-mode' buffer")))

(defun osm--each-pin (fun)
  "Call FUN for each pin on the map."
  (pcase osm-home
    (`(,lat ,lon ,zoom)
     (funcall fun 'osm-home lat lon zoom "Home")))
  (bookmark-maybe-load-default-file)
  (cl-loop for bm in bookmark-alist
           if (eq (bookmark-prop-get bm 'handler) #'osm-bookmark-jump) do
           (pcase-let ((`(,lat ,lon ,zoom) (bookmark-prop-get bm 'coordinates)))
             (funcall fun 'osm-bookmark lat lon zoom (car bm))))
  (dolist (file osm--gpx-files)
    (cl-loop for (lat lon name) in (cddr file) do
             (funcall fun 'osm-poi lat lon 15 name)))
  (cl-loop for (lat lon name) in osm--track do
           (funcall fun 'osm-track lat lon 15 name)))

(defun osm--pin-inside-p (x y lat lon)
  "Return non-nil if pin at LAT/LON is inside tile X/Y."
  (let ((p (/ (osm--lon-to-x lon osm--zoom) 256.0))
        (q (/ (osm--lat-to-y lat osm--zoom) 256.0)))
    (and (>= p (- x 0.125)) (< p (+ x 1.125))
         (>= q y) (< q (+ y 1.25)))))

(defun osm--add-pin (pins id lat lon _zoom name)
  "Add pin at LAT/LON with NAME and ID to the PINS hash table."
  (let* ((x (osm--lon-to-x lon osm--zoom))
         (y (osm--lat-to-y lat osm--zoom))
         (x0 (/ x 256))
         (y0 (/ y 256))
         (pin (list x y lat lon id name)))
    (push pin (gethash (cons x0 y0) pins))
    (cl-loop
     for i from -1 to 1 do
     (cl-loop
      for j from -1 to 0 do
      (let ((x1 (/ (+ x (* 32 i)) 256))
            (y1 (/ (+ y (* 64 j)) 256)))
        (unless (and (= x0 x1) (= y0 y1))
          (push pin (gethash (cons x1 y1) pins))))))))

;; TODO: The Bresenham algorithm used here to add the line segments to the tiles
;; has the issue that lines which go along a tile border may be drawn only
;; partially. Use a more precise algorithm instead.
(defun osm--add-track (tracks seg)
  "Add track segment SEG to TRACKS hash table."
  (when seg
    (let ((p0 (cons (osm--lon-to-x (or (car-safe (cdar seg)) (cdar seg)) osm--zoom)
                    (osm--lat-to-y (caar seg) osm--zoom))))
      (dolist (pt (cdr seg))
        (let* ((px1 (cdr pt))
               (px1 (osm--lon-to-x (if (consp px1) (car px1) px1) osm--zoom))
               (py1 (osm--lat-to-y (car pt) osm--zoom))
               (pdx (- px1 (car p0)))
               (pdy (- py1 (cdr p0))))
          ;; Ignore point if too close to last point
          (unless (< (+ (* pdx pdx) (* pdy pdy)) 50)
            (let* ((p1 (cons px1 py1))
                   (line (cons p0 p1))
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
                    (push line (gethash (cons x0 y0) tracks))
                    (unless (and (= x0 x1) (= y0 y1))
                      (when (and ey ex)
                        (push line (gethash (cons x0 (+ y0 sy)) tracks))
                        (push line (gethash (cons (+ x0 sx) y0) tracks)))
                      (when ey
                        (cl-incf err dy)
                        (cl-incf x0 sx))
                      (when ex
                        (cl-incf err dx)
                        (cl-incf y0 sy))
                      t)))
              (setq p0 p1))))))))

(defun osm--get-overlays (x y)
  "Compute overlays and return the overlays in tile X/Y."
  (unless (eq (car osm--overlays) osm--zoom)
    ;; TODO: Do not compute overlays for the entire map, only for a reasonable
    ;; view port around the current center, depending on the size of the
    ;; window. Otherwise the spatial hash map for the tracks gets very large if
    ;; a line segment spans many tiles.
    (let ((pins (make-hash-table :test #'equal))
          (tracks (make-hash-table :test #'equal)))
      (osm--each-pin (apply-partially #'osm--add-pin pins))
      (dolist (file osm--gpx-files)
        (dolist (seg (cadr file))
          (osm--add-track tracks seg)))
      (osm--add-track tracks osm--track)
      (setq osm--overlays (list osm--zoom pins tracks))))
  (let ((pins (gethash (cons x y) (cadr osm--overlays)))
        (tracks (gethash (cons x y) (caddr osm--overlays))))
    (and (or pins tracks) (cons pins tracks))))

(autoload 'svg--image-data "svg")
(defun osm--draw-tile (x y tpin)
  "Make tile at X/Y from FILE.
TPIN is an optional pin."
  (let ((file (osm--tile-file x y osm--zoom))
        overlays)
    (when (file-exists-p file)
      (if (or (setq overlays (osm--get-overlays x y)) (eq osm-tile-border t) tpin)
          (let* ((areas nil)
                 (x0 (* 256 x))
                 (y0 (* 256 y))
                 (svg-pin
                  (lambda (pin)
                    (pcase-let* ((`(,p ,q ,_lat ,_lon ,id ,name) pin)
                                 (bg (cdr (assq id osm-pin-colors))))
                      (setq p (- p x0) q (- q y0))
                      (push `((poly . [,p ,q ,(- p 20) ,(- q 40) ,p ,(- q 50) ,(+ p 20) ,(- q 40) ])
                              ,id (help-echo ,(truncate-string-to-width name 40 0 nil t)))
                            areas)
                      ;; https://commons.wikimedia.org/wiki/File:Simpleicons_Places_map-marker-1.svg
                      (format "
<g fill='%s' stroke='#000' stroke-width='9' transform='translate(%s %s) scale(0.09) translate(-256 -512)'>
<path d='M256 0C167.641 0 96 71.625 96 160c0 24.75 5.625 48.219 15.672
69.125C112.234 230.313 256 512 256 512l142.594-279.375
C409.719 210.844 416 186.156 416 160C416 71.625 344.375
0 256 0z M256 256c-53.016 0-96-43-96-96s42.984-96 96-96
c53 0 96 43 96 96S309 256 256 256z'/>
</g>" bg p q))))
                 (svg-text
                  (concat "<svg width='256' height='256' version='1.1'
xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink'>
<image xlink:href='"
                          (if (eval-when-compile (> emacs-major-version 27))
                              (file-name-nondirectory file)
                            ;; On Emacs 27, :base-uri and embedding by file path
                            ;; is not supported. Use the less efficient base64
                            ;; encoding.
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
  (pcase osm--pin
    ((and `(,lat ,lon ,_id ,name)
          (guard (osm--pin-inside-p x y lat lon)))
     (osm--draw-tile x y (list (osm--lon-to-x lon osm--zoom)
                               (osm--lat-to-y lat osm--zoom)
                               lat lon 'osm-selected name)))
    (_
     (let* ((key `(,osm-server ,osm--zoom ,x . ,y))
            (tile (and osm--tile-cache (gethash key osm--tile-cache))))
       (if tile
           (progn (setcar tile osm--tile-age) (cdr tile))
         (setq tile (osm--draw-tile x y nil))
         (when tile
           (when osm-max-tiles
             (unless osm--tile-cache
               (setq osm--tile-cache (make-hash-table :test #'equal :size osm-max-tiles)))
             (puthash key (cons osm--tile-age tile) osm--tile-cache))
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
  (pcase osm-home
    (`(,lat ,lon ,zoom)
     (osm--goto lat lon zoom nil 'osm-home "Home"))))

(defun osm--download-queue-info ()
  "Return queue info string."
  (when osm--download-processes
    (format "[%s/%s/%s]"
            (cl-loop for (_ . p) in osm--download-processes sum (length p))
            (length osm--download-active)
            (length osm--download-queue))))

(defun osm--revert (&rest _)
  "Revert osm buffers."
  (clear-image-cache t) ;; Make absolutely sure that the tiles are redrawn.
  (setq osm--tile-cache nil)
  (osm--each
    (setq osm--overlays nil)
    (osm--update)))

(defun osm--resize (&rest _)
  "Resize buffer."
  (when (eq major-mode #'osm-mode)
    (osm--update)))

(defun osm--header-button (text action)
  "Format header line button with TEXT and ACTION."
  (propertize text
              'keymap (define-keymap "<header-line> <mouse-1>"
                        (if (commandp action)
                            (lambda ()
                              (interactive "@")
                              (call-interactively action))
                          action))
              'face '(:box (:line-width -2 :style released-button))
              'mouse-face '(:box (:line-width -2 :style pressed-button))))

(defun osm--update-header ()
  "Update header line."
  (let* ((meter-per-pixel (/ (* 156543.03 (cos (/ osm--lat (/ 180.0 float-pi)))) (expt 2 osm--zoom)))
         (server (osm--server-property :name))
         (meter 1) (idx 0)
         (factor '(2 2.5 2))
         (sep #(" " 0 1 (display (space :width (1))))))
    (while (and (< idx 20) (< (/ (* meter (nth (mod idx 3) factor)) meter-per-pixel) 150))
      (setq meter (round (* meter (nth (mod idx 3) factor))))
      (cl-incf idx))
    (setq-local
     header-line-format
     (list
      (osm--header-button " ☰ " (osm--menu-item osm-mode-menu)) sep
      (osm--header-button (format " %s " server)
                          (osm--menu-item #'osm--server-menu)) sep
      (osm--header-button " + " #'osm-zoom-in) sep
      (osm--header-button " - " #'osm-zoom-out)
      (format " Z%-2d " osm--zoom)
      #(" " 0 1 (display (space :align-to (- center 15))))
      (format #(" %7.2f° %7.2f°" 0 14 (face bold)) osm--lat osm--lon)
      #(" " 0 1 (display (space :align-to (- right 20))))
      (format "%3s " (if (>= meter 1000) (/ meter 1000) meter))
      (if (>= meter 1000) "km " "m ")
      #(" " 0 1 (face (:inverse-video t) display (space :width (3))))
      (propertize " " 'face '(:strike-through t)
                  'display `(space :width (,(floor (/ meter meter-per-pixel)))))
      #(" " 0 1 (face (:inverse-video t) display (space :width (3))))))))

(defun osm--update ()
  "Update map display."
  (osm--barf-unless-osm)
  (osm--purge-tile-cache)
  (osm--purge-directory)
  (osm--rename-buffer)
  (osm--update-sizes)
  (osm--update-header)
  (osm--update-buffer)
  (osm--update-copyright)
  (osm--process-download-queue))

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
              (define-keymap "<tab-line> <mouse-1>"
                (lambda ()
                  (interactive)
                  (browse-url url)))))

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
                        (string-join (ensure-list copyright) " | ")
                        #(" " 0 1 (display (space :align-to (+ 42 right)))))))
      (add-face-text-property
       0 (length copyright)
       '(:inherit (header-line variable-pitch) :height 0.65)
       t copyright)
      (setq-local tab-line-format (list 'osm-copyright copyright)))))

(defun osm--update-buffer ()
  "Update buffer display."
  (with-silent-modifications
    (erase-buffer)
    (dotimes (_j osm--ny)
      (insert (make-string osm--nx ?\s) "\n"))
    (put-text-property (point-min) (point-max) 'pointer 'arrow)
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
                  for (_server zoom x y) = job
                  if (and (= zoom osm--zoom)
                          (>= x tx) (< x (+ tx osm--nx))
                          (>= y ty) (< y (+ ty osm--ny)))
                  collect job)
         (let ((tx (/ (osm--x) 256))
               (ty (/ (osm--y) 256)))
           (pcase-lambda (`(,_s1 ,_z1 ,x1 ,y1) `(,_s2 ,_z2 ,x2 ,y2))
             (setq x1 (- x1 tx) y1 (- y1 ty) x2 (- x2 tx) y2 (- y2 ty))
             (< (+ (* x1 x1) (* y1 y1)) (+ (* x2 x2) (* y2 y2)))))))
  (osm--download))

(defun osm--purge-tile-cache ()
  "Purge old tiles from the tile cache."
  (cl-incf osm--tile-age)
  (when (and osm--tile-cache (> (hash-table-count osm--tile-cache) osm-max-tiles))
    (let (items)
      (maphash (lambda (k v) (push (list (car v) (cdr v) k) items)) osm--tile-cache)
      (setq items (sort items #'car-less-than-car))
      (cl-loop repeat (- (hash-table-count osm--tile-cache) osm-max-tiles)
               for (_age tile key) in items do
               (image-flush tile t)
               (remhash key osm--tile-cache)))))

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

(defun osm--org-link-props ()
  "Return Org link properties."
  (pcase-let* ((`(,lat ,lon ,loc) (osm--fetch-location-data "New Org Link"))
               (name (osm--location-name lat lon loc 2)))
    (list :type "geo"
          :description
          (if (eq osm-server (default-value 'osm-server))
              (string-remove-suffix (concat " " (osm--server-property :name))
                                    name)
            name)
          :link
          (format "geo:%.6f,%.6f;z=%s%s"
                  lat lon osm--zoom
                  (if (eq osm-server (default-value 'osm-server)) ""
                    (format ";s=%s" osm-server))))))

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
Optionally place pin with ID and NAME."
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
          (and (equal (buffer-local-value 'major-mode buf) #'osm-mode)
               (equal (buffer-local-value 'osm-server buf) def-server)
               (equal (buffer-local-value 'osm--zoom buf) def-zoom)
               (equal (buffer-local-value 'osm--lat buf) def-lat)
               (equal (buffer-local-value 'osm--lon buf) def-lon)
               buf)))
       (generate-new-buffer "*osm*"))
    (unless (eq major-mode #'osm-mode)
      (osm-mode))
    (when (and server (not (eq osm-server server)))
      (setq-local osm-server server
                  osm--download-queue nil))
    (when (or (not (and osm--lon osm--lat)) lat)
      (setq osm--lat (or lat (nth 0 osm-home))
            osm--lon (or lon (nth 1 osm-home))
            osm--zoom (or zoom (nth 2 osm-home)))
      (when id
        (osm--set-pin id osm--lat osm--lon name)))
    (prog1 (pop-to-buffer (current-buffer))
      (osm--update))))

(defun osm--set-pin (id lat lon name &optional quiet)
  "Set pin at LAT/LON with ID and NAME.
Print NAME if not QUIET."
  (setq name (or name (format "Location %.6f° %.6f°" lat lon)))
  (setq osm--pin (list lat lon (or id 'osm-selected) name))
  (unless quiet (message "%s" name)))

(defun osm--set-pin-event (event &optional id name quiet)
  "Set selection pin with ID and NAME at location of EVENT.
Print NAME if not QUIET."
  (pcase-let ((`(,x . ,y) (posn-x-y (event-start event))))
    (osm--set-pin id
                  (osm--y-to-lat (+ (osm--y0) y) osm--zoom)
                  (osm--x-to-lon (+ (osm--x0) x) osm--zoom)
                  name quiet)))

;;;###autoload
(defun osm-goto (lat lon zoom)
  "Go to LAT/LON/ZOOM."
  (interactive
   (pcase-let ((`(,lat ,lon ,zoom)
                (mapcar #'string-to-number
                        (split-string (read-string "Lat Lon (Zoom): ") nil t))))
     (setq zoom (or zoom osm--zoom 11))
     (unless (and (numberp lat) (numberp lon) (numberp zoom))
       (error "Invalid coordinate"))
     (list lat lon zoom)))
  (osm--goto lat lon zoom nil 'osm-selected nil)
  nil)

;;;###autoload
(defun osm (&rest link)
  "Go to LINK.
When called interactively, call the function `osm-home'."
  (interactive (list 'home))
  (pcase link
    ('(home)
     (osm-home))
    (`(,lat ,lon ,zoom . ,server)
     (setq server (car server))
     (unless (and server (symbolp server)) (setq server nil)) ;; Ignore comment
     (osm--goto lat lon zoom server 'osm-selected "Elisp Link"))
    ((and `(,url . ,_) (guard (stringp url)))
     (if (string-match
          "\\`geo:\\([0-9.-]+\\),\\([0-9.-]+\\)\\(?:,[0-9.-]+\\)?\\(;.+\\'\\|\\'\\)" url)
         (let* ((lat (string-to-number (match-string 1 url)))
                (lon (string-to-number (match-string 2 url)))
                (args (url-parse-args (match-string 3 url) ""))
                (zoom (cdr (assoc "z" args)))
                (server (cdr (assoc "s" args))))
           (osm--goto lat lon
                      (and zoom (string-to-number zoom))
                      (and server (intern-soft server))
                      'osm-selected "Geo Link"))
       (osm-search (string-remove-prefix "geo:" url))))
    (_ (error "Invalid osm link"))))

;;;###autoload
(defun osm-bookmark-jump (bm)
  "Jump to osm bookmark BM."
  (interactive (list (osm--bookmark-read)))
  (pcase-let ((`(,lat ,lon ,zoom) (bookmark-prop-get bm 'coordinates)))
    (set-buffer (osm--goto lat lon zoom
                           (bookmark-prop-get bm 'server)
                           'osm-bookmark (car bm)))))
(put 'osm-bookmark-jump 'bookmark-handler-type "Osm")

;;;###autoload
(defun osm-bookmark-delete (bm)
  "Delete osm bookmark BM."
  (interactive (list (osm--bookmark-read)))
  (when (y-or-n-p (format "Delete bookmark `%s'? " bm))
    (bookmark-delete bm)
    (setq osm--pin nil)
    (osm--revert)))

;;;###autoload
(defun osm-bookmark-rename (old-name)
  "Rename osm bookmark OLD-NAME."
  (interactive (list (car (osm--bookmark-read))))
  (let ((new-name (read-from-minibuffer "New name: " old-name nil nil
                                        'bookmark-history old-name)))
    (when osm--pin (setf (cadddr osm--pin) new-name))
    (bookmark-rename old-name new-name)
    (osm--revert)))

(defun osm--bookmark-read ()
  "Read bookmark name."
  (bookmark-maybe-load-default-file)
  (or (assoc
       (pcase osm--pin
         (`(,_lat ,_lon osm-bookmark ,name) name)
         (_ (completing-read
             "Bookmark: "
             (or (cl-loop for bm in bookmark-alist
                          if (eq (bookmark-prop-get bm 'handler) #'osm-bookmark-jump)
                          collect (car bm))
                 (error "No bookmarks found"))
             nil t nil 'bookmark-history)))
       bookmark-alist)
      (error "No bookmark selected")))

(defun osm-bookmark-set ()
  "Create osm bookmark."
  (interactive)
  (osm--barf-unless-osm)
  (unwind-protect
      (pcase-let* ((`(,lat ,lon ,loc) (osm--fetch-location-data "New Bookmark"))
                   (def (osm--bookmark-name lat lon loc))
                   (name (read-from-minibuffer "Bookmark name: " def nil nil 'bookmark-history def))
                   (bookmark-make-record-function
                    (lambda () (osm--bookmark-record name lat lon loc))))
        (bookmark-set name)
        (message "Stored bookmark: %s" name)
        (setf (caddr osm--pin) 'osm-bookmark))
    (osm--revert)))

(defun osm--fetch-location-data (name)
  "Fetch location info for NAME."
  (when (mouse-event-p last-input-event)
    (osm--set-pin-event last-input-event 'osm-selected name))
  (let ((lat (or (car osm--pin) osm--lat))
        (lon (or (cadr osm--pin) osm--lon)))
    (osm--set-pin 'osm-selected lat lon name 'quiet)
    (message "%s: Fetching name of %.2f° %.2f° from %s..." name lat lon osm-search-server)
    ;; Redisplay before slow fetching
    (osm--update)
    (redisplay)
    (list lat lon
          (ignore-errors
            (alist-get
             'display_name
             (osm--fetch-json
              (format "%s/reverse?format=json&accept-language=%s&zoom=%s&lat=%s&lon=%s"
                      osm-search-server osm-search-language
                      (min 18 (max 3 osm--zoom)) lat lon)))))))

(defun osm--track-index ()
  "Return index of selected track way point."
  (cl-loop for idx from 0 for (lat lon _) in osm--track
           if (and (equal lat (car osm--pin)) (equal lon (cadr osm--pin)))
           return idx))

(defun osm--track-delete ()
  "Delete track way point."
  (when-let ((idx (osm--track-index)))
    ;; Delete pin
    (cl-callf2 delq (nth idx osm--track) osm--track)
    (setq osm--pin nil
          idx (min idx (1- (length osm--track))))
    ;; Select next pin
    (pcase (nth idx osm--track)
      (`(,lat ,lon ,name)
       (osm--set-pin 'osm-track lat lon name 'quiet)))
    ;; Rename pins after deletion
    (cl-loop for idx from (length osm--track) downto 1
             for pt in osm--track
             if (string-match-p "\\`WP[0-9]+\\'" (caddr pt)) do
             (setf (caddr pt) (format "WP%s" idx)))
    (osm--track-length)
    (osm--revert)))

(defun osm--track-rename ()
  "Rename track way point."
  (when-let ((pt (nth (osm--track-index) osm--track))
             (old-name (caddr pt))
             (new-name (read-from-minibuffer "New name: " old-name nil nil nil old-name)))
    (setf (caddr pt) new-name
          (cadddr osm--pin) new-name)
    (osm--revert)))

(defun osm-delete ()
  "Delete selected pin (bookmark or way point)."
  (interactive)
  (osm--barf-unless-osm)
  (pcase (caddr osm--pin)
    ('nil nil)
    ('osm-bookmark (osm-bookmark-delete (cadddr osm--pin)))
    ('osm-track (osm--track-delete))
    (_ (setq osm--pin nil) (osm--update))))

(defun osm-rename ()
  "Rename selected pin (bookmark or way point)."
  (interactive)
  (osm--barf-unless-osm)
  (pcase (caddr osm--pin)
    ('osm-bookmark (osm-bookmark-rename (cadddr osm--pin)))
    ('osm-track (osm--track-rename))))

;;;###autoload
(defun osm-jump ()
  "Jump to named pin."
  (interactive)
  (let (pins)
    (osm--each-pin (lambda (id lat lon zoom name)
                     (push (list name (capitalize (substring (symbol-name id) 4))
                                 id lat lon zoom)
                           pins)))
    (pcase (assoc (completing-read
                   "Jump: "
                     (lambda (str pred action)
                       (if (eq action 'metadata)
                           `(metadata
                             (group-function
                              . ,(lambda (pin transform)
                                   (if transform pin
                                     (cadr (assoc pin pins))))))
                         (complete-with-action action pins str pred)))
                     nil t nil 'osm--jump-history)
                  pins)
      (`(,name ,_group ,id ,lat ,lon ,zoom) (osm--goto lat lon zoom nil id name))
      (_ (user-error "No pin selected")))))

(defun osm--fetch-json (url)
  "Get json from URL."
  (osm--check-libraries)
  (with-temp-buffer
    (let* ((default-process-coding-system '(utf-8-unix . utf-8-unix))
           (status (apply #'call-process "curl" nil (current-buffer) nil
                          `(,@(split-string-and-unquote osm-curl-options) ,url))))
      (unless (eq status 0)
        (error "Fetching %s exited with status %s" url status)))
    (goto-char (point-min))
    (json-parse-buffer :array-type 'list :object-type 'alist)))

(defun osm--search (needle)
  "Globally search for NEEDLE and return the list of results."
  (message "Contacting %s" osm-search-server)
  (mapcar
   (lambda (x)
     (let ((lat (string-to-number (alist-get 'lat x)))
           (lon (string-to-number (alist-get 'lon x))))
       `(,(format "%s (%.6f° %.6f°)"
                  (alist-get 'display_name x)
                  lat lon)
         ,lat ,lon
         ,@(mapcar #'string-to-number (alist-get 'boundingbox x)))))
   (osm--fetch-json
    (format "%s/search?format=json&accept-language=%s&q=%s"
            osm-search-server osm-search-language
            (url-encode-url needle)))))

;;;###autoload
(defun osm-search (needle &optional lucky)
  "Globally search for NEEDLE on `osm-search-server' and display the map.
If the prefix argument LUCKY is non-nil take the first result and jump there.
See `osm-search-server' and `osm-search-language' for customization."
  (interactive
   (list
    (minibuffer-with-setup-hook
        (lambda ()
          (when (and (eq completing-read-function #'completing-read-default)
                     (not (bound-and-true-p vertico-mode)))
            ;; Override dreaded `minibuffer-complete-word' for default
            ;; completion.  When will this keybinding finally get removed from
            ;; default completion?
            (use-local-map (make-composed-keymap
                            (define-keymap "SPC" nil)
                            (current-local-map)))))
      (completing-read "Location: "
                       (osm--sorted-table osm--search-history)
                       nil nil nil 'osm--search-history))
    current-prefix-arg))
  ;; TODO: Add search bounded to current viewbox, bounded=1, viewbox=x1,y1,x2,y2
  (let* ((results (or (osm--search needle) (error "No results for `%s'" needle)))
         (selected
          (or
           (and (or lucky (not (cdr results))) (car results))
           (assoc
            (minibuffer-with-setup-hook
                (lambda ()
                  (when (and (eq completing-read-function #'completing-read-default)
                             (not (bound-and-true-p vertico-mode))
                             (not (bound-and-true-p icomplete-mode)))
                    (let ((message-log-max nil)
                          (inhibit-message t))
                      ;; Show matches immediately for default completion.
                      (minibuffer-completion-help))))
              (completing-read
               (format "Matches for '%s': " needle)
               (osm--sorted-table results)
               nil t nil t))
            results)
           (error "No selection"))))
    (osm--goto (cadr selected) (caddr selected)
               (apply #'osm--boundingbox-to-zoom (cdddr selected))
               nil 'osm-selected (car selected))))

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
  (osm--check-libraries)
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
              (list lat lon (dom-text (dom-child-by-tag pt 'name)))))))
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
  (cl-callf2 assoc-delete-all file osm--gpx-files)
  (osm--revert))

(defun osm--server-annotation (cand)
  "Annotation for server CAND."
  (when-let ((copyright (osm--server-property :copyright (get-text-property 0 'osm--server cand)))
             (str
              (replace-regexp-in-string
               "{\\(.*?\\)|.*?}"
               (lambda (str) (match-string 1 str))
               (string-join (ensure-list copyright) " | ") copyright)))
    (concat (propertize " " 'display `(space :align-to (- right ,(length str) 2)))
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
                     nil t nil 'osm--server-history
                     (format fmt
                             (osm--server-property :name)
                             (or (osm--server-property :description) "")))))
     (list
      (get-text-property 0 'osm--server
                         (or (car (member selected servers))
                             (error "No server selected"))))))
  (osm--goto nil nil nil server nil nil))

(defun osm-save-url (&optional arg)
  "Save coordinates as url in the kill ring.
If prefix ARG is given, store url as Elisp expression."
  (interactive "P")
  (osm--barf-unless-osm)
  (pcase-let* ((`(,lat ,lon ,loc) (osm--fetch-location-data "New Link"))
               (server (and (not (eq osm-server (default-value 'osm-server))) osm-server))
               (url (if arg
                        (format "(osm %.6f %.6f %s%s%s)"
                                lat lon osm--zoom
                                (if server (format " '%s" osm-server) "")
                                (if loc (format " %S" loc) ""))
                      (format "geo:%.6f,%.6f;z=%s%s%s"
                              lat lon osm--zoom
                              (if server (format ";s=%s" osm-server) "")
                              (if loc (format " (%s)" loc) "")))))
    (kill-new url)
    (message "Saved in the kill ring: %s" url)))

(cl-defun osm-add-server (server
                          &rest properties
                          &key name description group url max-connections
                          max-zoom min-zoom download-batch subdomains copyright)
  "Add SERVER with PROPERTIES to `osm-server-list'.
The properties are checked as keyword arguments.  See
`osm-server-list' for documentation of the keywords."
  (declare (indent 1))
  (ignore name description group url max-connections max-zoom
          min-zoom download-batch subdomains copyright)
  (dolist (sym '(:name :description :group :url))
    (unless (stringp (plist-get properties sym))
      (error "Server property %s is required" sym)))
  (unless (and server (symbolp server))
    (error "Server id must be a symbol"))
  (setf (alist-get server osm-server-list) properties)
  nil)

;;;###autoload
(when (>= emacs-major-version 28)
  (add-to-list 'browse-url-default-handlers '("\\`geo:" . osm)))

;;;###autoload
(eval-after-load 'ol
  (lambda ()
    (declare-function org-link-set-parameters "ol")
    (declare-function osm--org-link-props "ext:osm")
    (org-link-set-parameters
     "geo"
     :follow (lambda (link _) (osm (concat "geo:" link)))
     :store (lambda ()
              (when (eq major-mode 'osm-mode)
                (apply 'org-link-store-props (osm--org-link-props)))))))

(dolist (sym (list #'osm-center #'osm-up #'osm-down #'osm-left #'osm-right
                   #'osm-up-up #'osm-down-down #'osm-left-left #'osm-right-right
                   #'osm-zoom-out #'osm-zoom-in #'osm-bookmark-set
                   #'osm-save-url #'osm-rename #'osm-delete))
  (put sym 'command-modes '(osm-mode)))
(dolist (sym (list #'osm-mouse-drag #'osm-mouse-pin #'osm-mouse-select #'osm-mouse-track))
  (put sym 'completion-predicate #'ignore))

(provide 'osm)
;;; osm.el ends here
