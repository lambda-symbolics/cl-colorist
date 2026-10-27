;;;; -- Colorist --

(in-package #:cl-colorist)


;;;; -- Capabilities --

(deftype color-level ()
  "The amount of ANSI color understood by a terminal, in increasing order."
  '(member :none :basic :indexed :truecolor))

(defparameter *color-level* nil
  "Color-level override, or NIL to inspect the process environment.")

(defun colorist--getenv (name)
  "Return environment variable NAME without requiring an environment library."
  (let* ((package (or (find-package '#:uiop/os)
                      (find-package '#:uiop)))
         (symbol  (and package (find-symbol "GETENV" package))))
    (when (and symbol (fboundp symbol))
      (funcall symbol name))))

(defun colorist--contains-p (needle haystack)
  "Return true when HAYSTACK contains NEEDLE without regard to case."
  (and haystack
       (not (null (search needle haystack :test #'char-equal)))))

(defun terminal-color-level (&key (getenv #'colorist--getenv))
  "Infer a COLOR-LEVEL by calling GETENV with environment variable names.

The presence of NO_COLOR disables color.  TERM=dumb also disables it.  A
true-color COLORTERM selects :TRUECOLOR.  A 256-color TERM or COLORTERM
selects :INDEXED.  Other environments select :BASIC.  Supplying GETENV makes
capability decisions straightforward to test or embed in an unusual
environment."
  (let ((no-color  (funcall getenv "NO_COLOR"))
        (term      (funcall getenv "TERM"))
        (colorterm (funcall getenv "COLORTERM")))
    (cond ((not (null no-color))
           :none)
          ((and term (string-equal term "dumb"))
           :none)
          ((or (colorist--contains-p "truecolor" colorterm)
               (colorist--contains-p "24bit" colorterm))
           :truecolor)
          ((or (colorist--contains-p "256color" term)
               (colorist--contains-p "256" colorterm))
           :indexed)
          (t
           :basic))))

(defun effective-color-level (&optional level)
  "Return validated LEVEL, the dynamic override, or terminal detection.

NIL means that no explicit level was supplied."
  (let ((effective-level (or level *color-level* (terminal-color-level))))
    (check-type effective-level color-level)
    effective-level))


;;;; -- Colors --

(deftype basic-color-name ()
  "A named color from the basic 16-color ANSI palette."
  '(member :black
           :red
           :green
           :yellow
           :blue
           :magenta
           :cyan
           :white
           :bright-black
           :bright-red
           :bright-green
           :bright-yellow
           :bright-blue
           :bright-magenta
           :bright-cyan
           :bright-white))

(defparameter +basic-color-names+
  '(:black
    :red
    :green
    :yellow
    :blue
    :magenta
    :cyan
    :white
    :bright-black
    :bright-red
    :bright-green
    :bright-yellow
    :bright-blue
    :bright-magenta
    :bright-cyan
    :bright-white)
  "The basic ANSI colors in SGR palette order.")

(deftype color-component ()
  "One 8-bit sRGB channel value."
  '(integer 0 255))

(deftype rgb-triple ()
  "A list of exactly three 8-bit sRGB channel values."
  '(cons color-component (cons color-component (cons color-component null))))

(defstruct (color
            (:constructor color--create (kind value fallback indexed-fallback))
            (:predicate color-p)
            (:copier nil))
  "A validated basic, indexed, or 24-bit terminal color.

KIND is the keyword BASIC, INDEXED, or RGB.  VALUE is a basic color name, an
xterm palette index, or an RGB-TRIPLE.  FALLBACK is the basic color name used
when a richer color is rendered for a basic terminal.  INDEXED-FALLBACK is the
xterm palette index used when an RGB color is rendered for an indexed
terminal."
  (kind             :basic :type (member :basic :indexed :rgb) :read-only t)
  (value            :white     :type (or basic-color-name (integer 0 255) rgb-triple)
                    :read-only t)
  (fallback         nil :type (or null basic-color-name) :read-only t)
  (indexed-fallback nil :type (or null (integer 0 255)) :read-only t))

(defun colorist--color-load-form (color)
  "Return a form that reconstructs COLOR through its validated constructor."
  (when color
    (ecase (color-kind color)
      (:basic
       `(basic-color ',(color-value color)))
      (:indexed
       `(indexed-color ,(color-value color)
                       :fallback ',(color-fallback color)))
      (:rgb
       `(rgb-color ,@(color-value color)
                   :fallback (indexed-color ,(color-indexed-fallback color)
                                            :fallback ',(color-fallback color)))))))

(defmethod make-load-form ((color color) &optional environment)
  "Return a portable creation form for immutable COLOR."
  (declare (ignore environment))
  (values (colorist--color-load-form color) nil))

(defun basic-color-names ()
  "Return a fresh list of all basic ANSI color names."
  (copy-list +basic-color-names+))

(defun basic-color (name)
  "Return a COLOR for basic ANSI color NAME."
  (check-type name basic-color-name)
  (color--create :basic name nil nil))

(defun colorist--basic-color-name (designator)
  "Return the basic color name represented by DESIGNATOR."
  (cond ((typep designator 'basic-color-name)
         designator)
        ((and (color-p designator)
              (eq (color-kind designator) :basic))
         (color-value designator))
        (t
         (error 'color-type-error
                :datum         designator
                :expected-type '(or basic-color-name
                                    (satisfies colorist--basic-color-p))))))

(defun colorist--basic-color-p (object)
  "Return true when OBJECT is a basic COLOR."
  (and (color-p object)
       (eq (color-kind object) :basic)))

(defun indexed-color (index &key (fallback :white))
  "Return xterm-256 color INDEX with a basic COLOR FALLBACK.

FALLBACK may be a basic color name or the result of BASIC-COLOR."
  (check-type index (integer 0 255))
  (color--create :indexed index (colorist--basic-color-name fallback) nil))

(defparameter +basic-color-values+
  '((:black 0 0 0)
    (:red 205 0 0)
    (:green 0 205 0)
    (:yellow 205 205 0)
    (:blue 0 0 238)
    (:magenta 205 0 205)
    (:cyan 0 205 205)
    (:white 229 229 229)
    (:bright-black 127 127 127)
    (:bright-red 255 0 0)
    (:bright-green 0 255 0)
    (:bright-yellow 255 255 0)
    (:bright-blue 92 92 255)
    (:bright-magenta 255 0 255)
    (:bright-cyan 0 255 255)
    (:bright-white 255 255 255))
  "Representative sRGB values of the basic colors, from xterm's defaults.")

(defparameter +cube-levels+ '(0 95 135 175 215 255)
  "The channel values of the xterm-256 color cube, indices 16 to 231.")

(defun colorist--distance (triple other-triple)
  "Return the squared Euclidean distance between sRGB TRIPLE and OTHER-TRIPLE."
  (loop for component in triple
        for other-component in other-triple
        sum (expt (- component other-component) 2)))

(defun colorist--nearest-basic-color (red green blue)
  "Return the basic color name closest to an sRGB triple."
  (loop with best-name = :white
        with best-distance = nil
        for (name . other-triple) in +basic-color-values+
        for distance = (colorist--distance (list red green blue) other-triple)
        when (or (null best-distance) (< distance best-distance))
          do (setf best-name name
                   best-distance distance)
        finally (return best-name)))

(defun colorist--nearest-cube-level (component)
  "Return the position in +CUBE-LEVELS+ closest to channel COMPONENT."
  (loop with best-position = 0
        with best-distance = nil
        for level in +cube-levels+
        for position from 0
        for distance = (abs (- component level))
        when (or (null best-distance) (< distance best-distance))
          do (setf best-position position
                   best-distance distance)
        finally (return best-position)))

(defun colorist--nearest-indexed-color (red green blue)
  "Return the xterm-256 index closest to an sRGB triple.

Both the 6x6x6 color cube and the 24-step gray ramp are candidates; the
sixteen system colors are skipped because terminals redefine them."
  (let* ((red-position   (colorist--nearest-cube-level red))
         (green-position (colorist--nearest-cube-level green))
         (blue-position  (colorist--nearest-cube-level blue))
         (cube-index     (+ 16 (* 36 red-position) (* 6 green-position) blue-position))
         (cube-distance  (colorist--distance (list red green blue)
                                             (list (nth red-position +cube-levels+)
                                                   (nth green-position +cube-levels+)
                                                   (nth blue-position +cube-levels+))))
         (gray-step      (max 0 (min 23 (round (- (/ (+ red green blue) 3) 8) 10))))
         (gray-value     (+ 8 (* 10 gray-step)))
         (gray-distance  (colorist--distance (list red green blue)
                                             (list gray-value gray-value gray-value))))
    (if (< gray-distance cube-distance)
        (+ 232 gray-step)
        cube-index)))

(defun rgb-color (red green blue &key fallback)
  "Return a 24-bit color from sRGB channels with fallbacks for lesser terminals.

FALLBACK may be a basic color name, the result of BASIC-COLOR, or an indexed
color, which supplies both the xterm-256 and basic fallbacks.  Without it,
the closest xterm-256 index and basic color are derived from the channels."
  (check-type red color-component)
  (check-type green color-component)
  (check-type blue color-component)
  (multiple-value-bind (indexed-fallback basic-fallback)
      (cond ((null fallback)
             (values (colorist--nearest-indexed-color red green blue)
                     (colorist--nearest-basic-color red green blue)))
            ((and (color-p fallback) (eq (color-kind fallback) :indexed))
             (values (color-value fallback) (color-fallback fallback)))
            (t
             (values (colorist--nearest-indexed-color red green blue)
                     (colorist--basic-color-name fallback))))
    (color--create :rgb (list red green blue) basic-fallback indexed-fallback)))

(defun hex-color (string &key fallback)
  "Return the RGB-COLOR written as six hexadecimal digits in STRING.

A leading number sign is permitted, as in \"#193cb8\".  FALLBACK is passed to
RGB-COLOR."
  (check-type string string)
  (let ((digits (string-left-trim "#" string)))
    (unless (and (= (length digits) 6)
                 (= (length string) (+ (length digits)
                                       (if (and (plusp (length string))
                                                (char= (char string 0) #\#))
                                           1
                                           0)))
                 (every (lambda (character) (digit-char-p character 16)) digits))
      (error 'color-type-error
             :datum         string
             :expected-type '(satisfies colorist--hex-color-string-p)))
    (rgb-color (parse-integer digits :start 0 :end 2 :radix 16)
               (parse-integer digits :start 2 :end 4 :radix 16)
               (parse-integer digits :start 4 :end 6 :radix 16)
               :fallback fallback)))

(defun colorist--hex-color-string-p (object)
  "Return true when OBJECT is a six-digit hexadecimal color string."
  (and (stringp object)
       (let ((digits (string-left-trim "#" object)))
         (and (= (length digits) 6)
              (<= (length object) 7)
              (every (lambda (character) (digit-char-p character 16)) digits)))))

(defun colorist--color (designator)
  "Coerce a basic color name or COLOR DESIGNATOR to a COLOR."
  (cond ((color-p designator)
         designator)
        ((typep designator 'basic-color-name)
         (basic-color designator))
        (t
         (error 'color-type-error
                :datum         designator
                :expected-type '(or color basic-color-name)))))

(defun colorist--basic-color-index (name)
  "Return NAME's zero-based position in the basic ANSI palette."
  (or (position name +basic-color-names+)
      (error 'color-type-error :datum name :expected-type 'basic-color-name)))

(defun colorist--basic-sgr-code (name background-p)
  "Return the basic SGR code for color NAME and destination BACKGROUND-P."
  (let ((index (colorist--basic-color-index name)))
    (+ index
       (if background-p
           (if (< index 8) 40 92)
           (if (< index 8) 30 82)))))

(defun colorist--color-codes (color background-p level)
  "Return SGR parameters for COLOR at LEVEL and destination BACKGROUND-P."
  (ecase level
    (:none
     nil)
    (:basic
     (list (colorist--basic-sgr-code
            (if (eq (color-kind color) :basic)
                (color-value color)
                (color-fallback color))
            background-p)))
    (:indexed
     (ecase (color-kind color)
       (:basic
        (list (colorist--basic-sgr-code (color-value color) background-p)))
       (:indexed
        (list (if background-p 48 38) 5 (color-value color)))
       (:rgb
        (list (if background-p 48 38) 5 (color-indexed-fallback color)))))
    (:truecolor
     (ecase (color-kind color)
       (:basic
        (list (colorist--basic-sgr-code (color-value color) background-p)))
       (:indexed
        (list (if background-p 48 38) 5 (color-value color)))
       (:rgb
        (list* (if background-p 48 38) 2 (color-value color)))))))


;;;; -- Styles and rendering --

(defstruct (style
            (:constructor style--create
                (foreground background bold-p faint-p italic-p underline-p
                 reverse-p))
            (:predicate style-p)
            (:copier nil))
  "A validated collection of terminal text attributes."
  (foreground nil :type (or null color) :read-only t)
  (background nil :type (or null color) :read-only t)
  (bold-p     nil :type boolean :read-only t)
  (faint-p    nil :type boolean :read-only t)
  (italic-p   nil :type boolean :read-only t)
  (underline-p nil :type boolean :read-only t)
  (reverse-p  nil :type boolean :read-only t))

(defmethod make-load-form ((style style) &optional environment)
  "Return a portable creation form for immutable STYLE."
  (declare (ignore environment))
  (values
   `(make-style
     :foreground ,(colorist--color-load-form (style-foreground style))
     :background ,(colorist--color-load-form (style-background style))
     :bold ,(style-bold-p style)
     :faint ,(style-faint-p style)
     :italic ,(style-italic-p style)
     :underline ,(style-underline-p style)
     :reverse ,(style-reverse-p style))
   nil))

(defun make-style (&key foreground background bold faint italic underline reverse)
  "Return a validated STYLE from colors and text attribute flags.

FOREGROUND and BACKGROUND accept a basic color keyword or a COLOR object.
BOLD and FAINT are mutually exclusive intensity requests."
  (when (and bold faint)
    (error 'style-conflict-error :attributes '(:bold :faint)))
  (style--create (and foreground (colorist--color foreground))
                 (and background (colorist--color background))
                 (not (null bold))
                 (not (null faint))
                 (not (null italic))
                 (not (null underline))
                 (not (null reverse))))

(defun colorist--style-codes (style level)
  "Return all SGR parameter codes for STYLE rendered at LEVEL."
  (append (when (style-bold-p style)      '(1))
          (when (style-faint-p style)     '(2))
          (when (style-italic-p style)    '(3))
          (when (style-underline-p style) '(4))
          (when (style-reverse-p style)   '(7))
          (when (style-foreground style)
            (colorist--color-codes (style-foreground style) nil level))
          (when (style-background style)
            (colorist--color-codes (style-background style) t level))))

(defun sgr-sequence (style &key (level (effective-color-level)))
  "Return the ANSI SGR sequence that activates STYLE at color LEVEL.

The empty string is returned for :NONE and for a style without attributes."
  (check-type style style)
  (let* ((effective-level (effective-color-level level))
         (codes           (unless (eq effective-level :none)
                            (colorist--style-codes style effective-level))))
    (if codes
        (format nil "~c[~{~d~^;~}m" (code-char 27) codes)
        "")))

(defun reset-sequence (&key (level (effective-color-level)))
  "Return the ANSI SGR reset sequence at color LEVEL, or the empty string."
  (if (eq (effective-color-level level) :none)
      ""
      (format nil "~c[0m" (code-char 27))))

(defun colorize (text style
                 &key
                   (level (effective-color-level))
                   (reset t))
  "Wrap TEXT in STYLE's SGR sequence at LEVEL.

When RESET is true, append an SGR reset after styled text.  TEXT is returned
unchanged when styling is disabled or STYLE has no effective attributes."
  (check-type text string)
  (check-type style style)
  (let ((prefix (sgr-sequence style :level level)))
    (if (zerop (length prefix))
        text
        (concatenate 'string
                     prefix
                     text
                     (if reset (reset-sequence :level level) "")))))

(defun paint (text
              &key
                foreground
                background
                bold
                faint
                italic
                underline
                reverse
                (level (effective-color-level))
                (reset t))
  "Colorize TEXT with a one-shot STYLE described by keyword arguments."
  (colorize text
            (make-style :foreground foreground
                        :background background
                        :bold       bold
                        :faint      faint
                        :italic     italic
                        :underline  underline
                        :reverse    reverse)
            :level level
            :reset reset))


;;;; -- ANSI-aware text --

(defparameter +escape-character+ (code-char 27)
  "The ASCII escape character.")

(defun colorist--skip-csi (string start)
  "Return the index after a CSI body in STRING beginning at START."
  (loop for index from start below (length string)
        for code = (char-code (char string index))
        when (member code '(#x18 #x1a))
          return (1+ index)
        when (<= #x40 code #x7e)
          return (1+ index)
        finally (return (length string))))

(defun colorist--skip-control-string (string start bel-terminated-p)
  "Return the index after a canceled or ST-terminated control string.

When BEL-TERMINATED-P is true, BEL also terminates the string as permitted for
an operating-system command."
  (loop for index from start below (length string)
        for character = (char string index)
        for code = (char-code character)
        when (member code '(#x18 #x1a))
          return (1+ index)
        when (and bel-terminated-p (= code 7))
          return (1+ index)
        when (= code #x9c)
          return (1+ index)
        when (and (char= character +escape-character+)
                  (< (1+ index) (length string))
                  (char= (char string (1+ index)) #\\))
          return (+ index 2)
        finally (return (length string))))

(defun colorist--skip-escape (string start)
  "Return the index after STRING's escape sequence beginning at START."
  (let ((next (1+ start)))
    (if (>= next (length string))
        (length string)
        (case (char string next)
          (#\[
           (colorist--skip-csi string (1+ next)))
          (#\]
           (colorist--skip-control-string string (1+ next) t))
          ((#\P #\X #\^ #\_)
           (colorist--skip-control-string string (1+ next) nil))
          (otherwise
           (loop with index = next
                 while (and (< index (length string))
                            (<= #x20 (char-code (char string index)) #x2f))
                   do (incf index)
                 finally (return (min (1+ index) (length string)))))))))

(defun ansi-control-end (string index)
  "Return the end of a control sequence at INDEX, or NIL for visible text."
  (check-type string string)
  (check-type index (integer 0))
  (unless (< index (length string))
    (error 'text-index-error
           :datum         index
           :expected-type `(integer 0 ,(max 0 (1- (length string))))
           :text          string))
  (let ((code (char-code (char string index))))
    (cond ((char= (char string index) +escape-character+)
           (colorist--skip-escape string index))
          ((= code #x9b)
           (colorist--skip-csi string (1+ index)))
          ((= code #x9d)
           (colorist--skip-control-string string (1+ index) t))
          ((member code '(#x90 #x98 #x9e #x9f))
           (colorist--skip-control-string string (1+ index) nil))
          ((<= #x80 code #x9f)
           (1+ index))
          (t
           nil))))

(defun strip-ansi (string)
  "Remove ANSI CSI, control-string, C1 and ordinary escape sequences.

Incomplete control sequences are discarded through the end of STRING."
  (check-type string string)
  (with-output-to-string (clean)
    (loop with index = 0
          while (< index (length string))
          for escape-end = (ansi-control-end string index)
          do (if escape-end
                 (setf index escape-end)
                 (progn
                   (write-char (char string index) clean)
                   (incf index))))))

(defun visible-length (string)
  "Return STRING's character count with ANSI control sequences ignored.

This is deliberately a character count, not a Unicode terminal-cell width.
It lets a caller apply its own grapheme and cell-width policy after ANSI
presentation has been removed."
  (check-type string string)
  (loop with index = 0
        with count = 0
        while (< index (length string))
        for escape-end = (ansi-control-end string index)
        do (if escape-end
               (setf index escape-end)
               (progn
                 (incf count)
                 (incf index)))
        finally (return count)))


;;;; -- Conditions --

(define-condition color-type-error (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream "~S is not a valid color value; expected ~S."
                     (type-error-datum condition)
                     (type-error-expected-type condition))))
  (:documentation
   "A color, basic color name, or hex color string was invalid.

The datum is the rejected value and the expected type describes the values
the caller could have passed."))

(define-condition style-conflict-error (error)
  ((attributes
    :initarg       :attributes
    :reader        style-conflict-error-attributes
    :type          list
    :documentation "The keywords of the text attributes requested together."))
  (:report (lambda (condition stream)
             (format stream "A terminal style cannot be both ~{~(~A~)~^ and ~}."
                     (style-conflict-error-attributes condition))))
  (:documentation "A style requested text attributes that exclude each other."))

(define-condition text-index-error (type-error)
  ((text
    :initarg       :text
    :reader        text-index-error-text
    :type          string
    :documentation "The string the index was meant to point into."))
  (:report (lambda (condition stream)
             (format stream "Index ~S is outside a string of ~D characters."
                     (type-error-datum condition)
                     (length (text-index-error-text condition)))))
  (:documentation
   "An index given to an ANSI text function does not point at a character.

The datum is the rejected index."))
