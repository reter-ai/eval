;; eval/units-defs.scm -- SI base dimensions, derived units, and currency definitions
;; Loaded after quantity-oo.scm which provides define-dimension, define-unit, define-compound-unit.

;; ================================================================
;; SI base dimensions
;; ================================================================

(define-dimension "length"      "meter"    "m")
(define-dimension "mass"        "kilogram" "kg")
(define-dimension "time"        "second"   "s" "sec")
(define-dimension "current"     "ampere"   "A" "amp")
(define-dimension "temperature" "kelvin"   "K")
(define-dimension "amount"      "mole"     "mol")
(define-dimension "luminosity"  "candela"  "cd")

;; ================================================================
;; Length
;; ================================================================

(define-unit "kilometer"  1000      "meter" "km")
(define-unit "centimeter" 0.01      "meter" "cm")
(define-unit "millimeter" 0.001     "meter" "mm")
(define-unit "micrometer" 1e-6      "meter" "um")
(define-unit "nanometer"  1e-9      "meter" "nm")
(define-unit "mile"       1609.344  "meter" "mi")
(define-unit "inch"       0.0254    "meter" "in")
(define-unit "foot"       0.3048    "meter" "ft")
(define-unit "yard"       0.9144    "meter" "yd")

;; ================================================================
;; Mass
;; ================================================================

(define-unit "gram"      0.001     "kilogram" "g")
(define-unit "milligram" 1e-6      "kilogram" "mg")
(define-unit "tonne"     1000      "kilogram" "t")
(define-unit "pound"     0.453592  "kilogram" "lb")
(define-unit "ounce"     0.0283495 "kilogram" "oz")

;; ================================================================
;; Time
;; ================================================================

(define-unit "minute" 60      "second" "min")
(define-unit "hour"   3600    "second" "h" "hr")
(define-unit "day"    86400   "second")
(define-unit "week"   604800  "second")
(define-unit "year"   31557600 "second" "yr")

;; ================================================================
;; Derived SI units
;; ================================================================

(define-compound-unit "newton"  1 '(("kilogram" . 1) ("meter" . 1) ("second" . -2)) "N")
(define-compound-unit "joule"   1 '(("kilogram" . 1) ("meter" . 2) ("second" . -2)) "J")
(define-compound-unit "watt"    1 '(("kilogram" . 1) ("meter" . 2) ("second" . -3)) "W")
(define-compound-unit "pascal"  1 '(("kilogram" . 1) ("meter" . -1) ("second" . -2)) "Pa")
(define-compound-unit "hertz"   1 '(("second" . -1)) "Hz")
(define-compound-unit "volt"    1 '(("kilogram" . 1) ("meter" . 2) ("second" . -3) ("ampere" . -1)) "V")

;; ================================================================
;; Volume
;; ================================================================

(define-compound-unit "liter"      0.001 '(("meter" . 3)) "L")
(define-compound-unit "milliliter" 1e-6  '(("meter" . 3)) "mL")

;; ================================================================
;; Speed (compound)
;; ================================================================

(define-compound-unit "kph" 1 '(("kilometer" . 1) ("hour" . -1)))
(define-compound-unit "mph" 1 '(("mile" . 1) ("hour" . -1)))

;; ================================================================
;; Currency dimensions
;; Each currency is its own dimension — prevents cross-currency addition.
;; ================================================================

(define-dimension "currency_USD" "USD")
(define-dimension "currency_EUR" "EUR")
(define-dimension "currency_GBP" "GBP")
(define-dimension "currency_JPY" "JPY")
(define-dimension "currency_CHF" "CHF")
(define-dimension "currency_PLN" "PLN")
(define-dimension "currency_CAD" "CAD")
(define-dimension "currency_AUD" "AUD")
(define-dimension "currency_CNY" "CNY")
(define-dimension "currency_INR" "INR")
