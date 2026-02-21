# Quantities and Units

Eval has a built-in quantity system for dimensional analysis, unit conversion, and currency handling. A quantity is a number paired with units — meters, seconds, kilograms, dollars. Arithmetic operators enforce dimensional safety: you can add meters to meters but not meters to seconds, and multiplying meters by meters gives square meters.

```
define d = Qty(100, "km");
define t = Qty(2, "hr");
define speed = d / t;               // Qty(50, "km/hr")
speed->to("m/s");                   // Qty(13.889, "m/s")

Qty(5, "m") + Qty(3, "m");          // Qty(8, "m")
Qty(5, "m") + Qty(3, "s");          // ERROR: incompatible dimensions

define price = Qty("19.99", "USD");
price + Qty("1.50", "USD");         // Qty(21.49, "USD") — exact Decimal
price + Qty(5, "EUR");              // ERROR: incompatible dimensions
```

The design is inspired by Python's [pint](https://pint.readthedocs.io/) library. Money is a special case — currency quantities automatically use Decimal magnitudes for exact arithmetic.

## Creating Quantities

```
Qty(100, "km")                      // 100 kilometers
Qty(9.8, "m/s^2")                   // 9.8 meters per second squared
Qty(3, "N")                         // 3 newtons
Qty("19.99", "USD")                 // 19.99 US dollars (Decimal)
```

The first argument is the magnitude (number or string for Decimal). The second is a unit string — either a single unit name or a compound expression with `*`, `/`, and `^`.

Currency units are detected automatically. When the unit is a currency (USD, EUR, etc.), the magnitude is coerced to Decimal for exact arithmetic.

### Unit string syntax

```
"m"                                 // meter
"km"                                // kilometer
"m/s"                               // meters per second
"m/s^2"                             // meters per second squared
"kg*m/s^2"                          // kilogram meters per second squared (= newton)
"km/hr"                             // kilometers per hour
```

The parser supports `*` (multiply), `/` (divide), and `^` (exponent). Spaces are ignored.

## Arithmetic

Quantities work with the standard arithmetic operators `+`, `-`, `*`, `/`, `**`. Dimensional safety is enforced at runtime.

### Addition and subtraction

Operands must have compatible dimensions:

```
Qty(5, "m") + Qty(3, "m");          // Qty(8, "m")
Qty(100, "cm") + Qty(1, "m");       // Qty(200, "cm") — auto-converts to left operand's units
Qty(5, "m") + Qty(3, "s");          // ERROR: incompatible dimensions [length] + [time]
```

When the units differ but the dimensions match, the right operand is converted to the left operand's units.

### Multiplication and division

Units combine algebraically:

```
Qty(10, "m") * Qty(5, "m");         // Qty(50, "m^2")
Qty(100, "km") / Qty(2, "hr");      // Qty(50, "km/hr")
Qty(10, "N") * Qty(5, "m");         // Qty(50, "N*m")
```

Multiplying or dividing a quantity by a plain number scales the magnitude:

```
Qty(10, "m") * 5;                   // Qty(50, "m")
3 * Qty(10, "m");                   // Qty(30, "m")
Qty(100, "km") / 2;                 // Qty(50, "km")
```

When units cancel out, the result is a plain number:

```
Qty(10, "m") / Qty(5, "m");         // 2 (dimensionless)
```

### Exponentiation

```
Qty(5, "m") ** 2;                   // Qty(25, "m^2")
Qty(3, "m") ** 3;                   // Qty(27, "m^3")
```

The exponent must be a plain number, not a quantity.

### Negation

```
-Qty(5, "m");                       // Qty(-5, "m")
```

## Comparison

Quantities support `<`, `>`, `==` with dimensional checking:

```
Qty(5, "m") > Qty(3, "m");          // true
Qty(1, "km") > Qty(500, "m");       // true (cross-unit comparison)
Qty(5, "m") > Qty(3, "s");          // ERROR: incompatible dimensions
```

## Unit Conversion

### `->to(target)`

Convert a quantity to different units within the same dimension:

```
Qty(1, "mile")->to("km");           // Qty(1.609344, "km")
Qty(100, "cm")->to("m");            // Qty(1, "m")
Qty(1, "hr")->to("second");         // Qty(3600, "second")
Qty(50, "km/hr")->to("m/s");        // Qty(13.8889, "m/s")
```

Attempting to convert between incompatible dimensions is an error:

```
Qty(5, "m")->to("s");               // ERROR: incompatible dimensions
```

### `->to_base()`

Convert to SI base units:

```
Qty(1, "N")->to_base();             // Qty(1, "kg*m/s^2")
Qty(1, "km")->to_base();            // Qty(1000, "m")
Qty(50, "kph")->to_base();          // Qty(13.8889, "m/s")
```

## Properties

| Property | Type | Description |
|----------|------|-------------|
| `->value` | number/Decimal | The magnitude |
| `->magnitude` | number/Decimal | Alias for `->value` |
| `->amount` | number/Decimal | Alias for `->value` (Money compat) |
| `->units` | string | Unit string (e.g. `"km/hr"`) |
| `->dimension` | string | Dimension string (e.g. `"length"`) |
| `->currency` | symbol | Currency symbol (e.g. `USD`); error if not a currency |

```
define speed = Qty(100, "km") / Qty(2, "hr");
speed->value;                        // 50
speed->units;                        // "km/hr"
speed->dimension;                    // "length/time"

define price = Qty("19.99", "USD");
price->value;                        // Decimal("19.99")
price->currency;                     // USD
```

## Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `->to(target)` | Qty | Convert to target units |
| `->to_base()` | Qty | Convert to SI base units |
| `->format()` | string | `"50 km/hr"` or `"19.99 USD"` |
| `->round(n)` | Qty | Round magnitude to n decimal places |
| `->add(other)` | Qty | Addition (Money compat) |
| `->sub(other)` | Qty | Subtraction (Money compat) |
| `->mul(other)` | Qty | Multiplication (Money compat) |
| `->div(other)` | Qty | Division (Money compat) |

```
Qty(50, "km/hr")->format();          // "50 km/hr"
Qty("19.99", "USD")->format();       // "19.99 USD"

Qty(3.14159, "m")->round(2);         // Qty(3.14, "m")
```

## Currency

Each currency is its own dimension. This prevents accidental cross-currency arithmetic while allowing same-currency operations:

```
define price = Qty("19.99", "USD");
define tax = Qty("1.50", "USD");
price + tax;                         // Qty(21.49, "USD")
price * 3;                           // Qty(59.97, "USD")

price + Qty(5, "EUR");              // ERROR: incompatible dimensions
```

Currency magnitudes are automatically stored as Decimal for exact arithmetic — no floating-point rounding errors.

### Supported currencies

USD, EUR, GBP, JPY, CHF, PLN, CAD, AUD, CNY, INR.

### Currency conversion

Currency conversion is intentionally not built in — exchange rates change constantly. Convert manually:

```
define usd = Qty("100.00", "USD");
define rate = Decimal("0.92");
define eur = Qty(usd->value->mul(rate), "EUR");
f"{usd->format()} = {eur->format()}";
// => "100.00 USD = 92.00 EUR"
```

## Money backward compatibility

`Money` is an alias for `Qty`:

```
Money("19.99", "USD");               // same as Qty("19.99", "USD")
Money("19.99", "USD") + Money("5", "USD");  // Qty(24.99, "USD")
```

The `->amount`, `->currency`, `->add`, `->sub`, `->mul`, `->div`, and `->format` methods all work as before. The difference is that `+`, `-`, `*`, `/` operators now work directly — no need for method calls:

```
// Old style (still works)
Money("10", "USD")->add(Money("5", "USD"));

// New style (operators)
Qty("10", "USD") + Qty("5", "USD");
```

## F-string integration

Quantities convert to their format string in f-strings:

```
define speed = Qty(100, "km") / Qty(2, "hr");
f"Speed: {speed->format()}";         // "Speed: 50 km/hr"

define price = Qty("19.99", "USD");
f"Price: {price->format()}";         // "Price: 19.99 USD"
```

## User-defined units

Define new base dimensions and derived units at runtime:

```
// New base dimension
define_base_unit("pixel", "pixels", "px");

// Derived unit
define_unit("megapixel", 1e6, "pixel", "MP");

Qty(12, "MP")->to("pixel");          // Qty(12000000, "pixel")
Qty(12000000, "px")->to("MP");       // Qty(12, "MP")
```

`define_base_unit(name, ...aliases)` creates a new dimension where the named unit is the base. `define_unit(name, scale, ref_unit, ...aliases)` creates a derived unit relative to an existing one.

## Built-in units

### SI base dimensions

| Dimension | Base unit | Symbol |
|-----------|-----------|--------|
| length | meter | m |
| mass | kilogram | kg |
| time | second | s, sec |
| current | ampere | A, amp |
| temperature | kelvin | K |
| amount | mole | mol |
| luminosity | candela | cd |

### Length

| Unit | Symbol | Equivalent |
|------|--------|------------|
| kilometer | km | 1000 m |
| centimeter | cm | 0.01 m |
| millimeter | mm | 0.001 m |
| micrometer | um | 1e-6 m |
| nanometer | nm | 1e-9 m |
| mile | mi | 1609.344 m |
| inch | in | 0.0254 m |
| foot | ft | 0.3048 m |
| yard | yd | 0.9144 m |

### Mass

| Unit | Symbol | Equivalent |
|------|--------|------------|
| gram | g | 0.001 kg |
| milligram | mg | 1e-6 kg |
| tonne | t | 1000 kg |
| pound | lb | 0.453592 kg |
| ounce | oz | 0.0283495 kg |

### Time

| Unit | Symbol | Equivalent |
|------|--------|------------|
| minute | min | 60 s |
| hour | h, hr | 3600 s |
| day | — | 86400 s |
| week | — | 604800 s |
| year | yr | 31557600 s |

### Derived SI units

| Unit | Symbol | Dimensions |
|------|--------|------------|
| newton | N | kg·m/s² |
| joule | J | kg·m²/s² |
| watt | W | kg·m²/s³ |
| pascal | Pa | kg/(m·s²) |
| hertz | Hz | 1/s |
| volt | V | kg·m²/(s³·A) |

### Volume

| Unit | Symbol | Equivalent |
|------|--------|------------|
| liter | L | 0.001 m³ |
| milliliter | mL | 1e-6 m³ |

### Speed

| Unit | Equivalent |
|------|------------|
| kph | km/hr |
| mph | mi/hr |

### Currency

USD, EUR, GBP, JPY, CHF, PLN, CAD, AUD, CNY, INR

## Recipes

### Speed, distance, time

```
define distance = Qty(100, "km");
define time = Qty(1.5, "hr");
define speed = distance / time;
f"Speed: {speed->format()}";                  // "Speed: 66.6667 km/hr"
f"In m/s: {speed->to("m/s")->format()}";     // "In m/s: 18.5185 m/s"
```

### Force calculation

```
define mass = Qty(10, "kg");
define accel = Qty(9.8, "m/s^2");
define force = mass * accel;
f"Force: {force->format()}";                   // "Force: 98 kg*m/s^2"
f"In newtons: {force->to("N")->format()}";    // "In newtons: 98 N"
```

### Invoice with currency

```
define items = [
    ["Widget", Qty("9.99", "USD"), 3],
    ["Gadget", Qty("24.50", "USD"), 1],
    ["Cable",  Qty("4.99", "USD"), 5]
];

define subtotal = fold(function(item, acc) {
    acc + car(cdr(item)) * Decimal(car(cdr(cdr(item))));
}, Qty("0", "USD"), items);

define tax = subtotal * Decimal("0.08");
define total = subtotal + tax;

f"Subtotal: {subtotal->format()}";            // "Subtotal: 79.42 USD"
f"Tax:      {tax->round(2)->format()}";       // "Tax:      6.35 USD"
f"Total:    {total->round(2)->format()}";     // "Total:    85.77 USD"
```

### Mixed unit comparison

```
Qty(1, "mile") > Qty(1, "km");               // true
Qty(1, "kg") > Qty(1, "lb");                 // true
Qty(1, "hr") > Qty(3000, "second");          // true
```

## How it works

A Quantity is a C type with two GC-traced slots: a **magnitude** (number or Decimal) and a **unit-map** (sorted alist of `(symbol . exponent)` pairs). For example, `Qty(50, "km/hr")` stores magnitude `50` and unit-map `((hr . -1) (km . 1))`.

The unit registry is a global hash table mapping unit names to entries of `(dimension-map, scale, base-umap)`. When you write `Qty(100, "km")`, the constructor looks up `"km"` in the registry, finds its scale (1000) relative to the base unit (meter), and stores the quantity with `((km . 1))` as its unit-map.

Arithmetic operators check the C type tag via `%qty?` — a fast ~5ns predicate that runs on every `+`, `-`, `*`, `/` call. If either operand is a quantity, dispatch routes to the quantity arithmetic functions. Plain number arithmetic is unaffected.

Unit conversion works by computing each operand's value in SI base units (multiply by cumulative scale factors), then dividing by the target's scale factor. Dimensional compatibility is verified by expanding both unit-maps to their dimension maps and comparing.
