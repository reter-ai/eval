# Decimals and Dates

Eval provides built-in types for exact decimal arithmetic and date/time manipulation. All types use the `->` operator for method dispatch and integrate with f-strings.

```
define price = Decimal("19.99");
define tax = price->mul(Decimal("0.08"));
define total = price->add(tax);
f"Total: ${total->format(2)}";
// => "Total: $21.59"

define now = DateTime->now();
f"Today is {now->format("%A, %B %d, %Y")}";
// => "Today is Thursday, February 20, 2026"

define m = Qty("19.99", "USD");
f"{m->format()}";
// => "19.99 USD"
```

For quantities with units (including currency), see [UNITS.md](UNITS.md).

## Decimal

Arbitrary-precision decimal arithmetic. The mantissa is stored as a chibi-scheme bignum, so there is no limit on size or precision. Unlike floating-point, Decimal arithmetic is exact:

```
// The classic floating-point problem — solved
Decimal("0.1")->add(Decimal("0.2"))->to_string();
// => "0.3" (not 0.30000000000000004)
```

### Creating Decimals

```
Decimal("1.23")          // from string (recommended)
Decimal("0.005")         // leading zeros preserved
Decimal("-99.99")        // negative
Decimal(42)              // from integer
Decimal(3.14)            // from float (string round-trip for accuracy)
```

`Decimal` accepts strings, integers, and floats. String construction is recommended for maximum precision.

### Properties

| Property | Type | Description |
|----------|------|-------------|
| `->mantissa` | integer | Internal mantissa (e.g. 123 for "1.23") |
| `->scale` | integer | Number of decimal places (e.g. 2 for "1.23") |

### Arithmetic methods

All arithmetic methods return a new Decimal. Non-Decimal arguments are auto-coerced:

```
define a = Decimal("10.50");
define b = Decimal("3");

a->add(b);                // Decimal("13.50")
a->sub(b);                // Decimal("7.50")
a->mul(b);                // Decimal("31.50")
a->div(b);                // Decimal("3.5") (default 28 decimal places)
a->div(b, 2);             // Decimal("3.50") (2 decimal places)
a->negate();              // Decimal("-10.50")
a->abs();                 // Decimal("10.50")

// Auto-coercion: non-Decimal args converted automatically
a->add(5);                // same as a->add(Decimal(5))
a->mul("1.1");            // same as a->mul(Decimal("1.1"))
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->add(other)` | Decimal | Addition |
| `->sub(other)` | Decimal | Subtraction |
| `->mul(other)` | Decimal | Multiplication |
| `->div(other)` | Decimal | Division (default 28 decimal places) |
| `->div(other, precision)` | Decimal | Division with specified decimal places |
| `->negate()` | Decimal | Flip sign |
| `->abs()` | Decimal | Absolute value |

### Comparison methods

```
define a = Decimal("1.23");
define b = Decimal("4.56");

a->lt(b);                 // true
a->gt(b);                 // false
a->eq(Decimal("1.23"));   // true
a->lte(b);                // true
a->gte(b);                // false
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->lt(other)` | boolean | Less than |
| `->gt(other)` | boolean | Greater than |
| `->eq(other)` | boolean | Equal |
| `->lte(other)` | boolean | Less than or equal |
| `->gte(other)` | boolean | Greater than or equal |

### Rounding methods

```
define d = Decimal("1.2345");

d->round(2);              // Decimal("1.23") — half-up
d->ceil(2);               // Decimal("1.24")
d->floor(2);              // Decimal("1.23")
d->truncate(2);           // Decimal("1.23")

Decimal("-1.235")->round(2);  // Decimal("-1.24") — rounds away from zero
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->round(places)` | Decimal | Round half-up |
| `->ceil(places)` | Decimal | Round toward positive infinity |
| `->floor(places)` | Decimal | Round toward negative infinity |
| `->truncate(places)` | Decimal | Round toward zero |

### Conversion methods

```
define d = Decimal("1.23");

d->to_string();           // "1.23"
d->to_number();           // 1.23 (flonum — lossy for large/precise values)
d->format(2);             // "1.23" (round to N places, return string)
d->format(0);             // "1"
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->to_string()` | string | Exact string representation |
| `->to_number()` | number | Convert to floating-point (lossy) |
| `->format(places)` | string | Round to N places, return string |

### F-string integration

Decimals automatically convert to their string representation in f-strings:

```
f"price: {Decimal("9.99")}";        // "price: 9.99"
f"total: {Decimal("100")->div(Decimal("3"), 2)}";  // "total: 33.33"
```

---

## DateTime

Date and time with timezone offset. Internally stored as UTC epoch seconds plus an offset in minutes. All accessors return values adjusted for the timezone offset.

### Creating DateTimes

```
// Constructor: year, month, day, hour, minute, second, offset_minutes
DateTime(2026, 2, 20, 10, 30, 0, 0);       // UTC
DateTime(2026, 2, 20, 10, 30, 0, 60);      // UTC+01:00
DateTime(2026, 2, 20, 10, 30, 0, -300);    // UTC-05:00

// Static methods
DateTime->now();                             // current local time
DateTime->utc_now();                         // current UTC time
DateTime->from_epoch(1740000000);            // from Unix timestamp
DateTime->parse("2026-02-20T10:30:00Z");     // ISO 8601 parse
DateTime->parse("2026-02-20T10:30:00+01:00");
DateTime->parse("2026-02-20 10:30:00");      // space separator
```

### Properties

```
define dt = DateTime(2026, 2, 20, 10, 30, 45, 60);
dt->year;                 // 2026
dt->month;                // 2
dt->day;                  // 20
dt->hour;                 // 10
dt->minute;               // 30
dt->second;               // 45
dt->epoch;                // Unix timestamp (integer)
dt->offset;               // 60 (UTC offset in minutes)
```

| Property | Type | Description |
|----------|------|-------------|
| `->year` | integer | Year (e.g. 2026) |
| `->month` | integer | Month (1-12) |
| `->day` | integer | Day of month (1-31) |
| `->hour` | integer | Hour (0-23) |
| `->minute` | integer | Minute (0-59) |
| `->second` | integer | Second (0-59) |
| `->epoch` | integer | Unix epoch seconds (UTC) |
| `->offset` | integer | UTC offset in minutes |

### Formatting

```
define dt = DateTime(2026, 2, 20, 10, 30, 0, 0);
dt->format("%Y-%m-%d");              // "2026-02-20"
dt->format("%H:%M:%S");              // "10:30:00"
dt->format("%Y-%m-%d %H:%M:%S");    // "2026-02-20 10:30:00"
dt->format("%A, %B %d, %Y");        // "Friday, February 20, 2026"
dt->iso();                            // "2026-02-20T10:30:00Z"

// With offset
DateTime(2026, 2, 20, 10, 30, 0, 60)->iso();
// "2026-02-20T10:30:00+01:00"
```

Standard `strftime` format codes are supported: `%Y` (year), `%m` (month), `%d` (day), `%H` (hour), `%M` (minute), `%S` (second), `%A` (weekday name), `%B` (month name), etc.

### Arithmetic

```
define dt = DateTime(2026, 2, 20, 10, 0, 0, 0);

// Add a TimeDelta
dt->add(TimeDelta(3600));             // +1 hour
dt->add(TimeDelta(1, 0, 0, 0));      // +1 day

// Subtract a TimeDelta
dt->sub(TimeDelta(1800));             // -30 minutes

// Subtract another DateTime → TimeDelta
define dt2 = DateTime(2026, 1, 1, 0, 0, 0, 0);
define diff = dt->sub(dt2);
diff->days;                            // 50
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->add(timedelta)` | DateTime | Add a duration |
| `->sub(timedelta)` | DateTime | Subtract a duration |
| `->sub(datetime)` | TimeDelta | Difference between two datetimes |

### Timezone conversion

```
define dt = DateTime(2026, 2, 20, 10, 0, 0, 60);  // UTC+01:00

dt->to_utc();                          // same instant, offset=0
dt->to_utc()->iso();                   // "2026-02-20T09:00:00Z"

dt->to_offset(-300);                   // same instant, UTC-05:00
dt->to_offset(-300)->hour;             // 4
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->to_utc()` | DateTime | Convert to UTC (offset=0) |
| `->to_offset(minutes)` | DateTime | Convert to given UTC offset |
| `->to_date()` | Date | Extract date (drop time component) |

### Comparison

```
define a = DateTime(2026, 1, 1, 0, 0, 0, 0);
define b = DateTime(2026, 12, 31, 23, 59, 59, 0);

a->lt(b);                 // true
a->gt(b);                 // false
a->eq(a);                 // true
a->lte(b);                // true
a->gte(b);                // false
```

### F-string integration

DateTimes render as ISO 8601 strings in f-strings:

```
f"now: {DateTime->now()}";
// => "now: 2026-02-20T10:30:00Z"
```

---

## Date

A calendar date without a time component.

### Creating Dates

```
Date(2026, 2, 20)                    // year, month, day
Date->today()                         // today's date (local)
```

### Properties

```
define d = Date(2026, 2, 20);
d->year;                  // 2026
d->month;                 // 2
d->day;                   // 20
```

| Property | Type | Description |
|----------|------|-------------|
| `->year` | integer | Year |
| `->month` | integer | Month (1-12) |
| `->day` | integer | Day of month (1-31) |

### Methods

```
define d = Date(2026, 2, 20);

d->add_days(7);                       // Date(2026, 2, 27)
d->add_days(-1);                      // Date(2026, 2, 19)

d->diff(Date(2026, 1, 1));            // TimeDelta (50 days)
d->diff(Date(2026, 1, 1))->days;      // 50

d->format("%Y-%m-%d");                // "2026-02-20"
d->format("%B %d, %Y");              // "February 20, 2026"

d->to_datetime();                     // DateTime at midnight UTC
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->add_days(n)` | Date | Add/subtract days |
| `->diff(other)` | TimeDelta | Difference between two dates |
| `->format(fmt)` | string | Format with strftime codes |
| `->to_datetime()` | DateTime | Convert to DateTime at midnight UTC |
| `->lt(other)` | boolean | Before |
| `->gt(other)` | boolean | After |
| `->eq(other)` | boolean | Same date |
| `->lte(other)` | boolean | Before or same |
| `->gte(other)` | boolean | After or same |

### F-string integration

Dates render as `YYYY-MM-DD` in f-strings:

```
f"today: {Date->today()}";
// => "today: 2026-02-20"
```

---

## TimeDelta

A duration in seconds. Used for datetime arithmetic and representing differences between dates or times.

### Creating TimeDeltas

```
TimeDelta(3600)                       // 3600 seconds (1 hour)
TimeDelta(1, 2, 30, 0)               // 1 day, 2 hours, 30 minutes, 0 seconds
```

The 4-argument form is `TimeDelta(days, hours, minutes, seconds)`.

### Properties

```
define td = TimeDelta(1, 2, 30, 15);  // 1 day, 2h, 30m, 15s = 95415s
td->total_seconds;        // 95415
td->seconds;              // 95415 (alias for total_seconds)
td->days;                 // 1
td->hours;                // 2 (hours component, not total hours)
td->minutes;              // 30 (minutes component, not total minutes)
```

| Property | Type | Description |
|----------|------|-------------|
| `->total_seconds` | integer | Total duration in seconds |
| `->seconds` | integer | Alias for `total_seconds` |
| `->days` | integer | Days component (`total_seconds / 86400`) |
| `->hours` | integer | Hours component (remainder after days) |
| `->minutes` | integer | Minutes component (remainder after hours) |

### Methods

```
define td1 = TimeDelta(3600);
define td2 = TimeDelta(1800);

td1->add(td2);            // TimeDelta(5400)
td1->negate();             // TimeDelta(-3600)

td1->lt(td2);             // false
td1->gt(td2);             // true
td1->eq(TimeDelta(3600)); // true
```

| Method | Returns | Description |
|--------|---------|-------------|
| `->add(other)` | TimeDelta | Sum of two deltas |
| `->negate()` | TimeDelta | Flip sign |
| `->lt(other)` | boolean | Less than |
| `->gt(other)` | boolean | Greater than |
| `->eq(other)` | boolean | Equal |
| `->lte(other)` | boolean | Less than or equal |
| `->gte(other)` | boolean | Greater than or equal |

---

---

## Recipes

### Invoice calculation (Decimal)

```
define items = [
    ["Widget", Decimal("9.99"), 3],
    ["Gadget", Decimal("24.50"), 1],
    ["Cable",  Decimal("4.99"), 5]
];

define subtotal = fold(function(item, acc) {
    acc->add(car(cdr(item))->mul(Decimal(car(cdr(cdr(item))))));
}, Decimal("0"), items);

define tax = subtotal->mul(Decimal("0.08"))->round(2);
define total = subtotal->add(tax);

f"Subtotal: ${subtotal->format(2)}";   // "Subtotal: $79.42"
f"Tax:      ${tax->format(2)}";        // "Tax:      $6.35"
f"Total:    ${total->format(2)}";      // "Total:    $85.77"
```

For currency-aware arithmetic with dimensional safety, see `Qty` in [UNITS.md](UNITS.md).

### Date range iteration

```
define start = Date(2026, 2, 1);
define days = 7;

for(let i = 0, i < days, i++) {
    define d = start->add_days(i);
    display(d->format("%A %B %d"));
    newline();
};
// Sunday February 01
// Monday February 02
// ...
```

### Elapsed time measurement

```
define t0 = DateTime->now();
// ... do work ...
define t1 = DateTime->now();
define elapsed = t1->sub(t0);
f"Took {elapsed->total_seconds} seconds";
```

### Currency conversion

```
define usd = Qty("100.00", "USD");
define rate = Decimal("0.92");
define eur = Qty(usd->value->mul(rate), "EUR");
f"{usd->format()} = {eur->format()}";
// => "100.00 USD = 92.00 EUR"
```

See [UNITS.md](UNITS.md) for the full Qty reference including physical units and dimensional analysis.
