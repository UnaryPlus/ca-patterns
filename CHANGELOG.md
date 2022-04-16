# Revision history for ca-patterns

## 0.1.0.0 -- 2022-03-17

* First version. Released on an unsuspecting world.

## 0.2.0.0 -- 2022-04-15

* `Cell` is now an alias for `Bool`.
* Added `parseMany` function to `Text.RLE`.
* Added `Eq` and `Show` instances for `Pattern`.
* `trimLeft` and `trimRight` no longer recurse infinitely when given
the empty pattern.
* `setDimensions` creates a grid of dead cells when given the empty pattern.
* RLE parser returns width and height in the correct order.
* Added test suite.
