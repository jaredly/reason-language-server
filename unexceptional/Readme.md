# Unexceptional

Exception tracking for OCaml.

## Usage:

- put `[@pure]` on function definitions that you want to verify does not leek any exceptions
- this will verify that

  1. Any functions that are called from this function are either also pure, or their exceptions are caught via a switch / try
  2. If this function calls any functions that it gets as arguments, it is never called with functions that will raise unhandled exceptions.
