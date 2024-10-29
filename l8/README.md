```sh
make divide_ubsan            # Compile with UBSAN for divide-by-zero
make divide_no_sanitize      # Compile without UBSAN

make bounds_asan             # Compile with ASAN for out-of-bounds
make bounds_no_sanitize      # Compile without ASAN

make static_error_reject     # Compile with error detection enabled
make static_error_allow      # Compile without error detection

make all                     # Compile all
```