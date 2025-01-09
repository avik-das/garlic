#include <garlic.h>

#include <stdlib.h>
#include <unistd.h>

/* Can't be more than 256, as `getentropy` doesn't support larger sizes. */
static const size_t RAND_BUFFER_LENGTH = 128;

garlic_value_t rand_atom() {
  char *buf = malloc(RAND_BUFFER_LENGTH + 1); // leave room for NULL-terminator
  if (!buf) {
    error_and_exit("rand_atom: unable to allocate buffer");
  }

  int result = getentropy(buf, RAND_BUFFER_LENGTH);
  if (result != 0) {
    error_and_exit("rand_atom: unable to get enough random bytes");
  }

  // Convert any non-identifier-style characters into the right ASCII range
  for (int i = 0; i < RAND_BUFFER_LENGTH; i++) {
    unsigned char b = buf[i];

    // Alphabetical characters, upper or lowercase are okay
    if ((b >= 65 && b <= 90) ||
        (b >= 97 && b <= 122)) { continue; }

    // In the future, maybe we can support numeric characters in non-initial
    // positions. The tricky part is that the process of converting non-allowed
    // characters into allowed characters will differ based on whether numbers
    // are allowed in a given position.

    // Otherwise, bring the byte into the correct range. To do this, start by
    // sliding the byte value into a continuous range, meaning offset larger
    // values assuming the alphabetical range doesn't exist.
         if (b > 122) { b -= 52; } // Account for both cases
    else if (b >  90) { b -= 26; } // Account for only uppercase

    // With the byte in the range [0, 204), bring it into the [0, 52) range.
    // Note that this transformation is not uniform, so certain final values
    // are less likely than others.
    b = b % 52;

    if (b <= 25) { buf[i] = b      + 65; } // uppercase
    else         { buf[i] = b - 26 + 97; } // lowercase
  }

  buf[RAND_BUFFER_LENGTH] = 0;
  return garlic_intern_atom(buf);
}

garlic_native_export_t rand_exports[] = {
    {"rand-atom", rand_atom, 0},
    0
};
