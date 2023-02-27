ELF binary explorations
=======================

This folder contains some exploratory programs and explanations for understanding the ELF binary format. Long-term, this content will end up in a few places:

- Blog content or more permanent documentation. For now, I'll only document what I absolutely need to understand the format, and that too in an ad-hoc manner.

- Actual code generation by the recursive compiler.

Usage
-----

### Make a small ELF binary using a real compiler

```sh
# Make the binary by invoking a real compiler
make tiny

# Ensure the binary runs correctly
./tiny
echo $?  # should show "42"

# Explore the contents of the binary
readelf -a tiny
```

As a reference, it's good to generate a real-world binary to compare against.  While it's not easy to build a truly minimal binary using a real compiler, it's still useful to see how far we can take it. The idea is that this binary is guaranteed to work on the current machine.

### Make a minimal ELF binary "by hand"

```sh
# Make the binary by running the script
make from-scratch

# Ensure the binary runs correctly
./from-scratch
echo $?  # should show "42"

# Explore the contents of the binary
readelf -a from-scratch
```

Generate a static binary from scratch, by writing out individual bytes to a file. This is the one that shows how to actually generate binaries, instead of outsourcing that work to a production compiler.
