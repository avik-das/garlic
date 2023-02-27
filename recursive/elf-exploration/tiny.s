# Define _start directly to avoid needing a compiler to do extra work before
# calling into our program.
.global _start
.text
_start:
  mov $60, %rax  # Prepare to call exit(2) system call
  mov $42, %edi  # Return value is 42, just for fun
  syscall        # Perform the call
