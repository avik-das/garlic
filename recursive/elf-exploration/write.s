# Define _start directly to avoid needing a compiler to do extra work before
# calling into our program.
.global _start
.text
_start:
  mov   $1, %eax  # Prepare to call write(2) system call
  mov   $1, %edi  # Write to stdout (fd = 1)
  mov $msg, %esi  # Write `msg`
  mov  $14, %edx  # Write 14 bytes
  syscall         # Perform the call

  mov  $60, %eax  # Prepare to call exit(2) system call
  mov   $0, %edi  # Return 0 as the status code
  syscall         # Perform the call

.data
msg:
  .asciz "Hello, world!\n"
