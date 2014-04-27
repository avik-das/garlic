         .global scm_fncall

# stdlib functions

         .global stdlib_sum
         .global stdlib_display
         .global stdlib_newline

         .text

scm_fncall:
        # This function is called with the wrapped lambda as the only argument.
        # A new frame is created using the lambda's parent frame as the parent,
        # and the function pointer is retrieved and called.
        #
        # All the arguments to the lambda are stored in the stack right to
        # left (i.e. the right-most argument is pushed onto the stack first).
        # It is the callee's responsiblity to handle the arguments.
        push    %rdi                    # save the lambda on the stack
        mov     8(%rdi), %rdi           # create a new frame using the lambda's
        call    new_frame_with_parent   #   stored frame as the parent
        mov     %rax, %rdi
        pop     %rax                    # grab the lambda again
        push    %rdi                    # save the new frame
        mov     16(%rax), %rax          # dereference the lambda's function
        call    *%rax                   #   pointer and call it
        add     $8, %rsp                # remove the new frame from the stack
        ret

stdlib_sum:
        # ignore the pushed frame
        mov     24(%rsp), %rdi
        mov     32(%rsp), %rsi
        call    stdlib_impl_sum
        ret

stdlib_display:
        # ignore the pushed frame
        mov     24(%rsp), %rdi
        call    stdlib_impl_display
        ret

stdlib_newline:
        # ignore the pushed frame
        call    stdlib_impl_newline
        ret
