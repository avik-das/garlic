#if defined(__WIN32__) || defined(__APPLE__)
# define cdecl(s) _##s
#else
# define cdecl(s) s
#endif

         .global cdecl(scm_fncall)

# stdlib functions

         .global cdecl(stdlib_sum)
         .global cdecl(stdlib_mul)

         .global cdecl(stdlib_cons)
         .global cdecl(stdlib_car)
         .global cdecl(stdlib_cdr)
         .global cdecl(stdlib_nullp)

         .global cdecl(stdlib_display)
         .global cdecl(stdlib_newline)

         .text

cdecl(scm_fncall):
        # This function is called with the wrapped lambda as the only argument.
        # A new frame is created using the lambda's parent frame as the parent,
        # and the function pointer is retrieved and called.
        #
        # All the arguments to the lambda are stored in the stack right to
        # left (i.e. the right-most argument is pushed onto the stack first).
        # It is the callee's responsiblity to handle the arguments.
        push    %rdi                    # save the lambda on the stack
        mov     8(%rdi), %rdi           # create a new frame using the lambda's
        call    cdecl(new_frame_with_parent) # stored frame as the parent
        mov     %rax, %rdi
        pop     %rax                    # grab the lambda again
        push    %rdi                    # save the new frame
        mov     16(%rax), %rax          # dereference the lambda's function
        call    *%rax                   #   pointer and call it
        add     $8, %rsp                # remove the new frame from the stack
        ret

cdecl(stdlib_sum):
        # ignore the pushed frame
        mov     24(%rsp), %rdi
        mov     32(%rsp), %rsi
        call    cdecl(stdlib_impl_sum)
        ret

cdecl(stdlib_mul):
        # ignore the pushed frame
        mov     24(%rsp), %rdi
        mov     32(%rsp), %rsi
        call    cdecl(stdlib_impl_mul)
        ret

cdecl(stdlib_cons):
        # ignore the pushed frame
        mov     24(%rsp), %rdi
        mov     32(%rsp), %rsi
        call    cdecl(make_cons)
        ret

cdecl(stdlib_car):
        # ignore the pushed frame
        mov     24(%rsp), %rax
        mov     8(%rax), %rax
        ret

cdecl(stdlib_cdr):
        # ignore the pushed frame
        mov     24(%rsp), %rax
        mov     16(%rax), %rax
        ret

cdecl(stdlib_nullp):
        # ignore the pushed frame
        mov     24(%rsp), %rdi
        call    cdecl(stdlib_impl_nullp)
        ret

cdecl(stdlib_display):
        # ignore the pushed frame
        mov     24(%rsp), %rdi
        call    cdecl(stdlib_impl_display)
        ret

cdecl(stdlib_newline):
        # ignore the pushed frame
        call    cdecl(stdlib_impl_newline)
        ret
