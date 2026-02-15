/*
 * mitgcm_error_handler.c - Error recovery for MITgcm shared library mode
 *
 * Intercepts Fortran STOP statements (via gfortran runtime overrides) so that
 * MITgcm errors return control to Julia instead of killing the process.
 *
 * Provides "safe" wrappers around init/step that use setjmp/longjmp to catch
 * STOP calls and return an error code + message to the caller.
 */

#include <setjmp.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdint.h>

/* ================================================================
 * Error recovery state
 * ================================================================ */

static jmp_buf  mitgcm_jmp_buf;
static int      mitgcm_in_protected_call = 0;
static char     mitgcm_error_message[512] = {0};

/* ================================================================
 * Override gfortran's STOP handlers
 *
 * When MITgcm hits a STOP statement inside a protected call,
 * we longjmp back to the safe wrapper instead of exiting.
 * ================================================================ */

/*
 * _gfortran_stop_string - called by gfortran for STOP 'message'
 *
 * Signature varies by gfortran version:
 *   gfortran <= 12:  (const char *msg, int32_t len)
 *   gfortran >= 13:  (const char *msg, size_t len, _Bool is_char4)
 *
 * We use size_t for the length which works with both (int32 args are
 * zero-extended to 64-bit in registers on x86_64/arm64).
 */
void _gfortran_stop_string(const char *msg, size_t len) {
    if (mitgcm_in_protected_call) {
        size_t n = len < 511 ? len : 511;
        memcpy(mitgcm_error_message, msg, n);
        mitgcm_error_message[n] = '\0';
        longjmp(mitgcm_jmp_buf, 1);
    }
    /* Not in a protected call — fall through to normal behavior */
    fprintf(stderr, "Fortran STOP: %.*s\n", (int)len, msg);
    _exit(1);
}

/*
 * _gfortran_stop_numeric - called by gfortran for STOP <number>
 */
void _gfortran_stop_numeric(int32_t code) {
    if (mitgcm_in_protected_call) {
        snprintf(mitgcm_error_message, sizeof(mitgcm_error_message),
                 "Fortran STOP (code %d)", code);
        longjmp(mitgcm_jmp_buf, 1);
    }
    fprintf(stderr, "Fortran STOP %d\n", code);
    _exit(code);
}

/* ================================================================
 * Safe wrappers (called from Julia via ccall)
 *
 * Returns 0 on success, 1 on error (STOP intercepted).
 * After an error, call mitgcm_lib_get_error_msg_ to retrieve
 * the STOP message.
 * ================================================================ */

/* Forward declarations of Fortran subroutines */
extern void mitgcm_lib_init_(void);
extern void mitgcm_lib_step_(void);

int mitgcm_lib_safe_init_(void) {
    mitgcm_error_message[0] = '\0';
    mitgcm_in_protected_call = 1;

    if (setjmp(mitgcm_jmp_buf) == 0) {
        mitgcm_lib_init_();
        mitgcm_in_protected_call = 0;
        return 0;  /* success */
    } else {
        mitgcm_in_protected_call = 0;
        return 1;  /* error — STOP was intercepted */
    }
}

int mitgcm_lib_safe_step_(void) {
    mitgcm_error_message[0] = '\0';
    mitgcm_in_protected_call = 1;

    if (setjmp(mitgcm_jmp_buf) == 0) {
        mitgcm_lib_step_();
        mitgcm_in_protected_call = 0;
        return 0;
    } else {
        mitgcm_in_protected_call = 0;
        return 1;
    }
}

/* ================================================================
 * Error message retrieval (Fortran-callable, also callable from C)
 * ================================================================ */

void mitgcm_lib_get_error_msg_(char *buf, int *buflen) {
    int n = (int)strlen(mitgcm_error_message);
    if (n > *buflen) n = *buflen;
    memcpy(buf, mitgcm_error_message, n);
    /* Pad with spaces (Fortran string convention) */
    for (int i = n; i < *buflen; i++) buf[i] = ' ';
    *buflen = n;
}
