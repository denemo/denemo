#ifndef _HOOKS_H
#define _HOOKS_H 1

#include <libguile.h>

typedef enum denemo_c_hook_type {
  DENEMO_HOOK_NORMAL,
  DENEMO_HOOK_OR,
  DENEMO__HOOK_AND
} denemo_c_hook_type;

typedef struct denemo_hook {
    int arity;
    SCM name;
    SCM funcs;
} denemo_hook;


#endif /* _HOOKS_H */

