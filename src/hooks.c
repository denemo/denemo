#include "hooks.h"

static scm_t_bits hook_tag;

static SCM
c_make_denemo_hook(SCM name, int arity)
{
    SCM smob;

    struct denemo_hook *hook;

    hook = (struct denemo_hook *) scm_gc_malloc (sizeof (struct denemo_hook), "denemo-hook");
    SCM_NEWSMOB (smob, hook_tag, hook); /* FIXME: image_tag */
    
    hook->arity = arity;
    hook->funcs = SCM_EOL;
    hook->name  = name;
    
    return smob;
}

static SCM
mark_denemo_hook(SCM hook_smob)
{
    struct denemo_hook *hook = (struct denemo_hook *) SCM_SMOB_DATA (hook_smob);
    return hook->funcs;
}


static size_t
free_denemo_hook(SCM hook_smob)
{
    struct denemo_hook *hook = (struct denemo_hook *) SCM_SMOB_DATA (hook_smob);
    //    scm_gc_free(hook->name, "denemo-hook");
    return 0;
}


static int
print_denemo_hook(SCM hook_smob, SCM port, scm_print_state *pstate)
{
    struct denemo_hook *hook = (struct denemo_hook *) SCM_SMOB_DATA (hook_smob);
    
    scm_puts ("#<denemo-hook ", port);
    scm_display (hook->name, port);
    scm_puts(" ", port);
    scm_intprint (hook->arity, 10, port);
    scm_puts (">", port);

    return 1;
}

SCM_DEFINE (denemo_make_hook, "create-hook", 1, 1, 0,
            (SCM name, SCM arity),
            "Create a hook for storing procedure of arity @var{n_args}.\n"
            "@var{n_args} defaults to zero.  The returned value is a hook\n"
            "object to be used with the other hook procedures.")
#define FUNC_NAME denemo_make_hook
{
  unsigned int ar;

  if (SCM_UNBNDP (arity))
    ar = 0;
  else
    ar = scm_to_unsigned_integer (arity, 0, 16);

  return c_make_denemo_hook(name, ar);
}
#undef FUNC_NAME

SCM_DEFINE (denemo_hook_p, "denemo-hook?", 1, 0, 0,
            (SCM x),
            "Return @code{#t} if @var{x} is a denemo-hook, @code{#f} otherwise.")
#define FUNC_NAME denemo_hook_p
{
    return scm_from_bool(SCM_SMOB_PREDICATE (hook_tag, x));
}
#undef FUNC_NAME

SCM_DEFINE (denemo_hook_name, "denemo-hook-name", 1, 0, 0,
            (SCM hook),
            "Return the name of the denemo-hook @code{hook}.")
#define FUNC_NAME denemo_hook_name
{
    struct denemo_hook *h = (struct denemo_hook *) SCM_SMOB_DATA (hook);
    return h->name;
}
#undef FUNC_NAME


SCM_DEFINE (denemo_add_hook_x, "denemo-add-hook!", 2, 0, 0,
            (SCM hook, SCM proc),
            "Add the procedure @var{proc} to the hook @var{hook}.\n"
            "The return value of this procedure is not specified.")
#define FUNC_NAME denemo_add_hook_x
{
  SCM arity, rest;
  struct denemo_hook *hk;
  int hk_arity;


 
  /* FIXME: Validate smob type */
  hk = (struct denemo_hook *) SCM_SMOB_DATA (hook);
 
  SCM_ASSERT (scm_is_true (arity = scm_i_procedure_arity (proc)),
              proc, SCM_ARG2, FUNC_NAME);
  
  hk_arity = hk->arity;

  if (scm_to_int (SCM_CAR (arity)) > hk_arity
      || (scm_is_false (SCM_CADDR (arity))
          && (scm_to_int (SCM_CAR (arity)) + scm_to_int (SCM_CADR (arity))
              < hk_arity)))
      scm_wrong_type_arg (FUNC_NAME, 2, proc);
  
  //  rest = scm_delq_x (proc, hk->funcs);

  hk->funcs = scm_cons(proc, hk->funcs);
  
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (denemo_reset_hook_x, "denemo-reset-hook!", 1, 0, 0,
            (SCM hook),
            "Remove all procedures from the hook @var{hook}.  The return\n"
            "value of this procedure is not specified.")
#define FUNC_NAME denemo_reset_hook_x
{
    /* FIXME: Validate smob type */
    struct denemo_hook *hk = (struct denemo_hook *) SCM_SMOB_DATA (hook);
    hk->funcs = SCM_EOL;
    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (denemo_hook_to_list, "denemo-hook->list", 1, 0, 0,
            (SCM hook),
            "Convert the procedure list of @var{hook} to a list.")
#define FUNC_NAME denemo_hook_to_list
{
    /* FIXME: Validate smob type */
    struct denemo_hook *hk = (struct denemo_hook *) SCM_SMOB_DATA (hook);
    return scm_list_copy (hk->funcs);
}
#undef FUNC_NAME

/* FIXME: better docstring */
SCM_DEFINE (denemo_run_hook, "denemo-run-hook", 1, 0, 1,
            (SCM hook, SCM args),
            "Apply all procedures from the hook @var{hook} to the arguments\n"
            "@var{args}.  The order of the procedure application is first to\n"
            "last.")
#define FUNC_NAME denemo_run_hook
{
    /* FIXME: Validate smob type */
    struct denemo_hook *hk;
    int    arity;
    SCM procs, result;
    
    hk = (struct denemo_hook *) SCM_SMOB_DATA (hook);
    
    if (scm_ilength (args) != hk->arity)
        SCM_MISC_ERROR ("Hook ~S requires ~A arguments",
                        scm_list_2 (hook, scm_from_int(arity)));
    
    procs = hk->funcs;
    while (!scm_is_null(procs)) {
        result = scm_apply_0 (SCM_CAR (procs), args);
        if (scm_is_true(result)) return result;
        procs = SCM_CDR (procs);
    }
    return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


void
denemo_init_hooks ()
{
    hook_tag = scm_make_smob_type("denemo-hook", 0);
    scm_set_smob_mark(hook_tag, mark_denemo_hook);
    scm_set_smob_free(hook_tag, free_denemo_hook);
    scm_set_smob_print(hook_tag, print_denemo_hook);
#include "hooks.x"
}




