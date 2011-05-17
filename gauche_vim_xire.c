/*
 * gauche_vim_xire.c
 */

#include "gauche_vim_xire.h"

/*
 * The following function is a dummy one; replace it for
 * your C function definitions.
 */

ScmObj test_gauche_vim_xire(void)
{
    return SCM_MAKE_STR("gauche_vim_xire is working");
}

/*
 * Module initialization function.
 */
extern void Scm_Init_gauche_vim_xirelib(ScmModule*);

void Scm_Init_gauche_vim_xire(void)
{
    ScmModule *mod;

    /* Register this DSO to Gauche */
    SCM_INIT_EXTENSION(gauche_vim_xire);

    /* Create the module if it doesn't exist yet. */
    mod = SCM_MODULE(SCM_FIND_MODULE("vim.xire", TRUE));

    /* Register stub-generated procedures */
    Scm_Init_gauche_vim_xirelib(mod);
}
