#include <glib.h>
#include <unistd.h>

/* Integration tests are those which launch the program in different 
 * environments, and with different parameters.
 */

#define DENEMO "../src/denemo"

/** test_run_and_quit:
 * This is the simpliest test. It just launches denemo and quit.
 */
static void
test_run_and_quit (void)
{
  if (g_test_trap_fork (0, 0))
    {
      execl(DENEMO, DENEMO, "-n", "-a", "(d-Quit)", NULL);
    }
  g_test_trap_assert_passed ();
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  if(!g_file_test(DENEMO, G_FILE_TEST_EXISTS))
    g_error("Denemo has not been compiled successfully");
  
  g_test_add_func ("/integration/run-and-quit", test_run_and_quit);

  return g_test_run ();
}