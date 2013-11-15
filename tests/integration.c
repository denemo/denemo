#include <glib.h>
#include <unistd.h>

/* Integration tests are those which launch the program in different 
 * environments, and with different parameters.
 */

#define DENEMO "../src/denemo"
#define DATA_DIR "integration-data"
#define TEMP_DIR "integration-tmp"

/*******************************************************************************
 * SETUP AND TEARDOWN
 ******************************************************************************/

static void
setup(gpointer fixture, gconstpointer data)
{
  g_mkdir(TEMP_DIR, 777);
}

static void
teardown(gpointer fixture, gconstpointer data)
{
  g_remove(TEMP_DIR);
}

/*******************************************************************************
 * TEST FUNCTIONS
 ******************************************************************************/

/** test_run_and_quit:
 * This is the simpliest test. It just launches denemo and quit.
 */
static void
test_run_and_quit (gpointer fixture, gconstpointer data)
{
  if (g_test_trap_fork (0, 0))
    {
      execl(DENEMO, DENEMO, "-n", "-a", "(d-Quit)", NULL);
      g_warn_if_reached ();
    }
  g_test_trap_assert_passed ();
}

/** test_open_blank_file
 * Opens a blank file and quits.
 */
static void
test_open_blank_file(gpointer fixture, gconstpointer data)
{
  if (g_test_trap_fork (0, 0))
    {
      execl(DENEMO, DENEMO, "-n", DATA_DIR "/blank.denemo", NULL);
      g_warn_if_reached ();
    }
  g_test_trap_assert_passed ();
}

/*******************************************************************************
 * MAIN
 ******************************************************************************/

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  if(!g_file_test(DENEMO, G_FILE_TEST_EXISTS))
    g_error("Denemo has not been compiled successfully");

  g_test_add ("/integration/run-and-quit", void, NULL, setup, test_run_and_quit, teardown);
  g_test_add ("/integration/open-blank-file", void, NULL, setup, test_open_blank_file, teardown);

  return g_test_run ();
}