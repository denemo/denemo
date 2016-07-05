#include <glib.h>
#include <glib/gstdio.h>
#include <unistd.h>
#include <config.h>
#include "common.h"

static gchar* data_dir = NULL;
static gchar* temp_dir = NULL;
static gchar* example_dir = NULL;

/*******************************************************************************
 * SETUP AND TEARDOWN
 ******************************************************************************/

static void
setup(gpointer fixture, gconstpointer data)
{
  if(!g_file_test(temp_dir, G_FILE_TEST_EXISTS)){
    if(g_mkdir(temp_dir, 0777) < 0)
      g_warning("Could not create %s", temp_dir);
  }

  else{
    GDir* dir = g_dir_open(temp_dir, 0, NULL);
    const gchar* filename = NULL;
    while (filename = g_dir_read_name(dir))
      g_remove (g_build_filename(temp_dir, filename, NULL));
  }
}

static void
teardown(gpointer fixture, gconstpointer data)
{
  delete_if_exists(temp_dir);
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
  if (g_test_subprocess ())
    {
      execl(DENEMO, DENEMO, "-n", "-e", "-a", "(d-Quit)", NULL);
      g_warn_if_reached ();
    }
  g_test_trap_subprocess (NULL, 0, 0);
  g_test_trap_assert_passed ();
}

/** test_invalid_scheme
 * Tests the --fatal-scheme-errors program argument.
 */
static void
test_invalid_scheme(gpointer fixture, gconstpointer data)
{
  if (g_test_subprocess ())
    {
      execl(DENEMO, DENEMO, "-n", "--fatal-scheme-errors", "-a", "(d-InvalidSchemeFunction)(d-Quit)", NULL);
      g_warn_if_reached ();
    }
  g_test_trap_subprocess (NULL, 0, 0);
  g_test_trap_assert_failed ();
}

/** test_scheme_log
 * Tests (d-LogError) scheme function
 */
static void
test_scheme_log(gpointer fixture, gconstpointer data)
{
  if (g_test_subprocess ())
    {
      execl(DENEMO, DENEMO, "-n", "-e", "--verbose", "-a",
            "(d-Debug \"This is debug\")"
            "(d-Info \"This is info\")"
            "(d-Message \"This is message\")"
            "(d-Warning \"This is warning\")"
            "(d-Critical \"This is critical\")"
            "(d-Quit)",
            NULL);
      g_warn_if_reached ();
    }
  g_test_trap_subprocess (NULL, 0, 0);
  g_test_trap_assert_passed ();
}

/** test_scheme_log_error
 * Tests (d-LogError) scheme function
 */
static void
test_scheme_log_error(gpointer fixture, gconstpointer data)
{
  if (g_test_subprocess ())
    {
      execl(DENEMO, DENEMO, "-n", "--fatal-scheme-errors", "-a", "(d-Error \"This error is fatal\")(d-Quit)", NULL);
      g_warn_if_reached ();
    }
  g_test_trap_subprocess (NULL, 0, 0);
  g_test_trap_assert_failed ();
}

/** test_thumbnailer
 * Tries to create a thumbnail from a file and check that its exists
 */
static void
test_thumbnailer(gpointer fixture, gconstpointer data)
{
  gchar* thumbnail = g_build_filename(temp_dir, "thumbnail.png", NULL);
  gchar* scheme = g_strdup_printf( "(d-CreateThumbnail #f \"%s\")(d-Exit)", thumbnail);
  gchar* input = g_build_filename(data_dir, "denemo", "blank.denemo", NULL);

  g_test_print("Running scheme: %s %s\n", scheme, input);
  if (g_test_subprocess ())
    {
      execl(DENEMO, DENEMO, "-n", "-e", "-V", "-a", scheme, input, NULL);
      g_warn_if_reached ();
    }

  g_test_trap_subprocess (NULL, 0, 0);
  g_test_trap_assert_passed ();
  g_assert(g_file_test(thumbnail, G_FILE_TEST_EXISTS));
  g_assert(g_remove(thumbnail) >= 0);
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

  if(!data_dir)
    data_dir = g_build_filename(PACKAGE_SOURCE_DIR, "tests", FIXTURES_DIR, NULL);

  if(!temp_dir)
    temp_dir = g_build_filename(g_get_current_dir (), TEMP_DIR, NULL);

  if(!example_dir)
    example_dir = g_build_filename(PACKAGE_SOURCE_DIR, EXAMPLE_DIR, NULL);

  g_test_add ("/unit/run-and-quit", void, NULL, setup, test_run_and_quit, teardown);

  //g_test_add ("/unit/invalid-scheme", void, NULL, setup, test_invalid_scheme, teardown);
  g_test_add ("/unit/scheme-log", void, NULL, setup, test_scheme_log, teardown);
  g_test_add ("/unit/scheme-log-error", void, NULL, setup, test_scheme_log_error, teardown);
  g_test_add ("/unit/thumbnailer", void, NULL, setup, test_thumbnailer, teardown);

  return g_test_run ();
}
