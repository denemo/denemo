#include <glib.h>
#include <unistd.h>
#include <config.h>

/* Integration tests are those which launch the program in different 
 * environments, and with different parameters.
 */

#define DENEMO "../src/denemo"
#define EXAMPLE_DIR "examples"
#define DATA_DIR "integration-data"
#define TEMP_DIR "integration-tmp"

static gchar* data_dir = NULL;
static gchar* temp_dir = NULL;
static gchar* example_dir = NULL;

/*******************************************************************************
 * Utils
 ******************************************************************************/

static GList*
find_files_with_ext(gchar* dirname, gchar* ext){
  GList* list = NULL;
  GError* error = NULL;
  const gchar* filename = NULL;
  GDir* dir = g_dir_open(dirname, 0, &error);

  while (filename = g_dir_read_name(dir))
    if(g_str_has_suffix (filename, ext))
      list = g_list_append(list, filename);
  return list;
}

static gboolean
compare_denemo_files(gchar* fileA, gchar* fileB){
  gchar* contentA = NULL;
  gchar* contentB = NULL;

  g_file_get_contents(fileA, &contentA, NULL, NULL);
  g_file_get_contents(fileB, &contentB, NULL, NULL);

  gboolean equals = (g_strcmp0(contentA, contentB) == 0);

  if(!equals){
    if (g_test_trap_fork (0, 0))
      {
        //TODO: Dynamically find diff path if possible
        execl("/usr/bin/diff", "/usr/bin/diff", fileA, fileB, NULL);
        g_warn_if_reached ();
      }
    g_test_trap_assert_passed ();
  }
  
  return equals;
}

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
    gchar* filename = NULL;
    while (filename = g_dir_read_name(dir))
      g_remove (g_build_filename(temp_dir, filename, NULL));
  }
}

static void
teardown(gpointer fixture, gconstpointer data)
{  
  if(g_file_test(temp_dir, G_FILE_TEST_EXISTS)){
    if(g_remove(temp_dir) < 0)
      g_warning("Could not remove %s", temp_dir);
  }
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
      execl(DENEMO, DENEMO, "-n", "-e", "-a", "(d-Quit)", NULL);
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
  gchar* input = g_build_filename(data_dir, "blank.denemo", NULL);
  if (g_test_trap_fork (0, 0))
    {
      execl(DENEMO, DENEMO, "-n", "-e", input, NULL);
      g_warn_if_reached ();
    }
  g_test_trap_assert_passed ();
}

/** test_open_save_blank_file
 * Opens a blank file, saves it, tries to reopen it and quits.
 * the input one.
 */
static void
test_open_save_blank_file(gpointer fixture, gconstpointer data)
{
  const gchar* output = g_build_filename(temp_dir, "blank.denemo", NULL);
  const gchar* input  = g_build_filename(data_dir, "blank.denemo", NULL);
  gchar* input_contents = NULL;
  gchar* output_contents = NULL;

  if (g_test_trap_fork (0, 0))
    {
      gchar* scheme = g_strdup_printf("(d-SaveAs \"%s\")(d-Quit)", output);
      execl(DENEMO, DENEMO, "-n", "-e", "-a", scheme, input, NULL);
      g_warn_if_reached ();
    }
  g_test_trap_assert_passed ();

  g_assert(g_file_test(output, G_FILE_TEST_EXISTS));

  if (g_test_trap_fork (0, 0))
    {
      execl(DENEMO, DENEMO, "-n", "-e", output, NULL);
      g_warn_if_reached ();
    }
  g_test_trap_assert_passed ();
  
  g_assert(compare_denemo_files(input, output));
  g_remove(output);
}

/** test_open_save_complex_file
 * Opens a complex file, saves, tries to reopen it and quits.
 * the input one.
 */
static void
test_open_save_complex_file(gpointer fixture, gconstpointer data)
{
  const gchar* input = (const gchar*) data;
  gchar* filename = basename(input);
  const gchar* output_filename = g_strconcat(filename, ".denemo", NULL);
  const gchar* output = g_build_filename(temp_dir, output_filename, NULL);
  g_print("Opening %s\nSaving at %s\n", input, output);
  gchar* input_contents = NULL;
  gchar* output_contents = NULL;

  if (g_test_trap_fork (0, 0))
    {
      gchar* scheme = g_strdup_printf("(d-SaveAs \"%s\")(d-Quit)", output);
      execl(DENEMO, DENEMO, "-n", "-e", "-a", scheme, input, NULL);
      g_warn_if_reached ();
    }
  g_test_trap_assert_passed ();

  g_print("Finding and reopening %s\n", output);
  g_assert(g_file_test(output, G_FILE_TEST_EXISTS));

  if (g_test_trap_fork (0, 0))
  {
    execl(DENEMO, DENEMO, "-n", "-e", output, NULL);
    g_warn_if_reached ();
  }
  g_test_trap_assert_passed ();

  // Comparision
  if(g_str_has_suffix (filename, ".denemo"))
    g_assert(compare_denemo_files(input, output));
  else{
    gchar* ext = g_strrstr (input, ".");
    guint length = ext - input;
    gchar* basename = g_strndup(input, length);
    gchar* compare_file = g_strconcat(basename, ".denemo", NULL);
    if(g_file_test(compare_file, G_FILE_TEST_EXISTS))
      g_assert(compare_denemo_files(compare_file, output));
  }
  g_remove(output);
}

/** test_invalid_scheme
 * Tests the --fatal-scheme-errors program argument.
 */
static void
test_invalid_scheme(gpointer fixture, gconstpointer data)
{
  if (g_test_trap_fork (0, 0))
    {
      execl(DENEMO, DENEMO, "-n", "--fatal-scheme-errors", "-a", "(d-InvalidSchemeFunction)(d-Quit)", NULL);
      g_warn_if_reached ();
    }
  g_test_trap_assert_failed ();
}

/** test_scheme_log
 * Tests (d-LogError) scheme function
 */
static void
test_scheme_log(gpointer fixture, gconstpointer data)
{
  if (g_test_trap_fork (0, 0))
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
  g_test_trap_assert_passed ();
}

/** test_scheme_log_error
 * Tests (d-LogError) scheme function
 */
static void
test_scheme_log_error(gpointer fixture, gconstpointer data)
{
  if (g_test_trap_fork (0, 0))
    {
      execl(DENEMO, DENEMO, "-n", "--fatal-scheme-errors", "-a", "(d-Error \"This error is fatal\")(d-Quit)", NULL);
      g_warn_if_reached ();
    }
  g_test_trap_assert_failed ();
}

/** test_thumbnailer
 * Tries to create a thumbnail from a file and check that its exists
 */
static void
test_thumbnailer(gpointer fixture, gconstpointer data)
{
  gchar* thumbnail = g_build_filename(temp_dir, "thumbnail.png", NULL);
  gchar* scheme = g_strdup_printf( "(d-CreateThumbnail #f \"%s\")(d-Exit)", thumbnail, temp_dir);
  gchar* input = g_build_filename(data_dir, "blank.denemo", NULL);
  
  g_printf("Running scheme: %s %s\n", scheme, input);
  if (g_test_trap_fork (0, 0))
    {
      execl(DENEMO, DENEMO, "-n", "-e", "-V", "-a", scheme, input, NULL);
      g_warn_if_reached ();
    }

  g_test_trap_assert_passed ();
  g_assert(g_file_test(thumbnail, G_FILE_TEST_EXISTS));
  g_assert(g_remove(thumbnail) >= 0);
}

/** test_regression_check
 * Opens a user written scm file to check some features that have been failing
 * by the past.
 */
static void
test_regression_check(gpointer fixture, gconstpointer data)
{
  const gchar* scheme_file = (const gchar*) data;
  g_print("Opening %s\n", scheme_file);

  if (g_test_trap_fork (0, 0))
    {
      execl(DENEMO, DENEMO, "-n", "-e", "-i", scheme_file, NULL);
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

  if(!data_dir)
    data_dir = g_build_filename(PACKAGE_SOURCE_DIR, "tests", DATA_DIR, NULL);

  if(!temp_dir)
    temp_dir = g_build_filename(g_get_current_dir (), TEMP_DIR, NULL);

  if(!example_dir)
    example_dir = g_build_filename(PACKAGE_SOURCE_DIR, EXAMPLE_DIR, NULL);
  
  g_test_add ("/integration/run-and-quit", void, NULL, setup, test_run_and_quit, teardown);
  g_test_add ("/integration/open-blank-file", void, NULL, setup, test_open_blank_file, teardown);
  g_test_add ("/integration/open-and-save-blank-file", void, NULL, setup, test_open_save_blank_file, teardown);
  //g_test_add ("/integration/invalid-scheme", void, NULL, setup, test_invalid_scheme, teardown);
  g_test_add ("/integration/scheme-log", void, NULL, setup, test_scheme_log, teardown);
  g_test_add ("/integration/scheme-log-error", void, NULL, setup, test_scheme_log_error, teardown);
  g_test_add ("/integration/thumbnailer", void, NULL, setup, test_thumbnailer, teardown);

  // Parses example dir for .denemo files
  GList* files = find_files_with_ext(example_dir, ".denemo");
  while(files){
    gchar* filename = g_build_filename(example_dir, files->data, NULL);
    g_test_add ("/integration/open-and-save-complex-file", gchar*, filename, setup, test_open_save_complex_file, teardown);
    files = g_list_next(files);
  }

  // Parses integration-data dir for .mxml files
  files = find_files_with_ext(data_dir, ".mxml");
  while(files){
    gchar* filename = g_build_filename(data_dir, files->data, NULL);
    g_test_add ("/integration/open-and-save-complex-file", gchar*, filename, setup, test_open_save_complex_file, teardown);
    files = g_list_next(files);
  }

  // Parses integration-data dir for .scm files
  files = find_files_with_ext(data_dir, ".scm");
  while(files){
    gchar* filename = g_build_filename(data_dir, files->data, NULL);
    g_test_add ("/integration/regression-check", gchar*, filename, setup, test_regression_check, teardown);
    files = g_list_next(files);
  }

  return g_test_run ();
}
