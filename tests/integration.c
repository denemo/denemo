#include <glib.h>
#include <unistd.h>
#include <config.h>
#include "common.h"

/* Integration tests are those which launch the program in different 
 * environments, and with different parameters.
 */

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

static gchar*
get_basename(gchar* input){
  gchar* ext = g_strrstr (input, ".");
  if(NULL == ext)
    return g_strdup(input);
  guint length = ext - input;
  gchar* basename = g_strndup(input, length);
  return basename;
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
  g_test_print("Opening %s\nSaving at %s\n", input, output);

  if (g_test_trap_fork (0, 0))
    {
      gchar* scheme = g_strdup_printf("(d-SaveAs \"%s\")(d-Quit)", output);
      execl(DENEMO, DENEMO, "-n", "-e", "-a", scheme, input, NULL);
      g_warn_if_reached ();
    }
  g_test_trap_assert_passed ();

  g_test_print("Finding and reopening %s\n", output);
  g_assert(g_file_test(output, G_FILE_TEST_EXISTS));

  if (g_test_trap_fork (0, 0))
  {
    execl(DENEMO, DENEMO, "-n", "-e", output, NULL);
    g_warn_if_reached ();
  }
  g_test_trap_assert_passed ();

  // Comparision
  if(g_str_has_suffix (filename, ".denemo")){
    g_test_print("Comparing %s with %s\n", input, output);
    g_assert(compare_denemo_files(input, output));
  }
  else{
    gchar* base_name = get_basename(input);
    gchar* compare_file = g_strconcat(base_name, ".denemo", NULL);
    if(g_file_test(compare_file, G_FILE_TEST_EXISTS)){
      g_test_print("Comparing %s with %s\n", compare_file, output);
      g_assert(compare_denemo_files(compare_file, output));
    }
  }
  g_remove(output);
}

/** test_regression_check
 * Opens a user written scm file to check some features that have been failing
 * by the past.
 */
static void
test_regression_check(gpointer fixture, gconstpointer data)
{
  const gchar* scheme_file = (const gchar*) data;
  gchar* filename = basename(scheme_file);
  const gchar* output_filename = g_strconcat(filename, ".denemo", NULL);
  const gchar* output = g_build_filename(temp_dir, output_filename, NULL);
  g_test_print("Opening %s\n", scheme_file);

  if (g_test_trap_fork (0, 0))
    {
      gchar* scheme = g_strdup_printf("(d-SaveAs \"%s\")(d-Quit)", output);
      execl(DENEMO, DENEMO, "-n", "-e", "-i", scheme_file, "-a", scheme, NULL);
      g_warn_if_reached ();
    }
  g_test_trap_assert_passed ();
  
  g_assert(g_file_test(output, G_FILE_TEST_EXISTS));

  gchar* base_name = get_basename(scheme_file);
  gchar* compare_file = g_strconcat(base_name, ".denemo", NULL);
  if(g_file_test(compare_file, G_FILE_TEST_EXISTS))
    g_assert(compare_denemo_files(compare_file, output));

  g_remove(output);
}

/*******************************************************************************
 * MAIN
 ******************************************************************************/

static gchar*
parse_dir_and_run_complex_test(gchar* dir, const gchar* extension)
{
  GList* files = find_files_with_ext(dir, extension);
  while(files){
    gchar* filename = g_build_filename(dir, files->data, NULL);
    g_test_add ("/integration/open-and-save-complex-file", gchar*, filename, setup, test_open_save_complex_file, teardown);
    files = g_list_next(files);
  }
}

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
  
  g_test_add ("/integration/open-blank-file", void, NULL, setup, test_open_blank_file, teardown);
  g_test_add ("/integration/open-and-save-blank-file", void, NULL, setup, test_open_save_blank_file, teardown);

  parse_dir_and_run_complex_test(example_dir, ".denemo");
  parse_dir_and_run_complex_test(data_dir, ".denemo");
  parse_dir_and_run_complex_test(data_dir, ".mxml");
  parse_dir_and_run_complex_test(data_dir, ".scm");

  return g_test_run ();
}
