#include <glib.h>
#include <glib/gstdio.h>
#include <unistd.h>
#include <config.h>
#include "common.h"

/* Integration tests are those which launch the program in different
 * environments, and with different parameters.
 */

static gchar* fixtures_dir = NULL;
static gchar* temp_dir = NULL;
static gchar* example_dir = NULL;
static gchar* ref_dir = NULL;

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
    if (g_test_subprocess ())
      {
        //TODO: Dynamically find diff path if possible
        execl("/usr/bin/diff", "/usr/bin/diff", fileA, fileB, NULL);
        g_warn_if_reached ();
      }
    g_test_trap_subprocess (NULL, 0, 0);
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

static gboolean
mkdir_if_not_exists(gchar* dir){
  if(!g_file_test(dir, G_FILE_TEST_EXISTS)){
    if(g_mkdir(dir, 0777) < 0)
      g_warning("Could not create %s", temp_dir);
    return FALSE;
  }
  return TRUE;
}

/*******************************************************************************
 * SETUP AND TEARDOWN
 ******************************************************************************/

static void
setup(gpointer fixture, gconstpointer data)
{
  if(!mkdir_if_not_exists(temp_dir)){
    GDir* dir = g_dir_open(temp_dir, 0, NULL);
    gchar* filename = NULL;
    while (filename = g_dir_read_name(dir))
      g_remove (g_build_filename(temp_dir, filename, NULL));
  }

  gchar* dnm_fixtures = g_build_filename(temp_dir, "denemo", NULL);
  mkdir_if_not_exists(dnm_fixtures);
  g_free(dnm_fixtures);

  gchar* mxml_fixtures = g_build_filename(temp_dir, "mxml", NULL);
  mkdir_if_not_exists(mxml_fixtures);
  g_free(mxml_fixtures);

  gchar* scm_fixtures = g_build_filename(temp_dir, "scm", NULL);
  mkdir_if_not_exists(scm_fixtures);
  g_free(scm_fixtures);
}

static void
teardown(gpointer fixture, gconstpointer data)
{
  delete_if_exists(temp_dir);
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
  gchar* input = g_build_filename(fixtures_dir, "denemo", "blank.denemo", NULL);
  g_test_print("Opening %s\n", input);
  if (g_test_subprocess ())
    {
      execl(DENEMO, DENEMO, "-n", "-e", input, NULL);
      g_warn_if_reached ();
    }
  g_test_trap_subprocess (NULL, 0, 0);
  g_test_trap_assert_passed ();
}

/** test_open_save_blank_file
 * Opens a blank file, saves it, tries to reopen it and quits.
 * the input one.
 */
static void
test_open_save_blank_file(gpointer fixture, gconstpointer data)
{
  const gchar* output = g_build_filename(temp_dir, "denemo", "blank.denemo", NULL);
  const gchar* input  = g_build_filename(fixtures_dir, "denemo", "blank.denemo", NULL);
  gchar* input_contents = NULL;
  gchar* output_contents = NULL;

  if (g_test_subprocess ())
    {
      gchar* scheme = g_strdup_printf("(d-SaveAs \"%s\")(d-Quit)", output);
      execl(DENEMO, DENEMO, "-n", "-e", "-a", scheme, input, NULL);
      g_warn_if_reached ();
    }
  g_test_trap_subprocess (NULL, 0, 0);
  g_test_trap_assert_passed ();

  g_assert(g_file_test(output, G_FILE_TEST_EXISTS));

  if (g_test_subprocess ())
    {
      execl(DENEMO, DENEMO, "-n", "-e", output, NULL);
      g_warn_if_reached ();
    }
  g_test_trap_subprocess (NULL, 0, 0);
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
  gchar* filename = g_path_get_basename(input);
  gchar* base_name = get_basename(filename);
  gchar* extension = g_strrstr (input, ".") + 1;

  const gchar* output_filename = g_strconcat(base_name, ".denemo", NULL);
  const gchar* output = g_build_filename(temp_dir, extension, output_filename, NULL);
  const gchar* reference = g_build_filename(ref_dir, extension, output_filename, NULL);

  g_test_print("Opening %s\n", input);
  if (g_test_subprocess ())
    {
      gchar* scheme = g_strdup_printf("(d-SaveAs \"%s\")(d-Quit)", output);
      if(g_strcmp0("scm", extension) == 0)
        execl(DENEMO, DENEMO, "-n", "-e", "-i", input, "-a", scheme, NULL);
      else
        execl(DENEMO, DENEMO, "-n", "-e", "-a", scheme, input, NULL);
      g_warn_if_reached ();
    }
  g_test_trap_subprocess (NULL, 0, 0);
  g_test_trap_assert_passed ();


  g_test_print("Finding and reopening %s\n", output);
  g_assert(g_file_test(output, G_FILE_TEST_EXISTS));
  if (g_test_subprocess ())
  {
    execl(DENEMO, DENEMO, "-n", "-e", output, NULL);
    g_warn_if_reached ();
  }
  g_test_trap_subprocess (NULL, 0, 0);
  g_test_trap_assert_passed ();


  // Comparision
  if(g_file_test(reference, G_FILE_TEST_EXISTS)){
    g_test_print("Comparing %s with the reference %s\n", output, reference);
    g_assert(compare_denemo_files(output, reference));
  }
  else if(g_str_has_suffix (filename, ".denemo")){
    g_test_print("Comparing %s with %s\n", input, output);
    g_assert(compare_denemo_files(input, output));
  }
  else{
    gchar* compare_file = g_strconcat(base_name, ".denemo", NULL);
    if(g_file_test(compare_file, G_FILE_TEST_EXISTS)){
      g_test_print("Comparing %s with %s\n", compare_file, output);
      g_assert(compare_denemo_files(compare_file, output));
    }
  }
  g_remove(output);
  g_free(filename);
}

/*******************************************************************************
 * MAIN
 ******************************************************************************/

static gchar*
parse_dir_and_run_complex_test(gchar* path, const gchar* extension)
{
  GError* error = NULL;
  GDir* dir = g_dir_open(path, 0, &error);
  gchar* filename = NULL;
  while (filename = g_dir_read_name(dir)){
    filename = g_build_filename(path, filename, NULL);
    if(g_file_test(filename, G_FILE_TEST_IS_DIR))
      parse_dir_and_run_complex_test(filename, extension);
  }

  GList* files = find_files_with_ext(path, extension);
  // Ensure a unique test case path if called multiple times.
  // Use the given path and extension and append a counter to the end.
  gchar* test_case_path_fragment = g_strconcat("/integration/open-and-save-complex-file-", path, "-", extension, NULL);
  int test_case_path_counter = 1;
  while(files){
    filename = g_build_filename(path, files->data, NULL);
    gchar* test_case_path = g_strdup_printf("%s-%d", test_case_path_fragment, test_case_path_counter);
    g_test_add (test_case_path, gchar*, filename, setup, test_open_save_complex_file, teardown);
    g_free(test_case_path);
    test_case_path_counter ++;
    files = g_list_next(files);
  }
  g_free(test_case_path_fragment);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  if(!g_file_test(DENEMO, G_FILE_TEST_EXISTS))
    g_error("Denemo has not been compiled successfully");

  if(!fixtures_dir)
    fixtures_dir = g_build_filename(PACKAGE_SOURCE_DIR, "tests", FIXTURES_DIR, NULL);

  if(!temp_dir)
    temp_dir = g_build_filename(g_get_current_dir (), TEMP_DIR, NULL);

  if(!example_dir)
    example_dir = g_build_filename(PACKAGE_SOURCE_DIR, EXAMPLE_DIR, NULL);

  if(!ref_dir)
    ref_dir = g_build_filename(g_get_current_dir (), REFERENCE_DIR, NULL);

  g_test_add ("/integration/open-blank-file", void, NULL, setup, test_open_blank_file, teardown);
  g_test_add ("/integration/open-and-save-blank-file", void, NULL, setup, test_open_save_blank_file, teardown);

  parse_dir_and_run_complex_test(example_dir, ".denemo");
  parse_dir_and_run_complex_test(fixtures_dir, ".denemo");
  // parse_dir_and_run_complex_test(fixtures_dir, ".mxml");
  parse_dir_and_run_complex_test(fixtures_dir, ".scm");

  return g_test_run ();
}
