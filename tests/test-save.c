/*
 * test-save.c: test various aspects of saving
 *
 * Copyright (C) 2009 Red Hat Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *
 * Author: David Lutterkort <lutter@redhat.com>
 */

#include <config.h>
#include "augeas.h"
#include "internal.h"
#include "cutest.h"

#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>

const char *abs_top_srcdir;
const char *abs_top_builddir;
char *root = NULL, *src_root = NULL;
struct augeas *aug = NULL;

#define die(msg)                                                    \
    do {                                                            \
        fprintf(stderr, "%s:%d: Fatal error: %s\n", __FILE__, __LINE__, msg); \
        exit(EXIT_FAILURE);                                         \
    } while(0)

static void setup(CuTest *tc) {
    char *lensdir;

    if (asprintf(&root, "%s/build/test-save/%s",
                 abs_top_builddir, tc->name) < 0) {
        CuFail(tc, "failed to set root");
    }

    if (asprintf(&lensdir, "%s/lenses", abs_top_srcdir) < 0)
        CuFail(tc, "asprintf lensdir failed");

    run(tc, "test -d %s && chmod -R u+w %s || :", root, root);
    run(tc, "rm -rf %s", root);
    run(tc, "mkdir -p %s", root);
    run(tc, "cp -pr %s/* %s", src_root, root);
    run(tc, "chmod -R u+w %s", root);

    aug = aug_init(root, lensdir, AUG_NO_STDINC);
    CuAssertPtrNotNull(tc, aug);
}

static void teardown(ATTRIBUTE_UNUSED CuTest *tc) {
    aug_close(aug);
    aug = NULL;
    free(root);
    root = NULL;
}

static void testSaveNewFile(CuTest *tc) {
    int r;

    r = aug_match(aug, "/augeas/files/etc/yum.repos.d/new.repo/path", NULL);
    CuAssertIntEquals(tc, 0, r);

    r = aug_set(aug, "/files/etc/yum.repos.d/new.repo/newrepo/baseurl",
                "http://foo.com/");
    CuAssertIntEquals(tc, 0, r);

    r = aug_save(aug);
    CuAssertIntEquals(tc, 0, r);

    r = aug_match(aug, "/augeas/files/etc/yum.repos.d/new.repo/path", NULL);
    CuAssertIntEquals(tc, 1, r);
}

static void testNonExistentLens(CuTest *tc) {
    int r;

    r = aug_rm(aug, "/augeas/load/*");
    CuAssertTrue(tc, r >= 0);

    r = aug_set(aug, "/augeas/load/Fake/lens", "Fake.lns");
    CuAssertIntEquals(tc, 0, r);
    r = aug_set(aug, "/augeas/load/Fake/incl", "/fake");
    CuAssertIntEquals(tc, 0, r);
    r = aug_set(aug, "/files/fake/entry", "value");
    CuAssertIntEquals(tc, 0, r);

    r = aug_save(aug);
    CuAssertIntEquals(tc, -1, r);
    r = aug_error(aug);
    CuAssertIntEquals(tc, AUG_ENOLENS, r);
}

static void testMultipleXfm(CuTest *tc) {
    int r;

    r = aug_set(aug, "/augeas/load/Yum2/lens", "Yum.lns");
    CuAssertIntEquals(tc, 0, r);
    r = aug_set(aug, "/augeas/load/Yum2/incl", "/etc/yum.repos.d/*");
    CuAssertIntEquals(tc, 0, r);

    r = aug_set(aug, "/files/etc/yum.repos.d/fedora.repo/fedora/enabled", "0");
    CuAssertIntEquals(tc, 0, r);

    r = aug_save(aug);
    CuAssertIntEquals(tc, -1, r);
    r = aug_error(aug);
    CuAssertIntEquals(tc, AUG_EMXFM, r);
}

static void testMtime(CuTest *tc) {
    const char *s, *mtime2;
    char *mtime1;
    int r;

    r = aug_set(aug, "/files/etc/hosts/1/alias[last() + 1]", "new");
    CuAssertIntEquals(tc, 0, r);

    r = aug_get(aug, "/augeas/files/etc/hosts/mtime", &s);
    CuAssertIntEquals(tc, 1, r);
    mtime1 = strdup(s);
    CuAssertPtrNotNull(tc, mtime1);


    r = aug_save(aug);
    CuAssertIntEquals(tc, 0, r);

    r = aug_get(aug, "/augeas/files/etc/hosts/mtime", &mtime2);
    CuAssertIntEquals(tc, 1, r);

    CuAssertStrNotEqual(tc, mtime1, mtime2);
    CuAssertStrNotEqual(tc, "0", mtime2);
}

/* Test that handling of 'strange' characters in path names works as
 * expected. In particular, that paths with characters that have special
 * meaning in path expressions are escaped properly.
 *
 * This test isn't all that specific to save, but since these tests set up
 * a copy of tests/root/ that is modifiable, it was convenient to put this
 * test here.
 */
static void testPathEscaping(CuTest *tc) {
    /* Path expression with characters escaped */
    static const char *const weird =
        "/files/etc/sysconfig/network-scripts/ifcfg-weird\\ \\[\\!\\]\\ \\(used\\ to\\ fail\\)";
    /* Path without any escaping */
    static const char *const weird_no_escape =
        "/files/etc/sysconfig/network-scripts/ifcfg-weird [!] (used to fail)";

    char *fname = NULL, *s = NULL;
    const char *v;
    int r;

    /* Construct the file name in the file system and check the file is there */
    r = asprintf(&fname, "%s%s", root, weird_no_escape + strlen("/files"));
    CuAssertPositive(tc, r);

    r = access(fname, R_OK);
    CuAssertIntEquals(tc, 0, r);

    /* Make sure weird is in the tree */
    r = aug_match(aug, weird, NULL);
    CuAssertIntEquals(tc, 1, r);

    /* Make sure we can get to the metadata about weird */
    r = asprintf(&s, "/augeas%s/path", weird);
    CuAssertPositive(tc, r);

    r = aug_get(aug, s, &v);
    CuAssertIntEquals(tc, 1, r);
    CuAssertStrEquals(tc, weird_no_escape, v);

    /* Delete it from the tree and save it; make sure it gets removed
       from the file system */
    r = aug_rm(aug, weird);
    CuAssertPositive(tc, r);

    r = aug_save(aug);
    CuAssertRetSuccess(tc, r);

    r = access(fname, R_OK);
    CuAssertIntEquals(tc, -1, r);
    CuAssertIntEquals(tc, ENOENT, errno);
}

int main(void) {
    char *output = NULL;
    CuSuite* suite = CuSuiteNew();

    abs_top_srcdir = getenv("abs_top_srcdir");
    if (abs_top_srcdir == NULL)
        die("env var abs_top_srcdir must be set");

    abs_top_builddir = getenv("abs_top_builddir");
    if (abs_top_builddir == NULL)
        die("env var abs_top_builddir must be set");

    if (asprintf(&src_root, "%s/tests/root", abs_top_srcdir) < 0) {
        die("failed to set src_root");
    }

    CuSuiteSetup(suite, setup, teardown);

    SUITE_ADD_TEST(suite, testSaveNewFile);
    SUITE_ADD_TEST(suite, testNonExistentLens);
    SUITE_ADD_TEST(suite, testMultipleXfm);
    SUITE_ADD_TEST(suite, testMtime);
    SUITE_ADD_TEST(suite, testPathEscaping);

    CuSuiteRun(suite);
    CuSuiteSummary(suite, &output);
    CuSuiteDetails(suite, &output);
    printf("%s\n", output);
    free(output);
    return suite->failCount;
}

/*
 * Local variables:
 *  indent-tabs-mode: nil
 *  c-indent-level: 4
 *  c-basic-offset: 4
 *  tab-width: 4
 * End:
 */
