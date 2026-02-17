/*  eval_embedded_extract.c -- Extract embedded scheme files to temp dir  */

#include "eval_embedded_scm.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#include <direct.h>
#define PATH_SEP '\\'
#else
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>
#define PATH_SEP '/'
#endif

/* Create directory and all parent directories */
static int mkdirs(const char *path) {
    char tmp[1024];
    size_t len = strlen(path);
    if (len >= sizeof(tmp)) return -1;
    memcpy(tmp, path, len + 1);

    for (size_t i = 1; i < len; i++) {
        if (tmp[i] == '/' || tmp[i] == '\\') {
            tmp[i] = '\0';
#ifdef _WIN32
            _mkdir(tmp);
#else
            mkdir(tmp, 0755);
#endif
            tmp[i] = PATH_SEP;
        }
    }
#ifdef _WIN32
    return _mkdir(path);
#else
    return mkdir(path, 0755);
#endif
}

char *embedded_scm_extract(void) {
    char tmpdir[1024];

#ifdef _WIN32
    char tmp_base[512];
    DWORD len = GetTempPathA(sizeof(tmp_base), tmp_base);
    if (len == 0 || len >= sizeof(tmp_base)) return NULL;
    snprintf(tmpdir, sizeof(tmpdir), "%seval_scm_%lu",
             tmp_base, (unsigned long)GetCurrentProcessId());
#else
    snprintf(tmpdir, sizeof(tmpdir), "/tmp/eval_scm_%d", (int)getpid());
#endif

    mkdirs(tmpdir);

    for (int i = 0; i < embedded_scheme_file_count; i++) {
        const EmbeddedSchemeFile *ef = &embedded_scheme_files[i];
        char filepath[1200];
        snprintf(filepath, sizeof(filepath), "%s/%s", tmpdir, ef->path);

        /* Ensure parent directory exists */
        char dirpath[1200];
        memcpy(dirpath, filepath, sizeof(dirpath));
        char *last_sep = strrchr(dirpath, '/');
        if (!last_sep) last_sep = strrchr(dirpath, '\\');
        if (last_sep) {
            *last_sep = '\0';
            mkdirs(dirpath);
        }

        /* Write file content (size-1 to exclude NUL terminator) */
        FILE *f = fopen(filepath, "wb");
        if (!f) {
            fprintf(stderr, "warning: cannot write embedded file: %s\n", filepath);
            continue;
        }
        fwrite(ef->content, 1, ef->size - 1, f);
        fclose(f);
    }

    return strdup(tmpdir);
}

/* Recursively remove a directory */
static void rmdir_recursive(const char *path) {
#ifdef _WIN32
    char pattern[1200];
    snprintf(pattern, sizeof(pattern), "%s\\*", path);

    WIN32_FIND_DATAA fd;
    HANDLE h = FindFirstFileA(pattern, &fd);
    if (h == INVALID_HANDLE_VALUE) return;

    do {
        if (strcmp(fd.cFileName, ".") == 0 || strcmp(fd.cFileName, "..") == 0)
            continue;
        char child[1200];
        snprintf(child, sizeof(child), "%s\\%s", path, fd.cFileName);
        if (fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
            rmdir_recursive(child);
        else
            DeleteFileA(child);
    } while (FindNextFileA(h, &fd));

    FindClose(h);
    RemoveDirectoryA(path);
#else
    DIR *d = opendir(path);
    if (!d) return;

    struct dirent *ent;
    while ((ent = readdir(d)) != NULL) {
        if (strcmp(ent->d_name, ".") == 0 || strcmp(ent->d_name, "..") == 0)
            continue;
        char child[1200];
        snprintf(child, sizeof(child), "%s/%s", path, ent->d_name);
        struct stat st;
        if (stat(child, &st) == 0 && S_ISDIR(st.st_mode))
            rmdir_recursive(child);
        else
            unlink(child);
    }

    closedir(d);
    rmdir(path);
#endif
}

void embedded_scm_cleanup(char *tmpdir) {
    if (!tmpdir) return;
    rmdir_recursive(tmpdir);
    free(tmpdir);
}
