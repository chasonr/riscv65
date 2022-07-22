// browser.c -- Show a hierarchial display of the available files and allow
// the user to select one to run

#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <cbm.h>
#include <c64.h>
#include <conio.h>
#include "browser.h"
#include "petscii.h"
#include "reu.h"
#include "ultidos.h"

static const int dir_target = 2;

// The list of files in the current directory, resident in the REU
typedef struct FileNode {
    char path[MAX_PATH_LEN];
    uint8_t attrs;
} FileNode;

static uint32_t num_files;

static bool load_files(const char *dir);
static bool save_dir_entry(uint32_t entry, const char *path, uint8_t attrs);
static bool load_dir_entry(uint32_t entry, char *path, uint8_t *attrs);

// The current state of the screen
static unsigned top_row;
static unsigned cursor_pos;
static char current_dir[MAX_PATH_LEN];
static char search_str[40];

static void search(uint8_t ch);
static bool search_match(const char *path);
static void cursor_up(void);
static void cursor_down(void);
static void cursor_left(void);
static void cursor_right(void);
static void page_up(void);
static void page_down(void);
static bool select_file(void);
static void paint_screen(void);
static void paint_filename(uint32_t entry);
static void paint_row(uint8_t row, const char *line, uint8_t color);
static void paint_directory(void);
static void paint_error(const char *line);

void
browser(char *path)
{
    uint8_t old_bgcolor = bgcolor(COLOR_BLACK);
    uint8_t old_bordercolor = bordercolor(COLOR_GRAY1);
    uint8_t old_textcolor = textcolor(COLOR_GRAY1);
    bool stop = false;

    strcpy(current_dir, "/");

    clrscr();
    load_files(current_dir);
    paint_screen();

    // enter directory: right (CH_CURS_RIGHT)
    // leave directory: left (CH_CURS_LEFT)
    // change selection: up (CH_CURS_UP), down (CH_CURS_DOWN)
    // select: return (CH_ENTER)
    // page up: F1 (CH_F1)
    // page down: F7 (CH_F7)
    //
    // letters, digits (PETSCII): match names; ? wildcard

    while (!stop) {
        uint8_t ch = cgetc();
        if ((0x20 <= ch && ch <= 0x5F) || (0xC0 <= ch && ch <= 0xDE)) {
            search(ch);
        } else {
            search_str[0] = '\0';
        }
        paint_directory();
        switch (ch) {
        case CH_CURS_UP:
            cursor_up();
            break;

        case CH_CURS_DOWN:
            cursor_down();
            break;

        case CH_CURS_LEFT:
            cursor_left();
            break;

        case CH_CURS_RIGHT:
            cursor_right();
            break;

        case CH_F1:
            page_up();
            break;

        case CH_F7:
            page_down();
            break;

        case CH_ENTER:
            stop = select_file();
            break;
        }
    }
    bgcolor(old_bgcolor);
    bordercolor(old_bordercolor);
    textcolor(old_textcolor);
    clrscr();
    strcpy(path, current_dir); // actually selected file
}

// Load the files from the current directory
static bool
load_files(const char *dir)
{
    bool ok;
    char path[MAX_PATH_LEN];
    uint8_t attrs;

    num_files = 0;
    dos_close(dir_target);
    ok = dos_change_dir(dir_target, dir);
    if (!ok) {
        goto error;
    }

    ok = dos_open_dir(dir_target);
    if (!ok) {
        goto error;
    }

    ok = dos_read_dir_first(dir_target, path, sizeof(path), &attrs);
    if (!ok) {
        goto error;
    }
    ok = save_dir_entry(num_files++, path, attrs);
    if (!ok) {
        goto error;
    }
    while (dos_read_dir_next(dir_target, path, sizeof(path), &attrs)) {
        ok = save_dir_entry(num_files++, path, attrs);
        if (!ok) {
            goto error;
        }
    }

    dos_close(dir_target);
    top_row = 0;
    cursor_pos = 0;
    search_str[0] = '\0';
    return true;

error:
    dos_close(dir_target);
    return false;
}

static bool
save_dir_entry(uint32_t entry, const char *path, uint8_t attrs)
{
    uint32_t reu_addr;
    FileNode n;

    if (entry >= 0x1000000 / sizeof(FileNode)) {
        return false;
    }

    reu_addr = entry * sizeof(FileNode);
    strncpy(n.path, path, sizeof(n.path)-1);
    n.path[sizeof(n.path)-1] = '\0';
    n.attrs = attrs;
    reu_write(reu_addr, &n, sizeof(n));
    return true;
}

static bool
load_dir_entry(uint32_t entry, char *path, uint8_t *attrs)
{
    uint32_t reu_addr;
    FileNode n;

    if (entry >= 0x1000000 / sizeof(FileNode)) {
        return false;
    }

    reu_addr = entry * sizeof(FileNode);
    reu_read(reu_addr, &n, sizeof(n));
    strcpy(path, n.path);
    *attrs = n.attrs;
    return true;
}

static void
search(uint8_t ch)
{
    size_t len = strlen(search_str);
    if (len + 1 < sizeof(search_str)) {
        char path[MAX_PATH_LEN];
        uint8_t attr;
        uint32_t match;

        search_str[len] = ch;
        search_str[len+1] = '\0';

        for (match = 0; match < num_files; ++match) {
            load_dir_entry(match, path, &attr);
            if (search_match(path)) {
                break;
            }
        }
        if (match < num_files) {
            cursor_pos = match;
            if (match < 5) {
                top_row = 0;
            } else {
                top_row = match - 5;
            }
            paint_screen();
        }
    }
}

// Return true if search_str matches the path
// search_str is PETSCII; path is ASCII
static bool
search_match(const char *path)
{
    uint8_t i;
    for (i = 0; search_str[i] != '\0'; ++i) {
        if (toupper(ascii_to_petscii[(uint8_t)path[i]]) != toupper(search_str[i])) {
            return false;
        }
    }
    return true;
}

static void
cursor_up(void)
{
    if (cursor_pos != 0) {
        --cursor_pos;
        if (cursor_pos >= top_row) {
            paint_filename(cursor_pos+1);
            paint_filename(cursor_pos);
        } else {
            if (top_row < 24) {
                top_row = 0;
            } else {
                top_row -= 24;
            }
            paint_screen();
        }
    }
}

static void
cursor_down(void)
{
    if (cursor_pos+1 < num_files) {
        ++cursor_pos;
        if (cursor_pos < top_row+24) {
            paint_filename(cursor_pos-1);
            paint_filename(cursor_pos);
        } else {
            top_row = cursor_pos;
            paint_screen();
        }
    }
}

static void
cursor_left(void)
{
    size_t x = strlen(current_dir);
    // Trim any trailing slashes
    while (x != 0 && current_dir[x-1] == '/') {
        --x;
    }
    if (x == 0) {
        paint_error("Already at root directory");
    } else {
        // Count back to preceding slach
        while (x != 0 && current_dir[x-1] != '/') {
            --x;
        }
        // Trim trailing slashes again
        while (x != 0 && current_dir[x-1] == '/') {
            --x;
        }
        // Set new directory
        if (x == 0) {
            strcpy(current_dir, "/");
        } else {
            current_dir[x] = '\0';
        }

        // Load new directory
        load_files(current_dir);
        paint_screen();
    }
}

static void
cursor_right(void)
{
    if (num_files == 0) {
        paint_error("No directory");
    } else {
        char path[MAX_PATH_LEN];
        uint8_t attrs;
        size_t len;
        size_t new_len;

        // Get the selected path
        load_dir_entry(cursor_pos, path, &attrs);
        if (!(attrs & FA_DIRECTORY)) {
            paint_error("Not a directory");
            return;
        }

        // Check the length of the new path
        len = strlen(current_dir);
        while (len != 0 && current_dir[len-1] == '/') {
            --len;
        }
        new_len = len + 1 + strlen(path) + 1;
        if (new_len > sizeof(current_dir)) {
            paint_error("Path is too long");
            return;
        }

        // Build the new path
        strcpy(current_dir + len, "/");
        strcat(current_dir + len, path);

        // Load new directory
        load_files(current_dir);
        paint_screen();
    }
}

static void
page_up(void)
{
    if (num_files != 0 && cursor_pos != 0) {
        if (top_row == 0) {
            cursor_pos = 0;
        } else if (top_row >= 24) {
            top_row -= 24;
            cursor_pos -= 24;
        } else {
            cursor_pos -= top_row;
            top_row = 0;
        }
        paint_screen();
    }
}

static void
page_down(void)
{
    if (num_files != 0 && cursor_pos+1 <= num_files) {
        if (top_row+24 < num_files) {
            top_row += 24;
            cursor_pos += 24;
        } else {
            cursor_pos = num_files - 1;
        }
        paint_screen();
    }
}

static bool
select_file(void)
{
    if (num_files == 0) {
        paint_error("No file");
        return false;
    } else {
        char path[MAX_PATH_LEN];
        uint8_t attrs;
        size_t len;
        size_t new_len;

        // Get the selected path
        load_dir_entry(cursor_pos, path, &attrs);
        if (attrs & FA_DIRECTORY) {
            // If it's a directory, enter the directory
            cursor_right();
            return false;
        }

        // Check the length of the new path
        len = strlen(current_dir);
        while (len != 0 && current_dir[len-1] == '/') {
            --len;
        }
        new_len = len + 1 + strlen(path) + 1;
        if (new_len > sizeof(current_dir)) {
            paint_error("Path is too long");
            return false;
        }

        // Build the new path
        strcpy(current_dir + len, "/");
        strcat(current_dir + len, path);
        return true;
    }
}

static void
paint_screen(void)
{
    uint8_t i;

    if (num_files == 0) {
        gotoxy(0, 0);
        textcolor(COLOR_RED);
        cputs("No files in this directory              ");
        i = 1;
    } else {
        for (i = 0; i+top_row < num_files && i < 24; ++i) {
            paint_filename(i + top_row);
        }
    }
    for (; i < 24; ++i) {
        paint_row(i, "", 0);
    }
    paint_directory();
}

static void
paint_filename(uint32_t entry)
{
    static const uint8_t colors[] = {
        COLOR_LIGHTBLUE, COLOR_WHITE, // regular files
        COLOR_BROWN, COLOR_ORANGE     // directories
    };
    char path[MAX_PATH_LEN];
    uint8_t attrs;
    uint8_t color;

    load_dir_entry(entry, path, &attrs);
    color = ((attrs & FA_DIRECTORY) ? 2 : 0)
          | ((entry == cursor_pos ) ? 1 : 0);

    paint_row(entry - top_row, path, colors[color]);
}

static void
paint_error(const char *line)
{
    paint_row(24, "", 0);
    gotoxy(0, 24);
    textcolor(COLOR_RED);
    cputs(line);
}

static void
paint_row(uint8_t row, const char *line, uint8_t color)
{
    uint8_t *screen = (uint8_t *)0x0400 + row*40;
    uint8_t *color_mem = screen + 0xD800 - 0x0400;
    uint8_t j;

    if (memchr(line, '\0', 41) != NULL) {
        for (j = 0; line[j] != '\0'; ++j) {
            screen[j] = ascii_to_screen[(uint8_t)line[j]];
            color_mem[j] = color;
        }
        for (; j < 40; ++j) {
            screen[j] = petscii_to_screen[' '];
        }
    } else {
        for (j = 0; j < 39; ++j) {
            screen[j] = ascii_to_screen[(uint8_t)line[j]];
            color_mem[j] = color;
        }
        screen[39] = petscii_to_screen['>'];
        color_mem[39] = COLOR_YELLOW;
    }
}

static void
paint_directory(void)
{
    uint8_t *screen = (uint8_t *)0x0400 + 24*40;
    uint8_t *color_mem = (uint8_t *)0xD800 + 24*40;
    size_t len;
    uint8_t j;

    if (search_str[0] != '\0') {
        // Display search string
        char disp[sizeof(search_str)+10];
        strcpy(disp, "Find: ");
        strcat(disp, search_str);

        len = strlen(disp);
        if (len <= 40) {
            for (j = 0; disp[j] != '\0'; ++j) {
                screen[j] = petscii_to_screen[(uint8_t)disp[j]];
                color_mem[j] = COLOR_GRAY2;
            }
            for (; j < 40; ++j) {
                screen[j] = petscii_to_screen[' '];
            }
        } else {
            for (j = 0; j < 39; ++j) {
                screen[j] = petscii_to_screen[(uint8_t)disp[j+len-40]];
                color_mem[j] = COLOR_GRAY2;
            }
            screen[39] = petscii_to_screen['>'];
            color_mem[39] = COLOR_YELLOW;
        }
    } else {
        len = strlen(current_dir);
        if (len <= 40) {
            for (j = 0; current_dir[j] != '\0'; ++j) {
                screen[j] = ascii_to_screen[(uint8_t)current_dir[j]];
                color_mem[j] = COLOR_GRAY2;
            }
            for (; j < 40; ++j) {
                screen[j] = petscii_to_screen[' '];
            }
        } else {
            screen[0] = petscii_to_screen['<'];
            color_mem[0] = COLOR_YELLOW;
            for (j = 1; j < 40; ++j) {
                screen[j] = ascii_to_screen[(uint8_t)current_dir[j+len-40]];
                color_mem[j] = COLOR_GRAY2;
            }
        }
    }
}
