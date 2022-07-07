// fatfsgen.cpp

#include <iostream>
#include <fstream>
#include <list>
#include <regex>
#include <stdexcept>
#include <string>
#include <unordered_set>
#include <vector>
#include <cassert>
#include <cctype>
#include <cerrno>
#include <cinttypes>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <sys/stat.h>

// Various FAT file system constants
const unsigned fat12_min_clusters = 1;
const unsigned fat12_max_clusters = 0xFF4;
const unsigned fat16_min_clusters = fat12_max_clusters + 1;
const unsigned fat16_max_clusters = 0xFFF4;
const unsigned fat32_min_clusters = fat16_max_clusters + 1;
const std::uint32_t fat32_max_clusters = 0xFFFFFF4;

// Case folding tables
struct UNI_simple_map {
    char32_t from;
    char32_t to;
};

struct UNI_multi_map {
    char32_t from;
    const char16_t *to;
};

#include "casefold.h"

// The boot sector
struct FS_boot_sector {
    // Common to FAT12, FAT16 and FAT32
    uint8_t BS_jmpBoot[3];
    char    BS_OEMName[8];
    uint8_t BPB_BytsPerSec[2];
    uint8_t BPB_SecPerClus;
    uint8_t BPB_RsvdSecCnt[2];
    uint8_t BPB_NumFats;
    uint8_t BPB_RootEntCnt[2];
    uint8_t BPB_TotSec16[2];
    uint8_t BPB_Media;
    uint8_t BPB_FATSz16[2];
    uint8_t BPB_SecPerTrk[2];
    uint8_t BPB_NumHeads[2];
    uint8_t BPB_HiddSec[4];
    uint8_t BPB_TotSec32[4];
    union {
        // For FAT12 and FAT16
        struct {
            uint8_t BS_DrvNum;
            uint8_t BS_Reserved1;
            uint8_t BS_BootSig;
            uint8_t BS_VolID[4];
            uint8_t BS_VolLab[11];
            uint8_t BS_FilSysType[8];
        } b16;
        // For FAT32
        struct {
            uint8_t BPB_FATSz32[4];
            uint8_t BPB_ExtFlags[2];
            uint8_t BPB_FSVer[2];
            uint8_t BPB_RootClus[4];
            uint8_t BPB_FSInfo[2];
            uint8_t BPB_BkBootSec[2];
            uint8_t BPB_Reserved[12];
            uint8_t BS_DrvNum;
            uint8_t BS_Reserved1;
            uint8_t BS_BootSig;
            uint8_t BS_VolID[4];
            uint8_t BS_VolLab[11];
            uint8_t BS_FilSysType[8];
        } b32;
    };
};

// The FAT32 info sector
struct FS_info_sector {
    uint8_t FSI_LeadSig[4];
    uint8_t FSI_Reseerved1[480];
    uint8_t FSI_StrucSig[4];
    uint8_t FSI_Free_Count[4];
    uint8_t FSI_Nxt_Free[4];
    uint8_t FSI_Reserved2[12];
    uint8_t FSI_TrailSig[4];
};

// One 32-byte entry in the directory as recorded on disk
struct FS_dir_record {
    union {
        // Short name record
        struct {
            char DIR_Name[11];
            std::uint8_t DIR_Attr;
            std::uint8_t DIR_NTRes;
            std::uint8_t DIR_CrtTimeTenth;
            std::uint8_t DIR_CrtTime[2];
            std::uint8_t DIR_CrtDate[2];
            std::uint8_t DIR_AccDate[2];
            std::uint8_t DIR_FstClusHI[2];
            std::uint8_t DIR_WrtTime[2];
            std::uint8_t DIR_WrtDate[2];
            std::uint8_t DIR_FstClusLO[2];
            std::uint8_t DIR_FileSize[4];
        } s;
        // Long name record
        struct {
            std::uint8_t LDIR_Ord;
            std::uint8_t LDIR_Name1[5][2];
            std::uint8_t LDIR_Attr;
            std::uint8_t LDIR_Type;
            std::uint8_t LDIR_Chksum;
            std::uint8_t LDIR_Name2[6][2];
            std::uint8_t LDIR_FstClusLO[2];
            std::uint8_t LDIR_Name3[2][2];
        } l;
    };
};

// One directory entry
struct FS_dir_entry {
    // True if this is the root directory
    bool is_root;
    // For file: source for file contents
    std::string source;
    // For file: result of stat for source file
    struct stat fst;
    // For directory: list of entries
    std::list<FS_dir_entry> dir_entries;
    // For directory: contents of the directory
    std::vector<FS_dir_record> directory;
    // Size of the file or directory
    std::uint32_t size;
    // Starting cluster
    std::uint32_t first_cluster;
    // Long name for this entry
    std::u16string long_name;
    // Short name for this entry
    std::string short_name;
    // True if a long name should be created
    bool has_long_name;
    // Index to the short name entry
    std::size_t dir_index;
    // Attributes for this entry
    enum {
        ATTR_READ_ONLY = 0x01,
        ATTR_HIDDEN    = 0x02,
        ATTR_SYSTEM    = 0x04,
        ATTR_VOLUME    = 0x08,
        ATTR_DIRECTORY = 0x10,
        ATTR_ARCHIVE   = 0x20,
        ATTR_LONG_NAME = 0x0F,
        ATTR_LONG_NAME_MASK = 0x3F
    };
    unsigned attributes;

    FS_dir_entry() : is_root(false), size(0), has_long_name(false),
            dir_index(0), attributes(0)
    {
        std::memset(&fst, 0, sizeof(fst));
    }
    void fill_directory();
    std::uintmax_t get_total_size(unsigned cluster_size);
};

// Contents of parsed specification file
struct FS_specification {
    // Name of the volume on the disk
    std::string volume_name;
    // File system type
    enum { fstype_unknown, fstype_fat12, fstype_fat16, fstype_fat32 } fs_type;
    // Sector size
    unsigned sector_size;
    // Cluster size
    unsigned cluster_size;
    // Capacity
    std::uintmax_t capacity;
    // Root directory size
    unsigned root_dir_size;
    // Volume label
    std::string label;

    // One copy of the FAT
    std::vector<std::uint32_t> fat;

    // Root directory
    FS_dir_entry root_dir;

    FS_specification() : fs_type(fstype_unknown), sector_size(0),
            cluster_size(0), capacity(0), root_dir_size(0)
    {
        root_dir.is_root = true;
        root_dir.attributes = FS_dir_entry::ATTR_DIRECTORY;
    }

    bool parse(const char *spec_file);
    void insert_file(const std::string& path, unsigned attrs, const std::string& source);
    void fill_directory();
    void set_params();
    void allocate_clusters();
    void allocate_clusters(FS_dir_entry& entry);
    void create_cluster_chain(std::uint32_t size);
    std::uint32_t EOC();
    std::uintmax_t get_total_size(unsigned cluster_size);
    void write_data();
    void write_boot_sector(std::ofstream& outfp);
    void write_info_sector(std::ofstream& outfp);
    void write_fat(std::ofstream& outfp);
    void write_root_dir(std::ofstream& outfp);
    void write_tree(std::ofstream& outfp, FS_dir_entry const & entry);
    void write_file(std::ofstream& outfp, FS_dir_entry const & entry);
    void write_directory(std::ofstream& outfp, FS_dir_entry const & entry);
    void write_zeros(std::ofstream& outfp, std::uintmax_t count);
};

// Time in DOS format
struct DOS_time {
    std::uint16_t date;
    std::uint16_t time;
    std::uint8_t msec;
};

// Case-insensitive string comparison
static bool str_match(const std::string& str, const char *target)
{
    std::size_t i;
    for (i = 0; i < str.size(); ++i) {
        if (std::toupper(str.at(i)) != target[i]) {
            return false;
        }
    }
    return target[i] == '\0';
}

// Convert a numeric token
static std::uintmax_t num_convert(const std::string& str)
{
    // String should be numeric
    if (str.empty() || !std::isdigit(str.at(0))) {
        throw std::runtime_error("String \"" + str + "\" expected to be numeric");
    }

    char *ptr;
    errno = 0;
    std::uintmax_t num = std::strtoumax(str.c_str(), &ptr, 10);
    if (errno != 0) {
        throw std::runtime_error("String \"" + str + "\" out of range");
    }
    // String at ptr may be a modifier: K, M or G; it must have at most
    // one character
    if (ptr[0] != '\0' && ptr[1] != '\0') {
        throw std::runtime_error("String \"" + str + "\" has invalid suffix \"" + ptr + "\"");
    }
    unsigned shift;
    switch (ptr[0]) {
    case '\0':
        shift = 0;
        break;

    case 'k':
    case 'K':
        shift = 10;
        break;

    case 'm':
    case 'M':
        shift = 20;
        break;

    case 'g':
    case 'G':
        shift = 30;
        break;

    default:
        throw std::runtime_error("String \"" + str + "\" has invalid suffix \"" + ptr + "\"");
    }

    std::uintmax_t num2 = num << shift;
    if (num2 >> shift != num) {
        throw std::runtime_error("String \"" + str + "\" out of range");
    }

    return num2;
}

// Return true if the string is a valid volume label
static bool is_valid_label(const std::string& label)
{
    if (label.size() > 11) {
        return false;
    }
    for (auto i = label.cbegin(); i != label.cend(); ++i) {
        unsigned char ch = *i;
        if (ch < 0x20 || std::strchr("\"*+,./:;<=>?[\\]~", ch)) {
            return false;
        }
    }
    return true;
}

// Parse the specification file
bool FS_specification::parse(const char *spec_file)
{
    static std::regex token_rx(R"(\s+|[A-Za-z0-9_]+|".*?"|#.*|.)");

    // Open the file
    std::ifstream spec(spec_file);
    if (spec.fail()) {
        std::perror(spec_file);
        return false;
    }

    // Read lines until EOF
    unsigned linenum = 0;
    try {
        while (true) {
            char buf[4096];
            std::smatch m;

            spec.getline(buf, sizeof(buf));
            if (!spec.good()) {
                break;
            }
            std::string line(buf);
            ++linenum;

            // Extract the keywords
            std::vector<std::string> tokens;
            auto tok_begin = std::sregex_iterator(line.begin(), line.end(), token_rx);
            auto tok_end = std::sregex_iterator();
            for (auto i = tok_begin; i != tok_end; ++i) {
                auto token = i->str();
                if (token.empty() || std::isspace(token[0])) {
                    continue; // empty or whitespace
                }
                if (token[0] == '#') {
                    break; // comment
                }
                if (std::isalnum(token[0]) || token[0] == '"') {
                    tokens.push_back(token); // keyword, number or quoted string
                } else {
                    throw std::runtime_error("Unexpected character " + token);
                }
            }

            // Skip blank and comment lines
            if (tokens.empty()) {
                continue;
            }

            // Match the various declaration types
            if (str_match(tokens[0], "VOLUME")) {
                if (tokens.size() != 2) {
                    throw std::runtime_error("Usage: VOLUME \"<path>\"");
                }
                this->volume_name = tokens[1].substr(1, tokens[1].size()-2);
            } else if (str_match(tokens[0], "FSTYPE")) {
                bool ok = true;
                if (tokens.size() != 2) {
                    ok = false;
                } else if (str_match(tokens[1], "FAT12")) {
                    this->fs_type = fstype_fat12;
                } else if (str_match(tokens[1], "FAT16")) {
                    this->fs_type = fstype_fat16;
                } else if (str_match(tokens[1], "FAT32")) {
                    this->fs_type = fstype_fat32;
                } else {
                    ok = false;
                }
                if (!ok) {
                    throw std::runtime_error("Usage: FSTYPE {FAT12|FAT16|FAT32}");
                }
            } else if (str_match(tokens[0], "SECTOR_SIZE")) {
                if (tokens.size() != 2) {
                    throw std::runtime_error("Usage: SECTOR_SIZE <size>");
                }
                std::uintmax_t size = num_convert(tokens[1]);
                switch (size) {
                case 512:
                case 1024:
                case 2048:
                case 4096:
                    this->sector_size = (unsigned)size;
                    break;

                default:
                    throw std::runtime_error("Sector size \"" + tokens[1] + "\" not one of 512, 1024, 2048 or 4096");
                }
            } else if (str_match(tokens[0], "CLUSTER_SIZE")) {
                if (tokens.size() != 2) {
                    throw std::runtime_error("Usage: CLUSTER_SIZE <size>");
                }
                std::uintmax_t size = num_convert(tokens[1]);
                switch (size) {
                case 512:
                case 1024:
                case 2048:
                case 4096:
                case 8192:
                case 16384:
                case 32768:
                    this->cluster_size = (unsigned)size;
                    break;

                default:
                    throw std::runtime_error("Cluster size \"" + tokens[1] + "\" not one of 512, 1024, 2048, 4096, 8192, 16384 or 32768");
                }
            } else if (str_match(tokens[0], "CAPACITY")) {
                if (tokens.size() != 2) {
                    throw std::runtime_error("Usage: CAPACITY <size>");
                }
                std::uintmax_t size = num_convert(tokens[1]);
                if (size > fat32_max_clusters * (std::uintmax_t)32768) {
                    throw std::runtime_error("Capacity \"" + tokens[1] + "\" exceeds the maximum capacity of a FAT file system");
                }
                this->capacity = size;
            } else if (str_match(tokens[0], "ROOT_DIR_SIZE")) {
                if (tokens.size() != 2) {
                    throw std::runtime_error("Usage: ROOT_DIR_SIZE <size>");
                }
                std::uintmax_t size = num_convert(tokens[1]);
                if (size > 65535) {
                    throw std::runtime_error("Root directory size \"" + tokens[1] + "\" exceeds the maximum of 65535");
                }
                this->root_dir_size = (unsigned)size;
            } else if (str_match(tokens[0], "LABEL")) {
                if (tokens.size() != 2 || tokens[1].at(0) != '"') {
                    throw std::runtime_error("Usage: LABEL \"<text>\"");
                }
                std::string label = tokens[1].substr(1, tokens[1].size()-2);
                if (!is_valid_label(label)) {
                    throw std::runtime_error("Volume label " + tokens[1] + "is not valid");
                }
                this->label = label;
            } else if (str_match(tokens[0], "DIRECTORY")) {
                if (tokens.size() != 2 || tokens[1].at(0) != '"') {
                    throw std::runtime_error("Usage: DIRECTORY \"<path>\"");
                }
                std::string path = tokens[1].substr(1, tokens[1].size()-2);
                this->insert_file(path, FS_dir_entry::ATTR_DIRECTORY, "");
            } else if (str_match(tokens[0], "FILE")) {
                if (tokens.size() < 2 || tokens[1].at(0) != '"') {
                    throw std::runtime_error("Usage: FILE \"<path>\" {HIDDEN|SYSTEM|READONLY|SOURCE \"<path>\"}*");
                }
                std::string path = tokens[1].substr(1, tokens[1].size()-2);
                unsigned attrs = 0;
                std::string source = "";
                std::size_t i = 2;
                while (i < tokens.size()) {
                    if (str_match(tokens[i], "HIDDEN")) {
                        attrs |= FS_dir_entry::ATTR_HIDDEN;
                        ++i;
                    } else if (str_match(tokens[i], "SYSTEM")) {
                        attrs |= FS_dir_entry::ATTR_SYSTEM;
                        ++i;
                    } else if (str_match(tokens[i], "READONLY")) {
                        attrs |= FS_dir_entry::ATTR_READ_ONLY;
                        ++i;
                    } else if (str_match(tokens[i], "SOURCE")) {
                        ++i;
                        if (i >= tokens.size()) {
                            throw std::runtime_error("SOURCE requires a path after it");
                        }
                        source = tokens[i].substr(1, tokens[i].size()-2);
                        ++i;
                    } else {
                        throw std::runtime_error("Unknown FILE attribute: \"" + tokens[i] + "\"");
                    }
                }
                this->insert_file(path, attrs, source);
            } else {
                throw std::runtime_error("Unknown keyword \"" + tokens[0] + "\"");
                return false;
            }
        }
        if (!spec.eof()) {
            std::perror(spec_file);
            return false;
        }
    }
    catch (std::runtime_error& err)
    {
        std::cerr << spec_file << " line " << linenum << ": ";
        std::cerr << err.what() << "\n";
        return false;
    }

    return true;
}

// Convert a path component to UTF-16 for the long name records
// Check the component for validity
static std::u16string u16_convert(const std::string& str)
{
    std::u16string str16;

    std::size_t i = 0;
    while (i < str.size()) {
        unsigned char byte = str.at(i++);
        char32_t u32;
        unsigned count;
        char32_t min;

        if (byte < 0x80) {
            // One-byte character, equivalent to ASCII
            u32 = byte;
            count = 0;
            min = 0;
        } else if (byte < 0xC2) {
            // Dangling extension byte, or overlong one-byte character
            throw std::runtime_error("UTF-8 conversion error");
        } else if (byte < 0xE0) {
            // Two-byte character
            u32 = byte & 0x1F;
            count = 1;
            min = 0x80;
        } else if (byte < 0xF0) {
            // Three-byte character
            u32 = byte & 0x0F;
            count = 2;
            min = 0x800;
        } else if (byte < 0xF5) {
            // Four-byte character
            u32 = byte & 0x07;
            count = 3;
            min = 0x10000;
        } else {
            // Bytes that cannot occur in UTF-8
            throw std::runtime_error("UTF-8 conversion error");
        }
        // Add extension bytes
        for (; count != 0 && i < str.size(); --count) {
            byte = str.at(i);
            if (byte < 0x80 || 0xBF < byte) {
                break;
            }
            u32 = (u32 << 6) | (byte & 0x3F);
        }
        // Check for invalid Unicode characters
        if (count != 0 || u32 < min || (0xD800 <= u32 && u32 <= 0xDFFF)
            || u32 > 0x10FFFF) {
            // Incomplete character, overlong sequence, surrogate code point
            // or invalid code point
            throw std::runtime_error("UTF-8 conversion error");
        }
        // Check for characters not allowed in file names
        if (u32 < 0x20 || u32 == 0x7F) {
            char str[10];
            std::snprintf(str, sizeof(str), "U+%04X", (unsigned)u32);
            throw std::runtime_error(std::string(str) + " not allowed in file name");
        }
        if (u32 < 0x80 && std::strchr("\"*/:<>?\\|", (char)u32) != nullptr) {
            char str[10];
            std::snprintf(str, sizeof(str), "\"%c\"", (int)u32);
            throw std::runtime_error(std::string(str) + " not allowed in file name");
        }
        // Add to the converted long file name
        if (u32 < 0x10000) {
            str16 += (char16_t)u32;
        } else {
            str16 += (char16_t)(0xD7C0 + (u32 >> 10));
            str16 += (char16_t)(0xDC00 + (u32 & 0x3FF));
        }
    }
    // Check for excessive name length
    if (str16.size() > 255) {
        throw std::runtime_error("Name is too long");
    }

    return str16;
}

// Case-fold a single character
static std::u16string casefold(char32_t ch)
{
    std::u16string str;
    char16_t c16[3];

    // Search the simple-mapping table
    struct simple_comp {
        bool operator()(const UNI_simple_map& rec, char32_t ch) {
            return rec.from < ch;
        }
        bool operator()(char32_t ch, const UNI_simple_map& rec) {
            return ch < rec.from;
        }
    };
    auto ps = std::equal_range(simple_map.begin(), simple_map.end(), ch,
            simple_comp{});
    for (auto i = ps.first; i != ps.second; ++i) {
        if (i->to < 0x10000) {
            c16[0] = (char16_t)i->to;
            c16[1] = u'\0';
        } else {
            c16[0] = (char16_t)(0xD7C0 + (i->to >> 10));
            c16[1] = (char16_t)(0xD800 + (i->to & 0x3FF));
            c16[2] = u'\0';
        }
        str += c16;
    }

    if (!str.empty()) {
        return str;
    }

    // Search the multi-mapping table
    struct multi_comp {
        bool operator()(const UNI_multi_map& rec, char32_t ch) {
            return rec.from < ch;
        }
        bool operator()(char32_t ch, const UNI_multi_map& rec) {
            return ch < rec.from;
        }
    };
    auto pm = std::equal_range(multi_map.begin(), multi_map.end(), ch,
            multi_comp{});
    for (auto i = pm.first; i != pm.second; ++i) {
        str += i->to;
    }

    if (!str.empty()) {
        return str;
    }

    // If not found, return identity map
    if (ch < 0x10000) {
        c16[0] = (char16_t)ch;
        c16[1] = u'\0';
    } else {
        c16[0] = (char16_t)(0xD7C0 + (ch >> 10));
        c16[1] = (char16_t)(0xD800 + (ch & 0x3FF));
        c16[2] = u'\0';
    }
    return c16;
}

// Perform standard Unicode case folding
static std::u16string casefold(const std::u16string& str)
{
    std::u16string casestr;

    std::size_t i = 0;
    while (i < str.size()) {
        // Convert character from str
        char32_t ch = str.at(i++);
        if (0xD800 <= ch && ch <= 0xDBFF
        &&  i < str.size()
        &&  0xDC00 <= str.at(i) && str.at(i) <= 0xDFFF) {
            ch = 0x10000 + ((ch & 0x3FF) << 10) + (str.at(i++) & 0x3FF);
        }
        casestr += casefold(ch);
    }

    return casestr;
}

// Return true if names match, with standard Unicode case folding
static bool dir_match(const std::u16string& str1, const std::u16string& str2)
{
    return casefold(str1) == casefold(str2);
}

// Insert a file or a directory into the tree
void FS_specification::insert_file(
        const std::string& path,
        unsigned attrs,
        const std::string& source)
{
    static std::regex path_rx(R"([^/\\]+)");

    // Count the components
    auto path_begin = std::sregex_iterator(path.begin(), path.end(), path_rx);
    auto path_end = std::sregex_iterator();
    unsigned num_components = 0;
    for (auto i = path_begin; i != path_end; ++i) {
        ++num_components;
    }
    if (num_components == 0) {
        throw std::runtime_error("Path is empty");
    }

    // Split the path into components
    std::list<FS_dir_entry> *dir = &this->root_dir.dir_entries;
    unsigned count = 0;
    for (auto i = path_begin; i != path_end; ++i) {
        auto component = i->str();
        auto comp16 = u16_convert(component);
        FS_dir_entry *entry = nullptr;
        for (auto j = dir->begin(); j != dir->end(); ++j) {
            if (dir_match(comp16, j->long_name)) {
                entry = &(*j);
                break;
            }
        }
        if (entry == nullptr) {
            // New entry. If it is not the last component, make it a directory;
            // otherwise, use the type from the parameters.
            FS_dir_entry new_entry;
            dir->push_back(new_entry);
            entry = &(*dir->rbegin());
            if ((attrs & FS_dir_entry::ATTR_DIRECTORY) != 0
                || count + 1 < num_components) {
                // Make a directory
                entry->attributes = FS_dir_entry::ATTR_DIRECTORY;
            } else {
                // Make a regular file
                entry->attributes = attrs;
                entry->source = source.empty() ? path : source;
            }
            entry->long_name = comp16;
        } else {
            // Existing entry.
            // If not the last component, it must be a directory.
            // If this is the last component, ignore if creating a directory
            // over a directory; otherwise, raise an error.
            if (count + 1 < num_components) {
                // Must be a directory
                if ((entry->attributes & FS_dir_entry::ATTR_DIRECTORY) == 0) {
                    throw std::runtime_error("\"" + component + "\" is not a directory");
                }
            } else {
                if ((entry->attributes & FS_dir_entry::ATTR_DIRECTORY) == 0
                    || (attrs & FS_dir_entry::ATTR_DIRECTORY) == 0) {
                    throw std::runtime_error("\"" + component + "\" already exists");
                }
            }
        }
        dir = &entry->dir_entries;
        ++count;
    }
}

// Fill the root directory and everything under it
void FS_specification::fill_directory()
{
    // Include the volume label if one was specified
    if (!label.empty()) {
        FS_dir_record rec;
        std::memset(&rec, 0, sizeof(rec));
        std::memset(rec.s.DIR_Name, ' ', sizeof(rec.s.DIR_Name));
        assert(label.size() <= sizeof(rec.s.DIR_Name));
        std::memcpy(rec.s.DIR_Name, label.c_str(), label.size());
        rec.s.DIR_Attr = FS_dir_entry::ATTR_VOLUME;
        root_dir.directory.push_back(rec);
    }

    root_dir.fill_directory();
}

// Set any parameters not specified and increase capacities as needed
void FS_specification::set_params()
{
    // Optional parameters determined here are:
    // * file system type
    // * sector size
    // * cluster size
    // * capacity
    // * root directory size (FAT12 and FAT16)

    // If the sector size is unspecified, set it to 512
    if (this->sector_size == 0) {
        this->sector_size = 512;
    }

    // Expand the root directory to include all specified files
    if (this->root_dir_size < this->root_dir.directory.size()) {
        this->root_dir_size = this->root_dir.directory.size();
    }
    // Always at least one sector for the root directory
    if (this->root_dir_size < this->sector_size / 32) {
        this->root_dir_size = this->sector_size / 32;
    }

    // If the cluster size is unspecified, or smaller than the sector size,
    // make it equal to the sector size
    if (this->cluster_size < this->sector_size) {
        this->cluster_size = this->sector_size;
    }

    // Determine the total size of the files and directories.
    // Don't include the root directory. We only need to allocate clusters for
    // the root directory if we're making a FAT32 volume anyway.
    auto total_size = get_total_size(this->cluster_size);
    // Expand the capacity to fit all files and directories
    if (this->capacity < total_size) {
        this->capacity = total_size;
    }

    // If file system type is unspecified, determine according to capacity and
    // cluster size. Otherwise, adjust capacity or cluster size as needed to
    // be compatible with the requested file system type.
    auto num_clusters = (this->capacity + this->cluster_size - 1) / this->cluster_size;
    switch (this->fs_type) {
    case FS_specification::fstype_unknown:
        {
            if (num_clusters < fat12_min_clusters) {
                num_clusters = fat12_min_clusters;
                this->fs_type = FS_specification::fstype_fat12;
            } else if (num_clusters <= fat12_max_clusters) {
                this->fs_type = FS_specification::fstype_fat12;
            } else if (num_clusters <= fat16_max_clusters) {
                this->fs_type = FS_specification::fstype_fat16;
            } else {
                this->fs_type = FS_specification::fstype_fat32;
                // Include the root directory in the total size
                num_clusters += (this->root_dir.size + this->cluster_size - 1) / this->cluster_size;
                while (this->cluster_size < 65536 && num_clusters > fat32_max_clusters) {
                    // Too many clusters even for FAT32
                    // Increase the cluster size
                    this->cluster_size *= 2;
                    total_size = get_total_size(this->cluster_size);
                    num_clusters = (this->capacity + this->cluster_size - 1) / this->cluster_size;
                    num_clusters += (this->root_dir.size + this->cluster_size - 1) / this->cluster_size;
                }
                if (this->cluster_size >= 65536) {
                    throw std::runtime_error("File system is too large for FAT32");
                }
            }
        }
        break;

    case FS_specification::fstype_fat12:
        {
            if (num_clusters < fat12_min_clusters) {
                num_clusters = fat12_min_clusters;
            } else if (num_clusters > fat12_max_clusters) {
                // Cluster count is too large
                while (this->cluster_size < 65536 && num_clusters > fat12_max_clusters) {
                    this->cluster_size *= 2;
                    total_size = get_total_size(this->cluster_size);
                    if (this->capacity < total_size) {
                        this->capacity = total_size;
                    }
                    num_clusters = (this->capacity + this->cluster_size - 1) / this->cluster_size;
                }
                if (this->cluster_size >= 65536) {
                    throw std::runtime_error("File system is too large for FAT12");
                }
            }
        }
        break;

    case FS_specification::fstype_fat16:
        {
            if (num_clusters < fat16_min_clusters) {
                num_clusters = fat16_min_clusters;
            } else if (num_clusters > fat16_max_clusters) {
                // Cluster count is too large
                while (this->cluster_size < 65536 && num_clusters > fat16_max_clusters) {
                    this->cluster_size *= 2;
                    total_size = get_total_size(this->cluster_size);
                    if (this->capacity < total_size) {
                        this->capacity = total_size;
                    }
                    num_clusters = (this->capacity + this->cluster_size - 1) / this->cluster_size;
                }
                if (this->cluster_size >= 65536) {
                    throw std::runtime_error("File system is too large for FAT16");
                }
            }
        }
        break;

    case FS_specification::fstype_fat32:
        {
            // Include the root directory in the total size
            num_clusters += (this->root_dir.size + this->cluster_size - 1) / this->cluster_size;
            if (num_clusters < fat32_min_clusters) {
                num_clusters = fat32_min_clusters;
            } else if (num_clusters > fat32_max_clusters) {
                // Cluster count is too large
                while (this->cluster_size < 65536 && num_clusters > fat32_max_clusters) {
                    this->cluster_size *= 2;
                    total_size = get_total_size(this->cluster_size);
                    if (this->capacity < total_size) {
                        this->capacity = total_size;
                    }
                    num_clusters = (this->capacity + this->cluster_size - 1) / this->cluster_size;
                    num_clusters += (this->root_dir.size + this->cluster_size - 1) / this->cluster_size;
                }
                if (this->cluster_size >= 65536) {
                    throw std::runtime_error("File system is too large for FAT32");
                }
            }
        }
        break;
    }
    // Expand capacity as needed
    this->capacity = num_clusters * this->cluster_size;
}

// Allocate clusters for each file and directory
void FS_specification::allocate_clusters()
{
    // Set the first two FAT entries
    std::uint32_t eoc = EOC();
    // TODO: The first entry will need to be changed if media types other than
    // 0xF8 are ever used
    this->fat.push_back((eoc & 0xFFFFFF00) | 0xF8);
    this->fat.push_back(eoc);

    // For FAT32, allocate clusters for the root directory
    if (this->fs_type == fstype_fat32) {
        // If the volume is completely empty, allocate one cluster to the
        // root directory
        if (this->root_dir.size == 0) {
            FS_dir_record rec;
            std::memset(&rec, 0, sizeof(rec));
            this->root_dir.directory.push_back(rec);
            this->root_dir.size = 32;
        }
        this->root_dir.first_cluster = this->fat.size();
        create_cluster_chain(this->root_dir.size);
    }

    // Allocate clusters for each entry in the root directory
    for (auto i = this->root_dir.dir_entries.begin(); i != this->root_dir.dir_entries.end(); ++i) {
        auto first_cluster = this->fat.size();
        i->first_cluster = first_cluster;
        FS_dir_record *rec = &this->root_dir.directory[i->dir_index];
        rec->s.DIR_FstClusLO[0] = (uint8_t)(first_cluster >>  0);
        rec->s.DIR_FstClusLO[1] = (uint8_t)(first_cluster >>  8);
        rec->s.DIR_FstClusHI[0] = (uint8_t)(first_cluster >> 16);
        rec->s.DIR_FstClusHI[1] = (uint8_t)(first_cluster >> 24);
        allocate_clusters(*i);
        if ((i->attributes & FS_dir_entry::ATTR_DIRECTORY) != 0) {
            // Set the . entry
            FS_dir_record *dot = &i->directory[0];
            dot->s.DIR_FstClusLO[0] = (uint8_t)(first_cluster >>  0);
            dot->s.DIR_FstClusLO[1] = (uint8_t)(first_cluster >>  8);
            dot->s.DIR_FstClusHI[0] = (uint8_t)(first_cluster >> 16);
            dot->s.DIR_FstClusHI[1] = (uint8_t)(first_cluster >> 24);
            // Don't set the .. entry; it's supposed to point to 0
        }
    }

    if (this->fat.size() > EOC()-2) {
        std::runtime_error("FAT allocation exceeded maximum cluster number");
    }

    // Extend capacity if needed
    std::uintmax_t bytes = (std::uintmax_t)this->cluster_size * (this->fat.size()-2);
    if (this->capacity < bytes) {
        this->capacity = bytes;
    }
}

// Traverse the file tree to allocate clusters
void FS_specification::allocate_clusters(FS_dir_entry& entry)
{
    entry.first_cluster = this->fat.size();
    create_cluster_chain(entry.size);

    // dir_entries is empty for ordinary files, but may be non-empty
    // for directories
    for (auto i = entry.dir_entries.begin(); i != entry.dir_entries.end(); ++i) {
        if (i->size != 0) {
            auto first_cluster = this->fat.size();
            i->first_cluster = first_cluster;
            FS_dir_record *rec = &entry.directory[i->dir_index];
            rec->s.DIR_FstClusLO[0] = (uint8_t)(first_cluster >>  0);
            rec->s.DIR_FstClusLO[1] = (uint8_t)(first_cluster >>  8);
            rec->s.DIR_FstClusHI[0] = (uint8_t)(first_cluster >> 16);
            rec->s.DIR_FstClusHI[1] = (uint8_t)(first_cluster >> 24);
            allocate_clusters(*i);
            if ((i->attributes & FS_dir_entry::ATTR_DIRECTORY) != 0) {
                // Set the . entry
                FS_dir_record *dot = &i->directory[0];
                dot->s.DIR_FstClusLO[0] = (uint8_t)(first_cluster >>  0);
                dot->s.DIR_FstClusLO[1] = (uint8_t)(first_cluster >>  8);
                dot->s.DIR_FstClusHI[0] = (uint8_t)(first_cluster >> 16);
                dot->s.DIR_FstClusHI[1] = (uint8_t)(first_cluster >> 24);
                // Set the .. entry
                FS_dir_record *dotdot = &i->directory[1];
                dotdot->s.DIR_FstClusLO[0] = (uint8_t)(entry.first_cluster >>  0);
                dotdot->s.DIR_FstClusLO[1] = (uint8_t)(entry.first_cluster >>  8);
                dotdot->s.DIR_FstClusHI[0] = (uint8_t)(entry.first_cluster >> 16);
                dotdot->s.DIR_FstClusHI[1] = (uint8_t)(entry.first_cluster >> 24);
            }
        }
    }
}

// Create a cluster chain for a file or directory extending for the given
// number of bytes
void FS_specification::create_cluster_chain(std::uint32_t size)
{
    std::uint32_t num_clusters = (size + this->cluster_size - 1) / this->cluster_size;
    for (std::uint32_t i = 0; i+1 < num_clusters; ++i) {
        this->fat.push_back(this->fat.size()+1);
    }
    this->fat.push_back(EOC());
}

// Return end-of-chain value for the configured file type
std::uint32_t FS_specification::EOC()
{
    switch (this->fs_type)
    {
    case fstype_fat12:
        return 0xFF8;

    case fstype_fat16:
        return 0xFFF8;

    case fstype_fat32:
        return 0xFFFFFF8;

    default:
        throw std::runtime_error("EOC called without configuring the file system type");
    }
}

// Get total size in clusters of files and directories
// Don't include the root directory; it needs clusters only on FAT32
std::uintmax_t FS_specification::get_total_size(unsigned cluster_size)
{
    std::uintmax_t size = 0;
    for (auto i = root_dir.dir_entries.begin(); i != root_dir.dir_entries.end(); ++i) {
        size += i->get_total_size(cluster_size);
    }
    return size;
}

// Write the volume to the output file
void FS_specification::write_data()
{
    // Open the output file
    std::ofstream outfp(this->volume_name, std::ios::binary);
    if (outfp.fail()) {
        throw std::runtime_error(this->volume_name + ": " + std::strerror(errno));
    }

    // Round up root directory size to a multiple of the sector size
    unsigned entries_per_sector = this->sector_size / 32;
    this->root_dir_size += entries_per_sector - 1;
    this->root_dir_size -= this->root_dir_size % entries_per_sector;

    // Write the boot sector
    write_boot_sector(outfp);

    // For FAT32, write the other reserved sectors
    if (this->fs_type == fstype_fat32) {
        // Sector 1: info sector
        write_info_sector(outfp);
        // Sectors 2-5: empty
        write_zeros(outfp, this->sector_size * 4);
        // Sector 6: copy of boot sector
        write_boot_sector(outfp);
        // Sector 7: copy of info sector
        write_info_sector(outfp);
        // Sectors 8-31: empty
        write_zeros(outfp, this->sector_size * 24);
    }

    // Write the FAT, twice
    write_fat(outfp);
    write_fat(outfp);

    // For FAT12 and FAT16, the root directory has a dedicated area
    // For FAT32, a cluster chain is allocated for the root directory
    if (this->fs_type == fstype_fat32) {
        write_tree(outfp, this->root_dir);
    } else {
        write_root_dir(outfp);
        for (auto i = this->root_dir.dir_entries.cbegin();
             i != this->root_dir.dir_entries.cend(); ++i) {
            write_tree(outfp, *i);
        }
    }

    // Write free clusters up to the configured capacity
    auto num_clusters = (this->capacity + this->cluster_size - 1) / this->cluster_size;
    auto free_count = num_clusters - (this->fat.size() - 2);
    write_zeros(outfp, free_count * this->cluster_size);
}

// Write one copy of the boot sector
void FS_specification::write_boot_sector(std::ofstream& outfp)
{
    // A stub program to be run if something tries to boot this volume
    static const uint8_t boot_stub[] = {
        0xE8, 0x00, 0x00,               // CALL here # i.e. PUSH IP
                                        // here:
        0x5E,                           // POP SI
        0x83, 0xC6, 0x17,               // ADD SI,message - here
                                        // loop:
        0x2E, 0x8A, 0x04,               //     MOV AL,CS:[SI]
        0x08, 0xC0,                     //     OR AL,AL
        0x74, 0x0A,                     //     JZ end_loop
        0xB4, 0x0E,                     //     MOV AH,0x0E
        0xBB, 0x07, 0x00,               //     MOV BX,0x0007
        0xCD, 0x10,                     //     INT 0x10
        0x46,                           // INC SI
        0xEB, 0xEF,                     // JMP loop
                                        // end_loop:
                                        // wait:
        0xEB, 0xFE,                     // JMP wait
        // message:
        'T', 'h', 'i', 's', ' ', 'i', 's', ' ', 'n', 'o', 't', ' ', 'a', ' ',
        'b', 'o', 'o', 't', 'a', 'b', 'l', 'e', ' ', 'd', 'i', 's', 'k',
        '\r', '\0'
    };

    FS_boot_sector bsec;

    std::memset(&bsec, 0, sizeof(bsec));

    // Number of clusters
    auto num_clusters = (this->capacity + this->cluster_size - 1) / this->cluster_size;
    // Sectors before the first FAT
    unsigned reserved_sectors = (this->fs_type == fstype_fat32) ? 32 : 1; 
    // Sectors in each FAT
    std::uint32_t fat_sectors;
    if (this->fs_type == fstype_fat32) {
        auto fat_bytes = (num_clusters + 2) * 4;
        fat_sectors = (fat_bytes + this->sector_size - 1) / this->sector_size;
    } else {
        auto fat_bytes = (this->fs_type == fstype_fat16)
                       ? (num_clusters+2)*2
                       :((num_clusters+2)*3+1)/2;
        fat_sectors = (fat_bytes + this->sector_size - 1) / this->sector_size;
    }
    // Number of FATs
    unsigned num_fats = 2;
    // Number of sectors in the root directory area
    unsigned root_dir_sectors = (this->fs_type != fstype_fat32)
                              ? (this->root_dir_size*32 + this->sector_size - 1) / this->sector_size
                              : 0;
    // Number of sectors per cluster
    unsigned sectors_per_cluster = this->cluster_size / this->sector_size;
    // Number of sectors allocated for data
    auto num_data_sectors = num_clusters * sectors_per_cluster;
    // Total number of sectors in the volume
    auto num_sectors = reserved_sectors
                     + fat_sectors * num_fats
                     + root_dir_sectors
                     + num_data_sectors;

    bsec.BS_jmpBoot[0] = 0xEB; // JMP
    bsec.BS_jmpBoot[1] = sizeof(bsec) - 2; // offset
    bsec.BS_jmpBoot[2] = 0x90; // NOP
    std::memcpy(bsec.BS_OEMName, "MSWIN4.1", 8);
    bsec.BPB_BytsPerSec[0] = (uint8_t)(this->sector_size >> 0);
    bsec.BPB_BytsPerSec[1] = (uint8_t)(this->sector_size >> 8);
    bsec.BPB_SecPerClus = (uint8_t)sectors_per_cluster;
    bsec.BPB_RsvdSecCnt[0] = reserved_sectors;
    bsec.BPB_RsvdSecCnt[1] = 0;
    bsec.BPB_NumFats = num_fats;
    if (this->fs_type != fstype_fat32) {
        bsec.BPB_RootEntCnt[0] = (uint8_t)(this->root_dir_size >> 0);
        bsec.BPB_RootEntCnt[1] = (uint8_t)(this->root_dir_size >> 8);
    }
    if (num_sectors < 0x10000) {
        bsec.BPB_TotSec16[0] = (uint8_t)(num_sectors >> 0);
        bsec.BPB_TotSec16[1] = (uint8_t)(num_sectors >> 8);
    } else {
        bsec.BPB_TotSec32[0] = (uint8_t)(num_sectors >>  0);
        bsec.BPB_TotSec32[1] = (uint8_t)(num_sectors >>  8);
        bsec.BPB_TotSec32[2] = (uint8_t)(num_sectors >> 16);
        bsec.BPB_TotSec32[3] = (uint8_t)(num_sectors >> 24);
    }
    bsec.BPB_Media = 0xF8;
    std::time_t vol_id = time(nullptr);
    std::string label = this->label.empty()
                      ? "NO NAME     "
                      : (this->label + "          ").substr(0, 11);
    
    if (this->fs_type == fstype_fat32) {
        bsec.b32.BPB_FATSz32[0] = (uint8_t)(fat_sectors >>  0);
        bsec.b32.BPB_FATSz32[1] = (uint8_t)(fat_sectors >>  8);
        bsec.b32.BPB_FATSz32[2] = (uint8_t)(fat_sectors >> 16);
        bsec.b32.BPB_FATSz32[3] = (uint8_t)(fat_sectors >> 24);
        // bsec.b32.BPB_ExtFlags left set to 0: active FAT is #1, mirroring enabled
        // bsec.b32.BPB_FSVer left set to 0
        auto root_cluster = this->root_dir.first_cluster;
        bsec.b32.BPB_RootClus[0] = (uint8_t)(root_cluster >>  0);
        bsec.b32.BPB_RootClus[1] = (uint8_t)(root_cluster >>  8);
        bsec.b32.BPB_RootClus[2] = (uint8_t)(root_cluster >> 16);
        bsec.b32.BPB_RootClus[3] = (uint8_t)(root_cluster >> 24);
        bsec.b32.BPB_FSInfo[0] = 1;
        bsec.b32.BPB_FSInfo[1] = 0;
        bsec.b32.BPB_BkBootSec[0] = 6;
        bsec.b32.BPB_BkBootSec[1] = 0;
        // bsec.b32.BPB_Reserved left set to 0
        bsec.b32.BS_DrvNum = 0x80;
        // b32.BS_Reserved1 left set to 0
        bsec.b32.BS_BootSig = 0x29;
        bsec.b32.BS_VolID[0] = (uint8_t)(vol_id >>  0);
        bsec.b32.BS_VolID[1] = (uint8_t)(vol_id >>  8);
        bsec.b32.BS_VolID[2] = (uint8_t)(vol_id >> 16);
        bsec.b32.BS_VolID[3] = (uint8_t)(vol_id >> 24);
        std::memcpy(bsec.b32.BS_VolLab, label.c_str(), 11);
        std::memcpy(bsec.b32.BS_FilSysType, "FAT32   ", 8);
    } else {
        bsec.BPB_FATSz16[0] = (uint8_t)(fat_sectors >> 0);
        bsec.BPB_FATSz16[1] = (uint8_t)(fat_sectors >> 8);
        bsec.b16.BS_DrvNum = 0x80;
        // bsec.b16.BS_Reserved1 left set to 0
        bsec.b16.BS_BootSig = 0x29;
        bsec.b16.BS_VolID[0] = (uint8_t)(vol_id >>  0);
        bsec.b16.BS_VolID[1] = (uint8_t)(vol_id >>  8);
        bsec.b16.BS_VolID[2] = (uint8_t)(vol_id >> 16);
        bsec.b16.BS_VolID[3] = (uint8_t)(vol_id >> 24);
        std::memcpy(bsec.b16.BS_VolLab, label.c_str(), 11);
        std::memcpy(bsec.b16.BS_FilSysType,
                    this->fs_type == fstype_fat16 ? "FAT16   " : "FAT12   ",
                    8);
    }
    // bsec.BPB_SecPerTrk left set to 0
    // bsec.BPB_NumHeads left set to 0
    // bsec.BPB_HiddSec left set to 0

    // Write the boot parameters
    auto pos = outfp.tellp();
    outfp.write(reinterpret_cast<const char *>(&bsec), sizeof(bsec));

    // Write the boot sector stub program
    outfp.write(reinterpret_cast<const char *>(boot_stub), sizeof(boot_stub));

    // Write zeros to make 510 bytes
    write_zeros(outfp, 510 - (outfp.tellp() - pos));
    // Write the 55 AA signature
    outfp.put('\x55');
    outfp.put('\xAA');
    // Write zeros to make a sector
    write_zeros(outfp, this->sector_size - (outfp.tellp() - pos));
}

// Write one copy of the info sector
void FS_specification::write_info_sector(std::ofstream& outfp)
{
    FS_info_sector isec;

    std::memset(&isec, 0, sizeof(isec));
    std::memcpy(isec.FSI_LeadSig, "RRaA", 4);
    std::memcpy(isec.FSI_StrucSig, "rrAa", 4);
    auto num_clusters = (this->capacity + this->cluster_size - 1) / this->cluster_size;
    auto free_count = num_clusters - (this->fat.size() - 2);
    isec.FSI_Free_Count[0] = (uint8_t)(free_count >>  0);
    isec.FSI_Free_Count[1] = (uint8_t)(free_count >>  8);
    isec.FSI_Free_Count[2] = (uint8_t)(free_count >> 16);
    isec.FSI_Free_Count[3] = (uint8_t)(free_count >> 24);
    auto next_free = this->fat.size();
    isec.FSI_Nxt_Free[0] = (uint8_t)(next_free >>  0);
    isec.FSI_Nxt_Free[1] = (uint8_t)(next_free >>  8);
    isec.FSI_Nxt_Free[2] = (uint8_t)(next_free >> 16);
    isec.FSI_Nxt_Free[3] = (uint8_t)(next_free >> 14);
    isec.FSI_TrailSig[0] = 0x00;
    isec.FSI_TrailSig[1] = 0x00;
    isec.FSI_TrailSig[2] = 0x55;
    isec.FSI_TrailSig[3] = 0xAA;

    outfp.write(reinterpret_cast<const char *>(&isec), sizeof(isec));
    write_zeros(outfp, this->sector_size - sizeof(isec));
}

// Write one copy of the FAT
void FS_specification::write_fat(std::ofstream& outfp)
{
    std::uintmax_t pos = outfp.tellp();
    auto num_clusters = (this->capacity + this->cluster_size - 1) / this->cluster_size;
    std::uintmax_t fat_bytes;
    switch (this->fs_type) {
    case fstype_fat12:
        {
            fat_bytes = ((num_clusters+2)*3+1)/2;

            // Form three byte blocks, each containing two FAT entries
            uint8_t bytes[3];

            auto i = this->fat.cbegin();
            while (i != this->fat.cend()) {
                auto c = *(i++);
                bytes[0] = (uint8_t)(c >> 0);
                bytes[1] = (uint8_t)((c >> 8) & 0x0F);
                if (i == this->fat.cend()) {
                    // Odd cluster at end
                    outfp.write(reinterpret_cast<const char *>(bytes), 2);
                    break;
                }
                c = *(i++);
                bytes[1] |= (uint8_t)(c << 4);
                bytes[2] = (uint8_t)(c >> 4);
                outfp.write(reinterpret_cast<const char *>(bytes), 3);
            }
        }
        break;

    case fstype_fat16:
        {
            fat_bytes = num_clusters * 2;

            // Two bytes per FAT entry
            uint8_t bytes[2];
            for (auto i = this->fat.cbegin(); i != this->fat.cend(); ++i) {
                auto c = *i;
                bytes[0] = (uint8_t)(c >> 0);
                bytes[1] = (uint8_t)(c >> 8);
                outfp.write(reinterpret_cast<const char *>(bytes), 2);
            }
        }
        break;

    case fstype_fat32:
        {
            fat_bytes = num_clusters * 4;

            // Four bytes per FAT entry
            uint8_t bytes[4];
            for (auto i = this->fat.cbegin(); i != this->fat.cend(); ++i) {
                auto c = *i;
                bytes[0] = (uint8_t)(c >>  0);
                bytes[1] = (uint8_t)(c >>  8);
                bytes[2] = (uint8_t)(c >> 16);
                bytes[3] = (uint8_t)(c >> 24);
                outfp.write(reinterpret_cast<const char *>(bytes), 4);
            }
        }
        break;

    default:
        // This isn't supposed to happen
        throw std::runtime_error("File system type not set");
    }

    // Round fat_bytes up to sector size
    fat_bytes += this->sector_size - 1;
    fat_bytes -= fat_bytes % this->sector_size;

    // Fill with zeros
    write_zeros(outfp, pos + fat_bytes - outfp.tellp());
}

// Write the root directory to its special area
void FS_specification::write_root_dir(std::ofstream& outfp)
{
    auto pos = outfp.tellp();
    for (auto i = this->root_dir.directory.cbegin();
         i != this->root_dir.directory.cend(); ++i) {
        outfp.write(reinterpret_cast<const char *>(&(*i)), 32);
    }

    auto pos2 = outfp.tellp();
    auto size = this->root_dir_size * 32 + this->sector_size - 1;
    size -= size % this->sector_size;
    write_zeros(outfp, size - (pos2 - pos));
}

// Write a directory entry and all subdirectory contents
void FS_specification::write_tree(std::ofstream& outfp, FS_dir_entry const & entry)
{
    if (entry.attributes & FS_dir_entry::ATTR_DIRECTORY) {
        write_directory(outfp, entry);
        for (auto i = entry.dir_entries.cbegin();
             i != entry.dir_entries.cend(); ++i) {
            write_tree(outfp, *i);
        }
    } else {
        write_file(outfp, entry);
    }
}

// Write a directory (other than root on FAT12 or FAT16)
void FS_specification::write_directory(std::ofstream& outfp, FS_dir_entry const & entry)
{
    auto pos = outfp.tellp();
    for (auto i = entry.directory.cbegin(); i != entry.directory.cend(); ++i) {
        outfp.write(reinterpret_cast<const char *>(&(*i)), 32);
    }

    auto pos2 = outfp.tellp();
    auto extra = (pos2 - pos) % this->cluster_size;
    if (extra != 0) {
        write_zeros(outfp, this->cluster_size - extra);
    }
}

// Write a file
void FS_specification::write_file(std::ofstream& outfp, FS_dir_entry const & entry)
{
    auto pos = outfp.tellp();
    std::ifstream infp(entry.source, std::ios::binary);
    if (infp.fail()) {
        throw std::runtime_error(entry.source + ": " + std::strerror(errno));
    }

    while (true) {
        char buf[4096];

        infp.read(buf, sizeof(buf));
        auto count = infp.gcount();
        if (count == 0) {
            break;
        }
        outfp.write(buf, count);
    }

    infp.close();

    auto pos2 = outfp.tellp();
    auto extra = (pos2 - pos) % this->cluster_size;
    if (extra != 0) {
        write_zeros(outfp, this->cluster_size - extra);
    }
}

// Write a given number of zeros
void FS_specification::write_zeros(std::ofstream& outfp, std::uintmax_t count)
{
    static const char zeros[512] = "";

    std::uintmax_t i;
    for (i = 0; i+sizeof(zeros) < count; i += sizeof(zeros)) {
        outfp.write(zeros, sizeof(zeros));
    }
    outfp.write(zeros, count - i);
}

// Checksum for short names
static std::uint8_t short_name_checksum(const std::uint8_t *disk_name)
{
    std::uint8_t sum = 0;
    for (unsigned i = 0; i < 11; ++i) {
        sum = (std::uint8_t)(((sum << 7) | (sum >> 1)) + disk_name[i]);
    }
    return sum;
}

// Convert Unix time to DOS format
static DOS_time unix_to_dos_time(const timespec& utime)
{
    // Break time out into components
    auto comptime = std::localtime(&utime.tv_sec);
    DOS_time dostime;
    dostime.date = ((comptime->tm_year - 80) << 9)
                 | ((comptime->tm_mon + 1) << 5)
                 | comptime->tm_mday;
    dostime.time = (comptime->tm_hour << 11)
                 | (comptime->tm_min << 5)
                 | (comptime->tm_sec >> 1);
    dostime.msec = ((comptime->tm_sec & 1) ? 100 : 0)
                 + (utime.tv_nsec / 10000000);
    return dostime;
}

// Build the directory as it appears on disk
// Do the same for all subdirectories
void FS_dir_entry::fill_directory()
{
    static const std::regex short_name_regex(R">>([A-Z0-9$%'\-_@~`!(){}^%#&]{1,8}(?:.[A-Z0-9$%'\-_@~`!(){}^%#&]{1,3})?)>>");
    std::smatch m;
    std::unordered_set<std::string> short_names;

    // First pass: set file sizes of regular files, and call recursively for
    // directories
    for (auto i = dir_entries.begin(); i != dir_entries.end(); ++i) {
        if (!(i->attributes & FS_dir_entry::ATTR_DIRECTORY)) {
            // Regular file. Query the source file and set the size
            auto rc = stat(i->source.c_str(), &i->fst);
            if (rc < 0) {
                throw std::runtime_error(i->source + ": " + std::strerror(errno));
            }
            if (i->fst.st_size > UINT32_MAX) {
                throw std::runtime_error(i->source + ": File too large");
            }
            i->size = i->fst.st_size;
        } else {
            // Directory. Make recursive call to fill its entries
            i->fill_directory();
        }
    }

    // Second pass: identify entries with names that are acceptable short names
    for (auto i = dir_entries.begin(); i != dir_entries.end(); ++i) {
        // Build the possible short name by converting the long name to ASCII
        std::string short_name;
        for (std::size_t j = 0; j < i->long_name.size(); ++j) {
            auto ch = i->long_name[j];
            if (ch > 0x7F) {
                break; // Accept only ASCII characters
            }
            short_name += (char)ch;
        }
        // Name is acceptable if it contains only ASCII characters
        // and matches short_name_regex
        if (short_name.size() == i->long_name.size()
        &&  std::regex_match(short_name, m, short_name_regex)) {
            i->short_name = short_name;
            i->has_long_name = false;
            short_names.insert(short_name);
        }
    }

    // Third pass: generate short names for entries that don't yet have one
    for (auto i = dir_entries.begin(); i != dir_entries.end(); ++i) {
        if (i->short_name.empty()) {
            // Generate the basis name
            std::string name, ext;
            bool dot = false;
            for (std::size_t j = 0; j < i->long_name.size(); ++j) {
                auto ch = i->long_name[j];
                char ch8;
                if (ch < 0x80
                &&  (std::isalnum((char)ch) || std::strchr("$%'-_@~`!(){}^#&", (char)ch) != nullptr)) {
                    // Keep acceptable characters as they are, except convert
                    // to uppercase
                    ch8 = std::toupper((char)ch);
                } else if (ch == u' ') {
                    // Skip spaces
                    continue;
                } else if (ch == u'.') {
                    if (dot) {
                        // Convert second and subsequent period to '_'
                        ch = '_';
                    } else {
                        // Skip first period, and start building extension
                        dot = true;
                        continue;
                    }
                } else {
                    // Convert other characters to '_'
                    ch8 = '_';
                }
                if (!dot) {
                    if (name.size() < 8) {
                        name += ch8;
                    }
                } else {
                    if (ext.size() < 3) {
                        ext += ch8;
                    }
                }
            }
            // Avoid completely empty name
            if (name.empty()) {
                name = "_";
            }
            // Substitute tails until an unused name is found
            for (std::uint32_t num = 1; num <= 999999; ++num) {
                char tail[10];
                // Generate a tail: ~1, ~2, etc.
                std::snprintf(tail, sizeof(tail), "~%" PRIu32, num);
                std::size_t len = std::strlen(tail);
                // Build a candidate short name
                std::string trial_name;
                if (name.size() + len > 8) {
                    trial_name = name.substr(0, 8-len) + tail;
                } else {
                    trial_name = name + tail;
                }
                if (!ext.empty()) {
                    trial_name += "." + ext;
                }
                // If this name is not already in use, keep it
                if (short_names.find(trial_name) == short_names.end()) {
                    i->short_name = trial_name;
                    i->has_long_name = true;
                    short_names.insert(trial_name);
                    break;
                }
            }
            if (i->short_name.empty()) {
                // This should not happen unless there are a million names
                // in the directory
                throw std::runtime_error("Could not generate a short name");
            }
        }
    }

    // Build the . and .. entries
    FS_dir_record rec;
    if (!is_root) {
        // Directory other than root. Generate the . and .. entries.
        std::memset(&rec, 0, sizeof(rec));
        std::memset(rec.s.DIR_Name, ' ', sizeof(rec.s.DIR_Name));
        rec.s.DIR_Attr = FS_dir_entry::ATTR_DIRECTORY;
        timespec utime;
        clock_gettime(CLOCK_REALTIME, &utime);
        auto dostime = unix_to_dos_time(utime);
        rec.s.DIR_CrtTimeTenth = dostime.msec;
        rec.s.DIR_CrtTime[0] = (std::uint8_t)(dostime.time >> 0);
        rec.s.DIR_CrtTime[1] = (std::uint8_t)(dostime.time >> 8);
        rec.s.DIR_CrtDate[0] = (std::uint8_t)(dostime.date >> 0);
        rec.s.DIR_CrtDate[1] = (std::uint8_t)(dostime.date >> 8);
        rec.s.DIR_AccDate[0] = rec.s.DIR_CrtDate[0];
        rec.s.DIR_AccDate[1] = rec.s.DIR_CrtDate[1];
        rec.s.DIR_WrtTime[0] = rec.s.DIR_CrtTime[0];
        rec.s.DIR_WrtTime[1] = rec.s.DIR_CrtTime[1];
        rec.s.DIR_WrtDate[0] = rec.s.DIR_CrtDate[0];
        rec.s.DIR_WrtDate[1] = rec.s.DIR_CrtDate[1];
        rec.s.DIR_Name[0] = '.';
        directory.push_back(rec);
        rec.s.DIR_Name[1] = '.';
        directory.push_back(rec);
    }

    // Fourth pass: build the directory records
    for (auto i = dir_entries.begin(); i != dir_entries.end(); ++i) {
        // Short name as recorded on disk
        uint8_t disk_name[11];
        std::memset(disk_name, ' ', sizeof(disk_name));
        std::size_t dot = i->short_name.find('.');
        if (dot == std::string::npos) {
            // Short name has no extension
            assert(i->short_name.size() <= sizeof(disk_name));
            std::memcpy(disk_name, i->short_name.c_str(), i->short_name.size());
        } else {
            // Short name has an extension
            assert(dot <= sizeof(disk_name));
            std::memcpy(disk_name+0, i->short_name.c_str(), dot);
            auto ext = i->short_name.substr(dot+1);
            assert(ext.size() <= 3);
            std::memcpy(disk_name+8, ext.c_str(), ext.size());
        }

        if (i->has_long_name) {
            // Create records for the short name
            std::uint8_t cksum = short_name_checksum(disk_name);
            std::list<FS_dir_record> records;
            unsigned rec_num = 1;
            for (unsigned j = 0; j < i->long_name.size(); j += 13) {
                // Each long name record holds 13 characters
                char16_t name_seg[13];
                if (i->long_name.size() - j >= 13) {
                    for (unsigned k = 0; k < 13; ++k) {
                        name_seg[k] = i->long_name[j+k];
                    }
                } else {
                    std::memset(name_seg, 0xFF, sizeof(name_seg));
                    for (unsigned k = 0; j+k < i->long_name.size(); ++k) {
                        name_seg[k] = i->long_name[j+k];
                    }
                    name_seg[i->long_name.size()-j] = 0;
                }
                // Build one record
                rec.l.LDIR_Ord = rec_num;
                for (unsigned k = 0; k < 5; ++k) {
                    rec.l.LDIR_Name1[k][0] = (std::uint8_t)(name_seg[k+0] & 0xFF);
                    rec.l.LDIR_Name1[k][1] = (std::uint8_t)(name_seg[k+0] >> 8);
                }
                for (unsigned k = 0; k < 6; ++k) {
                    rec.l.LDIR_Name2[k][0] = (std::uint8_t)(name_seg[k+5] & 0xFF);
                    rec.l.LDIR_Name2[k][1] = (std::uint8_t)(name_seg[k+5] >> 8);
                }
                for (unsigned k = 0; k < 2; ++k) {
                    rec.l.LDIR_Name3[k][0] = (std::uint8_t)(name_seg[k+11] & 0xFF);
                    rec.l.LDIR_Name3[k][1] = (std::uint8_t)(name_seg[k+11] >> 8);
                }
                rec.l.LDIR_Attr = FS_dir_entry::ATTR_LONG_NAME;
                rec.l.LDIR_Type = 0;
                rec.l.LDIR_Chksum = cksum;
                rec.l.LDIR_FstClusLO[0] = 0;
                rec.l.LDIR_FstClusLO[1] = 0;
                records.push_back(rec);
                ++rec_num;
            }
            // Mark the last record
            records.rbegin()->l.LDIR_Ord |= 0x40;
            // Add to the directory in the reverse of the order created
            for (auto i = records.crbegin(); i != records.crend(); ++i) {
                this->directory.push_back(*i);
            }
        }
        // Create a record for the short name
        i->dir_index = directory.size();
        std::memset(&rec, 0, sizeof(rec));
        std::memcpy(rec.s.DIR_Name, disk_name, 11);
        rec.s.DIR_Attr = i->attributes;
        auto dostime = unix_to_dos_time(i->fst.st_ctim);
        rec.s.DIR_CrtTimeTenth = dostime.msec;
        rec.s.DIR_CrtTime[0] = (std::uint8_t)(dostime.time >> 0);
        rec.s.DIR_CrtTime[1] = (std::uint8_t)(dostime.time >> 8);
        rec.s.DIR_CrtDate[0] = (std::uint8_t)(dostime.date >> 0);
        rec.s.DIR_CrtDate[1] = (std::uint8_t)(dostime.date >> 8);
        dostime = unix_to_dos_time(i->fst.st_atim);
        rec.s.DIR_AccDate[0] = (std::uint8_t)(dostime.date >> 0);
        rec.s.DIR_AccDate[1] = (std::uint8_t)(dostime.date >> 8);
        dostime = unix_to_dos_time(i->fst.st_mtim);
        rec.s.DIR_WrtTime[0] = (std::uint8_t)(dostime.time >> 0);
        rec.s.DIR_WrtTime[1] = (std::uint8_t)(dostime.time >> 8);
        rec.s.DIR_WrtDate[0] = (std::uint8_t)(dostime.date >> 0);
        rec.s.DIR_WrtDate[1] = (std::uint8_t)(dostime.date >> 8);
        if (!(i->attributes & FS_dir_entry::ATTR_DIRECTORY)) {
            rec.s.DIR_FileSize[0] = (std::uint8_t)(i->size >>  0);
            rec.s.DIR_FileSize[1] = (std::uint8_t)(i->size >>  8);
            rec.s.DIR_FileSize[2] = (std::uint8_t)(i->size >> 16);
            rec.s.DIR_FileSize[3] = (std::uint8_t)(i->size >> 24);
        }
        directory.push_back(rec);
    }
    this->size = directory.size() * 32;
}

// Return the size of this file or directory in clusters
// If the entry is a directory, include the contents
std::uintmax_t FS_dir_entry::get_total_size(unsigned cluster_size)
{
    std::uintmax_t size = (this->size + cluster_size - 1) / cluster_size;
    if (this->attributes & FS_dir_entry::ATTR_DIRECTORY) {
        for (auto i = this->dir_entries.begin(); i != this->dir_entries.end(); ++i) {
            size += i->get_total_size(cluster_size);
        }
    }
    return size;
}

int
main(int argc, char **argv)
{
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <specification-file>\n";
        return EXIT_FAILURE;
    }

    FS_specification spec;
    bool ok = spec.parse(argv[1]);
    if (!ok) {
        return EXIT_FAILURE;
    }
    try {
        spec.fill_directory();
        spec.set_params();
        spec.allocate_clusters();
        spec.write_data();

        return EXIT_SUCCESS;
    }
    catch (std::runtime_error& err) {
        std::cerr << err.what() << "\n";
        return EXIT_FAILURE;
    }
}
