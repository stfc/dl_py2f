//  Copyright (C) 2025 The authors of DL_PY2F
//
//  This file is part of DL_PY2F.
//
//  DL_PY2F is free software: you can redistribute it and/or modify
//  it under the terms of the GNU Lesser General Public License as
//  published by the Free Software Foundation, either version 3 of the
//  License, or (at your option) any later version.
//
//  DL_PY2F is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//  GNU Lesser General Public License for more details.
//
//  You should have received a copy of the GNU Lesser General Public
//  License along with DL_PY2F. If not, see
//  <https://www.gnu.org/licenses/>.

// you.lu@stfc.ac.uk
#include <link.h>
#include <dl_py2f.h>
#include <cstdio>
#include <string.h>
#include <bits/stdc++.h> 
#include <fcntl.h>
using namespace std; 
static inline bool dl_relocate_ld(const struct dl_py2f_link_map *lm) {
    return !(lm->l_ld_readonly || DL_RO_DYN_SECTION);
}
#define D_PTR(map, i) ((map)->i->d_un.d_ptr + (dl_relocate_ld (map) ? 0 : (map)->l_addr))
static const char *tag = "dl_util";
static uint_fast32_t dl_gnu_hash(const char *s) {
    uint_fast32_t h = 5381;
    for (unsigned char c = *s; c != '\0'; c = *++s)
      h = h * 33 + c;
    return h & 0xffffffff;
}
class DL_F2PY {
    const char *dyn_tag_to_name(ElfW(Sxword) d_tag) {
        switch (d_tag) {
            case DT_STRTAB  : return("DT_STRTAB");
            case DT_SYMTAB  : return("DT_SYMTAB");
            case DT_GNU_HASH: return("DT_GNU_HASH");
            case DT_HASH    : return("DT_HASH");
            default: return("UNKNOWN");
        }
    }
    const char *dl_py2f_get_dl_symbols(const dl_py2f_link_map *lm) {
        const ElfW(Addr)  load_addr = lm->l_addr;
        const char       *strtab    = (const char *) D_PTR(lm, l_info[DT_STRTAB]);
        const ElfW(Sym)  *symtab    = (const ElfW(Sym) *) D_PTR(lm, l_info[DT_SYMTAB]);
        const char *undef_name = " ";
        const uint_fast32_t gnu_hash = dl_gnu_hash(undef_name);
        static std::string sbuff;
        sbuff = "";
        std::string sep = ";";
        Elf_Symndx symidx;
        const char *type;
        const ElfW(Addr) *bitmask = lm->l_gnu_bitmask;
        if (__glibc_likely (bitmask != NULL)) {
            ElfW(Addr) bitmask_word = bitmask[(gnu_hash/__ELF_NATIVE_CLASS) & lm->l_gnu_bitmask_idxbits];
            unsigned int hashbit1 = gnu_hash & (__ELF_NATIVE_CLASS - 1);
            unsigned int hashbit2 = ((gnu_hash >> lm->l_gnu_shift) & (__ELF_NATIVE_CLASS - 1));
            if (__glibc_unlikely ((bitmask_word >> hashbit1) & (bitmask_word >> hashbit2) & 1)) {
            }
            Elf32_Word bucket = lm->l_gnu_buckets[gnu_hash%lm->l_nbuckets];
            const Elf32_Word *hasharr = &lm->l_gnu_chain_zero[bucket];
            for (symidx=1; &symtab[symidx] != NULL; symidx++) {
                const ElfW(Sym) *const sym = &symtab[symidx];
                type = (ELFW(ST_BIND) (sym->st_info) == STB_WEAK)       ? "STB_WEAK":
                       (ELFW(ST_BIND) (sym->st_info) == STB_GLOBAL)     ? "STB_GLOBAL" :
                       (ELFW(ST_BIND) (sym->st_info) == STB_GNU_UNIQUE) ? "STB_GNU_UNIQUE" : NULL;
                if (type != NULL) {
                    if (ELFW(ST_BIND) (sym->st_info) != STB_WEAK && sym->st_value != 0) {
                        sbuff += strtab + sym->st_name + sep;
                    }
                } else {
                    break;
                }
            }
        } else {
            for (symidx=1; &symtab[symidx] != NULL; symidx++) {
                const ElfW(Sym) *const sym = &symtab[symidx];
                type = (ELFW(ST_BIND) (sym->st_info) == STB_WEAK)       ? "STB_WEAK":
                       (ELFW(ST_BIND) (sym->st_info) == STB_GLOBAL)     ? "STB_GLOBAL" :
                       (ELFW(ST_BIND) (sym->st_info) == STB_GNU_UNIQUE) ? "STB_GNU_UNIQUE" : NULL;
                if (type != NULL) {
                    if (ELFW(ST_BIND) (sym->st_info) != STB_WEAK && sym->st_value != 0) {
                        sbuff += strtab + sym->st_name + sep;
                    }
                } else {
                    break;
                }
            }
        }
        const char *cbuff = sbuff.c_str();
        return cbuff;
    }
    public:
    const char *parse_dlso(const char *full_path_to_lib) {
        void *handle;
        handle = dlopen(full_path_to_lib, RTLD_LAZY);
        if (!handle) {
            printf("Error[%s]: %s\n", tag, dlerror());
            return NULL;
        }
        struct dl_py2f_link_map *lm;
        int ld_ret = dlinfo(handle, RTLD_DI_LINKMAP, &lm);
        if (ld_ret) {
            printf("Error[%s]: %s\n", tag, dlerror());
            fflush(stdout);
            return NULL;
        }
        fflush(stdout);
        const char *cbuff = dl_py2f_get_dl_symbols(lm);
        do {
            if (!strcmp(full_path_to_lib, lm->l_name)) {
            }
            lm = lm->l_next;
        } while (lm != NULL);
        return cbuff;
    }
};
void ouch(int sig)
{
    printf("\n### OUCH! - I got signal %d\n", sig);
}
extern "C" {
    DL_F2PY *dl_f2py() {
        return new DL_F2PY();
    }
    const char *getSymbols(DL_F2PY *dl_f2py, const char *full_path_to_lib) {
        return dl_f2py->parse_dlso(full_path_to_lib);
    }
    int associated(unsigned long pointer) {
        size_t PS = sysconf(_SC_PAGESIZE);
        long int boundary = pointer - pointer%PS;
        void *addr = (void*) pointer;
        if (!addr) {
            return 999;
        }
        int nullfd = open("/dev/random", O_WRONLY);
        if (write(nullfd, addr, 8) < 0) {
            close(nullfd);
            return 999;
        }
        close(nullfd);
        return 0;
    }
}
