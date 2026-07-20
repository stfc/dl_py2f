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
static const char *tag = "dl_util";
class DL_F2PY {
    const char *dl_py2f_get_dl_symbols(const struct link_map *lm) {
        const ElfW(Addr)  load_addr = lm->l_addr;
        const char       *strtab    = NULL;
        const ElfW(Sym)  *symtab    = NULL;
        const uint32_t   *gnuhash   = NULL;
        const uint32_t   *elfhash   = NULL;
        for (const ElfW(Dyn) *dyn = lm->l_ld; dyn->d_tag != DT_NULL; dyn++) {
            ElfW(Addr) ptr = dyn->d_un.d_ptr;
            if (ptr < load_addr)
                ptr += load_addr;
            switch (dyn->d_tag) {
                case DT_STRTAB  : strtab  = (const char *)      ptr; break;
                case DT_SYMTAB  : symtab  = (const ElfW(Sym) *) ptr; break;
                case DT_GNU_HASH: gnuhash = (const uint32_t *)  ptr; break;
                case DT_HASH    : elfhash = (const uint32_t *)  ptr; break;
            }
        }
        static std::string sbuff;
        sbuff = "";
        if (strtab == NULL || symtab == NULL)
            return sbuff.c_str();
        size_t nsyms = 0;
        if (elfhash != NULL) {
            nsyms = elfhash[1];
        } else if (gnuhash != NULL) {
            const uint32_t  nbuckets  = gnuhash[0];
            const uint32_t  symoffset = gnuhash[1];
            const uint32_t  bloomsize = gnuhash[2];
            const uint32_t *buckets   = gnuhash + 4 + bloomsize*(sizeof(ElfW(Addr))/sizeof(uint32_t));
            const uint32_t *chains    = buckets + nbuckets;
            uint32_t last = 0;
            for (uint32_t ibucket = 0; ibucket < nbuckets; ibucket++)
                if (buckets[ibucket] > last)
                    last = buckets[ibucket];
            if (last >= symoffset) {
                while (!(chains[last-symoffset] & 1))
                    last++;
                nsyms = last + 1;
            } else {
                nsyms = symoffset;
            }
        }
        std::string sep = ";";
        const char *type;
        for (size_t symidx = 1; nsyms == 0 || symidx < nsyms; symidx++) {
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
        struct link_map *lm;
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
