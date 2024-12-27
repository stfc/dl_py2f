
// you.lu@stfc.ac.uk
// March 2024

#include <link.h>
#include <dl_py2f_link.h>
#include <cstdio>
#include <string.h>
#include <bits/stdc++.h> 
using namespace std; 
//#include <dl-hash.h>

// from sysdeps/generic/ldsodefs.h
static inline bool dl_relocate_ld(const struct dl_py2f_link_map *lm) {
    /* Don't relocate dynamic section if it is readonly  */
    return !(lm->l_ld_readonly || DL_RO_DYN_SECTION);
}

// a very important macro from sysdeps/generic/ldsodefs.h
#define D_PTR(map, i) ((map)->i->d_un.d_ptr + (dl_relocate_ld (map) ? 0 : (map)->l_addr))

static const char *tag = "dl_util";


// DT_GNU_HASH function (Bloom filter) from elf/dl-lookup.c, because DT_HASH is deprecated
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
        const char       *strtab    = (const void *) D_PTR (lm, l_info[DT_STRTAB]);
        const ElfW(Sym)  *symtab    = (const void *) D_PTR (lm, l_info[DT_SYMTAB]);
    
        const char *undef_name = " ";
        const uint_fast32_t gnu_hash = dl_gnu_hash(undef_name);

        // YL 20/03/2024: we must make this static because std::string is volatile and
        //                destroyed by returning sbuff.c_str(); if we want this function
        //                to be called in multiple threads in the future we will need
        //                ctypes.create_string_buffer in Python
        static std::string sbuff;
        // YL 23/03/2024: this mustn't be `static std::string sbuff = "";`!
        sbuff = "";
        std::string sep = ";";
    
        // some helper variables
        Elf_Symndx symidx;
        const char *type;
    
// DEBUGGING ONLY
//        printf("Dynamic Sections:\n");
//        printf("|%-18s|%16s|\n", "Tag", "Ptr");
//        int counter = 0;
//        const ElfW(Dyn) *const dyn_start = lm->l_ld;
//        for (const ElfW(Dyn) *dyn = dyn_start; dyn->d_tag != DT_NULL; ++dyn) {
//            counter++;
//            if ( dyn->d_tag == DT_STRTAB || dyn->d_tag == DT_SYMTAB || dyn->d_tag == DT_HASH || dyn->d_tag == DT_GNU_HASH )
//            printf("|%-18s|%16lx|%16p|\n",
//                   dyn_tag_to_name(dyn->d_tag),
//                   dyn->d_un.d_val,
//                   (const void *) (dyn->d_un.d_ptr));
//        }
// END DEBUGGING
    
        // see: elf/dl-lookup.c
        const ElfW(Addr) *bitmask = lm->l_gnu_bitmask;
        // 27/12/2024: we currently do the same for the following if and else
        if (__glibc_likely (bitmask != NULL)) {
    
            // TODO in glibc it only does check_match() if this condition is met but here it's never met
            ElfW(Addr) bitmask_word = bitmask[(gnu_hash/__ELF_NATIVE_CLASS) & lm->l_gnu_bitmask_idxbits];
            unsigned int hashbit1 = gnu_hash & (__ELF_NATIVE_CLASS - 1);
            unsigned int hashbit2 = ((gnu_hash >> lm->l_gnu_shift) & (__ELF_NATIVE_CLASS - 1));
            if (__glibc_unlikely ((bitmask_word >> hashbit1) & (bitmask_word >> hashbit2) & 1)) {
            }
    
            // TODO a symbol is placed into hash%nbuckets
            Elf32_Word bucket = lm->l_gnu_buckets[gnu_hash%lm->l_nbuckets];
            // here we get the information lm->l_nbuckets and lm->l_gnu_shift which could be very useful but we'll
            // keep it for future since we don't search in the hash table but only get a full list of symbols here;
            // this article is very helpful to understanding the algorithm:
            // https://flapenguin.me/elf-dt-gnu-hash
            // get the hash table
            const Elf32_Word *hasharr = &lm->l_gnu_chain_zero[bucket];
            // in the original elf/dl-lookup.c, check_match() is only performed if (((*hasharr^gnu_hash)>>1)==0)
            // and symidx is obtained by ELF_MACHINE_HASH_SYMIDX(lm,hasharr) while ((*hasharr++&1u)==0)
    
            // here we only loop until hitting the wall
            for (symidx=1; &symtab[symidx] != NULL; symidx++) {
                const ElfW(Sym) *const sym = &symtab[symidx];
                type = (ELFW(ST_BIND) (sym->st_info) == STB_WEAK)       ? "STB_WEAK":
                       (ELFW(ST_BIND) (sym->st_info) == STB_GLOBAL)     ? "STB_GLOBAL" :
                       (ELFW(ST_BIND) (sym->st_info) == STB_GNU_UNIQUE) ? "STB_GNU_UNIQUE" : NULL;
                if (type != NULL) {
                    // it seems symbols are useless when sym->st_value is 0
                    if (ELFW(ST_BIND) (sym->st_info) != STB_WEAK && sym->st_value != 0) {
                        sbuff += strtab + sym->st_name + sep;


// DEBUGGING ONLY
//                        printf("%5d: %s\n", symidx, strtab + sym->st_name);
//                        cout<<strtab+sym->st_name<<endl;
// END DEBUGGING
                    }
                // end of list of symbols
                } else {
                    break;
                }
            }
    
        } else {
    
// TODO old DT_HASH is deprecated
//            unsigned long int *old_hash;
//            *old_hash = _dl_elf_hash("YOUR_KEYWORD");
//            for (symidx = lm->l_buckets[*old_hash%lm->l_nbuckets];
//                 symidx != STN_UNDEF;
//                 symidx = lm->l_chain[symidx]) {
//                // TODO: do something here in case we need it one day
//            }
    
            /* Use the old SysV-style hash table.  Search the appropriate
               hash bucket in this object's symbol table for a definition
               for the same symbol name.  */
            for (symidx=1; &symtab[symidx] != NULL; symidx++) {
                const ElfW(Sym) *const sym = &symtab[symidx];
                type = (ELFW(ST_BIND) (sym->st_info) == STB_WEAK)       ? "STB_WEAK":
                       (ELFW(ST_BIND) (sym->st_info) == STB_GLOBAL)     ? "STB_GLOBAL" :
                       (ELFW(ST_BIND) (sym->st_info) == STB_GNU_UNIQUE) ? "STB_GNU_UNIQUE" : NULL;
                if (type != NULL) {
                    // it seems symbols are useless when sym->st_value is 0
                    if (ELFW(ST_BIND) (sym->st_info) != STB_WEAK && sym->st_value != 0) {
                        sbuff += strtab + sym->st_name + sep;


// DEBUGGING ONLY
//                        printf("%5d: %s\n", symidx, strtab + sym->st_name);
//                        cout<<strtab+sym->st_name<<endl;
// END DEBUGGING
                    }
                // end of list of symbols
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
    
        // load .so shared libraray
        handle = dlopen(full_path_to_lib, RTLD_LAZY);
        if (!handle) {
            printf("Error[%s]: %s\n", tag, dlerror());
            return NULL;
        }
    
        // load the link map structure into lm
        struct dl_py2f_link_map *lm;
        int ld_ret = dlinfo(handle, RTLD_DI_LINKMAP, &lm);
        if (ld_ret) {
            printf("Error[%s]: %s\n", tag, dlerror());
            fflush(stdout);
            return NULL;
        }
    
// DEBUG only
        printf("\n >>> DL_PY2F: lm->l_name = %s\n", lm->l_name);
        const char *cbuff = dl_py2f_get_dl_symbols(lm);
        fflush(stdout);
    
        // TODO we could use this loop to inspect all loaded shared libs
        do {
            if (!strcmp(full_path_to_lib, lm->l_name)) {
    //            dl_py2f_get_dl_symbols(lm);
            }
            lm = lm->l_next;
        } while (lm != NULL);
    
        return cbuff;

    }

// YL NB: this ";" is vital!!
};

// we need this C/C++ interface because Python talks to only C
extern "C" {
    DL_F2PY *dl_f2py() {
        return new DL_F2PY();
    }
    const char *getSymbols(DL_F2PY *dl_f2py, const char *full_path_to_lib) {
        return dl_f2py->parse_dlso(full_path_to_lib);
    }
}
