#include <stddef.h>
typedef ptrdiff_t index_type;
typedef struct descriptor_dimension
{
  index_type _stride;
  index_type lower_bound;
  index_type _ubound;
}
descriptor_dimension;
typedef struct dtype_type
{
  size_t elem_len;
  int version;
  signed char rank;
  signed char type;
  signed short attribute;
}
dtype_type;
#define GFC_ARRAY_DESCRIPTOR(type) \
struct {\
  type *base_addr;\
  size_t offset;\
  dtype_type dtype;\
  index_type span;\
  descriptor_dimension dim[];\
}
typedef GFC_ARRAY_DESCRIPTOR (void) gfc_array_void;
#define GFC_DESCRIPTOR_DATA(desc) ((desc)->base_addr)
#define ELFW(type)  _ElfW (ELF, __ELF_NATIVE_CLASS, type)
# define ELF_MACHINE_HASH_SYMIDX(map, hasharr) \
  ((hasharr) - (map)->l_gnu_chain_zero)
#define DT_THISPROCNUM        0
#define DL_FIXUP_VALUE_TYPE ElfW(Addr)
#define DL_RO_DYN_SECTION 0
struct r_file_id
  {
  };
struct link_map_machine
  {
  };
struct link_map;
struct r_scope_elem
{
  struct link_map **r_list;
  unsigned int r_nlist;
};
struct r_search_path_struct
  {
    struct r_search_path_elem **dirs;
    int malloced;
  };
struct dl_py2f_link_map
  {
    ElfW(Addr) l_addr;		
    char *l_name;		
    ElfW(Dyn) *l_ld;		
    struct dl_py2f_link_map *l_next, *l_prev; 
    struct link_map *l_real;
    Lmid_t l_ns;
    struct libname_list *l_libname;
    ElfW(Dyn) *l_info[DT_NUM + DT_THISPROCNUM + DT_VERSIONTAGNUM
		      + DT_EXTRANUM + DT_VALNUM + DT_ADDRNUM];
    const ElfW(Phdr) *l_phdr;	
    ElfW(Addr) l_entry;		
    ElfW(Half) l_phnum;		
    ElfW(Half) l_ldnum;		
    struct r_scope_elem l_searchlist;
    struct r_scope_elem l_symbolic_searchlist;
    struct link_map *l_loader;
    struct r_found_version *l_versions;
    unsigned int l_nversions;
    Elf_Symndx l_nbuckets;
    Elf32_Word l_gnu_bitmask_idxbits;
    Elf32_Word l_gnu_shift;
    const ElfW(Addr) *l_gnu_bitmask;
    union
    {
      const Elf32_Word *l_gnu_buckets;
      const Elf_Symndx *l_chain;
    };
    union
    {
      const Elf32_Word *l_gnu_chain_zero;
      const Elf_Symndx *l_buckets;
    };
    unsigned int l_direct_opencount; 
    enum			
      {
	lt_executable,		
	lt_library,		
	lt_loaded		
      } l_type:2;
    unsigned int l_relocated:1;	
    unsigned int l_init_called:1; 
    unsigned int l_global:1;	
    unsigned int l_reserved:2;	
    unsigned int l_main_map:1;  
    unsigned int l_visited:1;   
    unsigned int l_map_used:1;  
    unsigned int l_map_done:1;  
    unsigned int l_phdr_allocated:1; 
    unsigned int l_soname_added:1; 
    unsigned int l_faked:1;	
    unsigned int l_need_tls_init:1; 
    unsigned int l_auditing:1;	
    unsigned int l_audit_any_plt:1; 
    unsigned int l_removed:1;	
    unsigned int l_contiguous:1; 
    unsigned int l_symbolic_in_local_scope:1; 
    unsigned int l_free_initfini:1; 
    unsigned int l_ld_readonly:1; 
    unsigned int l_find_object_processed:1; 
    bool l_nodelete_active;
    bool l_nodelete_pending;
    unsigned int l_1_needed;
    struct r_search_path_struct l_rpath_dirs;
    struct reloc_result
    {
      DL_FIXUP_VALUE_TYPE addr;
      struct link_map *bound;
      unsigned int boundndx;
      uint32_t enterexit;
      unsigned int flags;
      unsigned int init;
    } *l_reloc_result;
    ElfW(Versym) *l_versyms;
    const char *l_origin;
    ElfW(Addr) l_map_start, l_map_end;
    ElfW(Addr) l_text_end;
    struct r_scope_elem *l_scope_mem[4];
    size_t l_scope_max;
    struct r_scope_elem **l_scope;
    struct r_scope_elem *l_local_scope[2];
    struct r_file_id l_file_id;
    struct r_search_path_struct l_runpath_dirs;
    struct link_map **l_initfini;
    struct link_map_reldeps
      {
	unsigned int act;
	struct link_map *list[];
      } *l_reldeps;
    unsigned int l_reldepsmax;
    unsigned int l_used;
    ElfW(Word) l_feature_1;
    ElfW(Word) l_flags_1;
    ElfW(Word) l_flags;
    int l_idx;
    struct link_map_machine l_mach;
    struct
    {
      const ElfW(Sym) *sym;
      int type_class;
      struct link_map *value;
      const ElfW(Sym) *ret;
    } l_lookup_cache;
    void *l_tls_initimage;
    size_t l_tls_initimage_size;
    size_t l_tls_blocksize;
    size_t l_tls_align;
    size_t l_tls_firstbyte_offset;
#ifndef NO_TLS_OFFSET
# define NO_TLS_OFFSET	0
#endif
#ifndef FORCED_DYNAMIC_TLS_OFFSET
# if NO_TLS_OFFSET == 0
#  define FORCED_DYNAMIC_TLS_OFFSET -1
# elif NO_TLS_OFFSET == -1
#  define FORCED_DYNAMIC_TLS_OFFSET -2
# else
#  error "FORCED_DYNAMIC_TLS_OFFSET is not defined"
# endif
#endif
    ptrdiff_t l_tls_offset;
    size_t l_tls_modid;
    size_t l_tls_dtor_count;
    ElfW(Addr) l_relro_addr;
    size_t l_relro_size;
    unsigned long long int l_serial;
  };
