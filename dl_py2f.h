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
