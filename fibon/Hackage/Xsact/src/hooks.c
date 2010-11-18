/* after a tip from David Roundy */
#include <Rts.h>
#include <RtsFlags.h>
#include <unistd.h>

const char * id = "$Rev: 650 $ $Date$";

void defaultsHook (void) {
  RtsFlags.GcFlags.maxStkSize  =  8*10000002 / sizeof(W_); /* 80M */

#ifdef _SC_PHYS_PAGES
  unsigned long long pagesize = sysconf(_SC_PAGESIZE);
  unsigned long long numpages = sysconf(_SC_PHYS_PAGES);
  unsigned long long mhs = numpages*pagesize*9/10;
  RtsFlags.GcFlags.maxHeapSize = 1ULL+mhs/BLOCK_SIZE_W;
#endif
}
