#include <math.h>

double po( double rd )
{ 
  return (0.5 + 0.5 * erf ((rd / 1.04) / (double) sqrt( (double) 2.0 )));
}

main() {
  printf( "%.16E\n", po(2.0) );
}
