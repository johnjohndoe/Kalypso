#include "vectorHelper.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

void BCE::Util::dumpBitVector( std::vector<bool>& bitset, std::ostream& out )
{
  out << "Bitset:\n";
  for( std::vector<bool>::const_iterator bIt = bitset.begin(); bIt != bitset.end(); bIt++ )
    out << *bIt << std::endl;
  out << std::endl;
}

