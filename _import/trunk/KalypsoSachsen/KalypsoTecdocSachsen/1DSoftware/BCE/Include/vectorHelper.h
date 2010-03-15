#if !defined(_VECTOR_HERLPER_H_INCLUDED_)
#define _VECTOR_HERLPER_H_INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <vector>
#include <ostream>

namespace BCE
{
  namespace Util
  {
    void dumpBitVector( std::vector<bool>&, std::ostream& );
  }
}

#endif // !defined(  )