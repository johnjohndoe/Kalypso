// hier werden die statischen Members von Observable definiert

#include "observable.h"

using namespace BCE;

Observable::Observable()
{
  m_obsState = OBS_STATE_UNKNOWN;
  
  m_obsProcess = 0;
  m_obsMaxProcess = 100; // default: 100 ( Prozent )
  
  m_obsReturnCode = OBS_RETURN_UNKNOWN;
}; // Konstruktor

unsigned int Observable::runAction( void* pParam )
{
  return OBS_RETURN_UNKNOWN;
};  // muss vom abgeleiteten Objekt implementiert werden
