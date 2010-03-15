#ifndef _TINCUT_EXCEPTIONS_H_INCLUDED_
#define _TINCUT_EXCEPTIONS_H_INCLUDED_

#include <exception>

namespace BCE
{
  // Diese Klasse stellt eine neue Art von Exceptions für TinCut zur Verfügung
  // Die Exception merkt sich einen Fehler-Code
  // Author: Belger, 16.1.2002
  class TinCutException : public exception
  {
  private:
    int m_errorCode;

  public:
    TinCutException( int errorCode )
    {
      m_errorCode = errorCode;
    };

    ~TinCutException() {};

    int getErrorCode() const { return m_errorCode; };
  }; // class TinCutException
}; // namespace BCE

#endif // _TINCUT_EXCEPTIONS_H_INCLUDED_
