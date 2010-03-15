// Observable.h: Schnittstelle für die Klasse Observable.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_OBSERVABLE_H__8372F803_0BF3_11D6_BED4_00104BB3E525__INCLUDED_)
#define AFX_OBSERVABLE_H__8372F803_0BF3_11D6_BED4_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <string>

// Konstanten für Oberservable

// Stati
#define OBS_STATE_UNKNOWN 0   // Unbekannter Zustand: das zu observierende Verhalten hat noch nicht begonnen
#define OBS_STATE_RUNNING 1   // es kann jetzt obersviert werden
#define OBS_STATE_FINISHED 2  // aslles fertig -> es gibt einen ReturnCode

// Return Codes: alles abgeleiteten Klassen sollten Code ab UNKNOWN verwenden
#define OBS_RETURN_UNKNOWN 0; // noch nicht fertig: undefiniert

namespace BCE
{
  class Observable
  {
    // Konstruktor 
  public:
    Observable();
    // Attribute
  private:
    int m_obsState;
    std::string m_obsText;
    unsigned int m_obsMaxProcess;
    unsigned int m_obsProcess;
    int m_obsReturnCode;
    
    
  protected:
    void setObsState( const int state ) { m_obsState = state; };
    void setObsText( const std::string& text ) { m_obsText = text; };
    void setObsMaxProcess( const unsigned int max ) { m_obsMaxProcess = max; };
    void setObsReturnCode( const int code ) { m_obsReturnCode = code; };
    void stepObsProcess() { m_obsProcess++; };
    
    // die öffentliche Schnittstele
  public:
    static unsigned int runAction( void* pParam );

    int getObsState() { return m_obsState; };
    const char* getObsText() { return m_obsText.c_str(); };
    
    unsigned int getObsMaxProcess() { return m_obsMaxProcess; };
    unsigned int getObsProcess() { return m_obsProcess; };
    
    int getObsReturnCode() { return m_obsReturnCode; };
   
    

  }; // class Obersvable
}; // namespace BCE
#endif // !defined(AFX_OBSERVABLE_H__8372F803_0BF3_11D6_BED4_00104BB3E525__INCLUDED_)
