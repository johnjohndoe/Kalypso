// Giostr.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef GIOSTR_H
#define GIOSTR_H

#include <fstream>

class LanguageSetter
{
private:
  CString m_oldLang;

public:
  LanguageSetter( const char* locale );
  ~LanguageSetter();
};

class gifstream : public ifstream
{
public:
  gifstream::gifstream();
   ~gifstream();
   
private:
  /** Change the locale for numeric values to English.
   *  This ensures that decimals in files use the symbol '.'
   *  and not the symbol for the locale (e.g. ',' in Germany!)
   */
  LanguageSetter m_lSetter;
};

class gofstream : public ofstream
{
public:
  gofstream();
  gofstream( const char *s, std::ios_base::openmode which = std::ios_base::out | std::ios_base::trunc );
  ~gofstream();
  
private:
  /** siehe gifstream */
  LanguageSetter m_lSetter;
};

#endif // GIOSTR_H