#include "stdafx.h"

#include "giostr.h"

  ////////////////////////////
  //  Klasse  gifstream
  ///////////////////////////

gifstream::gifstream() : ifstream(), m_lSetter( "English" )
{
}

gifstream::~gifstream()
{
}

  ////////////////////////////
  //  Klasse  gofstream
  ///////////////////////////

gofstream::gofstream() : ofstream(), m_lSetter( "English" )
{
}

gofstream::gofstream( const char* s, std::ios_base::openmode which ) : ofstream( s, which ), m_lSetter( "English" )
{
}

gofstream::~gofstream()
{
}

LanguageSetter::LanguageSetter( const char* locale ) : m_oldLang( setlocale( LC_NUMERIC, NULL ) ) 
{
  setlocale( LC_NUMERIC, locale );
};

LanguageSetter::~LanguageSetter()
{
  setlocale( LC_NUMERIC, m_oldLang );
}