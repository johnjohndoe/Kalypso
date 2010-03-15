// Log4Config.cpp: Implementierung der Klasse CLog4Config.
//
//////////////////////////////////////////////////////////////////////

//nur im DEBUG - Modus verwenden
#ifdef _DEBUG

#include <fstream>
#include <string>

#include "stdafx.h"

#include "log4cpp/category.hh"
#include "log4cpp/priority.hh"
#include "log4cpp/OstreamAppender.hh"
#include "log4cpp/FileAppender.hh"

#include "Log4Config.h"

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CLog4Config CLog4Config::theLog4Config; // das einzig existierende CLog4ConfigObject

/* static */
CLog4Config::CLog4Config()
{
  using namespace log4cpp;


  std::string testName( "C:/tmp/gernot.log" );

  try{ CFile::Remove( "C:/tmp/gernot.log" ); }
  catch( CFileException* e ) 
  {
    TRACE( "Log4U-Logfile could not be deleted\n" ); 
    e->Delete();
  };

  FileAppender* pTestApp = new FileAppender( "Test", testName );

  Category::getRoot().addAppender( pTestApp );
  Category::setRootPriority( Priority::DEBUG );
  
  //Category::getInstance( "CMultiDoc" ).addAppender( pTestFA );
  //Category::getInstance( "CMultiDoc" ).setPriority( Priority::DEBUG );
}



#endif // _DEBUG