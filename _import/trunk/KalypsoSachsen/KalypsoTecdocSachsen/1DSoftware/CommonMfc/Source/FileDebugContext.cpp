// FileDebugContext.cpp: Implementierung der Klasse CFileDebugContext.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "FileDebugContext.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

CFileDebugContext::CHelpMap CFileDebugContext::theContexts;

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CFileDebugContext::CFileDebugContext( CFile* pFile ) : CDumpContext( pFile )
{
  m_pFile = pFile;
}

CFileDebugContext::~CFileDebugContext()
{
  m_pFile->Close();
  delete m_pFile;
}

/* static */ CFileDebugContext* CFileDebugContext::CreateInstance( const CString& name, const CString& fileName )
// Erzeugt einen neuen Context
// Parameter:
//        const CString& name: der Name des neuen Contextes
//        const CString& fileName: der Name der Datei, in welchen die Ausgabe erfolgt; es wird 
//                            versucht diese Datei zu erzeugen
// R�ckgabewert:
//        CFileDebugContext*: der neue Context; falls bereits ein Context mit diesem Namen existiert,
//                            wird der alte zur�ckgegeben
//                            Falls die Datei nicht ge�ffnet werden kann, wird null zur�ckgegeben
{
  // falls schon ein Context mit diesem Namen existiert, diesen zur�ckgeben
  CFileDebugContext* newContext = GetInstance( name );
  if( newContext != NULL )
    return newContext;

  // jetzt versuchen die Datei zu erzeugen
  CFile* pFile = new CFile();
  if( !pFile->Open( fileName, CFile::modeCreate | CFile::modeWrite | CFile::shareDenyWrite ) )
  {
    // falls dies nicht gelingt, das CFile zerst�ren und NULL zur�ckgeben
    delete pFile;
    return NULL;
  };

  // jetzt einen neuen Context erzeugen und in die Liste aufnehmen
  // das CFile Objekt wird von dessen Konstruktor zerst�rt
  newContext = new CFileDebugContext( pFile );
  theContexts.SetAt( name, newContext );

  return newContext;
}; // CreateInstance

/*static */ CFileDebugContext* CFileDebugContext::GetInstance( const CString& name )
// gibt einen vorhandenen Context anhand des Namens zur�ck
// Parameter:
//        const CString& name: der Name des gesuchten Contexts
// R�ckgabewert:
//        CFileDebugContext: der gefundenen Context, sonst NULL
{
  CFileDebugContext* foundContext;

  if( theContexts.Lookup( name, foundContext ) )
    return foundContext;
  else
    return NULL;
}; // GetInstance

/* static */ BOOL CFileDebugContext::DestroyInstance( const CString& name )
//  Zerst�rt einen bestimmten Context
// Parameter:
//      const CString& name: der Name des zu zerst�renden Contextes
{
  CFileDebugContext* deleteContext;
  if( theContexts.Lookup( name, deleteContext ) )
  {
    delete deleteContext;
    theContexts.RemoveKey( name );
    return TRUE;
  }
  else
    return FALSE; // context nicht gefunden
}; // DestroyContext
