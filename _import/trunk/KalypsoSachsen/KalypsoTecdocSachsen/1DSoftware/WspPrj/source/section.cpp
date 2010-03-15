#include "stdafx.h"

#include "commonMfc\include\helper.h"

#include "project.h"
#include "profil.h"
#include "state.h"

#include "section.h"

  ////////////////////////////
  //  Klasse  Section
  ///////////////////////////

/* The Default Constructor */
Section::Section(int nClassType, Project* pProject)
{
	m_nClassType = nClassType;
	m_pProject = pProject;
	m_profil = NULL;
	m_nProfilNr = 0;
  m_bModified = FALSE;
}

Section::~Section()
{
	FlushProfil();
}

BOOL Section::LoadProfil()
{
	CString filename, path;

	filename = GetFileName();
	if ( filename.IsEmpty() )
		return FALSE;
  if ( m_profil )
    return TRUE;  // Profil bereits geladen
	if (m_pProject!=NULL)
	{
		if (m_nClassType==CLASS_TYPE_CSECTION)
			path = m_pProject->GetDataDir();
		else
			path = m_pProject->GetCalcDir();
	}
	path += filename;
	m_profil = new Profil(this);
	if (!m_profil->Load(path))
	{
		delete m_profil;
		m_profil = NULL;
		return FALSE;
	}

  m_bModified = FALSE;
	return TRUE;
}

BOOL Section::RemoveFile()
{
  if( m_pProject != NULL )
  {
    CString profPath = m_pProject->GetDataDir() + GetFileName();
    return CHelper::DeleteFileToDustbin( profPath );
  }
  else
    return FALSE;
}; // RemoveFile

BOOL Section::SaveProfil()
{
	CString filename, path;

	filename = GetFileName();
	if( filename.IsEmpty() || m_profil == NULL ) 
		return FALSE;

	if( m_nClassType == CLASS_TYPE_CSECTION ) 
		path = m_pProject->GetDataDir();
	else
		path = m_pProject->GetCalcDir();
	path += filename;
	if( !m_profil->Save( path ) ) 
	{
    CString rString = "Problem 2 ist aufgetreten !";
    AfxMessageBox(rString, MB_ERROR);
    return FALSE;
	}
  
  m_bModified = FALSE;
	return TRUE;
} // SaveProfil

void Section::SetProfil(Profil* prof)
{
	FlushProfil();
	m_profil = prof;
  SetModified();
}

void Section::FlushProfil()
{
	if (m_profil!=NULL)
	{
		delete m_profil;
		m_profil = NULL;
	}
}

void Section::SetFileName( CString& file, BOOL bSetOriginalFile /* = FALSE */ )
// setzt den Dateinamen unter welchem das Profil abgespeichert wird
// Parameter:
//        CString& file: der Dateiname
//        BOOL bSetOriginalFile: falls TRUE, wird der Originaldateiname des Profils auf diesen Wertr gesetzt
{
	int i = file.Find('.');
	if( i == -1 )
	{
		m_fileTitle = file;
		m_fileExt.Empty();
	}
	else
	{
		m_fileTitle = file.Left( i );
		m_fileExt = file.Right( file.GetLength() - i - 1 );
	}; // for i
	SetStatesModified();
  SetModified();

  Profil* profil = GetProfil();
  if( bSetOriginalFile && profil )
    profil->SetOriginalFile( file );
}; // SetFileName

CString Section::GetFileName()
{
	if (m_fileExt.IsEmpty())
		return m_fileTitle;
	else
		return m_fileTitle + '.' + m_fileExt;
}

int Section::GetClassType()
{
	return m_nClassType;
}

CString Section::GetFileTitle()
{
	return m_fileTitle;
}

CString Section::GetFileExt()
{
	return m_fileExt;
}

Profil* Section::GetProfil()
{
	return m_profil;
}

int Section::GetProfilNr()
{
	return m_nProfilNr;
}

void Section::SetProfilNr(int n)
{
	m_nProfilNr = n;
	if (m_profil!=NULL)
		m_profil->SetNum(n);
	SetStatesModified();
  SetModified();
}

double Section::GetStation()
{
	return 0;
}

void Section::GetVZK(CString& vzk)
{
	vzk.Empty();
}

/***************************************************************/
// State implementation

void Section::AddState( State* st )
{
  POSITION pos = m_States.GetHeadPosition();
	while( pos )
	{
		State* pSt = m_States.GetNext( pos );
    if( pSt == st )
      return;
	}; // while pos
	m_States.AddTail( st );
  SetModified();
}; // AddState

void Section::RemoveState( State* st )
{
  POSITION pos = m_States.Find( st );
  if( pos )
  {
	  m_States.RemoveAt( pos );
	  SetModified();
  }; // if pos
}; // RemoveState

State* Section::GetFirstState()
{
	m_StatePos = m_States.GetHeadPosition();
	return GetNextState();
}

State* Section::GetNextState()
{
	if (m_StatePos==NULL)
		return NULL;
	else
		return m_States.GetNext(m_StatePos);
}

void Section::SetStatesModified()
{
	POSITION pos;
	State *st;

	pos = m_States.GetHeadPosition();
	while (pos!=NULL)
	{
		st = m_States.GetNext(pos);
		st->SetModified();
	}
}

void Section::SetModified()
{
  m_bModified = TRUE;
  if ( m_pProject )
    m_pProject->SetModified();
}; // SetModified

void Section::RegisterEventHandle( CWnd* eventHandle )
// fügt einen EventHandle zu der Liste der EventHandle hinzu
// Parameter:
//        CWnd* eventHandle: der hinzuzufügende EventHandle
{
  m_eventHandles.Add( eventHandle );
}; // RegisterEventHandler

BOOL Section::UnregisterEventHandle( CWnd* eventHandle )
// löscht einen EventHandle aus der Liste der EventHandle
// Parameter:
//        CWnd* eventHandle: der zu löschende Handle
// Rückgabewert:
//        BOOL: TRUE, falls der Handle gelöscht wurde
//              FALSE, falls der Handle nicht in der Liste ist
// Bemerkung:
//        kommt der Handle öfter in der Liste vor, wird er nur einmal gelöscht
{
  for( int i = 0; i < m_eventHandles.GetSize(); i++ )
  {
    if ( m_eventHandles[i] == eventHandle )
    {
      m_eventHandles.RemoveAt( i );
      return TRUE;
    };
  }; // for i

  return FALSE; // handle nicht gefunden
}; // UnregisterEventHandle

void Section::NotifiyEventHandles( DWORD event )
// benachrichtigt alle EventHandles von einem Ereignis
// Parameter:
//        DWORD event: das zu berichtende Ereignis
{
  for( int i = 0; i < m_eventHandles.GetSize(); i++ )
  {
    CWnd* eventHandle = m_eventHandles[i];
    eventHandle->SendMessage( 0, 0, 0 );
  }; // for i
}; // NotifiyEventHandles
