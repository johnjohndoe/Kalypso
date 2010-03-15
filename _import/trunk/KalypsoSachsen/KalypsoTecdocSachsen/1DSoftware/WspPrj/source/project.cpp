#pragma warning(disable:4786)
#pragma warning(disable:4503)

#include "stdafx.h"

#include "bce/include/WSPFeatures.h"
#include "commonMfc/include/messageBox2.h"

#include "csection.h"
#include "state.h"
#include "giostr.h"
#include "calc.h"
#include "profil.h"
#include "coord.h"
#include "outflow.h"
#include "lsection.h"
#include "calcdata.h"
#include "3dcoord.h"
#include "datablck.h"

#include "project.h"


  ////////////////////////////
  //  Klasse  Project
  ///////////////////////////

// Standardkonstruktor der Klasse Projekt
// Parameter:
//        const CString& dir: Das Projektverzeichnis
// Bemerkung:
//        der Name des Projekts wird zuerst standardmässig auf den Namen des Projektverzeichnisses gesetzt
Project::Project( const CString& dir, const ProjectType& type /* = undefined */ )
{
	m_dir = dir;
  m_projectType = undefined;

  int index = m_dir.ReverseFind( '\\' );
  if ( index != -1 )
    m_name = m_dir.Mid( index + 1 );

  m_StatePos = NULL;
	m_CSectionPos = NULL;
	m_bModified = TRUE;
  m_bIsLoaded = FALSE;
	m_nNextState = 1;
}

Project::~Project()
{
	POSITION pos;
	CrossSection* cs;
	State* st;

	pos = m_CrossSections.GetHeadPosition();
	while (pos!=NULL)
	{
		cs = m_CrossSections.GetNext(pos);
		delete cs;
    cs = NULL;
	}
	m_CrossSections.RemoveAll();
	pos = m_States.GetHeadPosition();
	while (pos!=NULL)
	{
		st = m_States.GetNext(pos);
		delete st;
    st = NULL;
	}
	m_States.RemoveAll();
}

BOOL Project::Load()
{
  if( m_bIsLoaded )
    return TRUE;

  CWaitCursor wait;

	gifstream ifs;
	CString rString, filename;

	CFileStatus rStatus;
	BOOL bOK = FALSE;

	if( m_dir.IsEmpty() )
		return FALSE;

  LoadProjectName();

  // Read Zustaende
	filename = m_dir + "\\prof\\wsp.cfg";
  if( CFile::GetStatus( filename, rStatus ) )
	{
		ifs.open(filename, ios::in);
		if (!ifs.fail())
		{
			ifs >> *this;
			ifs.close();
			bOK = TRUE;
		}
	}

	if( !bOK )
	{
    rString.FormatMessage("Konnte Datei %1 nicht zum lesen öffnen.", filename);
    AfxMessageBox(rString, MB_ERROR);
		return FALSE;
	}

	// Read ProfProj.txt
	LoadProfProj();
	m_bModified = FALSE;
  m_bIsLoaded = TRUE;
	return TRUE;
}

BOOL Project::Save()
{
	if( m_dir.IsEmpty() )
		return FALSE;

  CString probezName = m_dir + "\\prof\\probez.txt";
  try
  {
    // Save project name
    gofstream probezGofs( probezName, ios::out );
    probezGofs << m_name ;
    probezGofs.close();
  }
  catch(...)
  {
    AfxMessageBox("Error writing " + probezName, MB_ERROR );
  }

  // Save Zustaende info
  CString wspName = m_dir + "\\prof\\wsp.cfg";
  try
  {
    gofstream wspGofs( wspName, ios::out );
    wspGofs << *this;
    wspGofs.close();
  }
  catch(...)
  {
    AfxMessageBox( "Error writing " + wspName, MB_ERROR );
  }

  // Save ProfProj.txt
	SaveProfProj();
  
	m_bModified = FALSE;

	return TRUE;
}

void Project::LoadProfProj()
{
	gifstream ifs;
	CString str, filename;
	char buffer[LINE_SIZE];
	int i;
	CFile file;
	CFileStatus rStatus;

	filename = m_dir + "\\prof\\profproj.txt";
	if (file.GetStatus(filename, rStatus))
	{
		ifs.open(filename, ios::in);
		if (!ifs.fail())
		{
			int n1, n2;
			CrossSection *cs;
			CTypedPtrList<CObList, CrossSection*> cslist;
			POSITION pos;
			
			n1 = n2 = 0;
			ifs >> n1 >> n2; 
			ifs.getline(buffer, LINE_SIZE, '\n');
			for (i=0; i<n1; i++)
			{
				cs = new CrossSection(this);
				// Gewaessername
				ifs.get(buffer, 9+1, '\n');
				str = buffer;
				str.TrimLeft();
				str.TrimRight();
				cs->SetWaterName(str);
				// Station
				ifs.get(buffer, 10+1, '\n');
				cs->SetStation(atof(buffer));
				// PK
				ifs.get(buffer, 9+1, '\n');
				str = buffer;
				str.TrimLeft();
				str.TrimRight();
				cs->SetPK(str);
				// VZK
				ifs.get(buffer, 5+1, '\n');
				cs->SetVZK(atoi(buffer));
				// Zustand
				ifs.get(buffer, 11+1, '\n');
				str = buffer;
				str.TrimLeft();
				str.TrimRight();
				cs->SetStateName(str);
				// Datei
				ifs.getline(buffer, LINE_SIZE, '\n');
				str = buffer;
				str.TrimLeft();
				str.TrimRight();
				cs->SetFileName(str);
				cslist.AddTail(cs);
			}
			ifs.getline(buffer, LINE_SIZE, '\n');
			for (i=0; i<n2; i++)
			{
				CrossSection *csec = NULL;
				State *st;
				CString file1, file2, str;
				POSITION oldpos;
				
				// Profildatei
				ifs.get(buffer, 12+1, '\n');
				file1 = buffer;
				file1.TrimLeft();
				file1.TrimRight();
				// Zustandsdatei
				ifs.getline(buffer, LINE_SIZE, '\n');
				file2 = buffer;
				file2.TrimLeft();
				file2.TrimRight();
				
				st = GetState(file2);
				if (st==NULL)
				{	// state doesn't exist!!!
					continue;
				}
				pos = cslist.GetHeadPosition();
				while (pos!=NULL)
				{
					oldpos = pos;
					cs = cslist.GetNext(pos);
					str = cs->GetFileName();
					if (file1.CompareNoCase(str)==0)
					{
						csec = cs;
						break;
					}
				}
				if (csec==NULL)
				{	// cross section not in state!!!
					continue;
				}
				if (st->FindCrossSection(file1))
				{	// cross section already in project
					delete csec;
					cslist.RemoveAt(oldpos);
				}
			}
			// remaining cross sections are in project but not in any state
			pos = cslist.GetHeadPosition();
			while (pos!=NULL)
			{
				cs = cslist.GetNext(pos);
				AddCrossSection(cs);
			}
			ifs.close();
		}
	}
}

void Project::SaveProfProj()
{
	gofstream ofs;
	CString filename;

	filename = m_dir + "\\prof\\profproj.txt";
	ofs.open(filename, ios::out);
	if (!ofs.fail())
	{
		int n1, n2;
		CrossSection *cs;
		State *st;
		POSITION pos;
		CString str;

		n1 = m_CrossSections.GetCount();
		n2 = 0;
		pos = m_States.GetHeadPosition();
		while (pos!=NULL)
		{
			st = m_States.GetNext(pos);
			n2 += st->GetNumCrossSections();
		}
		ofs.setf(ios::left);
		ofs << setw(5) << n1 << setw(5) << n2 << endl;
		pos = m_CrossSections.GetHeadPosition();
		while (pos!=NULL)
		{
			cs = m_CrossSections.GetNext(pos);
			str = cs->GetWaterName();
			ofs << setw(8) << str << " ";
			ofs.precision(4);
			ofs.setf(ios::right);
			ofs << setw(9) << cs->GetStation() << " ";
			str = cs->GetPK();
			ofs << setw(8) << str << " ";
			ofs << setw(4) << cs->GetVZK() << " ";
			str = cs->GetStateName();
			ofs << setw(10) << str << " ";
			str = cs->GetFileName();
			ofs << setw(12) << str << endl;
		}
		ofs << endl;
		pos = m_States.GetHeadPosition();
		while (pos!=NULL)
		{
			CString file;

			st = m_States.GetNext(pos);
			file = st->GetFileName();
			cs = st->GetFirstCrossSection();
			while (cs!=NULL)
			{
				str = cs->GetFileName();
				ofs << setw(12) << str << " " << setw(12) << file << endl;
				cs = st->GetNextCrossSection();
			}
		}
		ofs.close();
	}
}

CString Project::GetName() const
{
	return m_name;
}

CString Project::GetDir() const
{
	return m_dir;
}

CString Project::GetCalcDir() const
{
	return m_dir + "\\dath\\";
}

CString Project::GetDataDir() const
{
	return m_dir + "\\prof\\";
}

CString Project::GetMapDir() const
{
  return m_dir + "\\map\\";
};

CString Project::GetPlotQPDir() const
{
  return m_dir + "\\Plot_qp\\";
};

CString Project::GetPlotLSDir() const
{
  return m_dir + "\\Plot_ls\\";
};

int Project::GetResultFileType(CString& filename)
{
	CString str;
	int i;

	if (filename.GetLength()>=4)
	{
		str = filename.Mid(2, 2);
		str.MakeLower();
		for (i=0; i<N_RESULT_TYPES; i++)
		{
			CString comp;

			comp = szResultTypes[i][0];
			comp += szResultTypes[i][1];
			if (str==comp)
				return i;
		}
	}
	return RESULT_TYPE_NONE;
}

BOOL Project::FileIsOEM(CString& filename)
{
	int i;

	i = GetResultFileType(filename);
	switch (i)
	{
		default:
			return FALSE;

		case RESULT_TYPE_NONE:
			return FALSE;

		case RESULT_TYPE_ER:
		case RESULT_TYPE_TB:
		case RESULT_TYPE_WK:
		case RESULT_TYPE_UE:
		case RESULT_TYPE_MA:
		case RESULT_TYPE_EX:
		case RESULT_TYPE_PR:
		case RESULT_TYPE_VG:
			return TRUE;
	}
}

int Project::GetWaterCount()
{
	return m_waters.GetSize();
}

CString Project::GetWaterName(int n)
{
	if (n >= 0 && n < m_waters.GetSize())
		return m_waters[n];
	else
		return "";
}

void Project::SetName( const CString& name )
{
	m_name = name;
	m_bModified = TRUE;
}

void Project::SetModified()
{
	m_bModified = TRUE;
}

istream& operator>>( istream& is, Project &proj )
{
	// Read WSP.CFG file
	char buffer[LINE_SIZE];
	int i, j, nprof, nst;
	CString str;
	State* st;
	BOOL bFound;

	// 1. Zeile
	is >> nprof >> nst >> proj.m_nNextState;
	proj.m_nNextState++;

	is.getline( buffer, LINE_SIZE, '\n' );	// remove trash and end of line
  CString projektType( buffer );
  projektType.TrimLeft();

  if( projektType.GetLength() > 0 )
  {
    switch( projektType[0] )
    {
    case 'b':
      proj.m_projectType = Project::BCE;
      break;

    case 'l':
      proj.m_projectType = Project::LWA;
      break;

    default:
      proj.m_projectType = Project::undefined;
      break;
    }; // switch
  }
  else
    proj.m_projectType = Project::undefined;

	// folgende Zeile
	for( i = 0; i < nst && !is.eof(); i++ )
	{
		st = new State( &proj );
		// Gewaessername
		is.get(buffer, 16, '\n');
		str = buffer;
		str.TrimLeft();
		str.TrimRight();
		st->SetWaterName( str );
		bFound = FALSE;
		for ( j = 0; j < proj.m_waters.GetSize(); j++ )
		{
			if ( proj.m_waters[j] == str )
			{
				bFound = TRUE;
				break;
			}
		}
		if ( !bFound )
			proj.m_waters.Add(str);
		
    // Zustandname
		is.get(buffer, 16, '\n');
		str = buffer;
		str.TrimLeft();
		str.TrimRight();
		st->SetName( str );
		// Datum
		is.get( buffer, 11, '\n' );
		str = buffer;
		str.TrimLeft();
		str.TrimRight();
		st->SetDate(str);
		// Anfangsprofile
		is.get( buffer, 16, '\n' );
		str = buffer;
		str.TrimLeft();
		str.TrimRight();
		st->SetStartStation(atof(str));
		// Endprofile
		is.get( buffer, 16, '\n' );
		str = buffer;
		str.TrimLeft();
		str.TrimRight();
		st->SetEndStation(atof(str));
		// Dateiname
		is.getline(buffer, LINE_SIZE, '\n');
		str = buffer;
		str.TrimLeft();
		str.TrimRight();
		st->SetFileName(str);
		proj.m_States.AddTail(st);
		st->Load();
	}

	return is;
}

ostream& operator<<(ostream& os, Project &proj)
{
	// Save WSP.CFG file	
	
	// 1. Zeile
	os << setw(5) << proj.m_CrossSections.GetCount();
	os << setw(5) << proj.m_States.GetCount();
	os << setw(5) << proj.m_nNextState - 1;

  switch( proj.GetProjectType() )
  {
  case Project::LWA:
    os << " l";
    break;

  case Project::BCE:
    os << " b";
    break;

  default:
    break;
  }
  os << endl;
	
  // folgende Zeilen
	POSITION pos = proj.m_States.GetHeadPosition();
	while( pos )
	{
		State* st = proj.m_States.GetNext(pos);
		os.setf(ios::left);
		os << setw(15) << st->GetWaterName();
		os << setw(15) << st->GetName();
		os << setw(10) << st->GetDateString();
    CString str;
    str.Format( "%15.4lf", st->GetStartStation() );
    os << str;

    str.Format( "%15.4lf   ", st->GetEndStation() );
    os << str;
		os << st->GetFileName() << endl;
		st->Save();
	}
	return os;
}


/* static */
Project::ProjectType Project::AskCalcModel()
{
	bool bDefault = !WSPFeatures::Instance()->isEnabled("WSPWIN","wsp_ignore_default_proj");
	bool bLwaProj = WSPFeatures::Instance()->isEnabled("WSPWIN","wsp_default_lwa");

  if( bDefault )
    return bLwaProj == true ? Project::LWA : Project::BCE;
  else
  {
    CString ("").TrimLeft();
    CMessageBox2 dlg( CString( MAKEINTRESOURCE( IDS_RECHENKERN_CHOOSE_TITLE ) ), CString( MAKEINTRESOURCE( IDS_RECHENKERN_CHOOSE_TEXT ) ), "Pasche", "Knauf", !bLwaProj );
    switch( dlg.DoModal() )
    {
    case IDOK:
      return Project::BCE;

    case IDCANCEL:
      return Project::LWA;

    default:
      return Project::undefined;
    }
  };

} // AskCalcModel
    


/***************************************************************/
// State implementation

void Project::AddState(State* st)
{
	BOOL bFound;
	int j;

	st->SetNumber(m_nNextState++);
	m_States.AddTail(st);
	m_bModified = TRUE;
	bFound = FALSE;
	for (j=0; j<m_waters.GetSize(); j++)
	{
		if (m_waters[j] == st->GetWaterName())
		{
			bFound = TRUE;
			break;
		}
	}
	if (!bFound)
		m_waters.Add(st->GetWaterName());
}

State* Project::GetFirstState()
{
	m_StatePos = m_States.GetHeadPosition();
	return GetNextState();
}

State* Project::GetNextState()
{
	if (m_StatePos==NULL)
		return NULL;
	else
		return m_States.GetNext(m_StatePos);
}

State* Project::GetState( const CString& file )
{
	POSITION pos;
	State *st;
	CString str;

	pos = m_States.GetHeadPosition();
	while (pos!=NULL)
	{
		st = m_States.GetNext(pos);
		str = st->GetFileName();
		if (file.CompareNoCase(str)==0)
			return st;
	}

	return NULL;
}

/***************************************************************/
// CrossSection implementation

void Project::AddCrossSection( CrossSection* cs )
{
	if( m_CrossSections.Find( cs ) == NULL )
		m_CrossSections.AddTail( cs );
}; // AddCrossSection

CrossSection* Project::AddCrossSection( Profil* profil, const CString& waterName, const CString& stateName,
                                        const double station, const CString& pk, const int VZK )
// Erstellt eine neue CrossSection aus einem Profil und initialisiert alle Daten entsprechend
// Parameter:
//        Profil* profil: das neue Profil
// Rückgabewert:
//        CrossSection*: Referenz auf die neue CrossSection, NULL bei Fehler
{
  if( !profil )
    return NULL;

  // gleich die neue CrossSection erstellen
  CrossSection* newCs = new CrossSection( this );
  
  // zuerst das Profil setzten
  newCs->SetProfil( profil );

  profil->SetOwner( newCs );

  // die neue Crosssection initialisieren
  newCs->SetProfilNr( GetCrossSectionCount() + 1 );
  newCs->SetWaterName( waterName );
  newCs->SetStateName( stateName );
  newCs->SetStation( station );
  newCs->SetPK( pk );
  newCs->SetVZK( VZK );

  // und auch den Dateinamen setzen
  newCs->CreateFileName( NULL );

  // zuletzt die CrossSection auch tatsächlich hinzufügen
  AddCrossSection( newCs );

  return newCs;
}; // AddCrossSection

BOOL Project::RemoveCrossSection( CrossSection* cs )
// löscht die CrossSection aus der Liste, falls sie in keinem Zustand mehr existiert
// Rückgabewert:
//          TRUE, falls die CrossSection gelöscht wurde ( wird von keinem Zustand mehr Referenziert ), sonst FALSE
{
  POSITION pos = m_States.GetHeadPosition();
  while( pos != NULL )
  {
    State* st = m_States.GetNext( pos );
    if( st->CrossSectionExists( cs ) )
      return FALSE; // falls die CS in einem der States ist, nichts tun
  }

  // sonst aus der Liste nehmen
  pos = m_CrossSections.Find( cs );
  if( pos == NULL )
    return FALSE;

  m_CrossSections.RemoveAt( pos );
  return TRUE;
} // RemoveCrossSection

CrossSection* Project::GetFirstCrossSection()
{
	m_CSectionPos = m_CrossSections.GetHeadPosition();
	return GetNextCrossSection();
}

CrossSection* Project::GetNextCrossSection()
{
	if (m_CSectionPos==NULL)
		return NULL;
	else
		return m_CrossSections.GetNext(m_CSectionPos);
}

CrossSection* Project::FindCrossSection(CString& file)
{
	POSITION pos;
	CrossSection *cs;
	CString str;

	pos = m_CrossSections.GetHeadPosition();
	while (pos!=NULL)
	{
		cs = m_CrossSections.GetNext(pos);
		str = cs->GetFileName();
		if (file.CompareNoCase(str)==0)
			return cs;
	}
	return NULL;
}

int Project::GetCrossSectionCount()
{
	return m_CrossSections.GetCount();
}

BOOL Project::KeyExists(double station, int vzk, CString& pk)
{
	CrossSection *cs;
	POSITION pos;
	CString cpk;
	int cvzk;

	pos = m_CrossSections.GetHeadPosition();
	while (pos!=NULL)
	{
		cs = m_CrossSections.GetNext(pos);
		cvzk = cs->GetVZK();
		cpk = cs->GetPK();
		if (cs->GetStation()==station && cvzk==vzk && cpk==pk)
			return TRUE;
	}

	return FALSE;
}

State* Project::FindStateByName( const CString& stateName )
// sucht einen Zustand anhand des Namens
// Parameter:
//        const CString& stateName: der Name des gesuchten Zustandes
// Rückgabewert:
//        State*: der gefundene Zustand oder NULL
{
  POSITION pos = m_States.GetHeadPosition();
  while( pos )
  {
    State* state = m_States.GetNext( pos );
    if( state && state->GetName().CompareNoCase( stateName ) == 0 )
      return state;
  }; // while pos

  return NULL;
}; // FindStateByName

BOOL Project::Create()
// legt ein neues Projekt an
// der Projektpfad muss bereits vorhanden sein
// Rückgabewert:
//          TRUE / FALSE, je nach Erfolg der Operation
{
  BOOL bOK = TRUE;

  CFileStatus fileStatus;
  
  if ( m_dir.IsEmpty() )
    return FALSE;

  // Abbruch falls: projektverzeichniss existiert nocht nicht bzw. ist kein Verzeichnis
  //                prof oder dath Verzeichnis sind schon da
  if ( !CFile::GetStatus( m_dir, fileStatus ) || 
       !(( fileStatus.m_attribute & CFile::directory ) == CFile::directory ) ||
        CFile::GetStatus( m_dir + "\\prof", fileStatus )  || 
        CFile::GetStatus( m_dir + "\\dath", fileStatus ) )
    return FALSE;

  bOK = CreateDirectory( m_dir + "\\prof", NULL ) && CreateDirectory( m_dir + "\\dath", NULL );

  if ( bOK )
  {
    SetProjectType( Project::AskCalcModel() );
    bOK = Save();
  }

  return bOK;
}

BOOL Project::Remove()
// löscht das Projekt physikalisch in den papierkorb
// Rückgabewert:
//          TRUE / FALSE je nach Erfolg der Operation
// 
{
  CFileStatus fileStatus;
	
	if ( m_dir.IsEmpty() )
		return FALSE;

  if ( !CFile::GetStatus( m_dir, fileStatus ) ||
       !(( fileStatus.m_attribute & CFile::directory ) == CFile::directory ) ||
       !CFile::GetStatus( m_dir + "\\prof", fileStatus ) ||
       !(( fileStatus.m_attribute & CFile::directory ) == CFile::directory ) )
		return FALSE;


  // jetzt das komplette Projektverzeichniss in den Papierkorb löschen
  LPSTR files = (LPSTR)malloc( m_dir.GetLength() + 2 );
  strcpy( files, (LPCSTR)m_dir );
  files[ m_dir.GetLength() + 1 ] = '\0'; // mit einer doppel 0 abschliessen
  
  SHFILEOPSTRUCT foStruct;
  foStruct.hwnd = NULL;
  foStruct.wFunc = FO_DELETE;
  foStruct.pFrom = files;
  foStruct.pTo = NULL;
  foStruct.fFlags = FOF_ALLOWUNDO | FOF_NOCONFIRMATION;
  foStruct.lpszProgressTitle = NULL;
  
  int bResult = SHFileOperation( &foStruct );
  DWORD error = GetLastError();
  
  free( files );              

  return ( bResult == 0 );
}; // Remove


BOOL Project::ImportData( const int typ, const CString& inputFileName, State* zustand, DWORD data )
// Importfilter zum einlesen von da66, da50, wsv, tripple, hyk , wst , caddy, waspila und jabron Datenformaten
// eingabe:
//          type:  obige Namen des Importtyps  z.B.: "da66"
//          inputFileName: Name der Eingabedatei
//          zustand: Zustand, zu welchem die eingelesenen Profildaten hinzugefügt werden
//          DWORD data: zusätzliche Informationen zum Import
// ausgabe:
//          TRUE/FALSE je nach Erfolg der Operation
// doku:    liest Daten aus dem InputFile und erzeugt aus diesen neue CrossSections
{
  BOOL erfolg;
  CStdioFile inputFile;

  if( !zustand || !inputFile.Open( inputFileName, CFile::modeRead | CFile::shareDenyNone ) )
    return FALSE;

  switch( typ )
  {
  case ImportWsv:
    erfolg = ImportDataWsv( &inputFile, zustand );
    break;
  case ImportTripple:
    erfolg = zustand->ImportDataTripple( &inputFile );
    break;
  case ImportHyk:
    erfolg = ImportDataHyk( &inputFile, zustand );
    break;
  case ImportReli:
    erfolg = ImportDataReli( &inputFile, zustand );
    break;
  case ImportDa66:
    erfolg = zustand->ImportDataDa66( &inputFile );
    break;
  case ImportDa50:
    erfolg = zustand->ImportDataDa50( &inputFile, data );
    break;
  case ImportWst:
    erfolg = ImportDataWst( &inputFile, zustand );
    break;
  default:
    erfolg = FALSE;
  };
  inputFile.Close();
  return erfolg;
}


BOOL Project::ImportDataWsv( CStdioFile* eingabedatei, State* zustand )
// Importfilter zum Einlesen von Dateien im .wsv Format
// Input:
//        eingabedatei: bereits geöffnete Eingabedatei
//        zustand:  Zeiger auf Zustand an welchen die Daten ein-/ an-gefügt werden
// Output:
//        TRUE´/FALSE je nach Erfolg der Operation
// liest Geoinformationen aus der .wsv Datei aus und erstellt entsprechende profile
// der y-Wert wird immer relativ zum 31er Punkt gesetzt
// nur direkt hintereinanderliegende Datenblöcke (dh. 31,32,33,33,...) werden zu einer Station zusammengefasst
{
  const int MAX_WSV_LAENGE = 100;
  struct CPointDouble
  {
    double rw,hw;
  };
  
  BOOL dateianfang = TRUE;  // wurden schon Profile gelesen ?
  CString str, help_str;
  CPointDouble profil_anfang, profil_anfang_tmp;
  CPointDouble profil_gegen, profil_gegen_tmp;
  double station, station_tmp;
  CStringList files;
  CrossSection *cs = NULL;
  Profil *pr = NULL;

  CStdioFile abweichDatei;
  help_str = eingabedatei->GetFilePath().Left(eingabedatei->GetFilePath().GetLength()-3) + "abw"; 
  abweichDatei.Open(help_str, CFile::modeCreate | CFile::modeWrite);
  abweichDatei.WriteString("Fluss-km, Profilpunkt-Nummer, Abweichung in m \n");
  
  while (eingabedatei->ReadString(str))
  {
    double vx,vy;  // hilfsvektor zum Lotfällen
    
    if ((strlen(str) < 40) || (str[0] == '*')) continue;
    switch (atoi(str.Mid(6,2)))
    {
    case 31:    // 31 er Zeile lesen
      {
        profil_anfang_tmp.rw = atof(str.Mid(20,10)) / 1000;  // Rechtswert des Profilanfangspunktes auslesen
        profil_anfang_tmp.hw = atof(str.Mid(30,10)) / 1000;  // Hochwert auslesen: beide in Gauss Krüger mit 3 Nachkommastellen
        station_tmp = atof(str.Mid(9,6)) / 1000  ;           // Station in km auslesen
      };
      break;
    case 32:   // 32er Zeile lesen
      {
        profil_gegen_tmp.rw = atof(str.Mid(20,10)) / 1000; // Rechtswert des Profilgegenpunktes auslesen
        profil_gegen_tmp.hw = atof(str.Mid(30,10)) / 1000; // Hochwert auslesen: beide in Gauss Krüger mit 3 Nachkommastellen
      };
      break;
    case 33:
      {
        if ((dateianfang) ||
          ((fabs(profil_anfang.rw - profil_anfang_tmp.rw) > 0.0001) ||
          (fabs(profil_anfang.hw - profil_anfang_tmp.hw) > 0.0001) ||
          (fabs(profil_gegen.rw - profil_gegen_tmp.rw) > 0.0001) ||
          (fabs(profil_gegen.hw - profil_gegen_tmp.hw) > 0.0001)))
        {
          CString gewaessername;
          CString zustandsname;
          
          dateianfang = FALSE; // jetzt sicher nicht mehr am dateianfang
          
          // neue Cross Section erzeugen und Station setzen
          cs = new CrossSection(this);
          gewaessername = zustand->GetWaterName();
          cs->SetWaterName(gewaessername);
          zustandsname = zustand->GetName();
          cs->SetStateName(zustandsname);
          cs->SetStation(station_tmp);  // (alte) Station setzen
          help_str = "0";
          cs->SetPK(help_str);
          cs->SetVZK(0);
          cs->SetProfilNr(this->GetCrossSectionCount()+1);
          cs->CreateFileName(&files);
          files.AddTail(cs->GetFileName());
          zustand->AddCrossSection(cs);
          
          // neues Profile erzeugen
          pr = new Profil(cs);
          pr->SetOriginalFile(eingabedatei->GetFilePath());
          help_str.Format("%s %s", gewaessername, zustandsname);
          pr->SetPageDesc(0, help_str);
          help_str.Format("QUERPROFIL %d", cs->GetProfilNr());
          pr->SetPageDesc(1, help_str);
          CString defaultLang = setlocale(LC_NUMERIC, NULL);
          setlocale(LC_NUMERIC, "English");
          help_str.Format("STATION KM %.4f", station_tmp);
          setlocale(LC_NUMERIC, defaultLang);
          pr->SetPageDesc(2, help_str);
          cs->SetProfil(pr);
          
          // neue datenblöcke  für geländehöhe, rechts- und hochwert anlegen
          DataBlock* db;
          db = new DataBlock(pr);
          db->SetType(DST_GELAENDEHOEHE);
          pr->AddDataBlock(db);
          
          db = new DataBlock(pr);
          db->SetType(DST_RECHTSWERT);
          pr->AddDataBlock(db);
          
          db = new DataBlock(pr);
          db->SetType(DST_HOCHWERT);
          pr->AddDataBlock(db);
          
          profil_anfang = profil_anfang_tmp;
          profil_gegen = profil_gegen_tmp;
          station = station_tmp;
          vx = profil_gegen.rw - profil_anfang.rw;
          vy = profil_gegen.hw - profil_anfang.hw;
        };
        
        CPointDouble einpkt;   // Der ausgelesene Profilpunkt
        CPointDouble lotpkt;   // auf die Profillinie projizierte Rechts- und Hochwerte
        double hoehe;  // Höhe des Profilpunktes
        double yAbstand;  // Abstand des projizierten Punktes zum 1. Profilpunkt
        double xAbstand; // Abstand des eingelesenen Punktes von der Profillinie
        
        // aktuellen Profilpunkt auslesen
        
        einpkt.rw = atof(str.Mid(20,10)) / 1000;        // Rechtswert auslesen (Gaus-Krüger mit 3 Nachkommastellen)
        einpkt.hw = atof(str.Mid(30, 10)) / 1000;       // Hochwert auslesen
        hoehe = atof(str.Mid(48, 6)) / 1000;     // Hoehe in m ü. NN , 3 Nachkommastellen
        
        // Lot auf die Profillinie fällen
        double skal;
        skal = ((einpkt.rw - profil_anfang.rw) * vx + (einpkt.hw - profil_anfang.hw) * vy) / (vx * vx + vy * vy);
        lotpkt.rw = profil_anfang.rw + skal * vx;
        lotpkt.hw = profil_anfang.hw + skal * vy;
        
        
        // y-Wert (hier xAbstand) des Profilpunktes: hier abstand zum Profilanfangspunkt
        yAbstand = sqrt(pow(profil_anfang.rw - lotpkt.rw,2) + pow(profil_anfang.hw - lotpkt.hw,2));
        // Abstand des eingelesenen Punktes zum Lotpunkt
        xAbstand = sqrt(pow(einpkt.rw - lotpkt.rw,2) + pow(einpkt.hw - lotpkt.hw,2));
        
        // yAbstand in die Abweichungsdatei schreiben
        
        help_str.Format("%.3f, %i, %.3f \n", station, atoi(str.Mid(16,4)), xAbstand);
        abweichDatei.WriteString(help_str);
        
        // Coordinate zu den entspr. Datenblöcken hinzufügen
        DataBlock* db_hoehe =  pr->GetDataBlock(DST_GELAENDEHOEHE);
        DataBlock* db_rw = pr->GetDataBlock(DST_RECHTSWERT);
        DataBlock* db_hw = pr->GetDataBlock(DST_HOCHWERT);
        
        // Profilpunkt an die richtige Stelle im Profil schreiben
        int pos = db_hoehe->GetNumCoords();
        while (pos != 0)
        {
          Coord* crd = db_hoehe->GetCoordAt(pos-1);
          if (yAbstand  > crd->dx) break;
          pos--;
        };
        
        // Coordninaten einfügen
        Coord* crd_hoehe = new Coord;  // Koordinaten für hoehe, rechts- und hochwert erzeugen
        Coord* crd_rw = new Coord;
        Coord* crd_hw = new Coord;
        
        crd_hoehe->dx = yAbstand;
        crd_hoehe->dy = hoehe;
        crd_hoehe->xs = 0;
        crd_hoehe->ys = 0;
        
        crd_rw->dx = yAbstand;
        crd_rw->dy = lotpkt.rw;
        crd_rw->xs = 0;
        crd_rw->ys = 0;
        
        crd_hw->dx = yAbstand;
        crd_hw->dy = lotpkt.hw;
        crd_hw->xs = 0;
        crd_hw->ys = 0;
        
        if (pos == db_hoehe->GetNumCoords())    // noch kein Punkt vorhanden oder ganz hinten anhängen
        {
          db_hoehe->AddCoord(crd_hoehe);
          db_rw->AddCoord(crd_rw);
          db_hw->AddCoord(crd_hw);
        }
        else                                    // mittendrin einfügen
        {
          db_hoehe->InsertCoordAt(pos, crd_hoehe);
          db_rw->InsertCoordAt(pos, crd_rw);
          db_hw->InsertCoordAt(pos, crd_hw);
        };
      };
      break;
      default:
        {
          abweichDatei.Close();
          return FALSE;   // andere Zeilentypen als 31-33 nicht zugelassen
        };
    };
  };
  abweichDatei.Close();
  return TRUE;
}

BOOL Project::ImportDataHyk(CStdioFile *eingabedatei, State *zustand)
// Importfilter zum Einlesen von Dateien im  .hyk Format
// Input:
//        eingabedatei: bereits geöffnete Eingabedatei
//        zustand:  Zeiger auf Zustand an welchen die Daten ein-/ an-gefügt werden
// Output:
//        TRUE/FALSE je nach Erfolg der Operation
// liest Informationen über durchstroemte bereiche, Trennflaechen, Buhnen (Sonderbauwerke) 
// aus der .hyk Datei und fügt diese zu bereits bestehenden Querprofilen hinzu. 
{
  CString str;

  while( eingabedatei->ReadString( str ) )
  {
    double hyk_station;
    int hyk_p_jahr=0;
    int hyk_glied_anz=1;
    int hyk_fzone_anz=0;
    double hyk_h_unten, hyk_h_oben;

    double hyk_abstand_links,hyk_abstand_mitte,hyk_abstand_rechts;

    CStringList files;
    CrossSection *cs = NULL;
    Profil *pr = NULL;

    if (str.IsEmpty() || str[0] == '*')
      continue;  // Kommentarzeilen überlesen

    // 1. Zeile des Datenblocks lesen
    hyk_station = atof(str.Left(8));
    hyk_p_jahr = atoi(str.Mid(10,4)); //Jahr der Peilung
    hyk_glied_anz = atoi(str.Mid(16,2)); //Anzahl der Gliderungen

    // 2. Zeile des Datenblocks lesen
    if (!eingabedatei->ReadString(str))
      return FALSE;  
    hyk_fzone_anz = atoi(str.Left(2));     //Anzahl der Fließzonen  
    hyk_h_unten = atof(str.Mid(9,6));  // Höhe unten
    hyk_h_oben = atof(str.Mid(16,6));           //Höhe (oben)
    CString str4, str5, str6; // 4. und 5. Zeile eines Datenblocks
    if (!eingabedatei->ReadString(str))
      return FALSE;  // 3. Zeile des Datenblocks lesen
    if (!eingabedatei->ReadString(str4)) 
      return FALSE;  // 4. Zeile des Datenblocks lesen
    if (!eingabedatei->ReadString(str5))
      return FALSE;  // 5. Zeile des Datenblocks lesen
    if (!eingabedatei->ReadString(str6)) 
      return FALSE;  // 6. Zeile des Datenblocks lesen
    sscanf(str6, "%lf%lf%lf", &hyk_abstand_links, &hyk_abstand_mitte, &hyk_abstand_rechts);   // Abstände zum nächsten Profil einlesen

    if((cs = zustand->FindCrossSection(hyk_station)) == NULL)
      continue; // diese Station gibt es nicht, nächsten Datenblock bearbeiten
    cs->LoadProfil();
    if((pr = cs->GetProfil()) == NULL)
      return FALSE;

    // Datenblock Gleändehöhe holen (für Rechte Modellgrenze)
    double yKrdRechts = 999999.99; // grösste yKoordinate (999999.99 = Fehler)
    DataBlock* db_gelaende = pr->GetDataBlock(DST_GELAENDEHOEHE);
    if (!db_gelaende)
      continue;
    Coord* crd = db_gelaende->GetLastCoord();
    if (!crd)
      continue;
    yKrdRechts = crd->dx;

    // Datenblöcke für Grenzen vorbereiten
    DataBlock* db_modell = new DataBlock(pr);
    db_modell->SetType(DST_MODELLGRENZEN);
    int modell_count = 0;
    DataBlock* db_durchst = new DataBlock(pr);
    db_durchst->SetType(DST_DURCHST_BEREICH);
    int durchst_count = 0;
    DataBlock* db_trenn = new DataBlock(pr);
    db_trenn->SetType(DST_TRENNFLAECHEN);
    int trenn_count = 0;
    DataBlock* db_boeschung = new DataBlock(pr);
    db_boeschung->SetType(DST_BOESCHUNGSKANTEN);
    int boeschungs_count = 0;
    
    char oldArt = ' ';
    for (int i = 0; i < hyk_fzone_anz + 1; i++)
    { 
      char art;
      double ykrd;
      if (i < hyk_fzone_anz)
      {
        art = str[i * 3 + 1];              // Art der fliesszone auslesen
        ykrd = 
          (i < 8 ? atof(str4.Mid(i * 9, 7)) : atof(str5.Mid((i - 8) * 9, 7)));
      }
      else
      {
        art = ' ';
        ykrd = yKrdRechts;
      };
      if (ykrd > yKrdRechts)
        yKrdRechts = ykrd;

      Coord *crd = new Coord;
      crd->dx = ykrd;
      crd->dy = pr->GetGelaendehoehe( ykrd );

      switch ( oldArt + 256 * art )
      {
      case ' ' + 256 * 'R':
      case 'R' + 256 * 'R':
      case 'R' + 256 * ' ':
        modell_count++;
        db_modell->AddCoord( crd );
        break;
        
      case ' ' + 256 * 'V':
      case 'R' + 256 * 'V':
      case 'V' + 256 * 'V':
      case 'V' + 256 * ' ':
      case 'V' + 256 * 'R':
        durchst_count++;
        db_durchst->AddCoord( crd );
        break;
        
      case ' ' + 256 * 'B':
      case 'R' + 256 * 'B':
      case 'V' + 256 * 'B':
      case 'B' + 256 * 'B':
      case 'B' + 256 * 'V':
      case 'B' + 256 * 'R':
      case 'B' + 256 * ' ':
        boeschungs_count++;
        db_boeschung->AddCoord( crd );
        break;
        
      case ' ' + 256 * 'H':
      case 'R' + 256 * 'H':
      case 'V' + 256 * 'H':
      case 'B' + 256 * 'H':
      case 'H' + 256 * 'H':
      case 'H' + 256 * 'B':
      case 'H' + 256 * 'V':
      case 'H' + 256 * 'R':
      case 'H' + 256 * ' ':
        trenn_count++;
        crd->dy = trenn_count;
        db_trenn->AddCoord( crd );
        break;
        
      default:
        delete crd;
        break; // hier gibts keine Grenze
      };

      oldArt = art;
    }; // for i

    if (modell_count)
    {
      DataBlock* db = pr->GetDataBlock(DST_MODELLGRENZEN);
      if (db)
      {
        pr->RemoveDataBlock(db);
        delete db;
      };
      pr->AddDataBlock(db_modell);
    }
    else
      delete db_modell;
    if (durchst_count)
    {
      DataBlock *db = pr->GetDataBlock(DST_DURCHST_BEREICH);
      if (db)
      {
        pr->RemoveDataBlock(db);  // falls ein Datenblock 'Druchströmte Bereiche' schon vorhanden ist durch den neuen ersetzen
        delete db;
      };
      pr->AddDataBlock(db_durchst);
    }
    else
      delete db_durchst;
    if (trenn_count)
    {
      DataBlock *db = pr->GetDataBlock(DST_TRENNFLAECHEN);
      if (db)
      {
        pr->RemoveDataBlock(db);  // falls ein Datenblock 'Trennflaechen' schon vorhanden ist durch den neuen ersetzen
        delete db;
      };
      pr->AddDataBlock(db_trenn);
    }
    else
      delete db_trenn;
    if (boeschungs_count)
    {
      DataBlock *db = pr->GetDataBlock(DST_BOESCHUNGSKANTEN);
      if (db)
      {
        pr->RemoveDataBlock(db);
        delete db;
      };
      pr->AddDataBlock(db_boeschung);
    }
    else
      delete db_boeschung;
  }; // while eingabedatei->Read
  return TRUE;
};


BOOL Project::ImportDataReli( CStdioFile *eingabedatei, State *zustand )
// Importfilter zum Einlesen von Buhnendateien im Re/Li (.re; .li) Format
// Input:
//        eingabedatei: bereits geöffnete Eingabedatei
//        zustand:  Zeiger auf Zustand an welchen die Daten ein-/ an-gefügt werden
//        BOOL bRemove: falls TRUE, werden alte Buhneneinträge gelöscht, sonst nicht
// Output:
//        TRUE/FALSE je nach Erfolg der Operation
// liest Informationen über Buhnen aus der Eingabedatei und fügt diese als Datenblock
// BUHNEN an bereits vorhandene Profile an
{
  CString reli;
  CString str;

  CString dateiEndung = eingabedatei->GetFilePath().Right(2);  // Dateiendung "re" oder "li"
  dateiEndung.MakeLower();
  switch (dateiEndung[0])
  {
  case 'r':
    reli = "R";
    break;
  
  case 'l':
    reli = "L";
    break;

  default:
    AfxMessageBox("Falsche Dateiendung" ,MB_OK | MB_ICONEXCLAMATION);
    return FALSE;
  };

  while(eingabedatei->ReadString(str))
  {
    double station, yKoord, kopfhoehe;
    int ruecken, vorderseite;
    
    if (sscanf(str, "%lf %lf %lf %i %i", &station, &yKoord, &kopfhoehe, &ruecken, &vorderseite) != 5)
      continue;

    CrossSection *cs = zustand->FindCrossSection(station); // entsprechende Station suchen
    if(!cs)
      continue; // diese Station gibt es nicht, nächsten Datenblock bearbeiten
    cs->LoadProfil();  // Profil laden falls nicht schon geschehen
    Profil* profil = cs->GetProfil();  // Profil abholen
    if (!profil)
    { // Profil wurde nicht gefunden -> Project korrupt
      CString ausgabe;
      ausgabe.FormatMessage(IDM_INITIALISE_PROFILE_ERROR, station);
      AfxMessageBox(ausgabe, MB_OK | MB_ICONEXCLAMATION);
      return FALSE;
    };

    DataBlock *db_gelaende = profil->GetDataBlock(DST_GELAENDEHOEHE);
    if (!db_gelaende)
      continue;

    DataBlock* db_buhne = profil->GetDataBlock(DST_BUHNEN);
    if ( !db_buhne )
    {
      db_buhne = new DataBlock(profil);
      db_buhne->SetType(DST_BUHNEN);
      profil->AddDataBlock(db_buhne);
    };

    // Koordinaten der drei Stützpunkte ausrechnen
    Coord* crdM = new Coord;
    crdM->Set(yKoord, kopfhoehe);

    if (reli == 'R') // bei rechter Buhne Rückenneigung und Neigung der Vorderseite vertauschen
    {
      int tmp = ruecken;
      ruecken = vorderseite;
      vorderseite = tmp;
    }
    else // bei linker Buhne Vorzeichen der Neigung ändern
    {
      ruecken = -ruecken;
      vorderseite = - vorderseite;
    };

    Coord* crd1 = profil->GetSchnittpunkt(crdM, 1 / double(ruecken), FALSE);
    Coord* crd2 = profil->GetSchnittpunkt(crdM, 1 / double(vorderseite), TRUE);

    // Koordianten dem Datenblock hinzufügen
    db_buhne->AddCoord(crd1);
    db_buhne->AddCoord(crdM);
    db_buhne->AddCoord(crd2);

    db_buhne->SetName( db_buhne->GetName(1) + " " + reli, 1 );
  }; // while eingabedatei->ReadString()

  return TRUE;
}

BOOL Project::ImportDataWst(CStdioFile *eingabedatei, State *zustand)
// Importfilter zum Einlesen von Dateien im WST Format
// Input:
//        eingabedatei: bereits geöffnete Eingabedatei
//        zustand:  Zeiger auf Zustand an welchen die Daten ein-/ an-gefügt werden
// Output:
//        TRUE/FALSE je nach Erfolg der Operation
{
  CString zeile;
  int spaltenAnz = 0;

  // erste Zeile: Anzahl der Spalten
  if (!eingabedatei->ReadString(zeile))
    return FALSE;
  if (sscanf(zeile, "%d", &spaltenAnz) != 1)
    return FALSE;
  if (!eingabedatei->ReadString(zeile))  //Kommentarzeile
    return FALSE;
  if (!eingabedatei->ReadString(zeile))  // Zeile der Abfluesse
    return FALSE;

  CTypedPtrArray<CPtrArray, OutFlow*> outFlows;
  CTypedPtrArray<CPtrArray, Calculation*> calcs;
  CTypedPtrArray<CPtrArray, DataBlock*> dbs_sohl;
  CTypedPtrArray<CPtrArray, DataBlock*> dbs_level;

  for (int i = 0; i < spaltenAnz; i++) 
  {
    CString abStr = zeile.Mid(8 + 9 * i, 9);
    abStr.TrimLeft();
    abStr.TrimRight();
    double abfluss = atof(abStr);
    
    OutFlow *of = zustand->FindOutFlow("Q" + abStr);
    if(of)
    {
      zustand->RemoveOutFlow(of);
      delete of;
    };
    of = new OutFlow(this, zustand);
    of->SetName("Q" + abStr);
    zustand->AddOutFlow(of);
    outFlows.SetAtGrow(i, of);
    
    Calculation *calc = zustand->FindCalculation("Q = " + abStr);
    if (calc)
    {
      zustand->RemoveCalculation(calc);
      delete calc;
    };
    calc =  new Calculation(this, zustand);
    LengthSection *ls = new LengthSection(this);
    calc->SetName("Q = " + abStr);
    ls->SetName("Q = " + abStr);
    calc->CreateFileName();
    zustand->AddCalculation(calc);
    calcs.SetAtGrow(i, calc);
    
    CString calcFile = calc->GetFileName();
    calcFile.SetAt(2, 'w');  //pl für LWA wl für BCE
    calcFile.SetAt(3, 'l');
    ls->SetFileName(calcFile);
    // neues Profil erzeugen und zur LengthSection hinzufügen
    Profil *profil = new Profil(ls);
    profil->SetOriginalFile(eingabedatei->GetFilePath());

    CString gewaesser = zustand->GetWaterName();
    CString zustandsName = zustand->GetName();
    CString str;
    str.Format("%s %s", gewaesser, zustandsName);
    profil->SetPageDesc(0, str);
            
    profil->SetPageDesc(1, "SPIEGELLINIEN-LAENGSSCHNITT Q = " + abStr);
    ls->SetProfil(profil);

    // neue Datenblöcke erzeugen
    str = CString(' ', 199) +  "Q = " + abStr + "@" + gewaesser + "@" + zustandsName;
            
    DataBlock *db = new DataBlock(profil);
    db->SetType(DST_SOHLHOEHE);
    db->SetName(str,1);
    profil->AddDataBlock(db);
    dbs_sohl.SetAtGrow(i, db);
            
    db = new DataBlock(profil);
    db->SetType(DST_WASSERSPIEGEL);
    db->SetName(str,1);									
    profil->AddDataBlock(db);
    dbs_level.SetAtGrow(i, db);
    
    // Berechnungsdaten erzeugen
    CalcData *pcalcdata = new CalcData(calc, zustand, FALSE);
    pcalcdata->m_nAbfluss = i; // Nummer des Abflusses -> nur schwer möglich wirklich rauszufinden, wird in spezialfällen bzw. wenn bereits ander Abflüsse vorhanden sind nicht richtig funktionieren
    pcalcdata->m_strInfo = calc->GetName();
    pcalcdata->m_strQ = calc->GetName();
    calc->SetCalcData(pcalcdata);
    calc->SetLengthSection(ls);
    calc->SaveCalcData();
  };

  CArray<double, double> abfluesse;  // aktuelle Abfluesse
  BOOL bBlockAnfang = FALSE;
  BOOL bErsteStation = TRUE;
  while (eingabedatei->ReadString(zeile)) // schleife für die Datenblöcke
  {
    if (zeile.GetLength() > 1 && zeile[0] == '*' && zeile[1] == 0x1f)
    {
      for (int i = 0; i < spaltenAnz; i++)
      {
        CString abStr = zeile.Mid(8 + 9 * i, 9);
        abStr.TrimLeft();
        abStr.TrimRight();
        double abfluss = atof(abStr);
        abfluesse.SetAtGrow(i, abfluss);
        bBlockAnfang = TRUE;
      };
    };
    if ( zeile.GetLength() ==0 || zeile[0] == '*')  // Kommentarzeile überlesen (ebenfalls: Blockanfänge bereits bearbeitet)
      continue;

    double station;
    CString stationStr = zeile.Left(8);
    stationStr.TrimLeft();
    stationStr.TrimRight();
    if (stationStr.GetLength() < 3 || sscanf(zeile, "%lf", &station) != 1)
      continue;

    if (bBlockAnfang)
    { // jetzt erst die OutFlows beschreiben: Station erst hier bekannt
      bBlockAnfang = FALSE;
      for (int i = 0; i < spaltenAnz; i++)
      {
        C3DCoord* p3DCrd = new C3DCoord;
        p3DCrd->dx = station;
        p3DCrd->dy = abfluesse[i];
        outFlows[i]->AddCoord(p3DCrd);
      };
    };

    for (int i = 0; i < spaltenAnz; i++)
    {
      CalcData* calcData = calcs[i]->GetCalcData();
      if (bErsteStation)
      {
        calcs[i]->SetStartStation(station);
        calcs[i]->GetLengthSection()->SetStartStation(station);
        calcData->m_dAnfang = station;
      };
      calcs[i]->SetEndStation(station);
      calcs[i]->GetLengthSection()->SetEndStation(station);
      calcData->m_dEnde = station;

      CString levelStr = zeile.Mid(8 + 9 * i, 9);
      levelStr.TrimLeft();
      levelStr.TrimRight();
      double level = atof(levelStr);

      Coord *crd = new Coord;
      crd->dx = -1000.0 * station;
      crd->dy = 0.0;
      crd->xs = 0;
      crd->ys = 0;
      dbs_sohl[i]->AddCoord(crd);

      crd = new Coord;
      crd->dx = -1000.0 * station;
      crd->dy = level;
      crd->xs = 0;
      crd->ys = 0;
      dbs_level[i]->AddCoord(crd);
    }; // for i
    bErsteStation = FALSE;
  }; // while ReadString

  // Berechnungsvarianten abspeichern
  for ( i = 0; i < spaltenAnz; i++)
    calcs[i]->SaveCalcData();

  return TRUE;
}

int Project::ImportDataAccess( const CString& stationPath, const CString& profilePath )
// importtiert Daten im Format 'ACCESS'
// Parameter:
//        const Strint& stationPath, profilePath: die beiden Importdateien
// Rückgabewert:
//        Fehlercode: 0: kein Fehler
//                    1: Namen der Importdateien sind gleich
//                    2: Fehler beim Zugriff auf die StationsDatei
//                    3: Fehler beim Zugriff auf die Profildatei
//                    4: Keine Zustände hinzugefügt -> Dateinamen vertauscht?
{
  if ( stationPath.CompareNoCase( profilePath ) == 0 )
    return 1;
  
  
  CString buffer; // Puffer für ReadString
  int index;  // für Find
  const TCHAR trennZ = ';'; // das Trennzeichen zwischen den Einträgen
  
  
  CTypedPtrMap<CMapStringToPtr, CString, State*> stateMap;  // zuordnung Gewässername |-> Zustand
  CTypedPtrMap<CMapWordToPtr, WORD, CrossSection*> profileMap; // Zuordnung Profilnummer |-> Querprofil
  

  // Locale einstellungen auf englisch setzen
  CString m_defaultLang;
  m_defaultLang = setlocale(LC_NUMERIC, NULL);
	setlocale(LC_NUMERIC, "English");

  // zuerst die Stationsdatei auslesen und Zustände und Profile erzeugen
  try
  {
    CStdioFile stationFile( stationPath, CFile::modeRead );
    
    CStringList forbiddenFiles;

    while ( stationFile.ReadString( buffer ) )
    {
      int profilNummer;
      double station;
      CString gewaesser;
      
      // buffer Parsen
      CString helpStr;
      
      // ProfilNummer parsen
      index = buffer.Find( trennZ );
      if ( index == -1 )
        continue;
      
      helpStr = buffer.Left( index );
      buffer = buffer.Mid( min( index + 1, buffer.GetLength() - 1 ) );
      
      profilNummer = atoi( helpStr );
      
      // Profil-Station parsen
      index = buffer.Find( trennZ );
      if ( index == -1 )
        continue;
      
      helpStr = buffer.Left( index );
      buffer = buffer.Mid( min( index + 1, buffer.GetLength() - 1 ) );
      
      helpStr.Replace( TCHAR('+'), TCHAR('.') ); // das Plus-Kommazeichen durch einen Punkt ersetzen; die Nueric-Locale ist stets auf englisch gesetzt, scanf erkennt also '.'
      helpStr.Remove( TCHAR('\"') );
      
      station = atof( helpStr );
      
      // Gewässername parsen
      buffer.Remove( TCHAR('\"') );
      buffer.Remove( TCHAR(' ') );
      if ( buffer.IsEmpty() )
        continue;
      else
        gewaesser = buffer.Left( 8 ); // Gewääsernamen dürfen max. 8 Zeichen lang sein
      
      
      // falls erforderlich, neuen Zustand anlegen
      State* state = NULL;
      if ( !stateMap.Lookup( gewaesser, state ) )
      {  // dieses Gewääser gabs noch nicht, also neuen Zustand anlegen
        state = new State( this );
        
        state->SetDate( COleDateTime::GetCurrentTime() );
        state->SetName( TEXT("Import") );
        state->SetWaterName( gewaesser );
        state->CreateFileName();
        
        stateMap[gewaesser] = state;
      };
      
      // in jedem Fall, neues Profil anlegen
      CrossSection* cs = NULL;
      if ( profileMap.Lookup( profilNummer, cs ) )
        continue;  // diese Profilnummer war schon mal da
      
      cs = new CrossSection( this );
      
      cs->AddState( state );
      cs->SetProfilNr( profilNummer );
      cs->SetStation( station );
      cs->SetVZK( 0 );
      cs->SetPK( TEXT("0") );
      cs->SetStateName( state->GetName() );
      cs->SetWaterName( gewaesser );
      cs->CreateFileName( &forbiddenFiles );
      forbiddenFiles.AddTail( cs->GetFileName() );

      state->AddCrossSection( cs );
      
      profileMap[profilNummer] = cs;
    }; // while
    
    stationFile.Close();
  }
  catch( CFileException* e )
  {
    e->Delete();
    return 2;
  };

  int error = 0;
  // jetzt die Profildatei auslesen
  try
  {
    CStdioFile profileFile( profilePath, CFile::modeRead );
    while ( profileFile.ReadString( buffer ) )
    {
      int profilNummer;
      double gelaende, rechtsW, hochW;
      
      // buffer parsen
      CString helpStr;

      buffer.Replace( TCHAR(','), TCHAR('.') ); // alle Kommata durch Punkte ersetzen -> wir haben stets '.' als Dezimaltrennzeichen
      
      // Profilnummer parsen
      index = buffer.Find( trennZ );
      if ( index == -1 )
        continue;
      
      helpStr = buffer.Left( index );
      buffer = buffer.Mid( min( index + 1, buffer.GetLength() - 1 ) );
      
      profilNummer = atoi( helpStr );
      
      // Geländehöhe parsen
      index = buffer.Find( trennZ );
      if ( index == -1 )
        continue;
      
      helpStr = buffer.Left( index );
      buffer = buffer.Mid( min( index + 1, buffer.GetLength() - 1 ) );
      
      gelaende = atof( helpStr );
      
      // Rechtswert parsen
      index = buffer.Find( trennZ );
      if ( index == -1 )
        continue;
      
      helpStr = buffer.Left( index );
      buffer = buffer.Mid( min( index + 1, buffer.GetLength() - 1 ) );
      
      rechtsW = atof( helpStr );
      
      // hochwert parsen
      hochW = atof( buffer );
      
      
      // jetzt neue Profilpunkte erzeugen
      
      CrossSection* cs = NULL;
      if ( !profileMap.Lookup( profilNummer, cs ) )
        continue;  // diese Profilnummer ist nihct vorhanden
      
      Profil* profil = cs->GetProfil();
      if ( !profil )
      { // wenn noch kein Profil vorhanden, erstmal eins erzeugen
        profil = new Profil( cs );
        
        profil->SetDate( COleDateTime::GetCurrentTime() );
        profil->SetNum( profilNummer );
        profil->SetOriginalFile( profileFile.GetFileName() );

        CString helpStr;
        helpStr.Format( "%s %s", cs->GetWaterName(), cs->GetStateName() );
        profil->SetPageDesc( 0, helpStr );
        helpStr.Format( "STATION KM %.4f", cs->GetStation() );
        profil->SetPageDesc( 2, helpStr );
       
        cs->SetProfil( profil );
      }; // if !profil
      
      DataBlock* dbGel = profil->GetDataBlock( DST_GELAENDEHOEHE );
      DataBlock* dbRW = profil->GetDataBlock( DST_RECHTSWERT );
      DataBlock* dbHW = profil->GetDataBlock( DST_HOCHWERT );
      if ( !dbGel )
      { // erstmal die Datenblöcke erzeugen
        dbGel = new DataBlock( profil );
        dbRW = new DataBlock( profil );
        dbHW = new DataBlock( profil );
        
        dbGel->SetType( DST_GELAENDEHOEHE );
        dbRW->SetType( DST_RECHTSWERT );
        dbHW->SetType( DST_HOCHWERT );
        
        profil->AddDataBlock( dbGel );
        profil->AddDataBlock( dbRW );
        profil->AddDataBlock( dbHW );
      }; // if !dbGel
      
      
      // Koordinaten setzen
      
      double yKrd = 0;
      
      // y Koordinate ausrechnen
      Coord* oldRW = dbRW->GetLastCoord();
      Coord* oldHW = dbHW->GetLastCoord();
      
      if ( oldRW && oldHW )
      {
        double deltaRW = rechtsW - oldRW->dy;
        double deltaHW = hochW - oldHW->dy;
        
        yKrd = oldHW->dx + sqrt( deltaRW * deltaRW + deltaHW * deltaHW );
      };
      
      Coord* newGel = new Coord( yKrd, gelaende );
      Coord* newRW = new Coord( yKrd, rechtsW );
      Coord* newHW = new Coord( yKrd, hochW );
      
      dbGel->AddCoord( newGel );
      dbRW->AddCoord( newRW );
      dbHW->AddCoord( newHW );
    }; // while ReadString
    profileFile.Close();
  }
  catch( CFileException* e )
  {
    e->Delete();
    error = 3;
  };
    
  // locale Einstellungen wieder zurücksetzen
  setlocale(LC_NUMERIC, m_defaultLang);

  // jetzt Projekt speichern oder alle Daten löschen
  int stateCount = 0;

  POSITION pos = stateMap.GetStartPosition();
  while ( pos )
  {
    State* state;
    CString gewaesser;
    
    stateMap.GetNextAssoc( pos, gewaesser, state );
    if ( state )
    {
      if ( error == 0 && state->GetNumCrossSections() > 0 )
      {
        AddState( state );
        stateCount++;
      }
      else
      {
        // alle Crossections dieses Zustands löschen
        CrossSection* cs = state->GetFirstCrossSection();
        while ( cs )
        {
          if( state->RemoveCrossSection( cs ) )
            delete cs;
          cs = state->GetNextCrossSection();
        };
        delete state;
      };
    }; // if state
  }; // while pos

  if ( stateCount == 0 )
    error = 4;

  return error;
}; // ImportDataAccess


int Project::ExportData( enum Project::ExportType type, CrossSectionArray* csArray, CString ausgabeDatei )
//
//  Exportiert Profildaten in verschiedene Formate
//
// Parameter:
//        ExportType type: Typ des Exports
//        CrossSectionArray* csArray: Liste der zu exportierenden Profile
//        CString ausgabeDatei: Pfad der Ausgabedatei (mit Endung)
// Rückgabewert:
//        Fehlerkode:
//                  0 keine Fehler
//                  1 Fehler beim erstellen der Ausgabedatei (Messagebox)
//                  2 Unbekannter Exporttyp
//                  3 Fehler beim schliessen der Ausgabedatei
//               >100 Fehler der eigentlichen ImportRoutine (ImportData...())
{
  int erfolg = 0;
  CStdioFile outputFile;
  CFileException *e = new CFileException();
  if (!outputFile.Open(ausgabeDatei, CFile::modeCreate | CFile::modeWrite | CFile::shareDenyWrite, e))
  {
    TCHAR szCause[255];
    e->GetErrorMessage(szCause, 255); // geht schief ??
    AfxMessageBox(szCause);
    erfolg = 1;
  };
  e->Delete();
  if (erfolg != 0)
    return erfolg;

  switch (type)
  {
  case ExportDa66:
    erfolg = ExportDataDa66( outputFile, csArray );
    break;
  
  case ExportTripple:
    erfolg = ExportDataTripple( outputFile, csArray );
    break;

  case ExportCSV:
    erfolg = ExportDataCSV( outputFile, csArray );
    break;
    
  case ExportHyk:
    erfolg = ExportDataHyk( outputFile, csArray );
    break;

  case ExportReliL: // linke Buhnen exportieren
    erfolg = ExportDataReli( outputFile, csArray, TRUE );
    break;

  case ExportReliR: // rechte Buhnen exportieren
    erfolg = ExportDataReli(outputFile, csArray, FALSE );
    break;

  default:
    erfolg = 2;
  };
  TRY
  {
  outputFile.Close();
  }
  CATCH(CFileException, e)
  {
    TCHAR  szCause[255];
    e->GetErrorMessage(szCause, 255);
    AfxMessageBox(szCause);
    return 3;
  };
  END_CATCH
  if (erfolg > 0)
    return erfolg + 100;
  else
    return erfolg;
}

int Project::ExportDataDa66(CStdioFile& ausgabeDatei, CrossSectionArray* csArray )
//
// Exportiert Profildaten in Da66 Format
//
// Parameter:
//          CStdioFile &ausgabedatei: Zeiger auf eine zum schreiben geöffnete ausgabedatei
//          CrossSectionArray* csArray: Liste der zu exportierenden Profile
// Rückgabe:
//          Fehlerkode:
//                    0 keine Fehler
//                    1 mindestens eins der Profile nicht vorhanden oder lässt sich nicht laden
//                    2 Profil ließ sich nicht initialisieren
//                    3 Fehler beim schreiben (MessageBox)
{
  int error = 0;
  TRY
  {
    for (int i = 0; i < csArray->GetSize(); i++)
    {
      CrossSection *cs = csArray->GetAt( i );
      if ( cs && cs->LoadProfil() )
      {
        Profil *profil = cs->GetProfil();
        if (profil)
        {
          double station = cs->GetStation();
          int decimal, sign;
          CString stationStr = _fcvt( station, 3, &decimal, &sign );
          stationStr = CString(' ', min(10, 10 - decimal)) + stationStr + "000";

          // wenn Rechts und Hochwert vorhanden sind, erstmal eine 50er Zeile schreiben
          DataBlock* dbRW = profil->GetDataBlock( DST_RECHTSWERT );
          DataBlock* dbHW = profil->GetDataBlock( DST_HOCHWERT );
          if( dbRW && dbHW && dbRW->GetNumCoords() > 0 && dbRW->GetNumCoords() == dbHW->GetNumCoords() )
          {
            double xFst;
            double xLst;

            bool bGeorefZero = false; // TODO: als Parameter übergeben 

            if( bGeorefZero )
            {
              xFst = 0.0;
              xLst = max( 1.0, dbRW->GetLastCoord()->dx ); // mindestens mal 1 meter Abstand
            }
            else
            {
              xFst = dbRW->GetFirstCoord()->dx;
              xLst = max( xFst + 1.0, dbRW->GetLastCoord()->dx );
            }

            double rwFst = dbRW->GetDYAt( xFst, true ) * 1000.0;
            double hwFst = dbHW->GetDYAt( xFst, true ) * 1000.0;

            double rwLst = dbRW->GetDYAt( xLst, true ) * 1000.0;
            double hwLst = dbHW->GetDYAt( xLst, true ) * 1000.0;

            double length = sqrt( ( rwLst - rwFst ) * ( rwLst - rwFst ) + ( hwLst - hwFst ) * ( hwLst - hwFst ) );
            
            CString outStr;
            outStr.Format( "50%s  %10.0lf%10.0lf%10.0lf%0.0lf%10.0lf\n", stationStr, rwFst, hwFst, rwLst, hwLst, length );
            ausgabeDatei.WriteString( outStr );
          }

          DataBlock *db_gelaende = profil->GetDataBlock(DST_GELAENDEHOEHE);
          if( db_gelaende )
          {
            // 00er Zeile schreiben
            
            CString outStr = "00" + stationStr + " 1\n";
            ausgabeDatei.WriteString(outStr);
            // 66er Zeilen schreiben
            Coord *crd = db_gelaende->GetFirstCoord();
            int zeile = 0;
            int counter = 4;
            outStr = "";
            while (crd)
            {
              CString hlpStr;
              if (counter == 4)
              { 
                // zeile schreiben
                if (outStr != "")
                  ausgabeDatei.WriteString("66" + outStr + "\n");
                // neue Zeile anfangen
                zeile++;
                outStr = stationStr;
                if( zeile > 99 )
                  outStr += "**";
                else
                {
                  hlpStr.Format("%2d", zeile);
                  outStr += hlpStr;
                };
                counter = 0;
              }; // if counter == 4
              hlpStr.Format("%8ld%7ld", (long)(crd->dx * 1000), (long)(crd->dy * 1000));
              outStr += hlpStr;

              counter++;
              crd = db_gelaende->GetNextCoord();
            }; // while crd
            // 88er Zeile schreiben
            ausgabeDatei.WriteString("88" + outStr + "\n");
          }; // if db_gelaende
        }
        else
        {
          error = 2;
          break;
        }; // if profil
      }
      else
      {
        error = 1;
        break;
      }; // if cs
      cs->FlushProfil();
    }; // for i
  } // TRY
  CATCH(CFileException, e)
  {
    TCHAR  szCause[255];
    e->GetErrorMessage(szCause, 255);
    AfxMessageBox(szCause);
    return 3;
  }
  END_CATCH
    
  return error;
}; // ExportDataDa66


int Project::ExportDataTripple(CStdioFile& ausgabeDatei, CrossSectionArray* csArray )
//
// Exportiert Profildaten ins Tripple (.txt) Format
//
// Parameter:
//          CStdioFile &ausgabedatei: Zeiger auf eine zum schreiben geöffnete ausgabedatei
//          CrossSectionArray* csArray: Liste der zu exprotierenden Profile
// Rückgabe:
//          Fehlerkode:
//                    0 keine Fehler
//                    1 mindestens eins der Profile nicht vorhanden oder lässt sich nicht laden
//                    2 Profil ließ sich nicht initialisieren
//                    3 Fehler beim schreiben (MessageBox)
{
  int error = 0;
  TRY
  {
    ausgabeDatei.WriteString("FLUSS-KM,XCOORD,YCOORD,ZCOORD\n");
    for (int i = 0; i < csArray->GetSize(); i++)
    {
      CrossSection* cs = csArray->GetAt( i );
      if ( cs && cs->LoadProfil() )
      {
        Profil *profil = cs->GetProfil();
        if (profil)
        {
          DataBlock *db_gel = profil->GetDataBlock(DST_GELAENDEHOEHE);
          DataBlock *db_rw = profil->GetDataBlock(DST_RECHTSWERT);
          DataBlock *db_hw = profil->GetDataBlock(DST_HOCHWERT);
          if (db_gel && db_rw && db_hw && 
              db_gel->GetNumCoords() == db_rw->GetNumCoords() &&
              db_rw->GetNumCoords() == db_hw->GetNumCoords())
          {
            double station = cs->GetStation();
            Coord *gelCrd = db_gel->GetFirstCoord();
            Coord *rwCrd = db_rw->GetFirstCoord();
            Coord *hwCrd = db_hw->GetFirstCoord();
            while (rwCrd && hwCrd)
            {
              CString outStr;
              outStr.Format("%10.3lf,%11.3lf,%11.3lf,%7.3lf\n", station, rwCrd->dy, hwCrd->dy, gelCrd->dy);
              ausgabeDatei.WriteString(outStr);
              gelCrd = db_gel->GetNextCoord();
              rwCrd = db_rw->GetNextCoord();
              hwCrd = db_hw->GetNextCoord();
            };

          }; // if db_rw && db_hw && ...
        }
        else
        {
          error = 2;
          break;
        }; // if profil
      }
      else
      {
        error = 1;
        break;
      }; // if cs
      cs->FlushProfil();
    }; // for i
  } // TRY
  CATCH(CFileException, e)
  {
    TCHAR  szCause[255];
    e->GetErrorMessage(szCause, 255);
    AfxMessageBox(szCause);
    return 3;
  }
  END_CATCH
    
  return error;
}; // ExportDataTripple

/*
 * Exportiert Profildaten ins Tripple (.txt) Format
 *
 * @param ausgabedatei Zeiger auf eine zum schreiben geöffnete ausgabedatei
 * @param csArray Liste der zu exprotierenden Profile
 *
 * @return Fehlerkode:
 *      0 keine Fehler
 *      1 mindestens eins der Profile nicht vorhanden oder lässt sich nicht laden
 *      2 Profil ließ sich nicht initialisieren
 *      3 Fehler beim schreiben (MessageBox)
 */
int Project::ExportDataCSV( CStdioFile& ausgabeDatei, CrossSectionArray* csArray )
{
  const double X_TOLL = 0.0001;
  
  const CString TIEFPUNKT = "T";
  
  const CString MARKER_DB_LINKS = "PA";
  const CString MARKER_DB_RECHTS = "PE";
  
  const CString MARKER_TF_LINKS = "LU";
  const CString MARKER_TF_RECHTS = "RU";
  
  const CString MARKER_BV_LINKS = "BL";
  const CString MARKER_BV_RECHTS = "BR";

  const CString ZONE_HF = "HF";
  const CString ZONE_RR = "RR";
  const CString ZONE_RL = "RL";
  const CString ZONE_VL = "VL";
  const CString ZONE_VR = "VR";
  const CString ZONE_B = "B";

  int error = 0;
  try
  {
    ausgabeDatei.WriteString("DATEI,FLUSS-KM,BREITE,HOEHE,RW, HW, RAUHEIT, AX, AY, DP, FLIESSZONE, BORDVOLLZONE, MARKER_DB, MARKER_TF, MARKER_BV, TIEFPUNKT\n");
    for( int i = 0; i < csArray->GetSize(); i++ )
    {
      CrossSection* cs = csArray->GetAt( i );
      if( cs && cs->LoadProfil() )
      {
        Profil* profil = cs->GetProfil();
        if( profil )
        {
          DataBlock* db_gel = profil->GetDataBlock(DST_GELAENDEHOEHE);
          DataBlock* db_rw = profil->GetDataBlock(DST_RECHTSWERT);
          DataBlock* db_hw = profil->GetDataBlock(DST_HOCHWERT);
          DataBlock* db_db = profil->GetDataBlock( DST_DURCHST_BEREICH );
          DataBlock* db_bv = profil->GetDataBlock( DST_BORDVOLL );
          DataBlock* db_tf = profil->GetDataBlock( DST_TRENNFLAECHEN );

		  DataBlock* db_ax = profil->GetDataBlock( DST_AXM );
		  DataBlock* db_ay = profil->GetDataBlock( DST_AYM );
		  DataBlock* db_dp = profil->GetDataBlock( DST_DPM );

          DataBlock* db_rauheit = profil->GetDataBlock( DST_RAUHIGKEIT );
          if( !db_rauheit )
            db_rauheit = profil->GetDataBlock( DST_RAUHIGKEIT_KST );

          if( !db_gel )
            continue;

          // zuerst den Tiefpunkt rausfinden
          Coord* tiefCrd = profil->GetSohlHoehe( cs, false );

          const CString name = cs->GetFileName();
          const double station = cs->GetStation();
          
          const Coord* crdDBlinks = db_db ? db_db->GetFirstCoord() : 0;
          const Coord* crdDBrechts = db_db ? db_db->GetLastCoord() : 0;

          const Coord* crdTFlinks = db_tf ? db_tf->GetFirstCoord() : 0;
          const Coord* crdTFrechts = db_tf ? db_tf->GetLastCoord() : 0;

          const Coord* crdBVlinks = db_bv ? db_bv->GetFirstCoord() : 0;
          const Coord* crdBVrechts = db_bv ? db_bv->GetLastCoord() : 0;
          
          Coord* gelCrd = db_gel->GetFirstCoord();
          while( gelCrd )
          {
            const double breite = gelCrd->dx;
            const double hoehe = gelCrd->dy;
            
            Coord* crdRW = db_rw ? db_rw->GetCoord( breite, X_TOLL ) : 0;
            Coord* crdHW = db_hw ? db_hw->GetCoord( breite, X_TOLL ) : 0;
            Coord* crdRauheit = db_rauheit ? db_rauheit->GetCoord( breite, X_TOLL ) : 0;
            Coord* crdAx = db_ax ? db_ax->GetCoord( breite, X_TOLL ) : 0;
			Coord* crdAy = db_ay ? db_ay->GetCoord( breite, X_TOLL ) : 0;
			Coord* crdDp = db_dp ? db_dp->GetCoord( breite, X_TOLL ) : 0;

            const double rw = crdRW ? crdRW->dy : 0.0;
            const double hw = crdHW ? crdHW->dy : 0.0;
            const double rauheit = crdRauheit ? crdRauheit->dy : 0.0;
			const double ax = crdAx ? crdAx->dy : -1.0;
			const double ay = crdAy ? crdAy->dy : -1.0;
			const double dp = crdDp ? crdDp->dy : -1.0;

            CString markerDB;
            CString markerBV;
            CString markerTF;

            const CString markerT = ( tiefCrd && fabs( tiefCrd->dx - gelCrd->dx ) < X_TOLL ) ? TIEFPUNKT : "";

            CString zoneF;
            if( crdDBlinks && crdDBrechts && crdTFlinks && crdTFrechts )
            {
              const double dbLinks = crdDBlinks->dx;
              const double dbRechts = crdDBrechts->dx;

              const double tfLinks = crdTFlinks->dx;
              const double tfRechts = crdTFrechts->dx;

              if( fabs( breite - dbLinks ) < X_TOLL )
                markerDB = MARKER_DB_LINKS;
              else if( fabs( breite - dbRechts ) < X_TOLL )
                markerDB = MARKER_DB_RECHTS;

              if( fabs( breite - tfLinks ) < X_TOLL )
                markerTF = MARKER_TF_LINKS;
              else if( fabs( breite - tfRechts ) < X_TOLL )
                markerTF = MARKER_TF_RECHTS;

              if( breite < dbLinks )
                zoneF = ZONE_RL;
              else if( breite >= dbRechts )
                zoneF = ZONE_RR;
              else if( breite < tfLinks )
                zoneF = ZONE_VL;
              else if( breite >= tfRechts )
                zoneF = ZONE_VR;
              else
                zoneF = ZONE_HF;
            }

            CString zoneB;
            if( crdBVlinks && crdBVrechts )
            {
              const double bvLinks = crdBVlinks->dx;
              const double bvRechts = crdBVrechts->dx;

              if( fabs( breite - bvLinks ) < X_TOLL )
                markerBV = MARKER_BV_LINKS;
              else if( fabs( breite - bvRechts ) < X_TOLL )
                markerBV = MARKER_BV_RECHTS;

              if( bvLinks <= breite && breite < bvRechts )
                zoneB = ZONE_B;
            }

            CString ausgabe;
            ausgabe.Format( "%s, %10.3lf,%8.3lf, %8.3lf, %11.3lf, %11.3lf, %7.3lf, %7.3lf, %7.3lf, %7.3lf, %s, %s, %s, %s, %s, %s\n", name, station, breite, hoehe, rw, hw, rauheit, ax, ay, dp, zoneF, zoneB, markerDB, markerTF, markerBV, markerT );
            ausgabeDatei.WriteString( ausgabe );
            
            gelCrd = db_gel->GetNextCoord();
          }
        }
        else
        {
          error = 2;
          break;
        }; // if profil
      }
      else
      {
        error = 1;
        break;
      }; // if cs
      cs->FlushProfil();
    }; // for i
  } // TRY
  catch( CFileException* e )
  {
    TCHAR  szCause[255];
    e->GetErrorMessage(szCause, 255);
    AfxMessageBox(szCause);
    return 3;
  }
    
  return error;
};


int Project::ExportDataHyk(CStdioFile& ausgabeDatei, CrossSectionArray* csArray )
//
// Exportiert Profildaten ins Hyk Format
//
// Parameter:
//          CStdioFile &ausgabedatei: Zeiger auf eine zum schreiben geöffnete ausgabedatei
//          CStringArray &profilListe: Liste der zu exportierenden Profile
// Rückgabe:
//          Fehlerkode:
//                    0 keine Fehler
//                    1 mindestens eins der Profile nicht vorhanden oder lässt sich nicht laden
//                    2 Profil ließ sich nicht initialisieren
//                    3 Fehler beim schreiben (MessageBox)
{
  int error = 0;
  TRY
  {
    double oldStation = 0.0;

    // falls mehr als Zwei profile vorhanden sind, soll der erste Abstand zwischen den Profilen, der
    // des ersten und zweiten Profils sein
    // ansonsten ist der Abstand des n. Profils stets der zwischen n. und n-1. 
    if ( csArray->GetSize() > 1 )
    {
      CrossSection* cs = csArray->GetAt( 1 );
      if ( cs )
        oldStation = cs->GetStation();
    };

    for ( int i = 0; i < csArray->GetSize(); i++ )
    {
      CrossSection* cs = csArray->GetAt( i );
      if ( cs )
      {
        cs->LoadProfil();
        Profil *profil = cs->GetProfil();
        if (profil)
        {
          double station = cs->GetStation();

          // hydraulische Daten einlesen
          CArray<double, double> zonenY;
          CArray<char, char> zonenTyp;

          DataBlock *db_trenn = profil->GetDataBlock(DST_TRENNFLAECHEN);
          if (db_trenn)
          {
            Coord *crd = db_trenn->GetFirstCoord();
            while (crd)
            {
              // Koordinate an richtiger Stelle einsortieren
              int index = 0;
              for (; index < zonenY.GetSize(); index++)
                if (crd->dx < zonenY[index])
                  break;
              zonenY.InsertAt(index, crd->dx);
              zonenTyp.InsertAt(index, 't');
              crd = db_trenn->GetNextCoord();
            };
          }; // if db_trenn

          DataBlock *db_boesch = profil->GetDataBlock(DST_BOESCHUNGSKANTEN);
          if (db_boesch)
          {
            Coord *crd = db_boesch->GetFirstCoord();
            while (crd)
            {
              // Koordinate an richtiger Stelle einsortieren
              int index = 0;
              for (; index < zonenY.GetSize(); index++)
                if (crd->dx < zonenY[index])
                  break;
              zonenY.InsertAt(index, crd->dx);
              zonenTyp.InsertAt(index, 'b');
              crd = db_boesch->GetNextCoord();
            };
          }; // if db_boesch

          DataBlock *db_durch = profil->GetDataBlock(DST_DURCHST_BEREICH);
          if (db_durch)
          {
            Coord *crd = db_durch->GetFirstCoord();
            while (crd)
            {
              // Koordinate an richtiger Stelle einsortieren
              int index = 0;
              for (; index < zonenY.GetSize(); index++)
                if (crd->dx < zonenY[index])
                  break;
              zonenY.InsertAt(index, crd->dx);
              zonenTyp.InsertAt(index, 'd');
              crd = db_durch->GetNextCoord();
            };
          }; // if db_durch
          
          DataBlock *db_modell = profil->GetDataBlock(DST_MODELLGRENZEN);
          if (db_modell)
          {
            Coord *crd = db_modell->GetFirstCoord();
            while (crd)
            {
              // Koordinate an richtiger Stelle einsortieren
              int index = 0;
              for (; index < zonenY.GetSize(); index++)
                if (crd->dx < zonenY[index])
                  break;
              zonenY.InsertAt(index, crd->dx);
              zonenTyp.InsertAt(index, 'm');
              crd = db_modell->GetNextCoord();
            };
          }; // if db_modell

          int zonen = zonenY.GetSize();
          if (zonen == 0)
          {
            oldStation = station;
            continue;  // dieses Profil enthält keine Fliesszonendaten
          };

          // dritte Zeile erzeugen
          CString outStr;
          int i;
          BOOL bRL = FALSE;
          BOOL bVL = FALSE;
          BOOL bBL = FALSE;

          char oldChoice = ' ';
          for (i = 0; i < zonen - 1; i++)
          {
            char newTyp = zonenTyp[i + 1];
            switch ( (int)(oldChoice + 256 * zonenTyp[i] + 256 * 256 * newTyp) )
            {
            case ' ' + 256 * 'm' + 256 * 256 * 'm':
            case ' ' + 256 * 'm' + 256 * 256 * 'd':
            case ' ' + 256 * 'm' + 256 * 256 * 'b':
            case ' ' + 256 * 'm' + 256 * 256 * 't':
            case ' ' + 256 * 'b' + 256 * 256 * 'm':
            case ' ' + 256 * 't' + 256 * 256 * 'm':
              outStr += " RL";
              bRL = TRUE;
              break;
              
            case 'R' + 256 * 'm' + 256 * 256 * 'm':
            case 'V' + 256 * 'm' + 256 * 256 * 'm':
            case 'B' + 256 * 'm' + 256 * 256 * 'm':
            case 'H' + 256 * 'm' + 256 * 256 * 'm':
            case 'R' + 256 * 'm' + 256 * 256 * 'd':
            case 'V' + 256 * 'm' + 256 * 256 * 'd':
            case 'B' + 256 * 'm' + 256 * 256 * 'd':
            case 'H' + 256 * 'm' + 256 * 256 * 'd':
            case 'R' + 256 * 'm' + 256 * 256 * 'b':
            case 'V' + 256 * 'm' + 256 * 256 * 'b':
            case 'B' + 256 * 'm' + 256 * 256 * 'b':
            case 'H' + 256 * 'm' + 256 * 256 * 'b':
            case 'R' + 256 * 'm' + 256 * 256 * 't':
            case 'V' + 256 * 'm' + 256 * 256 * 't':
            case 'B' + 256 * 'm' + 256 * 256 * 't':
            case 'H' + 256 * 'm' + 256 * 256 * 't':
            case 'V' + 256 * 'd' + 256 * 256 * 'm':
            case 'V' + 256 * 'd' + 256 * 256 * 'd':
            case 'R' + 256 * 'b' + 256 * 256 * 'm':
            case 'V' + 256 * 'b' + 256 * 256 * 'm':
            case 'B' + 256 * 'b' + 256 * 256 * 'm':
            case 'H' + 256 * 'b' + 256 * 256 * 'm':
            case 'R' + 256 * 't' + 256 * 256 * 'm':
            case 'V' + 256 * 't' + 256 * 256 * 'm':
            case 'B' + 256 * 't' + 256 * 256 * 'm':
            case 'H' + 256 * 't' + 256 * 256 * 'm':
              outStr += " RR";
              break;

            case ' ' + 256 * 'd' + 256 * 256 * 'm':
            case ' ' + 256 * 'd' + 256 * 256 * 'd':
            case ' ' + 256 * 'd' + 256 * 256 * 'b':
            case ' ' + 256 * 'd' + 256 * 256 * 't':
            case ' ' + 256 * 'b' + 256 * 256 * 'd':
            case ' ' + 256 * 't' + 256 * 256 * 'd':
              outStr += " VL";
              bVL = TRUE;
              break;

            case 'R' + 256 * 'd' + 256 * 256 * 'm':
            case 'R' + 256 * 'd' + 256 * 256 * 'd':
            case 'B' + 256 * 'd' + 256 * 256 * 'd':
            case 'H' + 256 * 'd' + 256 * 256 * 'd':
            case 'R' + 256 * 'd' + 256 * 256 * 'b':
            case 'V' + 256 * 'd' + 256 * 256 * 'b':
            case 'R' + 256 * 'd' + 256 * 256 * 't':
            case 'R' + 256 * 't' + 256 * 256 * 'd':
              if ( bVL )
                outStr += " VR";
              else
              {
                outStr += " VL";
                bVL = TRUE;
              };
              break;

            case 'B' + 256 * 'd' + 256 * 256 * 'm':
            case 'H' + 256 * 'd' + 256 * 256 * 'm':
            case 'B' + 256 * 'd' + 256 * 256 * 'b':
            case 'H' + 256 * 'd' + 256 * 256 * 'b':
            case 'V' + 256 * 'd' + 256 * 256 * 't':
            case 'B' + 256 * 'd' + 256 * 256 * 't':
            case 'H' + 256 * 'd' + 256 * 256 * 't':
            case 'R' + 256 * 'b' + 256 * 256 * 'd':
            case 'V' + 256 * 'b' + 256 * 256 * 'd':
            case 'B' + 256 * 'b' + 256 * 256 * 'd':
            case 'H' + 256 * 'b' + 256 * 256 * 'd':
            case 'B' + 256 * 'b' + 256 * 256 * 'b':
            case 'V' + 256 * 't' + 256 * 256 * 'd':
            case 'B' + 256 * 't' + 256 * 256 * 'd':
            case 'H' + 256 * 't' + 256 * 256 * 'd':
            case 'H' + 256 * 't' + 256 * 256 * 't':
              outStr += " VR";
              break;

            case ' ' + 256 * 'b' + 256 * 256 * 'b':
            case ' ' + 256 * 'b' + 256 * 256 * 't':
            case ' ' + 256 * 't' + 256 * 256 * 'b':
              outStr += " BL";
              bBL = TRUE;
              break;

            case 'R' + 256 * 'b' + 256 * 256 * 'b':
            case 'V' + 256 * 'b' + 256 * 256 * 'b':
            case 'R' + 256 * 'b' + 256 * 256 * 't':
            case 'V' + 256 * 'b' + 256 * 256 * 't':
            case 'R' + 256 * 't' + 256 * 256 * 'b':
              if ( bBL )
                outStr += " BR";
              else
              {
                outStr += " BL";
                bBL = TRUE;
              };
              break;

            case 'H' + 256 * 'b' + 256 * 256 * 'b':
            case 'B' + 256 * 'b' + 256 * 256 * 't':
            case 'H' + 256 * 'b' + 256 * 256 * 't':
            case 'V' + 256 * 't' + 256 * 256 * 'b':
            case 'B' + 256 * 't' + 256 * 256 * 'b':
            case 'H' + 256 * 't' + 256 * 256 * 'b':
              outStr += " BR";
              break;

            case ' ' + 256 * 't' + 256 * 256 * 't':
            case 'R' + 256 * 't' + 256 * 256 * 't':
            case 'V' + 256 * 't' + 256 * 256 * 't':
            case 'B' + 256 * 't' + 256 * 256 * 't':
              outStr += " HF";
              break;

            default:
              ASSERT(FALSE); // darf nicht sein
            }; // switch Typ
            oldChoice = outStr[outStr.GetLength() - 2];
            if (outStr.GetLength() >= 3 * 16)
              break;  // maximal 16 Fliesszonen dürfen geschrieben werden
          }; // for i

          zonen = int(outStr.GetLength()) / 3;

          // Daten schreiben
          CString string;
          // erste Zeile schreiben
          ausgabeDatei.WriteString("*-------------------------------------------*\n");
          string.Format("%8.3lf         1\n", station);
          ausgabeDatei.WriteString(string);
          // zweite Zeile schreiben
          string.Format("%2d         0.00 999.99\n", zonen);
          ausgabeDatei.WriteString(string);
          // dritte Zeile schreiben
          ausgabeDatei.WriteString(outStr + "\n");
          // vierte Zeile schreiben
          outStr = "";
          for (i = 0; i < min(8, zonen); i++)
          {
            CString hlpStr;
            hlpStr.Format("%7.1lf  ", zonenY[i]);
            outStr += hlpStr;
          };
          ausgabeDatei.WriteString(outStr + "\n");  
          // fünfte Zeile schreiben
          outStr = "";
          for (i = 0; i < zonen - 8; i++)
          {
            CString hlpStr;
            hlpStr.Format("%7.1lf  ", zonenY[i + 8]);
            outStr += hlpStr;
          };
          ausgabeDatei.WriteString(outStr + "\n");
          //sechste Zeile schreiben
          double abstand = min ( fabs( station - oldStation ) * 1000, 99999.0 );
          outStr.Format( "%#5.0lf  %#5.0lf  %#5.0lf\n", abstand, abstand, abstand );
          ausgabeDatei.WriteString(outStr);

          oldStation = station;
        }
        else
        {
          error = 2;
          break;
        }; // if profil
      }
      else
      {
        error = 1;
        break;
      }; // if cs
      cs->FlushProfil();
    }; // for i
  } // TRY
  CATCH(CFileException, e)
  {
    TCHAR  szCause[255];
    e->GetErrorMessage(szCause, 255);
    AfxMessageBox(szCause);
    return 3;
  }
  END_CATCH

  return error;
}; // ExportDataHyk

int Project::ExportDataReli( CStdioFile& ausgabeDatei, CrossSectionArray* csArray, BOOL bLeft )
//
// Exportiert Profildaten ins ReLi Format
//
// Parameter:
//          CStdioFile &ausgabedatei: Zeiger auf eine zum schreiben geöffnete ausgabedatei
//          CrossSectionArray* csArray: Liste der zu exprotierenden Profile
//          BOOL bLeft:  bei TRUE werden linke, sonst rechte Buhnen exportiert
// Rückgabe:
//          Fehlerkode:
//                    0 keine Fehler
//                    1 mindestens eins der Profile nicht vorhanden oder lässt sich nicht laden
//                    2 Profil ließ sich nicht initialisieren
//                    3 Fehler beim schreiben (MessageBox)
{
  int error = 0;
  CString outString;

  TRY // für eventuelle Schreibfehler
  {
    outString = "Station Abstand K-Hoehe Neigung\n";
    ausgabeDatei.WriteString( outString );
    outString = "[km]      [m]  [NN+m] Rue Ko\n";
    ausgabeDatei.WriteString( outString );

    for (int i = 0; i < csArray->GetSize(); i++)
    {
      CrossSection* cs = csArray->GetAt( i );
      if (cs && cs->LoadProfil())
      {
        Profil *profil = cs->GetProfil();
        if (profil)
        {
          DataBlock* db = profil->GetDataBlock(DST_BUHNEN);
          if ( db )
          {
            CString dbName = db->GetName( 1 );  // Information über Ausrichtung der Buhne
            dbName.MakeUpper();
            int numCoords = db->GetNumCoords();

            Coord* crd1 = db->GetFirstCoord();
            while ( crd1 )
            {
              char reli = 'R';  //im Zweifelsfalle ists eine rechte Buhne
                
              if ( dbName.GetLength() >= 2 )
              {
                reli = dbName[1];
                dbName = dbName.Mid( 2 ); // die beiden ersten Stellen abschneiden
              };
              Coord* crdM = db->GetNextCoord();
              Coord* crd2 = db->GetNextCoord();
              
              double frontSlope = ( crd2->dx - crdM->dx ) / ( crd2->dy - crdM->dy ); // Neigung der Vorderseite ( Kehrwert der Geradenneigung )
              double backSlope = ( crdM->dx - crd1->dx ) / ( crdM->dy - crd1->dy ); // Neigung der Rückseite
              if ( reli == 'L' && bLeft )
              { // Vorzeichen umkehren
                frontSlope = -frontSlope;
                backSlope = -backSlope;
              }
              else if ( reli == 'R' && !bLeft )
              { // Vroder- und Rückseite vertauschen
                double tmp = frontSlope;
                frontSlope = backSlope;
                backSlope = tmp;
              }
              else
              {
                crd1 = db->GetNextCoord();
                continue;
              };
              
              outString.Format( "%9.3lf %7.2lf %7.2lf %3.0f %3.0f\n", 
                cs->GetStation(), crdM->dx, crdM->dy, backSlope, frontSlope );
              ausgabeDatei.WriteString( outString );
              
              crd1 = db->GetNextCoord();
            }; // while crd
          }; // if db
        }
        else
        {
          error = 2;
          break;
        }; // if profil
      }
      else
      {
        error = 1;
        break;
      }; // if cs
      cs->FlushProfil(); // Profil wieder freigeben
    }; // for i
  }
  CATCH( CFileException, e )
  {
    TCHAR  szCause[255];
    e->GetErrorMessage(szCause, 255);
    AfxMessageBox(szCause);
    return 3; // Schreibfehler
  }
  END_CATCH
    
    return error;
};

BOOL Project::RemoveState(State* zustand)
{
  POSITION pos = m_States.Find(zustand);
  if (pos)
  {
    // nachschauen, ob es der letzte Zustand seines Gewaessers war
    State *st = GetFirstState();
    BOOL bFound = FALSE;
    while (st)
    {
      if (st != zustand && st->GetWaterName() == zustand->GetWaterName())
      {
        bFound = TRUE;
        break;
      };
      st = GetNextState();
    };
    if (!bFound)
    { // Gewaesser löschen
      for (int i = 0; i < m_waters.GetSize(); i++)
      {
        if (m_waters[i] == zustand->GetWaterName())
        {
          m_waters.RemoveAt(i);
          break;
        };
      };
    };

    // Zustand löschen
    m_States.RemoveAt(pos);
    delete zustand;
    m_bModified = TRUE;
    return TRUE;
  }
  else
    return FALSE;
}

BOOL Project::LoadProjectName()
// liest den Projektnamen ein
{
  CFileStatus fileStatus;
  CString filename = m_dir + "\\prof\\probez.txt";

  if ( CFile::GetStatus( filename, fileStatus ) )
  {
    try
    {
      CStdioFile file( filename, CFile::modeRead );
      file.ReadString( m_name );
      file.Close();
      m_name.TrimLeft();
      m_name.TrimRight();
      return TRUE;
    }
    catch( CFileException* e)
    {
      e->Delete();
      m_name = "";
      return FALSE;
    }; // catch

    return TRUE;
  };

  return FALSE;
}; // LoadProjectName

BOOL Project::SaveProjectName() const
// speichert den Projektnamen in \\prof\\probez.txt
{
  try
  {
    CStdioFile file( GetDataDir() + "probez.txt", CFile::modeCreate | CFile::modeWrite );
    file.WriteString( m_name );
    file.Close();
  }
  catch( CFileException* e )
  {
    e->Delete();
    return FALSE;
  };

  return TRUE;
}; // SaveProjectName

Project::Vernetzung* Project::GetVernetzung( CrossSectionArray* csArray /* = NULL */, 
                                             StatesArray* stateArray /* = NULL */ )
// gibt eine für jedes Profil eine Liste seiner Vorgänger und Nachfolger zurück ( siehe Def. von Vernetzung )
// Parameter:
//        CrossSectionArray* csArray: falls != NULL wird nur innerhalb dieser Querprofile vernetzt;
//                                  d.h. nur für Elemente aus csArray werden die Vorgänger etc. gesucht;
//                                    und es werden immer die nächsten aus dieser Liste bestimmt
//        StatesArray* stateArray: Information, welche CrossSection zu welchem State zugeordnet werden soll. Nur
//                                Querprofile aus dem gleichen Zustand werden verglichen
// Rückgabewert:
//        Vernetzung*: Bedeutung siehe Definition; NULL bei Fehler
// Bemerkung:
//        die beiden Eingabearrays müssen gleich gross sein
{
  if( csArray && ( !stateArray || csArray->GetSize() != stateArray->GetSize() ) )
    return NULL;

  CrossSectionArray* querprofs = csArray;
  StatesArray* zustaende = stateArray;

  // falls keine Angaben gemacht wurden, wird alles erzeugt, die Profile werden jeweils ihrem ersten Zustand
  // zugeordnet
  if( !querprofs )
  {
    querprofs = new CrossSectionArray;
    zustaende = new StatesArray;

    POSITION pos = m_CrossSections.GetHeadPosition();
    while( pos )
    {
      CrossSection* cs = m_CrossSections.GetNext( pos );
      if( cs )
      {
        querprofs->Add( cs );
        zustaende->Add( cs->GetFirstState() ); // möglicherweise NULL
      }; // if cs
    }; // while pos
    if( querprofs->GetSize() == 0 )
    {
      delete querprofs;
      querprofs = NULL;
      delete zustaende;
      zustaende = NULL;
    }; // if size == 0
  }; // if querprofs == NULL

  if( !querprofs || querprofs->GetSize() == 0 || !zustaende )
    return NULL;

  // so, jetzt die Vernetzung anlegen
  Vernetzung* netz = new Vernetzung;

  // für jedes Profil seine Vorgänger und nachfolger suchen
  for( int i = 0; i < querprofs->GetSize(); i++ )
  {
    CrossSection* cs = querprofs->GetAt( i ); // die aktuelle um die es geht
    State* zs = zustaende->GetAt( i );

    double csStation = cs->GetStation();
    int csVzk = cs->GetVZK();
    CString csPk = cs->GetPK();

    // falls die cs nicht ok oder nicht unter einem Zustand gemeldet ist überspringen
    if( !cs || !zs )
      continue;

    // wir brauchen ein Liste der CrossSections aus diesem Zustand
    // Profile mit gleicher Station und gleicher VZK werden weggelassen -> Mehrfeldbrücke
    CrossSectionArray csSubArray;
    for( int k = 0; k < csArray->GetSize(); k++ )
    {
      CrossSection* otherCs = csArray->GetAt( k );
      State* otherState = stateArray->GetAt( k );
      if( i == k || !otherCs || !otherState )
        continue;

      double otherStation = otherCs->GetStation();
      double otherVZK = otherCs->GetVZK();
      CString otherPK = otherCs->GetPK();

      // nur zufügen, falls im gleichen Zustand
      // und falls es keine Mehrfeldbrücke ist oder station oder vzk sind unterschiedlich sind
      if( otherState == zs && 
          ( csPk.IsEmpty() || csVzk != otherVZK || fabs( csStation - otherStation ) > 0.00001 ) )
        csSubArray.Add( csArray->GetAt( k ) );
    }; // for k

    // das Netz vorbereiten
    CrossSectionArray* csNetz = new CrossSectionArray;
    netz->SetAt( cs, csNetz );

    // einmal vorwärts, einmal rückwärts suchen; doppelte rausfiltern
    for( int b = 0; b < 2; b++ )
    {
      CrossSection* leftCs = zs->GetFollowingCs( cs, csSubArray, b, TRUE );
      CrossSection* rightCs = zs->GetFollowingCs( cs, csSubArray, b, FALSE );
      if( rightCs == leftCs )
        rightCs = NULL;
      csNetz->Add( leftCs ); // linker Vorgänger
      csNetz->Add( rightCs  ); // rechter Vorgänger
    }; // for b
  }; // for i

  if( !csArray )
  {
    delete querprofs; // dann wurde es oben erzeugt;
    querprofs = NULL;
  }; // if csArray == NULL


  if( netz->GetCount() == 0 )
  {
    delete netz;
    netz = NULL;
  }; // if netzt.GetCount = 0

  return netz;
}; // GetVernetzung


/**
 * Creates a textual summary of the project
 * @param string All output is appended to this array
 */
void Project::CreatePrintSummary( CStringArray& strings )
{
  static CString underscore = "______________________________________________________________________________________________";

  Load();

  strings.Add( "**********************************************************************************************" );
	strings.Add( "*                                                                                            *" );
	strings.Add( CString( MAKEINTRESOURCE( IDS_PROJ_OVIEW ) ) );
	strings.Add( "*                                                                                            *" );
	strings.Add( "**********************************************************************************************" );
  strings.Add( "" );
  strings.Add( CString( MAKEINTRESOURCE( IDS_PROJ_NAME ) ) + GetName() );
  strings.Add( CString( MAKEINTRESOURCE( IDS_PROJ_DIR ) ) + GetDir() );
  strings.Add( "" );

	for( int i = 0; i < GetWaterCount(); i++ )
	{
    CString wname = GetWaterName( i );
		
    CString str, temp;
    temp.Format( "%d", i + 1 );
    str.FormatMessage( IDS_WATERWAYN, temp, wname );
		
    strings.Add( str );
    strings.Add( underscore );

		State* st = GetFirstState();
    while( st )
    {
      CString str = st->GetWaterName();
      if( str == wname )
        st->CreatePrintSummary( strings );

      st = GetNextState();
    }
  }
}