/*! Time-stamp: <@(#)profil.cpp   29.08.02 - 11:46:50   Belger>
 *********************************************************************
 *  @file   : profil.cpp
 *
 *  Project : WSPWIN
 *
 *  Package : Projektmanager
 *
 *  Company : BCE
 *
 *  Author  : Belger                              Date: 29.08.02
 *
 *  Purpose : Implementation of methods for class Profil
 *
 *********************************************************************
 */
#include "stdafx.h"

#include "textBlock.h"
#include "dataBlck.h"
#include "giostr.h"
#include "section.h"
#include "csection.h"
#include "coord.h"
#include "triple.h"

#include "profil.h"

  ////////////////////////////
  //  Klasse  Profil
  ///////////////////////////

/* The Default Constructor */
Profil::Profil(Section* pOwner)
{
	for ( int i = 0; i < 6; i++ )
	{
		m_nControlSizes[i] = 0;
		m_dControlData[i] = 0;
	}
	for ( i = 0; i < 4; i++ )
		m_nControlData[i] = 0;
	m_DBlockPos = NULL;
	m_nNr = 0;
	m_pOwner = pOwner;
	m_bModified = FALSE;
  m_date.SetDateTime( 1900, 1, 1, 0, 0, 0 );
  m_strPageNum = "B-1";
};

Profil::~Profil()
{
	POSITION pos;
	DataBlock* db;

	pos = m_DataBlocks.GetHeadPosition();
	while (pos!=NULL)
	{
		db = m_DataBlocks.GetNext(pos);
		delete db;
	}
	m_DataBlocks.RemoveAll();
}

BOOL Profil::Load( const CString& filename )
{
	gifstream ifs;
	CString rString;
	CFileStatus rStatus;
	BOOL bOK = FALSE;

	if (CFile::GetStatus(filename, rStatus))
	{
		ifs.open(filename, ios::in);
		if (!ifs.fail())
		{
			ifs >> *this;
			ifs.close();
			bOK = TRUE;
		}
	}

	if (!bOK)
	{
		rString.FormatMessage("Konnte Datei %1 nicht zum lesen öffnen.", filename);
		AfxMessageBox(rString, MB_ERROR);
		return FALSE;
	}
	m_bModified = FALSE;

	return TRUE;
}

BOOL Profil::Save( const CString& filename )
{
	gofstream ofs;
	CString rString;

	if( m_bModified )
	{
		ofs.open( filename, ios::out );
		if( ofs.fail() )
		{
			rString.FormatMessage("Konnte Datei %1 nicht zum Schreiben öffnen.", filename);
			AfxMessageBox(rString, MB_ERROR);
			return FALSE;
		}
		else
		{
			ofs << *this;
			ofs.close();
		}
	}
	m_bModified = FALSE;

	return TRUE;
}

Profil* Profil::Clone(Section *sec)
{
	DataBlock *db, *newDB;
	Profil *clone;
	int i;

	clone = new Profil(sec);

	for (i=0; i<2; i++)
		clone->m_strClient[i] = m_strClient[i];
	for (i=0; i<3; i++)
		clone->m_strProjectDesc[i] = m_strProjectDesc[i];
	for (i=0; i<3; i++)
		clone->m_strPageDesc[i] = m_strPageDesc[i];
	clone->m_strProjectNum = m_strProjectNum;
	clone->m_date = m_date;
	clone->m_strPageNum = m_strPageNum;
	for (i=0; i<6; i++)
		clone->m_nControlSizes[i] = m_nControlSizes[i];
	for (i=0; i<3; i++)
		clone->m_strCrossRef[i] = m_strCrossRef[i];
	clone->m_zeichnungsueberschrift = m_zeichnungsueberschrift;
	for (i=0; i<6; i++)
		clone->m_dControlData[i] = m_dControlData[i];
	for (i=0; i<4; i++)
		clone->m_nControlData[i] = m_nControlData[i];
	db = GetFirstDataBlock();
	while (db!=NULL)
	{
		newDB = db->Clone( clone );
		clone->AddDataBlock(newDB);
		db = GetNextDataBlock();
	}

	clone->SetModified();
	return clone;
}

void Profil::SetModified()
{
	m_bModified = TRUE;
  if ( m_pOwner )
    m_pOwner->SetModified();
}

void Profil::AddDataBlock( DataBlock* db )
// der Datenblock wird sortiert hinzugefügt: es wird nach der Typ-Nummer sortiert
// negative TypNummern kommen ganz ans Ende
{
  if( !db )
    return;

  int typ = db->GetType();
  if( typ <= 0 )
    typ = -typ + 1000;

  POSITION pos = m_DataBlocks.GetHeadPosition();
  POSITION lastPos = NULL;
  BOOL bFound = FALSE;
  while( pos )
  {
    lastPos = pos;
    DataBlock* oldDb = m_DataBlocks.GetNext( pos );
    if( oldDb && oldDb->GetType() > typ )
    {
      bFound = TRUE;
      break;
    }; // if oldDb
  }; // while pos

  if( bFound && lastPos )
    m_DataBlocks.InsertBefore( lastPos, db );
  else
    m_DataBlocks.AddTail(db);

	SetModified();
}; // AddDataBlock

void Profil::RemoveDataBlock( DataBlock* db )
{
	POSITION pos = m_DataBlocks.Find( db );
	if( pos != NULL )
		m_DataBlocks.RemoveAt( pos );
	SetModified();
}

DataBlock* Profil::GetFirstDataBlock()
{
	m_DBlockPos = m_DataBlocks.GetHeadPosition();
	return GetNextDataBlock();
}

DataBlock* Profil::GetNextDataBlock()
{
	if (m_DBlockPos==NULL)
		return NULL;
	else
		return m_DataBlocks.GetNext(m_DBlockPos);
}

DataBlock* Profil::GetDataBlockByIndex( int index )
// einen speziellen Datenblock zurück
// Parameter:
//        int index: 0 basierter Index des gesuchten Datenblocks
// Rückgabewert:
//        DataBlock*: NULL, falls kein Datenbock gefunden wurde
{
  POSITION pos = m_DataBlocks.FindIndex( index );
  if( pos )
    return m_DataBlocks.GetAt( pos );
  else
    return NULL;
}; // GetDataBlockBayIndex

//index bedeutet -> "der wievielte" weil mehrere gleiche sein kann zB WSP_HOEHE
DataBlock* Profil::GetDataBlock( const int type , const int index )
{
	int anz = 0;
	POSITION pos = m_DataBlocks.GetHeadPosition();
	while (pos!=NULL)
	{
		DataBlock* db = m_DataBlocks.GetNext(pos);
    if( db->GetType() == type )
    {
      anz++;
      if( index < 1 || index == anz )
        return db;
    }
  }
  return NULL;
} // GetDataBlock

//index bedeutet -> "der wievielte" weil mehrere gleiche sein kann zB WSP_HOEHE
DataBlock* Profil::GetDataBlock( const int type , const int index ) const
{
	int anz = 0;
	POSITION pos = m_DataBlocks.GetHeadPosition();
	while (pos!=NULL)
	{
		DataBlock* db = m_DataBlocks.GetNext(pos);
    if( db->GetType() == type )
    {
      anz++;
      if( index < 1 || index == anz )
        return db;
    }
  }
  return NULL;
} // GetDataBlock


int Profil::GetDataBlockNum( const int type ) const  //gibt Anzahl der gleichen Datensätzen zB WSP_HOEHE
{
  int anz = 0;
  POSITION pos = m_DataBlocks.GetHeadPosition();
  while( pos )
	{
    DataBlock* db = m_DataBlocks.GetNext( pos );
    if( db && db->GetType() == type )
      anz++;
  }; // while pos
  return anz;
} // GetDataBlockNum

int Profil::GetNumDataBlocks() const
{
	return m_DataBlocks.GetCount();
}

int Profil::GetDataBlockIndex(DataBlock* db)
{
	POSITION pos;
	DataBlock *dbtest;
	int nIndex = 0;
	BOOL bFound = FALSE;

	pos = m_DataBlocks.GetHeadPosition();
	while (pos!=NULL)
	{
		dbtest = m_DataBlocks.GetNext(pos);
		if (dbtest==db)
		{
			bFound = TRUE;
			break;
		}
		nIndex++;
	}
	if (bFound)
		return nIndex;

	return -1;
}

POSITION Profil::GetDataBlockPos() const
{
	return m_DBlockPos;
}

void Profil::SetDataBlockPos(POSITION pos)
{
	m_DBlockPos = pos;
}

Section* Profil::GetOwner()
{
	return m_pOwner;
}; // GetOwner

void Profil::SetOwner( Section* section )
// setzt den Besitzer dieses Profils
// Parameter:
//        Section* section: der neue Besitzer
// Bemerkung:
//        wird nur ausgeführt, falls noch kein Besitzer vorhanden ist
{
  if( !m_pOwner )
    m_pOwner = section;
}; // SetOwner

CString Profil::GetOriginalFile() const
{
	return m_strOriginFile;
}

void Profil::SetOriginalFile( const CString& str )
{
	m_strOriginFile = str;
}

CString Profil::GetClient( const int n ) const
{
	CString text;

	if( n >= 0 && n < 2 )
		text = m_strClient[n];
  return text;
}

void Profil::SetClient( const int n, const CString& str )
{
	if (n>=0 && n<2)
		m_strClient[n] = str;
	SetModified();
}

CString Profil::GetProjectDesc( const int n ) const
{
	if ( n >= 0 && n < 3 )
		return m_strProjectDesc[n];
  else
    return CString();
}

void Profil::SetProjectDesc( const int n, const CString& str )
{
	if (n>=0 && n<3)
		m_strProjectDesc[n] = str;
	SetModified();
}

CString Profil::GetPageDesc( const int n ) const
{
	CString text;

	if ( n >= 0 && n < 3 )
		text = m_strPageDesc[n];
	return text;
}

void Profil::SetPageDesc( const int n, const CString& str )
{
	if (n>=0 && n<3)
		m_strPageDesc[n] = str;
	SetModified();
}

CString Profil::GetProjectNum() const
{
	return m_strProjectNum;
}

void Profil::SetProjectNum( const CString& str )
{
	m_strProjectNum = str;
	SetModified();
}

COleDateTime Profil::GetDate() const
{
	return m_date;
}

void Profil::SetDate( const COleDateTime& date )
{
	m_date = date;
	SetModified();
}

CString Profil::GetPageNum() const
{
	return m_strPageNum;
}

CString Profil::GetDrawingTitle() const
{
	return m_zeichnungsueberschrift;
}

void Profil::SetPageNum( const CString& str )
{
	m_strPageNum = str;
	SetModified();
}

int Profil::GetNum() const
{
	return m_nNr;
}

void Profil::SetNum(int n)
{
	m_nNr = n;
	m_strPageDesc[1].Format("Querprofil %d", n);
	SetModified();
}

void Profil::SetStateAndWaterName( LPCTSTR stateName, LPCTSTR waterName )
// setzt Zeile 0 der PageDescription
// Parameter:
//        LPCTSTR stateName, waterName: Zustands undGewässername, werden einfach in dieser Zeile abgelegt
{
  CString description;
  description.Format( "%s %s", stateName, waterName );
  SetPageDesc( 0, description );
}; // SetStateAndWaterName

void Profil::SetStateName( LPCTSTR stateName )
{
  SetStateAndWaterName( stateName, GetWaterName() );
}; // SetStateName

void Profil::SetWaterName( LPCTSTR waterName )
{
  SetStateAndWaterName( GetStateName(), waterName );
}; // SetWaterName

CString Profil::GetStateName() const
// gibt den ersten Teil der ersten Zeile der PageDescription zurück
{
  CString description = GetPageDesc( 0 );
  TCHAR stateName[1000];
  stateName[0] = '\0'; // kann sein das sscanf nix reinschreibt, dann steht vielleicht Schrott drin
  TCHAR waterName[1000];
  waterName[0] = '\0';

  sscanf( description, "%s %s", stateName, waterName );
  return CString( stateName );
}; // GetStateName

CString Profil::GetWaterName() const
// gibt den zweiten Teil der ersten Zeile der PageDescription zurück
{
  CString description = GetPageDesc( 0 );
  TCHAR stateName[1000];
  stateName[0] = '\0';
  TCHAR waterName[1000];
  waterName[0] = '\0';

  sscanf( description, "%s %s", stateName, waterName );
  return CString( waterName );
}; // GetStateName

void Profil::SetStation( const double station )
// setzt die 3.Zeile der PageDexcription
{
  CString stationStr;
  stationStr.Format( "STATION KM %.4f", station );
  SetPageDesc( 2, stationStr );
}; // SetStation

double Profil::GetAbstandShriftfeld() const
{
	return m_dControlData[0];
}

double Profil::GetBlattlaenge() const
{
	return m_dControlData[1];
}

double Profil::GetBezugshoehe() const
{
	return m_dControlData[2];
}

double Profil::GetXMaszstab() const
{
	return m_dControlData[3];
}

double Profil::GetYMaszstab() const
{
	return m_dControlData[4];
}

double Profil::GetBlatthoehe() const
{
	return m_dControlData[5];
}

int Profil::GetSchriftfeldUT() const
{
	return m_nControlData[0];
}

int Profil::GetWertenUD() const
{
	return m_nControlData[1];
}

int Profil::GetBezugshoeheUD() const
{
	return m_nControlData[2];
}

int Profil::GetSchriftfeldZUT() const
{
	return m_nControlData[3];
}

istream& operator>>(istream& is, Profil &prof)
{
	char buffer[LINE_SIZE];
	CString str;
	int i, m;
	CArray<int, int> nCoord;
	DataBlock* db;

	// 1. Zeile
	is.getline(buffer, LINE_SIZE, '\n');
	prof.m_strOriginFile = buffer;
	prof.m_strOriginFile.TrimLeft();
	prof.m_strOriginFile.TrimRight();
	// 2. Zeile
	is.getline(buffer, LINE_SIZE, '\n');
	prof.m_strClient[0] = buffer;
	prof.m_strClient[0] = prof.m_strClient[0].Left(36);
	prof.m_strClient[0].TrimLeft();
	prof.m_strClient[0].TrimRight();
	// 3. Zeile
	is.getline(buffer, LINE_SIZE, '\n');
	prof.m_strClient[1] = buffer;
	prof.m_strClient[1] = prof.m_strClient[1].Left(36);
	prof.m_strClient[1].TrimLeft();
	prof.m_strClient[1].TrimRight();
	if( prof.m_pOwner && prof.m_pOwner->GetClassType() == CLASS_TYPE_CSECTION )
	{
		str = ((CrossSection*)prof.m_pOwner)->GetStateName();
		if (str.IsEmpty())
		{
			str = buffer;
			str = str.Right(str.GetLength()-36);
			str.TrimLeft();
			str.TrimRight();
			((CrossSection*)prof.m_pOwner)->SetStateName(str);
		}
	}
	// 4. Zeile
	is.getline(buffer, LINE_SIZE, '\n');
	prof.m_strProjectDesc[0] = buffer;
	prof.m_strProjectDesc[0] = prof.m_strProjectDesc[0].Left(36);
	prof.m_strProjectDesc[0].TrimLeft();
	prof.m_strProjectDesc[0].TrimRight();
	// 5. Zeile
	is.getline(buffer, LINE_SIZE, '\n');
	prof.m_strProjectDesc[1] = buffer;
	prof.m_strProjectDesc[1] = prof.m_strProjectDesc[1].Left(36);
	prof.m_strProjectDesc[1].TrimLeft();
	prof.m_strProjectDesc[1].TrimRight();
	// 6. Zeile
	is.getline(buffer, LINE_SIZE, '\n');
	prof.m_strProjectDesc[2] = buffer;
	prof.m_strProjectDesc[2] = prof.m_strProjectDesc[2].Left(36);
	prof.m_strProjectDesc[2].TrimLeft();
	prof.m_strProjectDesc[2].TrimRight();
	if( prof.m_pOwner && prof.m_pOwner->GetClassType() == CLASS_TYPE_CSECTION )
	{
		str = ((CrossSection*)prof.m_pOwner)->GetWaterName();
		if (str.IsEmpty())
		{
			str = buffer;
			str = str.Right(str.GetLength()-36);
			str.TrimLeft();
			str.TrimRight();
			((CrossSection*)prof.m_pOwner)->SetWaterName(str);
		}
	}
	// 7. Zeile
	is.getline(buffer, LINE_SIZE, '\n');
	prof.m_strPageDesc[0] = buffer;
	prof.m_strPageDesc[0] = prof.m_strPageDesc[0].Left(36);
	prof.m_strPageDesc[0].TrimLeft();
	prof.m_strPageDesc[0].TrimRight();
	// 8. Zeile
	is.getline(buffer, LINE_SIZE, '\n');
	prof.m_strPageDesc[1] = buffer;
	prof.m_strPageDesc[1] = prof.m_strPageDesc[1].Left(36);
	prof.m_strPageDesc[1].TrimLeft();
	prof.m_strPageDesc[1].TrimRight();
	if( prof.m_pOwner && prof.m_strPageDesc[1].Left(10).CompareNoCase("Querprofil") == 0 )
		prof.m_pOwner->SetProfilNr( atoi( prof.m_strPageDesc[1].Right(prof.m_strPageDesc[1].GetLength() - 10 ) ) );
	
  // 9. Zeile
	is.getline(buffer, LINE_SIZE, '\n');
	prof.m_strPageDesc[2] = buffer;
	prof.m_strPageDesc[2] = prof.m_strPageDesc[2].Left(36);
	prof.m_strPageDesc[2].TrimLeft();
	prof.m_strPageDesc[2].TrimRight();
	if( prof.m_pOwner && prof.m_pOwner->GetClassType() == CLASS_TYPE_CSECTION )
	{
		str = prof.m_strPageDesc[2];
		i = str.ReverseFind(' ');
		if (i!=-1)
		{
			str = str.Right(str.GetLength()-i-1);
			((CrossSection*)prof.m_pOwner)->SetStation(atof(str));
		}
	}
	// 10. Zeile
	is.getline(buffer, LINE_SIZE, '\n');
	prof.m_strProjectNum = buffer;
	prof.m_strProjectNum = prof.m_strProjectNum.Left(16);
	prof.m_strProjectNum.TrimLeft();
	prof.m_strProjectNum.TrimRight();
	// 11. Zeile
	is.getline(buffer, LINE_SIZE, '\n');
	prof.m_date.ParseDateTime(buffer, VAR_DATEVALUEONLY);
	// 12. Zeile
	is.getline(buffer, LINE_SIZE, '\n');
	str = buffer;
	str += " ";
	str.TrimLeft();
	prof.m_strPageNum = str.Left(str.Find(" "));
	str = str.Right(str.GetLength()-str.Find(" "));
	str.TrimLeft();
	for (i=0; i<6 && !str.IsEmpty(); i++)
	{
		prof.m_nControlSizes[i] = atoi(str.Left(str.Find(" ")));
		str = str.Right(str.GetLength()-str.Find(" "));
		str.TrimLeft();
	}
	for (i=0; i<3 && !str.IsEmpty(); i++)
	{
		prof.m_strCrossRef[i] = str.Left(str.Find(" "));
		str = str.Right(str.GetLength()-str.Find(" "));
		str.TrimLeft();
	}
	// 13. Zeile
	is.getline(buffer, LINE_SIZE, '\n');
	prof.m_zeichnungsueberschrift = buffer;
	prof.m_zeichnungsueberschrift = prof.m_zeichnungsueberschrift.Left(36);
	prof.m_zeichnungsueberschrift.TrimLeft();
	prof.m_zeichnungsueberschrift.TrimRight();
	
  // 14. Zeile
	is.getline( buffer, LINE_SIZE, '\n' );
	str = buffer;
  str.TrimRight();
	str += " ";
	str.TrimLeft();
	
  const int n = atoi( str.Left( str.Find(" ") ) );
	str = str.Right( str.GetLength()-str.Find(" ") );
	str.TrimLeft();
	for( i = 0; i < n; i++ )
	{
    if( str.IsEmpty() )
    {
      CString fileStr( "<unknown>" ); 
      if( prof.GetOwner() != NULL )
        fileStr = prof.GetOwner()->GetFileName();

      CString str;
      str.FormatMessage( "Error in Profile File %1 : Line 14", fileStr );

      AfxMessageBox( str, MB_ERROR );
    }
    
		m = atoi( str.Left(str.Find(" ")) );
		nCoord.SetAtGrow( i, m );
		str = str.Right( str.GetLength() - str.Find(" ") );
		str.TrimLeft();
	}
	// 15. Zeile
	is.getline(buffer, LINE_SIZE, '\n');
	str = buffer;
	str += " ";
	str.TrimLeft();
	for (i=0; i<6 && !str.IsEmpty(); i++)
	{
		prof.m_dControlData[i] = atof(str.Left(str.Find(" ")));
		str = str.Right(str.GetLength()-str.Find(" "));
		str.TrimLeft();
	}
	for (i=0; i<4 && !str.IsEmpty(); i++)
	{
		prof.m_nControlData[i] = atoi(str.Left(str.Find(" ")));
		str = str.Right(str.GetLength()-str.Find(" "));
		str.TrimLeft();
	}
	// folgende Zeilen
	for( i = 0; i < n && !is.eof(); i++ )
	{
		db = new DataBlock( &prof );
		db->SetCoordNum( nCoord[i] );
		is >> *db;
    prof.AddDataBlock( db ); // sortiert einfügen
	}

	return is;
}

ostream& operator<<( ostream& os, const Profil &prof )
{
	int i;
	CString str;

  if( !&prof )
    return os;

  int classType = CLASS_TYPE_CSECTION + CLASS_TYPE_LSECTION; // weder L noch C -Section
  Section* owner = prof.m_pOwner; // muss direkt drauf zugreifen weil diese Funktion const ist, aber GetOwner nicht
  if( owner )
    classType = owner->GetClassType();

	os.flags( ios::left );
	os.setf( ios::fixed );
	os.precision( 3 );
	
  // 1. Zeile
	os << prof.m_strOriginFile << endl;
	
  // 2. Zeile
	os << setw(36) << prof.m_strClient[0] << endl;
	
  // 3. Zeile
	if( owner && classType == CLASS_TYPE_CSECTION )
		str = ((CrossSection*)owner)->GetStateName();
	os << setw(40) << prof.m_strClient[1] << str << endl;
	
  // 4. Zeile
	os << setw(36) << prof.m_strProjectDesc[0] << endl;
	
  // 5. Zeile
	if( owner && classType == CLASS_TYPE_CSECTION )
		str.Format( "%d", ((CrossSection*)owner)->GetVZK() );
	else
		str.Empty();
	os << setw(40) << prof.m_strProjectDesc[1] << str << endl;
	
  // 6. Zeile
	if( owner && classType == CLASS_TYPE_CSECTION )
		str = ((CrossSection*)owner)->GetWaterName();
	os << setw(40) << prof.m_strProjectDesc[2] << str << endl;
	
  // 7. Zeile
	if( owner && classType == CLASS_TYPE_CSECTION )
		str = ((CrossSection*)owner)->GetPK();
	os << setw(40) << prof.m_strPageDesc[0] << str << endl;
	
  // 8. Zeile
	os << setw(36) << prof.m_strPageDesc[1] << endl;
	
  // 9. Zeile
	os << setw(36) << prof.m_strPageDesc[2] << endl;
	
  // 10. Zeile
	os << setw(16) << prof.m_strProjectNum << endl;
	
  // 11. Zeile

  if( prof.m_date.m_status == COleDateTime::valid )
	  str = prof.m_date.Format("%x");
  else
     str = "";
	os << str << endl;
	
  // 12. Zeile
	os << prof.m_strPageNum << " ";
	for (i=0; i<6; i++)
		os << prof.m_nControlSizes[i] << " ";
	for (i=0; i<3; i++)
		os << prof.m_strCrossRef[i] << " ";
	os << endl;
	
  // 13. Zeile
	os << prof.m_zeichnungsueberschrift << endl;
	os.setf(ios::right);
	
  // 14. Zeile
	os << setw(4) << prof.m_DataBlocks.GetCount();
	
  // anstatt einfach die Datenblöcke in der vorhandenen Reihenfolge
  // rauszuschreiben, werden sie in der Reihe ihrer Typnummern rausgeschrieben,
  // die negativen ganz an den Schluss
  DataBlockArray dbArray;
  prof.GetSortedDBs( &dbArray );

  for( int dbID = 0; dbID < dbArray.GetSize(); dbID++ )
  {
    DataBlock* db = dbArray[dbID];
    int num = 0;
    switch( db->GetSonderProfilType() )
    {
    case DB_SP_COMMENT:
      num = db->GetCommentCount();
      break;

    case DB_SP_BEREICHE:
      num = db->GetBereichCount();
      break;
      
    default:
      num = db->GetNumCoords();
      break;
    }; // switch SonderProfilType
    os << setw(4) << num << " ";
  }; // for dbID
	os << endl;

  // 15. Zeile
	for (i=0; i<6; i++)
		os << prof.m_dControlData[i] << " ";
	for (i=0; i<4; i++)
		os << prof.m_nControlData[i] << " ";
	os << endl;
	
  // jetzt die Datenblöcke in der oben gewählten Reihenfolge rausschreiben
  for( dbID = 0; dbID < dbArray.GetSize(); dbID++ )
    os << *dbArray[dbID];

	return os;
}; // operator<<

ostream& operator<<( ostream& os, const CString& string )
{
	os << LPCTSTR( string );

	return os;
}

double Profil::GetGelaendehoehe( const double yKrd )
// gibt die Geländehöhe an der Postition yKrd zurück
//
// Parameter:  double yKrd:  die gescuhte Position
// rückgabewert: die gesuchte Geländehöhe bei yKrd oder -99.99 bei Fehler
//
// Bemerkung: - funktioniert nicht richtig bei Unterschneidungen (TODO)
//            - liegt die yKrd ausserhalb des Profils, wird anhand der letzten beiden
//            - Geländepunkte interpoliert
{
  double ergebnis = -99.99;
  // Geländehöhen holen
  DataBlock* db = GetDataBlock( DST_GELAENDEHOEHE );
  if (db)
  {
    Coord* crd = db->GetFirstCoord();
    while (crd)
    {
      if (crd->dx > yKrd)
        break;
      crd = db->GetNextCoord();
    };

    int index = db->GetCurrentCoordIndex() - 1;
    // falls yKrd nicht zwischen zwei Geländepunkte liegt am Rand linear Fortsetzen
    if (index == 0)
      index++;
    else if (!crd)
      index--;
    
    crd = db->GetCoordAt(index);
    Coord* prevCrd = db->GetCoordAt(index - 1);
    
    double y1 = prevCrd->dx;
    double z1 = prevCrd->dy;
    double y2 = crd->dx;
    double z2 = crd->dy;

    if ( y2 - y1 != 0.0 )
      ergebnis = z1 + (z2 - z1) * (yKrd - y1) / (y2 - y1);
  };

  return ergebnis;
};

Coord* Profil::GetSchnittpunkt(Coord* aufpunkt, double neigung, BOOL vorwaerts)
// Berechnet einen Schnittpunkt zwischen dem Profil und einer Geraden
//
// Parameter: Coord* aufpunkt
//            double neigung: durch aufpunkt und Neigung gegebene Gerade; aufpunkt muss innerhalb des Profils liegen (d.h. zwischen kleinstem und grösstem y-Wert)
//            BOOL vorwaerts: in welcher Richtung der Schnittpunkt gesucht werden soll (TRUE: vom aufpunkt aus nach rechts, sonst nach links)
// Rückgabewert: Coord* Koordianten des Schnittpunktes, NULL bei Misserfolg
// Bemerkung: liefert bei mehreren Schnittpunkt stets denjenigen mit kleinstem y-abstand zum Aufpunkt
//            Ein Datenblock Gelaendehoehe mit mehr als zwei Koordinaten muss vorhanden sein
{
  Coord* ergebnis = NULL;
  DataBlock* db_gelaende = GetDataBlock(DST_GELAENDEHOEHE);
  if (db_gelaende && db_gelaende->GetNumCoords() > 1)
  {
    // Koordinate mit nächst grösserem y-Wert suchen
    Coord* crd = db_gelaende->GetFirstCoord();
    while (crd && crd->dx <= aufpunkt->dx)
      crd = db_gelaende->GetNextCoord();
    
    int index = db_gelaende->GetCurrentCoordIndex() - 1;

    Coord* crd1;
    Coord* crd2;

    double x, y;
    x = y = 0.0;

    while (index > 0 && index < db_gelaende->GetNumCoords())
    {
      crd1 = db_gelaende->GetCoordAt(index - 1);
      crd2 = db_gelaende->GetCoordAt(index);

      // den Schnittpunkt zweier Geraden ausrechnen: Gerade1 : crd1, crd2; Gerade2: aufpunkt, neigung;
      double nenner = crd2->dx - crd1->dx;
      if (nenner != 0.0) // division by 0
      {
        double neigung2 = (crd2->dy - crd1->dy) / nenner;
        double b = aufpunkt->dy - neigung * aufpunkt->dx;
        double b2 = crd1->dy - neigung2 * crd1->dx ;
        
        if (neigung != neigung2) // division by 0  (Geraden sind parallel)
        {
          x = ( b - b2 ) / ( neigung2 - neigung );
          y = neigung * x + b;
          
          if (index > 1 && index < db_gelaende->GetNumCoords() && x >= crd1->dx && x <= crd2->dx)
            break;  // falls Treffpunkt in der Strecke crd1-crd2 liegt, abbrechen
          
        }; // if neigung != neigung2
      }; // if nenner != 0.0
      if (vorwaerts)
        index++;
      else
        index--;
    };
    if (x != 0.0 || y != 0.0)
    {
      ergebnis = new Coord;
      ergebnis->Set(x, y);
    };
  };

  return ergebnis;
}; // GetSchnittPunkt()

CRect Profil::GetExtent() const
// gibt die Aussmasse des Profils zurück
// Rückgabewert:
//          CRect: der maximale und minimale y-Wert, und der maximale und minimale z-Wert aller Datenblöcke in mm
{
  CRect rect( 1000000000, -1000000000, -1000000000, 1000000000 );
  POSITION pos = m_DataBlocks.GetHeadPosition();
  if ( pos )
  {
    DataBlock* db = m_DataBlocks.GetAt( pos );
    if ( db )
      rect = db->GetExtent();
  }; // if pos

  while( pos )
  {
    DataBlock* db = m_DataBlocks.GetNext( pos );
    if ( db )
    {
      CRect dbRect = db->GetExtent();
      rect.left = min( rect.left, dbRect.left );
      rect.right = max( rect.right, dbRect.right );
      rect.top = max( rect.top, dbRect.top );
      rect.bottom = min( rect.bottom, dbRect.bottom );
    };

  }; //  while pos
    
  return rect;
}; // GetExtent

void Profil::Paint( CDC* dc, CRect* extent )
// zeichnet das Profil in einen DeviceKontext
// Parameter:
//        CDC* dc: der Device Kontext: der Mapping mode etc. müssen so gewählt sein, dass
//                  - das Profil seine Koordinaten als logische Koordinaten benutzen kann
//                  - die clipBox komplett bezeichnet werden darf
//        CRect* exten: die Aussmasse des Profils ( wird übergeben, damit nicht jedesmal neu ausgerechent werden muss )
{
  //die Datenblöcke in der richtigen Reihenfolge zeichnen
  // zur Zeit: einfach alle zeichnen
  DataBlock* db = GetFirstDataBlock();
  while( db )
  {
    db->Paint( dc, extent );
    db = GetNextDataBlock();
  }; // while db
}; // Paint

CString Profil::GetComment()
// gibt den Inhalt des Datenblocks 'Kommentar' zurück
{
  CString comment;
  DataBlock* db = GetDataBlock( DST_COMMENT );
  if( db )
  {
    for( int i = 0; i < db->GetLineCount(); i++ )
    {
      if( i > 0 )
        comment += TEXT(" - ");

      CString line = db->GetLine( i );
      if( line.GetLength() >= 3 )
        comment += line.Mid( 3 ); // die ersten Zeichen sind immer "CC "
    }; // for i
  }; // if db

  return comment;
}; // GetComment

/*!
 * Fügt eine Kommentarzeile zum Profil hinzu.
 * Ist noch kein Datenblock 'Comment' vorhanden, 
 * wird dieser erzeugt
 *
 * @param text : die Kommentarzeile. Es sollten keine Zeilenumbrüche darinnen sein
 */
void Profil::AddComment( const CString& text )
{
  // den Commentarblock holen
  DataBlock* db = GetDataBlock( DST_COMMENT );

  // ist noch keiner vorhanden, diesen jetzt erzeugen
  if( !db )
  {
    db = new DataBlock( this, DST_COMMENT );
    AddDataBlock( db );
  }; // if !db

  db->SetCommentLine( text );
}; // AddComment


Coord* Profil::GetSohlHoehe(Section* cs, bool DurchstroemterBereich)
/* Gibt die Sohlhoehe des Querprofils zurück
 * Parameter:
 *			Section* cs (CrossSection), bool DurchstroemterBereich
 *
 * Rückgabewerte:
 *			Coord*
 *
 * Erklärung:
 * Die Mehtode ermittelt die Sohlhöhe inerhalb eines Querprofils, indem sie alle Koordinaten durfchläuft und die 
 * kleinste zurückgibt. Über die Option DurchstroemterBereich == True wird nur der durchströmte Bereich nach der
 * Sohlhöhe durchsucht. (Gleiche Ergebnisse liefert auch der WSPWIN-Rechenkern)
 */
{
  Profil* profil = cs->GetProfil();
  DataBlock* DBGelaendeHoehe = profil->GetDataBlock(DST_GELAENDEHOEHE);
  Coord* newCrd;
  if(DurchstroemterBereich)
  {
	  DataBlock* DBDurchstroemterBereich = NULL;
	  //finde erste Koordinate des durchströmten Bereichs
	  //gibt es einen Datenblock "durchströmter Bereich"
	  if (profil->GetDataBlock(DST_DURCHST_BEREICH) != NULL)
	  {
		  DBDurchstroemterBereich = profil->GetDataBlock(DST_DURCHST_BEREICH);
	  }
	  //wenn nicht, dann ermittle die Sohlhöhe ohne Berücksichtigung des durchströmten Bereichs
	  else
	  {
		  newCrd = DBGelaendeHoehe->GetCoordAt(0);
	    for (int i = 1; i < DBGelaendeHoehe->GetNumCoords(); i++)
	    {
		    if (DBGelaendeHoehe->GetCoordAt(i)->dy <= newCrd->dy)	//vergleichen der dy-Werte
		    {
			    newCrd = DBGelaendeHoehe->GetCoordAt(i);
		    }
	    }
	  }

	  //Finden der ersten und der letzten Koordinate des durchströmten Bereichs
	  Coord* firstCrd = DBDurchstroemterBereich->GetFirstCoord();
	  Coord* lastCrd  = DBDurchstroemterBereich->GetLastCoord();

		  
	  //erste Koordinate aus Geländedatenblock ermitteln
	  DBGelaendeHoehe = profil->GetDataBlock(DST_GELAENDEHOEHE);
	  newCrd = DBGelaendeHoehe->GetFirstCoord();

	  
	  //Bereich mit Koordinaten festlegen, die im durchströmten Bereich definiert sind.
	  //durchlaufe alle Geländehöhenkoordinaten bis zur ersten linken durchströmten Abgrenzung
  //TODO
  //sollte der Fehler auftreten ,dass *NewCrd  n i e  == *firstCrd, dann Overflow !!!!!
    //--------------------------------------------
	  int anfangIndex = 0;
	  while (*newCrd != *firstCrd)
	  {
		  newCrd = DBGelaendeHoehe->GetNextCoord();
		  anfangIndex = DBGelaendeHoehe->GetCurrentCoordIndex();
	  }
	  //anfangIndex ist der von uns gesuchte Index der ersten linken durchströmten Abgrenzung

	  int endeIndex = 0;
	  while (*newCrd != *lastCrd)
	  {
		  newCrd = DBGelaendeHoehe->GetNextCoord();
		  endeIndex = DBGelaendeHoehe->GetCurrentCoordIndex();
	  }
	  //endeIndex ist der von uns gesuchte Index der letzten rechtesten durchströmten Abgrenzung


	  //newCrd(Vergleichskoordinate) wieder auf erste relevante Koordinate setzen.
	  newCrd = DBGelaendeHoehe->GetCoordAt(anfangIndex);
	  for (int i = anfangIndex; i < endeIndex; i++)
	  {
		  if (DBGelaendeHoehe->GetCoordAt(i)->dy <= newCrd->dy)	//vergleichen der dy-Werte
		  {
			  newCrd = DBGelaendeHoehe->GetCoordAt(i);
		  }
	  }
  }
  else
  {
    newCrd = DBGelaendeHoehe->GetCoordAt(0);
	  for (int i = 1; i < DBGelaendeHoehe->GetNumCoords(); i++)
	  {
		  if (DBGelaendeHoehe->GetCoordAt(i)->dy <= newCrd->dy)	//vergleichen der dy-Werte
		  {
			  newCrd = DBGelaendeHoehe->GetCoordAt(i);
		  }
	  }
  }
  return newCrd;
}
	

/*!
 * Löscht alle Datenblöcke angegebener Typen aus diesem Profil
 *
 * @param const CArray<int, int>& dbTypes
 *
 * @return UINT  : Summe der gelöschten Datenblöcke
 */
UINT Profil::DeleteDataBlocks( const CArray<int, int>& dbTypes )
{
  UINT count = 0; // summe der gelöschten Datenblöcke

  // die unerwünschten Typen der Reihe nach durchgegen
  for( int i = 0; i < dbTypes.GetSize(); i++ )
  {
    int type = dbTypes[i];

    // es können mehrere Blöcke des gleichen Typs vorkommen
    // also solange löschen bis alle weg sind
    DataBlock* db = NULL;
    do
    {
      db = GetDataBlock( type );
      if( db != NULL )
      {
        count++;
        RemoveDataBlock( db );
        delete db;
      }; // if db != NULL

    }
    while( db != NULL );
  }; // for i

  return count;
} // DeleteDataBlocks



/*!
 * Gibt die Datenblöcke in einer Definierten Reihenfolge zurück.
 * Die Datenblöcke werden nach ihrer TypID sortiert, 0 bis N_DSTYPES
 * und danach die Negativen 0,...,-4
 *
 * @param : dbArray : zu diesem DataBlockArray werden die Datenblöcke in der 
 *                    entsprechenden Reihenfolge hinzugefügt
 *
 * @return DataBlockArray&  : 
 */
void Profil::GetSortedDBs( DataBlockArray* dbArray ) const
{
  if( !dbArray )
    return;

  int typID = 1; // mit Gelände beginnen
  while( typID > N_DSTYPES_MIN - 2 )
  {
    // jetzt alle Datenblöcke diesen Typs rausschreiben
    int typCount = GetDataBlockNum( typID );
    for( int i = 1; i < typCount + 1; i++ ) // es muss bei 1 angefangen werden zu zählen!
      dbArray->Add( GetDataBlock( typID, i ) );

    // seltsame Logik für seltsame Nummerierung
    if( 0 < typID && typID < N_DSTYPES )
      typID++;
    if( typID <= 0 )
      typID--;
    if( typID == N_DSTYPES )
      typID = 0;
  }; // while typID
}; // GetSortedDBs


/*!
 * Spiegelt das Profil am Profilnullpunkt.
 */
void Profil::FlipHorizontal()
{
  // einfach alle Datenblöcke spiegeln
  POSITION pos = m_DataBlocks.GetHeadPosition();
  while( pos )
  {
    DataBlock* db = m_DataBlocks.GetNext( pos );
    db->FlipHorizontal();
  }
  
  SetModified();
}


/*!
 * Schränkt ein Profil auf einen bestimmten BEreich ein, d.h. es löscht alle
 * Koordinaten ausserhalb des Bereichs.
 *
 * @param from : linke Grenze
 * @param to : rechte Grenze
 *
 */
void Profil::Constrain( const double from, const double to )
{
  // einfach alle Datenblöcke spiegeln
  POSITION pos = m_DataBlocks.GetHeadPosition();
  while( pos )
  {
    DataBlock* db = m_DataBlocks.GetNext( pos );

    switch( db->GetSonderProfilType() )
    {
    case 0:
      {
        DataBlock* textDB = GetDataBlock( DST_LP_TEXT );
        TextBlock* tb = textDB ? textDB->GetTextBlock() : 0;

        // falls es nicht der Bezugsdatensatz ist, den Textblock nicht berücksichtigen
        if( tb && tb->GetBezugsdatensatz() != db )
            tb = 0;

        for( int i = 0; i < db->GetNumCoords(); i++ )
        {
          Coord* crd = db->GetCoordAt( i );
          if( crd->dx < from || crd->dx > to )
          {
            // falls Text da ist, auch den Text ändern
            if( tb )
               tb->DeleteWithRef( i + 1 );

            db->RemoveCoord( crd );
            i--;
          }
        } // for i

        if( tb && tb->GetTextSatzCount() == 0 )
          RemoveDataBlock( textDB );

        if( db->GetNumCoords() == 0 )
          RemoveDataBlock( db );
      }
      break;
      
    case 23:
      {
        for( int i = 0; i < db->GetBereichCount(); i++ )
        {
          DataBlock::Bereich& b = db->GetBereich( i );
          double newFrom = max( b.from ,from );
          double newTo = min( b.to, to );
          
          if( newFrom >= newTo )
          {
            db->RemoveBereich( i );
            i--;
          }
          else
          {
            b.from = newFrom;
            b.to = newTo;
          }
        }

        if( db->GetBereichCount() == 0 )
          RemoveDataBlock( db );
      }
      break;
    } // switch
  } // while
  
  SetModified();
}

/* static */
Profil* Profil::CreateFromTriple( const TripleArray& tripArray )
{
  if( tripArray.size() <= 0 )
    return NULL;
  
  Profil* profil = new Profil();
  profil->SetPageDesc( 1, CString( TEXT("QUERPROFIL 0") ) );
  profil->SetPageDesc( 2, CString( TEXT("STATION KM 0.0000") ) );
  
  // create new data block and set info
  DataBlock* dbGel = new DataBlock( profil );
  dbGel->SetType( DST_GELAENDEHOEHE );												
  profil->AddDataBlock( dbGel );
  
  DataBlock* dbRw = new DataBlock( profil );
  dbRw->SetType( DST_RECHTSWERT );												
  profil->AddDataBlock( dbRw );
  
  DataBlock* dbHw = new DataBlock( profil );
  dbHw->SetType( DST_HOCHWERT );												
  profil->AddDataBlock( dbHw );
  
  for( TripleArray::const_iterator tIt = tripArray.begin(); tIt != tripArray.end(); tIt++ )
  {
    Coord* crdGel = new Coord( (*tIt)->breite, (*tIt)->hoehe );
    Coord* crdRw = new Coord( (*tIt)->breite, (*tIt)->rw );
    Coord* crdHw = new Coord( (*tIt)->breite, (*tIt)->hw );
    
    dbGel->AddCoord( crdGel );
    dbRw->AddCoord( crdRw );
    dbHw->AddCoord( crdHw );
  };
  
  return profil;
};

void Profil::DumpCSV( BCE::CSV<double, CString, double>& table, const CString& columnPostfix )
{
	for( int i = 0; i < GetNumDataBlocks(); i++ )
	{
		DataBlock* db = GetDataBlockByIndex( i );

		const CString& columnName = db->GetName( 0 ) + columnPostfix;
		for( int j = 0; j < db->GetNumCoords(); j++ )
		{
			Coord* crd = db->GetCoordAt( j );
			double x = crd->dx;
			double y = crd->dy;

			table.PutValue( x, columnName, y );
		}
	}
}
