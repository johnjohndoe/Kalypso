#include "stdafx.h"

#include "dtypes.h"

#include "connect.h"
#include "calc.h"
#include "outflow.h"
#include "loss.h"
#include "brnchtab.h"
#include "branch.h"
#include "giostr.h"
#include "project.h"
#include "csection.h"
#include "lsection.h"
#include "3dcoord.h"
#include "profil.h"
#include "coord.h"
#include "datablck.h"

#include "state.h"

#include <iostream>

////////////////////////////
//  Klasse  State
///////////////////////////

#define STATIONTOLERANZ  0.0001 // = 1dm // minimaler Abstand zwischen zwei Stationen ( wird benutzt um Stationen zu finden etc. )

/* The Default Constructor */
State::State(Project* pProject)
{
  m_pProject = pProject;
  m_CSectionPos = NULL;
  m_ConnectionPos = NULL;
  m_CalculationPos = NULL;
  m_OutFlowPos = NULL;
  m_LossPos = NULL;
  m_pBranchTable = NULL;
  m_bModified = FALSE;
  m_bBERModified = FALSE;
  m_bOutFlowModified = FALSE;
  m_bLossModified = FALSE;
  m_bBranchTableModified = FALSE;
  // damit AddCrossSection richtig die mins undmaxs ausrechnet
  SetStartStation( std::numeric_limits<double>::infinity() );
  SetEndStation( -std::numeric_limits<double>::infinity() );
}

State::~State()
{
  Connection *con;
  Calculation *calc;
  OutFlow *of;
  Loss *loss;
  POSITION pos;
  
  // die crosssections werden nicht gelöscht, weil die Zeiger doppelt benutzt werden und 
  // Normalerweise schon in ~Project gelöscht werden. Das ist blöd! TODO!!
  
  pos = m_Connections.GetHeadPosition();
  while (pos!=NULL)
  {
    con = m_Connections.GetNext(pos);
    delete con;
  }
  m_Connections.RemoveAll();
  
  pos = m_Calculations.GetHeadPosition();
  while (pos!=NULL)
  {
    calc = m_Calculations.GetNext(pos);
    delete calc;
  }
  m_Calculations.RemoveAll();
  
  pos = m_OutFlows.GetHeadPosition();
  while (pos!=NULL)
  {
    of = m_OutFlows.GetNext(pos);
    delete of;
  }
  m_OutFlows.RemoveAll();
  
  pos = m_Losses.GetHeadPosition();
  while (pos!=NULL)
  {
    loss = m_Losses.GetNext(pos);
    delete loss;
  }
  m_Losses.RemoveAll();
  
  if (m_pBranchTable!=NULL)
  {
    delete m_pBranchTable;
    m_pBranchTable = NULL;
  }
}

State* State::Clone( Project* pProject, const CString& name, BOOL createNewFiles /* = TRUE */ )
// kopiet sich selbst
// Parameter:
//        const CString& name: der neue Name des Zustandes
//        BOOL createNewFiles: falls TRUE, werden neue Dateien erzeugt ( muss durch ein Save noch ausgelöst werden )
// Rückgabewert:
//        Sate* : Zeiger auf die Kopie, NULL bei Misserfolg
// Bemerkung:
{
  State* state = new State( pProject );
  
  state->SetWaterName( m_WaterName );
  state->SetDate( m_date );
  state->SetName( name );
  state->SetFileName( GetFileName() );
  
  // falls gewünsacht, neuen Dateinamen erzeugen
  // muss ganz am Anfang stehen, damit alle anderen Clones
  // von state abgeleitete Namen erzeugen
  if ( createNewFiles )
  {
    state->SetFileName( CString("") );
    state->CreateFileName();
  };
  
  // Profile kopieren
  POSITION pos = m_CrossSections.GetHeadPosition();
  while ( pos )
  {
    CrossSection* cs = m_CrossSections.GetNext( pos )->Clone( pProject, createNewFiles );
    if ( cs )
      state->AddCrossSection( cs );
  }; // while pos
  
  // Berechnungsvarianten
  pos = m_Calculations.GetHeadPosition();
  while ( pos )
  {
    Calculation* calc = m_Calculations.GetNext( pos )->Clone( pProject, state, createNewFiles );
    if ( calc )
      state->AddCalculation( calc );
  }; // while pos
  
  // Connections müssen nicht kopiert werden, werden durch AddCrosssection automatisch erstellt
  
  // Verluste
  pos = m_Losses.GetHeadPosition();
  while ( pos )
  {
    Loss* loss = m_Losses.GetNext( pos )->Clone();
    if ( loss )
      state->AddLoss( loss );
  }; // while pos
  
  // Abflüsse
  pos = m_OutFlows.GetHeadPosition();
  while ( pos )
  {
    OutFlow* outFlow = m_OutFlows.GetNext( pos )->Clone( pProject, state );
    if ( outFlow )
      state->AddOutFlow( outFlow );
  }; // while pos
  
  // Verzweigungstabelle
  if ( m_pBranchTable )
  {
    BranchTable* branchTable = m_pBranchTable->Clone();
    if ( branchTable )
      state->SetBranchTable( branchTable );
  };
  
  return state;
}; // Clone


BOOL State::Load()
{
  gifstream ifs;
  CString rString, path, file;
  CFileStatus rStatus;
  
  if (m_fileTitle.IsEmpty())
    return FALSE;
  path = m_pProject->GetDataDir();
  file = GetFileName();
  path += file;
  if (CFile::GetStatus(path, rStatus))
  {
    ifs.open(path, ios::in);
    if (ifs.fail())
    {
      rString.FormatMessage("Konnte Datei %1 nicht zum lesen öffnen.", file);
      AfxMessageBox(rString, MB_ERROR);
      return FALSE;
    }
    else
    {
      ifs >> *this;
      ifs.close();
    }
  }
  LoadCalculations();
  LoadOutFlows();
  LoadLosses();
  LoadBranchTable();
  m_bModified = FALSE;
  
  return TRUE;
}

BOOL State::Save()
{
  if( m_fileTitle.IsEmpty() )
    return FALSE;
  
  if( m_bModified )
  {
    CString file = GetFileName();
    CString path = m_pProject->GetDataDir() + file;

    try
    {
      gofstream ofs( path, ios::out );
      ofs << *this;
      ofs.close();
    }
    catch(...)
    {
      CString rString;
      rString.FormatMessage( "Konnte Datei %1 nicht zum Schreiben öffnen.", file );
      AfxMessageBox(rString, MB_ERROR);
      return FALSE;
    }
  }
  m_bModified = FALSE;
  
  // now save Querprofile
  POSITION pos = m_CrossSections.GetHeadPosition();
  while( pos )
  {
    CrossSection* cs = m_CrossSections.GetNext( pos );
    cs->SaveProfil();
  }

  SaveCalculations();
  SaveOutFlows();
  SaveLosses();
  SaveBranchTable();
  
  return TRUE;
}

void State::SetModified()
{
  m_bModified = TRUE;
  if (m_pProject!=NULL)
    m_pProject->SetModified();
}

void State:: SetBERModified()
{
  m_bBERModified = TRUE;
  SetModified(); // falls die Berechnungs geändert wurde, wurde also auch der Zustand geändert und solllte gepseichert werden
}

void State:: SetOutFlowModified()
{
  m_bOutFlowModified = TRUE;
}

void State:: SetLossModified()
{
  m_bLossModified = TRUE;
}

void State:: SetBranchTableModified()
{
  m_bBranchTableModified = TRUE;
}

void State::SetNumber( const int num )
{
  m_nNumber = num;
  SetModified();
}

void State::SetWaterName( const CString& name )
{
  m_WaterName = name;
  SetModified();
}

void State::SetName( const CString& name )
{
  m_name = name;
  SetModified();
};

void State::SetDate( const CString& date )
{
  m_date.ParseDateTime(date, VAR_DATEVALUEONLY);
  SetModified();
}

void State::SetDate( const COleDateTime& date )
{
  m_date = date;
}; // SetDate

void State::SetStartStation( const double value )
{
  m_dStartStation = min( value, 10000000.0000 ); // sonst steht schrott in der Strangdatei
  SetModified();
}

void State::SetEndStation( const double value )
{
  m_dEndStation = max( value, -10000000.0000 );
  SetModified();
}

void State::SetFileName( const CString& file )
{
  int i;
  CString str;
  
  i = file.Find('.');
  if (i==-1)
  {
    m_fileTitle = file;
    m_fileExt.Empty();
  }
  else
  {
    m_fileTitle = file.Left(i);
    m_fileExt = file.Right(file.GetLength()-i-1);
  }
  // Extract number from file title
  str = m_fileTitle.Right( m_fileTitle.GetLength() - 2 );
  m_nNumber = atoi(str);
  SetModified();
}

void State::CreateFileName()
{
  CString filename, filepath, str;
  int i;
  CFile file;
  CFileStatus rStatus;
  
  filename = GetFileName();
  if (filename.IsEmpty())
  {
    i = m_pProject->m_nNextState;
    
    filepath = m_pProject->GetDataDir();
    filename = m_WaterName.Left(2);
    str.Format("%06d", i);
    filename += str + ".str";
    str = filepath + filename;
    while (file.GetStatus(str, rStatus))
    {
      i++;
      filename = m_WaterName.Left(2);
      str.Format("%06d", i);
      filename += str + ".str";
      str = filepath + filename;
    }
    SetFileName(filename);
  }
}

CString State::GetFileName()
{
  if (m_fileExt.IsEmpty())
    return m_fileTitle;
  else
    return m_fileTitle + '.' + m_fileExt;
}

CString State::GetFileTitle()
{
  return m_fileTitle;
}

CString State::GetFileExt()
{
  return m_fileExt;
}

Project* State::GetProject()
{
  return m_pProject;
}

CString State::GetWaterName()
{
  return m_WaterName;
}

const CString& State::GetName() const
{
  return m_name;
}

CString State::GetDateString()
{
  return m_date.Format("%x");
}

double State::GetStartStation()
{
  return m_dStartStation;
}

double State::GetEndStation()
{
  return m_dEndStation;
}

istream& operator>>(istream& is, State &zs)
{
  // Read STR file
  char buffer[LINE_SIZE];
  CString str;
  int i, nprof, ncon;
  CrossSection *cs, *csOld;
  Connection *con;
  POSITION pos;
  
  ncon = nprof = 0;
  is >> nprof >> ncon;
  is.getline(buffer, LINE_SIZE, '\n');	//remove trash and end of line
  for (i=0; i<nprof && !is.eof(); i++)
  {
    if (is.peek()=='\n' || is.peek()==EOF)
    {
      is.getline(buffer, LINE_SIZE, '\n');
      continue;
    }
    cs = new CrossSection(zs.m_pProject);
    // Gewaessername
    is.get(buffer, 10+1, '\n');
    str = buffer;
    str.TrimLeft();
    str.TrimRight();
    cs->SetWaterName(str);
    // Station
    is.get(buffer, 9+1, '\n');
    str = buffer;
    str.TrimLeft();
    str.TrimRight();
    cs->SetStation(atof(str));
    // PK
    is.get(buffer, 10+1, '\n');
    str = buffer;
    str.TrimLeft();
    str.TrimRight();
    cs->SetPK(str);
    // VZK
    is.get(buffer, 4+1, '\n');
    cs->SetVZK(atoi(buffer));
    // Statename
    is.get(buffer, 11+1, '\n');
    str = buffer;
    str.TrimLeft();
    str.TrimRight();
    cs->SetStateName(str);
    // Dateiname
    is.getline(buffer, LINE_SIZE, '\n');
    str = buffer;
    str.TrimLeft();
    str.TrimRight();
    cs->SetFileName(str);
    csOld = zs.m_pProject->FindCrossSection(str);
    if (csOld==NULL)
      zs.AddCrossSection(cs);
    else
    {
      delete cs;
      zs.AddCrossSection(csOld);
    }
  }
  //	OrderListByStation(zs.m_CrossSections);
  // delete existing connections (added by AddCrossSection)
  pos = zs.m_Connections.GetHeadPosition();
  while (pos!=NULL)
  {
    con = zs.m_Connections.GetNext(pos);
    delete con;
  }
  zs.m_Connections.RemoveAll();
  // now read in new connections
  for (i=0; i<ncon; i++)
  {
    con = new Connection;
    is >> *con;
    zs.m_Connections.AddTail(con);
  }
  
  return is;
}

ostream& operator<<( ostream& os, State &zs )
{
  //OrderListByStation(zs.m_CrossSections);

  // 1. Zeile
  os.setf( ios::right );
  os << setw( 5 ) << zs.m_CrossSections.GetCount();
  os << " " << setw( 5 ) << zs.m_Connections.GetCount();
  os << " " << setw( 10 ) << zs.m_WaterName;
  os << " " << setw( 10 ) << zs.m_name << endl;
  os.setf( ios::left );
  os.setf( ios::fixed );
  os.precision( 4 );
  
  // Profile
  POSITION cpos = zs.m_CrossSections.GetHeadPosition();
  while( cpos )
  {
    CrossSection* cs = zs.m_CrossSections.GetNext( cpos );
    
    os << setw( 9 ) << cs->GetWaterName() << " ";
    os << setw( 9 ) << cs->GetStation() << " ";
    os << setw( 8 ) << cs->GetPK() << " ";
    os << setw( 3 ) << cs->GetVZK() << " ";
    os << setw( 10 ) << cs->GetStateName() << " ";
    os << cs->GetFileName() << endl;
  }
  os << endl;
  
  // connections
  POSITION npos = zs.m_Connections.GetHeadPosition();
  while( npos )
  {
    Connection* con = zs.m_Connections.GetNext( npos );
    os << *con;
  }
  return os;
}

/***************************************************************/
// CrossSection implementation

void State::AddCrossSection( CrossSection* cs )
// Fügt eine CrossSection zum Zustand hinzu und sortiert sie anhand 
// Station, VZK und PK ein
// Parameter:
//        CrossSection* cs:
// Bemerkung:
//        es werden auch Anfangs und EndStation des Zustandes gesetzt
{
  m_pProject->AddCrossSection( cs ); // zum Projekt hinzufügen, falls nicht schon geschehen
  
  CString file = cs->GetFileName();
  if( !CrossSectionExists( cs ) && FindCrossSection( file ) == NULL )
  {
    // die CrossSection sortiert einfügen
    POSITION pos = m_CrossSections.GetHeadPosition();
    CrossSection* csOther = NULL; 
    
    while( pos )
    {
      csOther = m_CrossSections.GetAt( pos );
      if( *cs < *csOther )
        break;
      
      m_CrossSections.GetNext( pos );
    }; // while pos
    
    // zuerst mal den Vorgänger und den Nachfolger dieser Position holen und 
    // Versuchen einen Eintrag in die Strangtabelle zu machen
    CrossSection* predCs = NULL;
    CrossSection* postCs = NULL;
    
    if( pos )
    {
      POSITION helpPos = pos;
      m_CrossSections.GetPrev( helpPos );
      if( helpPos )
        predCs = m_CrossSections.GetPrev( helpPos );
      
      postCs = m_CrossSections.GetAt( pos );
    }
    else
    {
      if( !m_CrossSections.IsEmpty() )
        predCs = m_CrossSections.GetTail();
    }; // if pos
    
    if( predCs || postCs )
    {
      if( !InsertConnection( cs, predCs, postCs ) )
        return;
    }; // if predCs || postCs
    
    // falls pos und insertPos beide NULL an ende Stellen, sonst vor insertPos einfügen
    if( pos == NULL )
      m_CrossSections.AddTail( cs );
    else
      m_CrossSections.InsertBefore( pos, cs );
    
    // der CrossSection den neuen Zustand mitteilen
    cs->AddState( this );
    
    // auch noch Anfangs und Endstation aktualisieren
    m_dStartStation = min( m_dStartStation, cs->GetStation() );
    m_dEndStation = max( m_dEndStation, cs->GetStation() );
    
    SetModified();
  }; // if !CrossSectionExists( cs ) && ...
}; // AddCrossSection

CrossSection* State::GetFirstCrossSection()
{
  m_CSectionPos = m_CrossSections.GetHeadPosition();
  return GetNextCrossSection();
}

CrossSection* State::GetNextCrossSection()
{
  if (m_CSectionPos==NULL)
    return NULL;
  else
    return m_CrossSections.GetNext(m_CSectionPos);
}

BOOL State::CrossSectionExists(CrossSection* cs)
{
  return m_CrossSections.Find(cs) != NULL;
}

BOOL State::RemoveCrossSection( CrossSection* cs )
// löscht die CrossSection aus dem Zustand und aus dem Projekt, falls sie in keinem
// anderen Zustand mehr ist
// Rückgabewert:
//        TRUE, falls sie nicht mehr im Projekt vorhanden ist, sonst FALSE
{
  POSITION pos = m_CrossSections.Find( cs );
  
  if( pos != NULL )
  {
    m_CrossSections.RemoveAt( pos );
    SetModified();
    
    // noch alle davon abhängenden Daten updaten
    
    // Connections
    RemoveCSFromConnections( cs );
    
    // Anfangs und Endstation
    CalcAnfEndStation();
    
    // TODO: update Branchtable, Losses, OutFlows!!!!
    
    cs->RemoveState( this );
    
    // die CS auch aus dem Projekt löschen ( falls sie in keinem anderen Zustand mehr ist )
    return m_pProject->RemoveCrossSection( cs );
  } // if pos != NULL
  
  return FALSE;
}; // RemoveCrossSection

CrossSection* State::FindCrossSection(CString& file)
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

CrossSection* State::FindCrossSection(double station)
{
  POSITION pos;
  CrossSection *cs;
  
  pos = m_CrossSections.GetHeadPosition();
  while (pos!=NULL)
  {
    cs = m_CrossSections.GetNext(pos);
    if ( fabs( station - cs->GetStation() ) < STATIONTOLERANZ )
      return cs;
  }
  return NULL;
}

int State::GetNumCrossSections()
{
  return m_CrossSections.GetCount();
}

int State::GetNumConnections()
{
	return m_Connections.GetCount();
}

/***************************************************************/
// Connection implementation

BOOL State::InsertConnection( CrossSection* newCs, CrossSection* predCs, CrossSection* postCs )
// fügt einen neuen Strang in die Strangtabelle ein
// Parameter:
//        CrossSection* newCs
//        CrossSection* predCs, postCs: zwischen diesen beiden wird ein neuer Strang eingefügt
// Rückgabewert:
//        BOOL: FALSE, wenn der Strang nicht erzeugt werden konnte
// Bemerkung:
//        es gibt drei Fälle: 
//              - predCs und postCs sind ungleich NULL:
//                  dann müssen sie direkt benachbart sein ( in der Strangtabelle )
//              - eine der beiden ist NULL:
//                  dann muss der Andere am Stranganfang oder am Strangende sitzen,
//                  der neue Strang wird dann vorne oder hinten angehängt
//                  möglicherweise ist die Strangtabelle noch leer -> dann einen einzigen neuen Strang erzeugen
//        Die Abstände werden anhand der Stationierung ausgerechnet
{
  if( !newCs )
    return FALSE;
  
  // die Daten der neuen CrossSection
  double newStation = newCs->GetStation();
  CString newProf = newCs->GetFileName();
  
  // der neue Strang
  Connection* newCon = NULL;
  POSITION insertPos = NULL;
  
  BOOL bAfter = TRUE; // ob der neue Strang vor oder nach "insertPos" eingefügt werden soll
  
  // erster Fall: beide ungleich NULL:
  if( predCs && postCs )
  {
    CString predProf = predCs->GetFileName();
    CString postProf = postCs->GetFileName();
    
    // Strang suchen, in welchem beide sind
    POSITION pos = m_Connections.GetHeadPosition();
    while( pos )
    {
      insertPos = pos;
      bAfter = TRUE;
      Connection* conn = m_Connections.GetNext( pos );
      CString anfCs = conn->GetAnfProf();
      CString endCs = conn->GetEndProf();
      if( ( anfCs.CompareNoCase( predProf ) == 0 && endCs.CompareNoCase( postProf ) == 0 ) ||
        ( anfCs.CompareNoCase( predProf ) == 0 && endCs.CompareNoCase( postProf ) == 0 ) )
        break;
    }; // while pos
    
    if( !insertPos )
      return FALSE;
    
    Connection* oldCon = m_Connections.GetAt( insertPos );
    if( !oldCon )
      return FALSE;
    
    // jetzt einen neuen Strang erzeugen und einfügen
    newCon = new Connection;
    
    newCon->SetEndStation( oldCon->GetEndStation() );
    newCon->SetEndProf( oldCon->GetEndProf() );
    
    oldCon->SetEndStation( newStation );
    newCon->SetAnfStation( newStation );
    
    oldCon->SetEndProf( newProf );
    newCon->SetAnfProf( newProf );
    
    // noch die Abstände setzen; einfach nach der Stationierung
    double oldAbstand = fabs( oldCon->GetAnfStation() - oldCon->GetEndStation() );
    oldCon->SetVorlandLinks( oldAbstand );
    oldCon->SetFluss( oldAbstand );
    oldCon->SetVorlandRechts( oldAbstand );
  } // if predCs && postCs
  else if( predCs || postCs )
  {
    CrossSection* oldCs = predCs ? predCs : postCs; // diejenige welche != NULL
    CString oldProf = oldCs->GetFileName();
    double oldStation = oldCs->GetStation();
    
    // mal suchen, obs einen Strang diesen Namens gibt
    BOOL bAnf = predCs ? TRUE : FALSE; // obs der Anfang oder das Ende ist
    BOOL bFound = FALSE;
    POSITION pos = m_Connections.GetHeadPosition();
    while( pos )
    {
      POSITION oldPos = pos;
      Connection* con = m_Connections.GetNext( pos );
      CString anfProf = con->GetAnfProf();
      CString endProf = con->GetEndProf();
      if( anfProf.CompareNoCase( oldProf ) == 0 )
      {
        if( bFound ) 
          return FALSE; // mehr als einmal gefunden -> Fehler
        bFound = TRUE;
        insertPos = oldPos;
        bAfter = FALSE; // vor dieser Position einfügen
      }
      else if( endProf.CompareNoCase( oldProf ) == 0 )
      {
        if( bFound )
          return FALSE;
        
        bFound = TRUE;
        insertPos = oldPos;
        bAfter = TRUE; // hinter dieser Position einfügen
      }; // if oldProf == anfang oder ende
    }; // while pos
    
    // falls nichts gefunden, haben wir möglicherweise eine leere Strangtabelle
    if( bFound )
    {
      // wir sind tatsächlich am Stranganfang bzw. am Strangende
      // einfach einen neuen Strang erzeugen, die Werte setzten und fertig
      Connection* oldCon = m_Connections.GetAt( insertPos );
      newCon = new Connection;
      
      if( bAnf )
      {
        newCon->SetEndProf( newProf );
        newCon->SetEndStation( newStation );
        
        newCon->SetAnfProf( oldCon->GetEndProf() );
        newCon->SetAnfStation( oldCon->GetEndStation() );
      }
      else
      {
        newCon->SetAnfProf( newProf );
        newCon->SetAnfStation( newStation );
        
        newCon->SetEndProf( oldCon->GetAnfProf() );
        newCon->SetEndStation( oldCon->GetAnfStation() );
      }; // if bAnf
      
    }
    else
    {
      // kein Strang gefunden, die Strangtabelle muss leer sein´und wir fügen einfach einen neuen ein
      if( m_Connections.GetCount() != 0 )
        return FALSE;
      
      newCon = new Connection;
      if( *oldCs < *newCs )
      {
        newCon->SetEndProf( newProf );
        newCon->SetEndStation( newStation );
        
        newCon->SetAnfProf( oldProf );
        newCon->SetAnfStation( oldStation );
      }
      else
      {
        newCon->SetAnfProf( newProf );
        newCon->SetAnfStation( newStation );
        
        newCon->SetEndProf( oldProf );
        newCon->SetEndStation( oldStation );
      }; // if newCs < oldCs
    }; // if bFound
    
  } // if predCs || postCs
  else 
    return FALSE;
  
  if( newCon )
  {
    // noch schnell die Abstände setzen    
    double newAbstand = fabs( newCon->GetAnfStation() - newCon->GetEndStation() );
    newCon->SetVorlandLinks( newAbstand );
    newCon->SetFluss( newAbstand );
    newCon->SetVorlandRechts( newAbstand );
    
    if( bAfter )
      m_Connections.InsertAfter( insertPos, newCon );
    else
      m_Connections.InsertBefore( insertPos, newCon );
    
    return TRUE;
  } // if newCon && bInsert
  else
    return FALSE;
}; // InsertCrossSection

Connection* State::GetFirstConnection()
{
  m_ConnectionPos = m_Connections.GetHeadPosition();
  return GetNextConnection();
}

Connection* State::GetNextConnection()
{
  if (m_ConnectionPos==NULL)
    return NULL;
  else
    return m_Connections.GetNext(m_ConnectionPos);
}

Connection* State::GetLastConnection()
{
  m_ConnectionPos = m_Connections.GetTailPosition();
  return GetPrevConnection();
}

Connection* State::GetPrevConnection()
{
  if (m_ConnectionPos==NULL)
    return NULL;
  else
    return m_Connections.GetPrev(m_ConnectionPos);
}

/***************************************************************/
// Calculation implementation

BOOL State::LoadCalculations()
{
  gifstream ifs;
  Calculation *calc;
  LengthSection *ls;
  char buffer[LINE_SIZE];
  CString str, temp, path, filename;
  CFile file;
  CFileStatus rStatus;
  
  path = m_pProject->GetDataDir();
  path += m_fileTitle + ".ber";
  if (!file.GetStatus(path, rStatus))
    return TRUE;
  ifs.open(path, ios::in);
  if (ifs.fail())
    return FALSE;
  ifs.getline(buffer, LINE_SIZE, '\n');		// ignore first line
  ifs.getline(buffer, LINE_SIZE, '\n');		// get next line
  str = buffer;
  while (!str.IsEmpty())
  {
    calc = new Calculation(m_pProject, this);
    ls = new LengthSection(m_pProject);
    temp = str.Left(61);
    temp.TrimLeft();
    temp.TrimRight();
    calc->SetName(temp);
    ls->SetName(temp);
    if (str.GetLength()>61)
      str = str.Right(str.GetLength()-61);
    else
      str.Empty();
    temp = str.Left(9);
    calc->SetStartStation(atof(temp));
    ls->SetStartStation(atof(temp));
    if (str.GetLength()>9)
      str = str.Right(str.GetLength()-9);
    else
      str.Empty();
    temp = str.Left(9);
    calc->SetEndStation(atof(temp));
    ls->SetEndStation(atof(temp));
    if (str.GetLength()>9)
      str = str.Right(str.GetLength()-9);
    else
      str.Empty();
    str.TrimLeft();
    str.TrimRight();
    calc->SetFileName(str);
    AddCalculation(calc);
    str.SetAt(2, 'p');
    str.SetAt(3, 'l');
    ls->SetFileName(str);
    // now check if Laengsschnitt has been created
    path = m_pProject->GetCalcDir();
    filename = m_fileTitle;
    if (filename.GetLength()>3)
    {
      filename.SetAt(2, 'p');
      filename.SetAt(3, 'l');
    }
    str = ls->GetFileExt();
    filename += '.' + str;
    ls->SetVZK(filename);
    path += filename;
    if (file.GetStatus(path, rStatus))
      calc->SetLengthSection(ls);
    else
    {	// look for BCE Laengsschnitt
      str = calc->GetFileName();
      str.SetAt(2, 'w');
      str.SetAt(3, 'l');
      ls->SetFileName(str);
      // now check if Laengsschnitt has been created
      path = m_pProject->GetCalcDir();
      filename = m_fileTitle;
      if (filename.GetLength()>3)
      {
        filename.SetAt(2, 'w');
        filename.SetAt(3, 'l');
      }
      str = ls->GetFileExt();
      filename += '.' + str;
      ls->SetVZK(filename);
      path += filename;
      if (file.GetStatus(path, rStatus))
        calc->SetLengthSection(ls);
      else
        delete ls;
    }
    ifs.getline(buffer, LINE_SIZE, '\n');			// get next line
    str = buffer;
  }
  ifs.close();
  m_bBERModified = FALSE;
  return TRUE;
}

BOOL State::SaveCalculations()
{
  if( m_bBERModified )
  {
    CString path = m_pProject->GetDataDir() + m_fileTitle + ".ber";
    if( GetNumCalculations() == 0 )
    {
      try
      {
        CFile::Remove( path );
      }
      catch( CFileException* e )
      {
        // ignore it, file probably does not exist
        e->Delete();
      }
    }
    else
    {
      try
      {
        gofstream ofs( path, ios::out );
        ofs.setf( ios::fixed | ios::left );
        ofs.precision( 4 );
      
        ofs << GetNumCalculations() << endl;
      
        POSITION pos = m_Calculations.GetHeadPosition();
        while( pos )
        {
          Calculation* calc = m_Calculations.GetNext( pos );
          ofs << setw(61) << calc->GetName();
          ofs << setw(9) << calc->GetStartStation();
          ofs << setw(9) << calc->GetEndStation();
          ofs << calc->GetFileName() << endl;
        }

        ofs.close();
      }
      catch( ... )
      {
        // ignore
        return FALSE;
      }
    }
  }

  // now save CalcData
  POSITION pos = m_Calculations.GetHeadPosition();
  while( pos )
  {
    Calculation* calc = m_Calculations.GetNext( pos );
    calc->SaveCalcData();
    LengthSection* ls = calc->GetLengthSection();
    
    if( ls )
      ls->SaveProfil();
  }
  
  m_bBERModified = FALSE;
  return TRUE;
}

void State::AddCalculation(Calculation* ca)
{
  m_Calculations.AddTail(ca);
  SetBERModified();
}

void State::RemoveConnection( Connection* conn )
{
  POSITION pos = m_Connections.Find( conn );
  if( pos != NULL )
  {
    m_Connections.RemoveAt( pos );
    SetBERModified();
  };
}

void State::RemoveCalculation(Calculation* ca)
{
  POSITION pos;
  
  pos = m_Calculations.Find(ca);
  if (pos!=NULL)
    m_Calculations.RemoveAt(pos);
  SetBERModified();
}

Calculation* State::GetFirstCalculation()
{
  m_CalculationPos = m_Calculations.GetHeadPosition();
  return GetNextCalculation();
}

Calculation* State::GetNextCalculation()
{
  if (m_CalculationPos==NULL)
    return NULL;
  else
    return m_Calculations.GetNext(m_CalculationPos);
}

Calculation* State::GetLastCalculation()
{
  m_CalculationPos = m_Calculations.GetTailPosition();
  return GetPrevCalculation();
}

Calculation* State::GetPrevCalculation()
{
  if (m_CalculationPos==NULL)
    return NULL;
  else
    return m_Calculations.GetPrev(m_CalculationPos);
}

Calculation* State::FindCalculation(CString& file)
{
  POSITION pos;
  Calculation *ca;
  CString str;
  
  pos = m_Calculations.GetHeadPosition();
  while (pos!=NULL)
  {
    ca = m_Calculations.GetNext(pos);
    str = ca->GetFileName();
    if (file.CompareNoCase(str)==0)
      return ca;
  }
  return NULL;
}

BOOL State::CalculationExists(Calculation* ca)
{
  BOOL bExists;
  
  bExists = (BOOL)(m_Calculations.Find(ca)!=NULL);
  return bExists;
}

int State::GetNumCalculations()
{
  return m_Calculations.GetCount();
}

/***************************************************************/
// Outflow implementation

BOOL State::LoadOutFlows()
{
  gifstream ifs;
  OutFlow *of;
  char buffer[LINE_SIZE];
  CString str, path, filename;
  CFile file;
  CFileStatus rStatus;
  int i, j, n;
  
  path = m_pProject->GetDataDir();
  path += m_fileTitle + ".qwt";
  if (!file.GetStatus(path, rStatus))
    return TRUE;
  ifs.open(path, ios::in);
  if (ifs.fail())
    return FALSE;
  while (!ifs.eof())
  {
    of = new OutFlow(m_pProject, this);
    ifs.getline(buffer, LINE_SIZE, '\n');
    str = buffer;
    if (str.IsEmpty())
    {
      delete of;
      continue;
    }
    i = str.ReverseFind(' ');
    if (i==-1)
    {
      delete of;
      return FALSE;
    }
    n = atoi(str.Right(str.GetLength()-i-1));
    str = str.Left(i);
    str.TrimLeft();
    str.TrimRight();
    of->SetName(str);
    for (i=0; i<n; i++)
    {
      C3DCoord *pCrd = new C3DCoord;
      
      ifs.getline(buffer, LINE_SIZE, '\n');
      str = buffer;
      str += " ";
      str.TrimLeft();
      pCrd->dx = atof(str.Left(str.Find(" ")));
      str = str.Right(str.GetLength()-str.Find(" "));
      str.TrimLeft();
      pCrd->dy = atof(str.Left(str.Find(" ")));
      str = str.Right(str.GetLength()-str.Find(" "));
      str.TrimLeft();
      if (FindCrossSection(pCrd->dx)==NULL)
        delete pCrd;
      else
        of->AddCoord(pCrd);
    }
    if (of->GetNumCoords()==0)
      delete of;
    else
      AddOutFlow(of);
  }
  ifs.close();
  path = m_pProject->GetDataDir();
  path += m_fileTitle + ".wsf";
  if (!file.GetStatus(path, rStatus))
    return TRUE;
  ifs.open(path, ios::in);
  if (ifs.fail())
    return FALSE;
  while (!ifs.eof())
  {
    ifs.getline(buffer, LINE_SIZE, '\n');
    str = buffer;
    if (str.IsEmpty())
      continue;
    i = str.ReverseFind(' ');
    if (i==-1)
      return FALSE;
    n = atoi(str.Right(str.GetLength()-i-1));
    str = str.Left(i);
    str.TrimLeft();
    str.TrimRight();
    of = FindOutFlow(str);
    if (of==NULL)
      return FALSE;
    for (i=0; i<n; i++)
    {
      double x, y, z;
      C3DCoord *pCrd;
      
      ifs.getline(buffer, LINE_SIZE, '\n');
      str = buffer;
      str += " ";
      str.TrimLeft();
      x = atof(str.Left(str.Find(" ")));
      str = str.Right(str.GetLength()-str.Find(" "));
      str.TrimLeft();
      y = atof(str.Left(str.Find(" ")));
      str = str.Right(str.GetLength()-str.Find(" "));
      str.TrimLeft();
      z = atof(str.Left(str.Find(" ")));
      str = str.Right(str.GetLength()-str.Find(" "));
      str.TrimLeft();
      for (j=0; j<of->GetNumCoords(); j++)
      {
        pCrd = of->GetCoordAt(j);
        if (pCrd->dx==x)
        {
          pCrd->dz = z;
          break;
        }
      }
    }
  }
  ifs.close();
  m_bOutFlowModified = FALSE;
  return TRUE;
}

BOOL State::SaveOutFlows()
{
  gofstream ofs;
  OutFlow *of;
  POSITION pos;
  CString str, path;
  int i, n;
  
  if (m_bOutFlowModified)
  {
    path = m_pProject->GetDataDir();
    path += m_fileTitle + ".qwt";
    if( GetNumOutFlows() == 0 )
    {
      try
      {
        CFile::Remove( path );
      }
      catch( CFileException* e )
      {
        TCHAR msg[1000];
        e->GetErrorMessage( (char*)(&msg), 1000 );
        //        afxDump << "CFileException in State::SaveOutFlows::" << e << " " << " " << msg << " " << e->m_strFileName << "\n";
        e->Delete();
      }
      m_bOutFlowModified = FALSE;
    }
    else
    {
      ofs.open(path, ios::out);
      if (ofs.fail())
        return FALSE;
      ofs.setf(ios::fixed | ios::left);
      ofs.precision(4);
      pos = m_OutFlows.GetHeadPosition();
      while (pos!=NULL)
      {
        of = m_OutFlows.GetNext(pos);
        str = of->GetName();
        n = of->GetNumCoords();
        ofs << str << " " << n << endl;
        for (i=0; i<n; i++)
        {
          C3DCoord *pCrd;
          
          pCrd = of->GetCoordAt(i);
          ofs << setw(10) << pCrd->dx << setw(10) << pCrd->dy << endl;
        }
      }
      ofs.close();
    }
    path = m_pProject->GetDataDir();
    path += m_fileTitle + ".wsf";
    if (GetNumOutFlows()==0)
    {
      try
      {
        CFile::Remove( path );
      }
      catch( CFileException* e )
      {
        TCHAR msg[1000];
        e->GetErrorMessage( (char*)(&msg), 1000 );
        //        afxDump << "CFileException in State::SaveOutFlows::" << e << " " << " " << msg << " " << e->m_strFileName << "\n";
        e->Delete();
      }
      
      return TRUE;
    }
    ofs.open(path, ios::out);
    if (ofs.fail())
      return FALSE;
    ofs.setf(ios::fixed | ios::left);
    ofs.precision(4);
    pos = m_OutFlows.GetHeadPosition();
    while (pos!=NULL)
    {
      of = m_OutFlows.GetNext(pos);
      str = of->GetName();
      n = 0;
      for (i=0; i<of->GetNumCoords(); i++)
      {
        C3DCoord *pCrd;
        
        pCrd = of->GetCoordAt(i);
        if( pCrd->dz != std::numeric_limits<double>::infinity() )
          n++;
      }
      ofs << str << " " << n << endl;
      for (i=0; i<of->GetNumCoords(); i++)
      {
        C3DCoord *pCrd;
        
        pCrd = of->GetCoordAt(i);
        if( pCrd->dz != std::numeric_limits<double>::infinity() )
          ofs << setw(10) << pCrd->dx << setw(10) << pCrd->dy << setw(10) << pCrd->dz << endl;
      }
    }
    ofs.close();
  }
  m_bOutFlowModified = FALSE;
  return TRUE;
}

void State::AddOutFlow(OutFlow* of)
{
  m_OutFlows.AddTail(of);
  m_bOutFlowModified = TRUE;
}

void State::RemoveOutFlow(OutFlow* of)
{
  POSITION pos;
  
  pos = m_OutFlows.Find(of);
  if (pos!=NULL)
  {
    m_OutFlows.RemoveAt(pos);
    m_bOutFlowModified = TRUE;
  }
}

OutFlow* State::GetFirstOutFlow()
{
  m_OutFlowPos = m_OutFlows.GetHeadPosition();
  return GetNextOutFlow();
}

OutFlow* State::GetNextOutFlow()
{
  if (m_OutFlowPos==NULL)
    return NULL;
  else
    return m_OutFlows.GetNext(m_OutFlowPos);
}

OutFlow* State::FindOutFlow(CString& name)
{
  OutFlow *of;
  CString str;
  POSITION pos;
  
  pos = m_OutFlows.GetHeadPosition();
  while (pos!=NULL)
  {
    of = m_OutFlows.GetNext(pos);
    str = of->GetName();
    if (str.CompareNoCase(name)==0)
      return of;
  }
  
  return NULL;
}

int State::GetNumOutFlows()
{
  return m_OutFlows.GetCount();
}

/***************************************************************/
// Loss implementation

BOOL State::LoadLosses()
{
  gifstream ifs;
  Loss *loss;
  char buffer[LINE_SIZE];
  CString str, path, filename;
  CFile file;
  CFileStatus rStatus;
  int i;
  CString type;
  
  path = m_pProject->GetDataDir();
  path += m_fileTitle + ".psi";
  if (!file.GetStatus(path, rStatus))
    return TRUE;
  ifs.open(path, ios::in);
  if (ifs.fail())
    return FALSE;
  while (!ifs.eof())
  {
    loss = new Loss(m_pProject, this);
    ifs.getline(buffer, LINE_SIZE, '\n');
    str = buffer;
    str.TrimLeft();
    str.TrimRight();
    if (str.IsEmpty())
    {
      delete loss;
      continue;
    }
    str += " ";
    if (str.Left(8).CompareNoCase("STATION ")!=0)
    {
      delete loss;
      return FALSE;
    }
    str = str.Right(str.GetLength()-8);
    str.TrimLeft();
    loss->SetStation(atof(str.Left(str.Find(" "))));
    str = str.Right(str.GetLength()-str.Find(" "));
    
    i = 0;
    while (!str.IsEmpty() && i<N_VLTYPES)
    {
      str.TrimLeft();
      if (!str.IsEmpty())
      {
        type = str.Left(str.Find(" "));
        str = str.Right(str.GetLength()-str.Find(" "));
        if (type.CompareNoCase("EINLAUF")==0)
          loss->SetType(i, VL_EINLAUF);
        else if (type.CompareNoCase("KRÜMMER")==0)
          loss->SetType(i, VL_KRUEMMER);
        else if (type.CompareNoCase("ZUSATZVERLUST")==0)
          loss->SetType(i, VL_ZUSATZ);
        else if (type.CompareNoCase("RECHEN")==0)
          loss->SetType(i, VL_RECHEN);
        else if (type.CompareNoCase("AUSLAUF")==0)
          loss->SetType(i, VL_AUSLAUF);
        str.TrimLeft();
        loss->SetValue(i, atof(str.Left(str.Find(" "))));
        str = str.Right(str.GetLength()-str.Find(" "));
        i++;
      }
    }
    AddLoss(loss);
  }
  ifs.close();
  m_bLossModified = FALSE;
  return TRUE;
}

BOOL State::SaveLosses()
{
  if( m_bLossModified )
  {
    CString path = m_pProject->GetDataDir() + m_fileTitle + ".psi";
    
    if( GetNumLosses() == 0 )
    {
      try
      {
        CFile::Remove( path );
      }
      catch( CFileException* e )
      {
        // die Exception einfach ignorieren
        e->Delete();
      }
      m_bLossModified = FALSE;
      return TRUE;
    } // if GetNumLosses == 0
    
    gofstream ofs;
    ofs.open( path, ios::out );
    if( ofs.fail() )
      return FALSE;
    ofs.setf( ios::fixed | ios::left );
    ofs.precision( 6 );
    
    POSITION pos = m_CrossSections.GetHeadPosition();
    while( pos != NULL )
    {
      BOOL bStation = FALSE;
      
      CrossSection* cs = m_CrossSections.GetNext( pos );
      Loss* loss = FindLoss( cs->GetStation() );
      if( loss == NULL )
        continue;
      for( int i = 0; i < N_VLTYPES; i++ )
      {
        if( loss->ValueDefined( i ) )
        {
          if( !bStation )
          {
            ofs << "STATION " << loss->GetStation();
            bStation = TRUE;
          }
          switch( loss->GetType( i ) )
          {
          case VL_EINLAUF:
            ofs << " EINLAUF ";
            break;
            
          case VL_KRUEMMER:
            ofs << " Krümmer ";
            break;
            
          case VL_ZUSATZ:
            ofs << " ZUSATZVERLUST ";
            break;
            
          case VL_RECHEN:
            ofs << " Rechen ";
            break;
            
          case VL_AUSLAUF:
            ofs << " AUSLAUF ";
            break;
          }
          CString str;
          str.Format("%.*g", DBL_DIG, loss->GetValue( i ) );
          if( str.Find('.') == -1 )
            str += ".0";
          ofs << str;
        }
      }
      if( bStation )
        ofs << endl;
    }
    ofs.close();
    
    m_bLossModified = FALSE;
  } // if m_bLossModified
  
  return TRUE;
} // SaveLosses

void State::AddLoss(Loss* loss)
{
  m_Losses.AddTail( loss );
  m_bLossModified = TRUE;
}

void State::RemoveLoss(Loss* loss)
{
  POSITION pos = m_Losses.Find( loss );
  if (pos!=NULL)
  {
    m_Losses.RemoveAt(pos);
    m_bLossModified = TRUE;
  }
}

Loss* State::GetFirstLoss()
{
  m_LossPos = m_OutFlows.GetHeadPosition();
  return GetNextLoss();
}

Loss* State::GetNextLoss()
{
  if( m_LossPos == NULL )
    return NULL;
  else
    return m_Losses.GetNext( m_LossPos );
}

Loss* State::FindLoss( double station )
{
  POSITION pos = m_Losses.GetHeadPosition();
  while( pos != NULL )
  {
    Loss* loss = m_Losses.GetNext( pos );
    if( station == loss->GetStation() )
      return loss;
  }
  
  return NULL;
} // FindLoss

int State::GetNumLosses()
{
  return m_Losses.GetCount();
}

/***************************************************************/
// BranchTable implementation

BranchTable* State::GetBranchTable()
{
  return m_pBranchTable;
}

void State::SetBranchTable(BranchTable* bt)
{
  if (m_pBranchTable!=NULL)
    delete m_pBranchTable;
  m_pBranchTable = bt;
}

BOOL State::LoadBranchTable()
{
  CString path;
  CFileStatus rStatus;
  gifstream ifs;
  
  if (m_pBranchTable!=NULL)
    delete m_pBranchTable;
  
  path = m_pProject->GetDataDir();
  path += m_fileTitle + ".vzk";
  if (!CFile::GetStatus(path, rStatus))
    return TRUE;
  ifs.open(path, ios::in);
  if (ifs.fail())
    return FALSE;
  
  m_pBranchTable = new BranchTable();
  ifs >> *m_pBranchTable;
  ifs.close();
  
  m_bBranchTableModified = FALSE;
  return TRUE;
}

BOOL State::SaveBranchTable()
{
  CString path;
  gofstream ofs;
  
  if (m_bBranchTableModified)
  {
    path = m_pProject->GetDataDir();
    path += m_fileTitle + ".vzk";
    if (m_pBranchTable==NULL || m_pBranchTable->GetNumBranches()==0)
    {
      CFile::Remove(path);
      m_bBranchTableModified = FALSE;
      return TRUE;
    }
    ofs.open(path, ios::out);
    if (ofs.fail())
      return FALSE;
    
    ofs << *m_pBranchTable;
    ofs.close();
  }
  
  m_bBranchTableModified = FALSE;
  return TRUE;
}


/*!
* fügt Wasserspiegel in alle Querprofile dieses Zustandes ein
*
* @param LengthSectionArray* lsArray : Liste der einzufügenden Wsps
* @param BOOL bDurchst = TRUE : falls TRUE, werden Wasserpiegel durch durchst. Bereiche begrenzt
*
* @return int  : Fehlerkode:
*                  0: kein Fehler
*                  1: keine Wsps vorhanden
* Bemrkung:
ausführliche Fehlermeldung werden per MessageBox an den Benutzer ausgegeben
*/
int State::InsertWsp( LengthSectionArray* lsArray, BOOL bDurchst, const CString& strAbflussFormat )
{
  CString message;
  
  if ( !lsArray )
    return 1;
  
  int count = 0; // Anzahl der erfolgreichen LengthSections
  
  for ( int i = 0; i < lsArray->GetSize(); i++ )
  {
    LengthSection* ls = lsArray->GetAt( i );
    
    if ( !ls )
      continue;
    
    Profil* profil = NULL;
    if ( !ls->LoadProfil() || !( profil = ls->GetProfil() ) )
    {
      message.Format( "Längsschnitt %s konnte nicht geladen werden", ls->GetName() );
      AfxMessageBox( message );
      continue;
    };
    
    // die Wasserpiegelkennung erzeugen
    CString wspKennung = ls->GetName() + TEXT("@") + GetWaterName() + TEXT("@") + GetName();
    
    // Datenblöcke Wasserspiegel und Abfluss sind interessant
    DataBlock* dbWsp = profil->GetDataBlock( DST_WASSERSPIEGEL );
    DataBlock* dbQ = profil->GetDataBlock( DST_ABFLUSS );
    
    // jetzt die Wasserpiegel in die Querprofile eintragen
    int csCount = 0;
    CrossSection* cs = GetFirstCrossSection();
    while ( cs )
    {
      double station = -1000 * cs->GetStation(); // die yKoordinaten im Längschnitt sind negativ und in Metern
      
      if( dbWsp != NULL )
      {
        // wspHoehe muss immer da sein
        Coord* wspCrd = dbWsp->GetCoord( station, 0.01 ); // Station auf centimeter genau suchen
        
        // falls es einen Abfluss gibt, diesen auch holen
        double abfluss = 0;
        if( dbQ != NULL )
        {
          Coord* qCrd = dbQ->GetCoord( station, 0.01 );
          if( qCrd != NULL )
            abfluss = qCrd->dy;
        } // if dbQ
        
        if ( wspCrd && cs->InsertWsp( wspKennung, wspCrd->dy, bDurchst, abfluss, strAbflussFormat ) )
          csCount++;
      } // if dbWsp
      
      cs = GetNextCrossSection();
    }; // while cs
    
    if ( csCount == 0 )
    {
      message.Format( "Es konnten keine Wasserpiegel aus Längsschnitt %s eingetragen werden", ls->GetName() );
      AfxMessageBox( message );
    }
    else
      count++;
  }; // for i
  
  return count == 0 ? 1 : 0;
}; // InsertWsp

int State::RemoveWsp( LengthSectionArray* lsArray )
// löscht Wasserspiegel aus allen Querprofile dieses Zustandes
// Parameter:
//        LengthSectionArray* lsArray: Liste der zu löschenden Wsps
// Rückgabewert:
//        Fehlerkode:
//                0: kein Fehler
//                1: keine Wsps vorhanden

{
  CString message;
  
  if ( !lsArray )
    return 1;
  
  int count = 0; // Anzahl der erfolgreichen LengthSections
  
  for ( int i = 0; i < lsArray->GetSize(); i++ )
  {
    LengthSection* ls = lsArray->GetAt( i );
    
    if ( !ls )
      continue;
    
    Profil* profil = NULL;
    if ( !ls->LoadProfil() || !( profil = ls->GetProfil() ) )
    {
      message.Format( "Längsschnitt %s konnte nicht geladen werden", ls->GetName() );
      AfxMessageBox( message );
      continue;
    };
    
    // die Wasserpiegelkennung erzeugen
    CString wspKennung = ls->GetName() + TEXT("@") + GetWaterName() + TEXT("@") + GetName();
    
    // nur der Wasserspiegel des Längschnittes ist interessant
    DataBlock* db = profil->GetDataBlock( DST_WASSERSPIEGEL );
    CString lsKennung = db->GetName( 1 );
    if( lsKennung.GetLength() > 99 )
      lsKennung = lsKennung.Mid( 99 ); // ist meisst ab 100 oder 200
    lsKennung.TrimLeft(); // vorsichthalber nochmal trimmen
    lsKennung.TrimRight();
    
    // falls die Kennungen nicht passen ist irgendwas falsch
    if ( lsKennung.CompareNoCase( wspKennung ) != 0 )
    {
      message.Format( "Kennung %s passt nicht zum Längschnitt %s", wspKennung, ls->GetName() );
      AfxMessageBox( message );
      continue;
    };
    
    // jetzt die Wasserpiegel aus den Querprofile löschen
    int csCount = 0;
    CrossSection* cs = GetFirstCrossSection();
    while ( cs )
    {
      double station = cs->GetStation();
      
      // die yKoordinaten im Längschnitt sind negativ und in Metern
      Coord* wspCrd = db->GetCoord( -1000 * station, 0.01 ); // Station auf centimeter genau suchen
      
      if ( wspCrd && cs->RemoveWsp( wspKennung ) )
        csCount++;
      
      cs = GetNextCrossSection();
    }; // while cs
    
    if ( csCount == 0 )
    {
      message.Format( "Es sind keine Wasserpiegel aus Längschnitt %s vorhanden", ls->GetName() );
      AfxMessageBox( message );
    };
    
    count++;
  }; // for i
  
  return 0;
}; // // RemoveWsp

////////////////
// Operatoren //
////////////////

BOOL State::IsPredOf( const CrossSection& cs1, const CrossSection& cs2 )
// stellt fest, ob cs2 ein Nachfolger ( im Sinne fes Flussverlaufs ) von cs1 ist
// Voraussetzung:
//          cs1 und cs2 gehören beide zu diesem Zustand
{
  int vzk1 = cs1.GetVZK();
  int vzk2 = cs2.GetVZK();
  
  // mal schauen, ob die Verzweigunskennung ein Verbindung zulässt
  BranchTable* bt = GetBranchTable();
  if( bt )
  {
    // falls eine Branchtable vorhanden ist und diese sagt, dass keine Verbindung möglich ist, dann ist schluss
    if( !bt->IsConnected( vzk1, vzk2 ) )
      return FALSE;
  }
  else
  {
    // sonst muss mindestens eine der beiden im Hauptstrom liegen( der ist an alles angebunden )
    if( vzk1 != 0 && vzk2 != 0 )
      return FALSE;
  }; // if bt
  
  // wenn prinzipiell die beiden Stränge verbunden sind hängts nur noch an der Station
  // Umgekehrte Stationierung: also falls station1 > station2 ists ok
  return ( cs1.GetStation()  - cs2.GetStation() > -0.0001 );
}; // IsPredOf

CrossSection* State::GetFollowingCs( CrossSection* cs, const CrossSectionArray& csArray, 
                                    const BOOL bVorwaerts, const BOOL bLeft )
                                    // findet aus der Liste der Profile das folgende im Strang
                                    // Parameter:
                                    //        CrossSection* cs: von diesem Profil wird der nachfolger gesucht
                                    //        CrossSectionArray* csArray: falls != NULL und nichtleer wird nur aus diesen Profilen der Nachfolger ermittelt
                                    //        BOOL bVorwaerts: falls TRUE wird in Fliessrichtung ( gegen die Stationierung ) gesucht, sonst dagegen
                                    //        BOOL bLeft: falls TRUE wird bei Verzeigungen stets links, sonst stets rechts abgebogen
                                    // Rückgabewert:
                                    //        NULL, falls nix gefunden wurde oder Fehler, sonst das gesuchte Profil
{
  if( !cs )
    return FALSE;
  
  CString profilFile = cs->GetFileName();
  
  // aus der Strangtabelle die richtige Connection raussuchen
  CString nextFile;
  POSITION pos = m_Connections.GetHeadPosition();
  while( pos )
  {
    Connection* conn = m_Connections.GetNext( pos );
    if( bVorwaerts && profilFile.CompareNoCase( conn->GetEndProf() ) == 0 )
    {
      nextFile = conn->GetAnfProf();
      break;
    };
    if( !bVorwaerts && profilFile.CompareNoCase( conn->GetAnfProf() ) == 0 )
    {
      nextFile = conn->GetEndProf();
      break;
    };
  }; // while pos
  
  // falls nix gefunden, gleich zurück
  if( nextFile.IsEmpty() )
    return NULL; 
  
  // das Profil dazu suchen
  CrossSection* nextCs = NULL;
  pos = m_CrossSections.GetHeadPosition();
  while( pos )
  {
    CrossSection* helpCs = m_CrossSections.GetNext( pos );
    if( nextFile.CompareNoCase( helpCs->GetFileName() ) == 0 )
    {
      nextCs = helpCs;
      break;
    }; // if nextFile = helpCs.GetFileName()
  }; // while pos
  
  // falls kein Profil gefunden, gleich zurück
  if( !nextCs )
    return NULL;
  
  // mal schauen, ob wir an einer Verzweigung sind
  int vzk = cs->GetVZK();
  
  BranchTable* bt = GetBranchTable();
  if( vzk != nextCs->GetVZK() && bt )
  {
    // erstmal eine Liste der möglichen Folgestränge suchen ( können mehrere sein, falls cs.VZK = 0 )
    CUIntArray vzkArray;
    
    if( vzk == 0 )
    {
      for( int i = 0; i < bt->GetNumBranches(); i++ )
      {
        Branch* b = bt->GetBranch( i );
        if( b )
        {
          if( ( bVorwaerts && b->GetZFK( 0 ) == b->GetZFK( 1 ) && b->GetZFK( 1 ) == 0 ) ||
            ( !bVorwaerts && b->GetAFK( 0 ) == b->GetAFK( 1 ) && b->GetAFK( 1 ) == 0 )   )
            vzkArray.Add( b->GetVZK() );
        }; // if b
      }; // for i
    }
    else
    {
      Branch* b = bt->FindBranch( vzk );
      if( b )
      {
        if( bVorwaerts )
        {
          if( !bLeft && b->GetAFK( 0 ) != b->GetAFK( 1 ) && b->GetAFK( 1 ) != 0 )
            vzkArray.Add( b->GetAFK( 1 ) );
          else
            vzkArray.Add( b->GetAFK( 0 ) );
        }
        else
        {
          if( !bLeft && b->GetZFK( 0 ) != b->GetZFK( 1 ) && b->GetZFK( 1 ) != 0 )
            vzkArray.Add( b->GetZFK( 1 ) );
          else
            vzkArray.Add( b->GetZFK( 0 ) );
        }; // if bVorwaerts
      }; // if b
    }; // if vzk == 0
    
    // jetzt die Liste der Profile durchsuchen, und das kleinste Profil finden, welches eines der gefundenen VZK besitzt
    double abstand = 1e36; // sehr gross
    double station = cs->GetStation();
    
    nextCs = NULL; // neu suchen
    POSITION pos = m_CrossSections.GetHeadPosition();
    while( pos )
    {
      CrossSection* helpCs = m_CrossSections.GetNext( pos );
      
      BOOL bFound = FALSE;
      for( int i = 0; i < vzkArray.GetSize(); i++ )
      {
        if( (UINT)helpCs->GetVZK() == vzkArray[i] )
        {
          bFound = TRUE;
          break;
        }; // if helpCs.VZK == vzk[i]
      }; // for i
      
      if( !bFound)
        continue;
      
      // ist im gesuchten Strang, mit vorhandenem Profil vergleichen
      double helpStation = helpCs->GetStation();
      double dist = station - helpStation;
      if( !bVorwaerts )
        dist = -dist;
      
      if( 0 < dist && dist < abstand )
      {
        abstand = dist;
        nextCs = helpCs;
      }; // if 0 < dist < abstand
    }; // while pos
    
  }
  else
  {
    // falls es keine Verzweigung ist, wir aber rechts suchen gibts keinen Nachfolger oder Vorgänger
    if( !bLeft )
      return FALSE;
  }; // if cs.VZK != nextCs.GetVZK
  
  if( !nextCs )
    return FALSE;
  
  // zuletzt noch schauen, ob nextCs in der Liste der übergebenen profile ist, sonst weitersuchen
  if( &csArray && csArray.GetSize() > 0 )
  {
    for( int i = 0; i < csArray.GetSize(); i++ )
    {
      if( nextCs == csArray[i] )
        return nextCs;
    }; // for i
    
    // falls nicht inder Liste, den nachfolger des Nachfolgers suchen
    return GetFollowingCs( nextCs, csArray, bVorwaerts, bLeft );
  } // if csArray
  else
    // ansonsten einfach die gefundene CrossSection zurückgeben
    return nextCs; 
  
  // hier kommt man niemals an
}; // GetFollowingCs

void State::RemoveCSFromConnections( CrossSection* cs )
// löscht eine CrossSection aus der Liste der Connections
{
  POSITION pos = m_Connections.GetHeadPosition();
  
  // erste Verbindung finden
  Connection* firstConn = NULL;
  while( pos )
  {
    Connection* conn = m_Connections.GetNext( pos );
    if( conn && conn->GetEndProf().CompareNoCase( cs->GetFileName() ) == 0 )
    {
      firstConn = conn;
      break;
    };
  } // while pos
  
  // zweite Verbindung finden
  Connection* secondConn = NULL;
  pos = m_Connections.GetHeadPosition();
  while( pos )
  {
    Connection* conn = m_Connections.GetNext( pos );
    if( conn && conn->GetAnfProf().CompareNoCase( cs->GetFileName() ) == 0 )
    {
      secondConn = conn;
      break;
    };
  } // while pos
  
 
  if( !secondConn ) // falls nur eine Connection da ist ( d.h Anfang oder Ende ) einfach löschen
  {
    RemoveConnection( firstConn );
    delete firstConn;
    firstConn = NULL;
  }
  else if( !firstConn )
  {
    RemoveConnection( secondConn );
    delete secondConn;
    secondConn = NULL;
  }
  else // wenn es zwei gibt, diese zu einer zusammenfassen
  {
    firstConn->SetEndProf( secondConn->GetEndProf() );
    firstConn->SetEndStation( secondConn->GetEndStation() );
    firstConn->SetFluss( firstConn->GetFluss() + secondConn->GetFluss() );
    firstConn->SetVorlandLinks( firstConn->GetVorlandLinks() + secondConn->GetVorlandLinks() );
    firstConn->SetVorlandRechts( firstConn->GetVorlandRechts() + secondConn->GetVorlandRechts() );
    
    // jetzt kann die zweite einfach gelöscht werden
    RemoveConnection( secondConn );
    delete secondConn;
    secondConn = NULL;
  };
}; // RemoveCSFromCalculations();

void State::CalcAnfEndStation()
// aktualisiert die Anfangs und EndStation
{
  double anfStation = std::numeric_limits <double>::infinity();
  double endStation = -anfStation;
  
  POSITION pos = m_CrossSections.GetHeadPosition();
  while( pos )
  {
    CrossSection* cs = m_CrossSections.GetNext( pos );
    if( cs )
    {
      anfStation = min( anfStation, cs->GetStation() );
      endStation = max( endStation, cs->GetStation() );
    }; // if cs
  }; // while pos
  
  SetStartStation( anfStation );
  SetEndStation( endStation );
} // CalcAnfEndStation


BOOL State::ImportDataDa66( CStdioFile* eingabedatei )
// Importfilter zum Einlesen von Dateien im Da66 (.d66) Format
// Stationen, welche bereits existieren werden übersprungen
// Input:
//        eingabedatei: bereits geöffnete Eingabedatei
// Output:
//        TRUE´/FALSE je nach Erfolg der Operation
// liest Profilinformationen aus der .d66 Datei und erstellt aus diesen neue Profile
// nur Datenbloecke GELAENDEHOEHE
{
  if( GetProject() == NULL )
    return FALSE; // das Project muss existieren
  
  // es gibt jeweils einen aktuellen DataBlock
  DataBlock* db = NULL;
  
  CStringList files; // Liste der bereits erzeugten Dateien
  
  double lastStation = 0; // die Station der zuletzt gelesenen Zeile
  while( TRUE )
  {
    CString str;
    BOOL bStop = FALSE;
    
    if( !eingabedatei->ReadString( str ) )
      bStop = TRUE;

    str.TrimRight();
    
    // die Datenart auslesen; Kommentarzeilen o.ä. überlesen
    
    // DatenArt und station rausfinden
    int DatArt = -1;
    double station;
    
    if( !bStop )
    {
      // nur wenn 66 oder 88 da ist, noch weitermachen
      int DatArt = -1;
      if( sscanf( str.Left( 2 ), "%d", &DatArt ) != 1 || ( DatArt != 66 && DatArt != 88 ) )
        continue;

      CString stStr = str.Mid( 9, 9 );
	  stStr.TrimRight();
	  stStr = stStr + CString( '0', 9 - stStr.GetLength() );
      
      station = (double)atoi( stStr ) / 1000000;

      char buffer [256];
      sprintf( buffer, "%10.4lf\0", station );
      sscanf( buffer, "%lf", &station );
    };
    
    // bei einem Stationswechsel oder ganz am Anfang oder ganz am Ende: neues Profil erzeugen und hinzufügen
    if( fabs( station - lastStation ) > 0.00000001 || bStop )
    {
      // falls ein Datenblock existiert, ein neues Profil erzeugen und zum Zustand hinzufügen
      if( db != NULL )
      {
        // gibt es diese Station denn schon?
        if( FindCrossSection( lastStation ) != NULL )
        {
          // falls diese Station schon existiert, den Datenblock einfach verwerfen
          delete db; 
          db = NULL;
        }
        else
        {
          // ansonsten ein neues Profil erzeugen und hinzufügen
          CrossSection* cs = new CrossSection( GetProject() );
          
          cs->SetWaterName( GetWaterName() );
          cs->SetStateName( GetName() );
          cs->SetStation( lastStation );
          cs->SetPK( "0" );
          cs->SetVZK( 0 );
          
          cs->SetProfilNr( GetProject()->GetCrossSectionCount() + 1 );
          cs->CreateFileName( &files );
          files.AddTail( cs->GetFileName() );
          
          // create new profil and set info
          Profil* pr = new Profil( cs );
          pr->SetOriginalFile( eingabedatei->GetFilePath() );
          
          CString help_str;
          help_str.Format("%s %s", GetWaterName(), GetName() );
          pr->SetPageDesc( 0, help_str );
          help_str.Format( "QUERPROFIL %d", cs->GetProfilNr() );
          pr->SetPageDesc( 1, help_str );
          help_str.Format( "STATION KM %.4f", lastStation );
          pr->SetPageDesc( 2, help_str );
          cs->SetProfil( pr );
          
          db->SetProfil( pr );
          pr->AddDataBlock( db );
          db = NULL; // jetzt diesen Datenblock nicht mehr benutzen
          
          AddCrossSection( cs );
        }  // if FindCrossSection
        
      } // if db != NULL
    } // if bStop      
    
    if( bStop )
      break;
    
    // falls jetzt der Datenblock NULL ist, einen erzeugen
    if( db == NULL )
      db = new DataBlock( NULL, DST_GELAENDEHOEHE );
    
    int k = 20; // Position im str
    while( k < str.GetLength() )
    {
      CString xStr = str.Mid( k, 8 );
      k += 8;
      if( k >= str.GetLength() )
        break;
      
      CString yStr = str.Mid( k, 7 );
      k += 7;

      double x = atoi( xStr ) / 1000.0;
      double y = atoi( yStr ) / 1000.0;
      
      Coord* pCrd = new Coord( x, y );
      db->AddCoord( pCrd );
    } // while k
    
    lastStation = station;
  } // while TRUE
  
  if( db != NULL )
    delete db; // bei Kommentarzeilen am Ende kann das Vorkommen
  
  return TRUE;
} // Import Da66

BOOL State::ImportDataDa50( CStdioFile* eingabedatei, DWORD data )
// Importfilter zum Einlesen von Dateien im Da50 (.d50) Format
// Input:
//        CStdioFile* eingabedatei: bereits geöffnete Eingabedatei
//        DWORD data: falls gleich STATE::IMPORT_DA50_REFZERO, wird
//                    der Nullpunkt des Profils, als erster Referenzpunkt gewähl, ansonsten
//                    wird der erste genommen; der zweite ist stets der letzte
// Output:
//        TRUE´/FALSE je nach Erfolg der Operation
// Geoinformationen zu d60 Dateien
// liest Geoinformationen (Rechtswert, Hochwert) aus der .d50 Datei und fügt diese
// zu bereits bestehenden Profilen zu
{
  if( data != IMPORT_DA50_REFZERO && data != IMPORT_DA50_REFFIRST )
    return FALSE;

  CString inputfilename = eingabedatei->GetFilePath();
  std::string logfilename = inputfilename.Left( 
    eingabedatei->GetFilePath().GetLength() - 4 ) + "_import.log"; 
  std::ofstream logstream( logfilename.c_str(), ios::out );
  logstream << "Datenimport D50" << std::endl;
  logstream << "Datei: " << LPCTSTR(inputfilename) << std::endl;


  BOOL bRefFirst = ( data == IMPORT_DA50_REFFIRST );
  logstream << "Modus: ";
  if( bRefFirst )
	  logstream << "Referenzierung auf erste Breitenkoordinate";
  else
	  logstream << "Referenzierung auf Breitenkoordinate 0.0";
  logstream << std::endl << std::endl;

  int georefcount = 0;
  CString str;
  while( eingabedatei->ReadString( str ) )
  {
    if ( str.GetLength() < 60 || str.Left( 2 ) != "50" )
      continue;
    
	// Station auslesen und mit 0en auffüllen, sonst klappt das umrechnen in m nicht immer
    CString help_str = str.Mid( 9,9 );
	help_str.TrimRight();
	help_str = help_str + CString( '0', 9 - help_str.GetLength() );

    int temp = 0;
    sscanf( help_str, "%d", &temp );
    double station = (double)temp / 1000000;

	logstream << "Geokoordinaten gefunden für Station: " << station << std::endl;

    CrossSection* cs = FindCrossSection( station );
    if( cs )
    {
		logstream << "Querprofil mit gleicher Station gefunden" << std::endl;

      double rw_start, rw_end,hw_start,hw_end;
      __int64 rh_wert;
      help_str=str.Mid(21-1,10);
      if(sscanf(help_str,"%I64d",&rh_wert)==EOF)
        continue;
      rw_start=(double)rh_wert/1000.0;
      help_str=str.Mid(31-1,10);
      if(sscanf(help_str,"%I64d",&rh_wert)==EOF)
        continue;
      hw_start=(double)rh_wert/1000.0;
      help_str=str.Mid(41-1,10);
      if(sscanf(help_str,"%I64d",&rh_wert)==EOF)
        continue;
      rw_end=(double)rh_wert/1000.0;
      help_str=str.Mid(51-1,10);
      if(sscanf(help_str,"%I64d",&rh_wert)==EOF)
        continue;
      hw_end=(double)rh_wert/1000.0;
      
      cs->LoadProfil();
      Profil* pr = cs->GetProfil();
      
      if( !pr )
        continue;
      
      DataBlock* gelaende = pr->GetDataBlock( DST_GELAENDEHOEHE );
      
      DataBlock* db_rw = new DataBlock( pr );
      db_rw->SetType( DST_RECHTSWERT );
      DataBlock* oldDb = pr->GetDataBlock( DST_RECHTSWERT );
      if( oldDb )
      {
        pr->RemoveDataBlock( oldDb );  // falls ein Datenblock  schon vorhanden ist durch den neuen ersetzen
        delete oldDb;
      };
      pr->AddDataBlock( db_rw );

      DataBlock* db_hw = new DataBlock( pr );
      db_hw->SetType( DST_HOCHWERT );
      oldDb = pr->GetDataBlock( DST_HOCHWERT );
      if( oldDb )
      {
        pr->RemoveDataBlock( oldDb );  // falls ein Datenblock  schon vorhanden ist durch den neuen ersetzen
        delete oldDb;
      };
      pr->AddDataBlock( db_hw );
      
      // den Vektor der Profilachse ausrechnen
      double vx = rw_end - rw_start;
      double vy = hw_end - hw_start;
      double vl = sqrt( vx * vx + vy * vy );
      if( vl == 0.0 )
        continue;

      // den Vektor normieren
      vx /= vl;
      vy /= vl;
      
      Coord* crd = gelaende->GetFirstCoord();
      double firstY = 0;
      if( bRefFirst )
        firstY = crd->dx;

      while( crd )
      {
        double y = crd->dx - firstY;
        
        double rw = rw_start + y * vx;
        double hw = hw_start + y * vy;
        
        // originale Breite nehmen, nicht die verschobene!
        db_rw->AddCoord( new Coord( crd->dx, rw ) );
        db_hw->AddCoord( new Coord( crd->dx, hw ) );
        
        crd = gelaende->GetNextCoord();
      };

    	georefcount++;
    }
	else
		logstream << "Kein passenden Querprofil gefunden" << std::endl;

	logstream << std::endl;
  }; // while eingabedatei


  logstream << "Es wurden insgesamt " << georefcount << " Querprofile georeferenziert." << std::endl;

  return TRUE;
} // ImportDataDa50

/**
 * Liest Geoinformationen aus der .txt Datei aus und erstellt entsprechende profile
 * der y-Wert wird immer relativ zum ersten Profilpunkt gesetzt
 * Punkte werden nach Lage zum ersten Punkt einsortiert.  Achtung: d.h. es darf keine Punkte (geometrisch) vor dem ersten punkt geben
 *
 * Importfilter zum Einlesen von Dateien im Tripple (.txt) Format
 * @param eingabedatei bereits geöffnete Eingabedatei
 * @return TRUE/FALSE je nach Erfolg der Operation
 */
BOOL State::ImportDataTripple( CStdioFile* eingabedatei )
{
  // Log-Datei
  CStdioFile logDatei;
  CString logFilename = eingabedatei->GetFilePath().Left( 
    eingabedatei->GetFilePath().GetLength() - 3 ) + "log"; 
  logDatei.Open( logFilename, CFile::modeCreate | CFile::modeWrite );
  logDatei.WriteString( "Logdatei zum Dateiimport "+ eingabedatei->GetFileName() + "\n\n" );
  
  CString inputStr;
  if( !eingabedatei->ReadString( inputStr ) )
    return FALSE;  // erste Zeile (kommentar) überlesen
  
  // Tripple-Typ anhand des Strings festlegen
  enum { NORMAL, KS, KST };

  int type = NORMAL;
  if( inputStr.Find( "RAUHEIT KST" ) != -1 )
	  type = KST;
  else if( inputStr.Find( "RAUHEIT KS" ) != -1 )
	  type = KS;

  switch( type )
  {
  case NORMAL:
	  logDatei.WriteString( "Lese ohne Rauheitswerte\n\n" );
	  break;
  case KS:
	  logDatei.WriteString( "Lese mit Rauheitswerte KS\n\n" );
	  break;
  case KST:
	  logDatei.WriteString( "Lese mit Rauheitswerte KST\n\n" );
	  break;
  }
  
  Profil* pr = NULL; // das zur Zeit aktuelle Profil
  CStringList files; // Liste der bereits benutzten Dateinamen
  
  Coord* crdT1 = NULL; // die Koordinaten für die rechte und linke Trennfläche
  Coord* crdT2 = NULL;
  Coord* crdD1 = NULL; // die Koordinaten für den rechten und linken durchströmten Bereich
  Coord* crdD2 = NULL;
  Coord crdLast; // stets die zuletzt gelesene Coordinate
  Coord crdLastKnick;  // jeweils der letzte Knickpunkt
  Coord crdLastKnickGeo; // die Geokoordinaten des letzten Knickpunktes
  BOOL bEOF = FALSE;
  double station; // jeweils die letzte Station
  double yKPVersatz = 0.0; //für die Umrechnung vom relativen Abstand zum KP in absoluten Abstand
  
  station = std::numeric_limits<double>::infinity();
  
  while ( TRUE )
  {
    double station_neu;
    double rw, hw , hoehe;
	double rauheit = 0.0; // initialier Wert ks = 0, falls keine Rauheitsspalte vorhanden
    CString kz_hydra;						//neue Variable zum Einlesen der letzten Spalte (LU,RU,KP)
    
    BOOL bEOF = !eingabedatei->ReadString( inputStr );
    
    // jetzt die Zeile parsen
    if( !bEOF )
    {
		switch( type )
		{
		case NORMAL:
			sscanf( inputStr, "%lf,%lf,%lf,%lf,%s", &station_neu, &rw, &hw, &hoehe, 
				kz_hydra.GetBuffer( 1000 ) );  // Zeile lesen
			kz_hydra.ReleaseBuffer();
			break;

		case KST:
		case KS:
			sscanf( inputStr, "%lf,%lf,%lf,%lf,%lf,%s", &station_neu, &rw, &hw, &hoehe, &rauheit,
				kz_hydra.GetBuffer( 1000 ) );  // Zeile lesen
			break;
		}
    }; // if !EOF
    
    BOOL bStationNeu = ( fabs( station - station_neu ) > 0.00001 ); // falls die beiden stationen nicht übereinstimmen, heisst das, eine neue fängt an
    
    if( pr && ( bEOF || bStationNeu ) )// aktuelles Profil zuende -> noch Trennflächen und Durchst Bereiche schreiben
    {
		// falls keine Trennflaechen und durchstroemte Bereiche gefunden wurden
		// neue erzeugen und auf Profilanfang und Ende setzen
		if( !crdT1 || !crdT2 )
		{
			logDatei.WriteString( TEXT("Keine Trennflächen über Kennzeichen definiert.\nTrennflächen und durchströmte Bereiche  werden ans Ende des Profil gesetzt.\n") );


			DataBlock* db_hoehe = pr->GetDataBlock(DST_GELAENDEHOEHE);
			Coord* fstCrd = db_hoehe->GetFirstCoord();
			Coord* lstCrd = db_hoehe->GetLastCoord();
			crdT1 = new Coord( fstCrd->dx, 1.0 );
			crdT2 = new Coord( lstCrd->dx, 2.0 );

			crdD1 = new Coord( fstCrd->dx, fstCrd->dy );
			crdD2 = new Coord( lstCrd->dx, lstCrd->dy );
		}

      if( crdT1 && crdT2 )
      { // beide Trennflächen sind da, also rauschreiben
        DataBlock* db = new DataBlock( pr );
        pr->AddDataBlock( db );
        db->SetType( DST_TRENNFLAECHEN );
        db->AddCoord( crdT1 );
        db->AddCoord( crdT2 );
        
        ASSERT( crdD1 ); // muss auf den letzten Knickpunkt zeigen
        if( !crdD2 ) // crdD2 nicht initialisiert, heisst das, dass nach crdT2 kein Knickpunkt mehr kam -> letzte Coordinate nehemn
          crdD2 = new Coord( crdLast );
        
        // jetzt noch die durchst. Bereiche erzeugen
        db = new DataBlock( pr );
        pr->AddDataBlock( db );
        db->SetType( DST_DURCHST_BEREICH );
        db->AddCoord( crdD1 );
        db->AddCoord( crdD2 );
      }
      else
      { // ansonsten alle löschen ( falls ein paar NULL macht das nix )
        delete crdT1;
        delete crdT2;
        delete crdD1;
        delete crdD2;
      }; // if crdT1 && crdT2
      crdT1 = crdT2 = crdD1 = crdD2 = NULL; // entweder wurden die Coordinaten gelöscht oder zu Datenblöcken hinzugefügt
      
      logDatei.WriteString( TEXT("\n") );
    }; // if pr && ( bEOF || station != station_neu )
    
    
    // falls die Datei zuende ist, jetzt abbrechen
    if( bEOF )
      break; 
    
    // jetzt testen, ob ein neuen Profil anfängt
    if( bStationNeu )
    {
      DataBlock* db;
      CString defaultLang;						
      CString gewaessername, zustandsname;
      CString help_str;
      station = station_neu;
      
      help_str.Format( "Lese Station %.4lf\n", station );
      logDatei.WriteString( help_str );
      
      // create new cross section and set info
      CrossSection* cs = new CrossSection( GetProject() );
      gewaessername = GetWaterName();
      cs->SetWaterName(gewaessername);
      zustandsname = GetName();
      cs->SetStateName( zustandsname );
      cs->SetStation( station );
      help_str = "0";
      cs->SetPK( help_str );
      cs->SetVZK( 0 );
      cs->SetProfilNr( GetProject()->GetCrossSectionCount() + 1 );
      cs->CreateFileName(&files);
      files.AddTail(cs->GetFileName());
      AddCrossSection( cs );
      
      // create new profil and set info
      pr = new Profil(cs);
      pr->SetOriginalFile(eingabedatei->GetFilePath());
      help_str.Format("%s %s", gewaessername, zustandsname);
      pr->SetPageDesc(0, help_str);
      help_str.Format("QUERPROFIL %d", cs->GetProfilNr());
      pr->SetPageDesc(1, help_str);
      help_str.Format("STATION KM %.4f", station);
      pr->SetPageDesc(2, help_str);
      cs->SetProfil(pr);
      
      // create new data block and set info
      db = new DataBlock(pr);
      db->SetType(DST_GELAENDEHOEHE);
      pr->AddDataBlock(db);
      
      db = new DataBlock(pr);
      db->SetType(DST_RECHTSWERT);
      pr->AddDataBlock(db);
      
      db = new DataBlock(pr);
      db->SetType(DST_HOCHWERT);
      pr->AddDataBlock(db);

	  switch( type )
	  {
	  case KS:
		  // auch ohne angabe immer einen datenblock rauheit erzeugen
	  case NORMAL:
		  {
			  DataBlock* ksDB = new DataBlock( pr );
			  ksDB->SetType( DST_RAUHIGKEIT );
			  pr->AddDataBlock( ksDB );
		  }
		  break;

	  case KST:
		  {
			  DataBlock* kstDB = new DataBlock( pr );
			  kstDB->SetType( DST_RAUHIGKEIT_KST );
			  pr->AddDataBlock( kstDB );
		  }
		  break;
	  }
      
      crdLastKnickGeo = Coord( rw, hw );
      crdLastKnick = Coord( 0, hoehe );
      yKPVersatz = 0.0; // mit y-Abständen neu anfangen
    }; // if bStationNeu
    
    double yAbstand = yKPVersatz + sqrt( pow( crdLastKnickGeo.dx - rw, 2.0 ) + 
      pow( crdLastKnickGeo.dy - hw, 2.0 ) );  // Abstand zum ersten Profilpunkt
    
    // Coordinate zu den entspr. Datenblöcken hinzufügen
    DataBlock* db_hoehe = pr->GetDataBlock(DST_GELAENDEHOEHE);
    DataBlock* db_rw = pr->GetDataBlock(DST_RECHTSWERT);
    DataBlock* db_hw = pr->GetDataBlock(DST_HOCHWERT);
	DataBlock* db_rauh = 0;
	switch( type )
	{
	case KS:
		// auch ohne angabe immer einen datenblock rauheit erzeugen
	case NORMAL:
		db_rauh = pr->GetDataBlock( DST_RAUHIGKEIT );
		break;
	case KST:
		db_rauh = pr->GetDataBlock( DST_RAUHIGKEIT_KST );;
		break;
	}
    
    // Profilpunkt an die richtige Stelle im Profil schreiben
    int pos = db_hoehe->GetNumCoords();
    while( pos != 0 )
    {
      Coord* crd = db_hoehe->GetCoordAt( pos - 1 );
      if( yAbstand > crd->dx )
        break;
      pos--;
    };
    
    // Coordinaten einfügen
    Coord* crd_hoehe = new Coord( yAbstand, hoehe );  // Koordinaten für hoehe, rechts- und hochwert erzeugen
    Coord* crd_rw = new Coord( yAbstand, rw );
    Coord* crd_hw = new Coord( yAbstand, hw );
	Coord* crd_rauh = 0;
	if( db_rauh )
		crd_rauh = new Coord( yAbstand, rauheit );

    if( pos == db_hoehe->GetNumCoords() )
    {    // noch kein Punkt vorhanden oder ganz hinten anhängen
      db_hoehe->AddCoord(crd_hoehe);
      db_rw->AddCoord(crd_rw);
      db_hw->AddCoord(crd_hw);
	  if( db_rauh )
		  db_rauh->AddCoord( crd_rauh );
    }
    else
    { 
      CString hStr;
      hStr.Format( "Füge Punkt ein bei Index: %d\n", pos );
      logDatei.WriteString( hStr );
      
      // mittendrin einfügen
      db_hoehe->InsertCoordAt( pos, crd_hoehe );
      db_rw->InsertCoordAt( pos, crd_rw );
      db_hw->InsertCoordAt( pos, crd_hw );

	  if( db_rauh )
		db_rauh->InsertCoordAt( pos, crd_rauh );
    };
    
    // Überpruft den Wert von kz_hydra
    if ( kz_hydra == "KP" )
    {
      crdLastKnick = Coord( yAbstand, hoehe );
      crdLastKnickGeo = Coord( rw, hw );
      yKPVersatz = yAbstand;
      
      if( crdT2 && !crdD2 ) // es gab ein linkes Ufer, aber bisher noch keinen Knickpunkt
        crdD2 = new Coord( yAbstand, hoehe );
    }
    else if ( kz_hydra == "LU" )
    {
      if( crdT1 )
        logDatei.WriteString( "zweites linkes Ufer\n" );
      else
      {
        crdT1 = new Coord( yAbstand, 1.0 );
        crdD1 = new Coord( crdLastKnick );
      }; // if crdT1
    }
    else if ( kz_hydra == "RU" )
    {
      if( crdT2 )
        logDatei.WriteString( "zweites Rechtes Ufer\n" );
      else
        crdT2 = new Coord( yAbstand, 2.0 );
    }; // kz_hydra == ?
    
    crdLast = Coord( yAbstand, hoehe ); // immmer die letzte Koordinate
  }; // while true
  
  return TRUE;
}; // ImportDataTripple

void State::CreatePrintSummary( CStringArray& strings )
{
  static CString underscore = "______________________________________________________________________________________________";

  strings.Add( CString( MAKEINTRESOURCE( IDS_STATE ) ) + GetName() );
  strings.Add( "" );
					
  if( GetNumCrossSections() > 0 )
  {
    strings.Add( CString( MAKEINTRESOURCE( IDS_CROSSSECTIONS ) ) + ":" );
    strings.Add( CString( MAKEINTRESOURCE( IDS_STATION ) ) + "\t" + 
                 CString( MAKEINTRESOURCE( IDS_PK ) ) + "\t" +    
                 CString( MAKEINTRESOURCE( IDS_VZK ) ) + "\t" +
                 CString( MAKEINTRESOURCE( IDS_FILE ) ) + "\t" );
    strings.Add( underscore );
    
    CrossSection* cs = GetFirstCrossSection();
    while( cs )
    {
      cs->CreatePrintSummary( strings );
      cs = GetNextCrossSection();
    }
    
    strings.Add( "" );
    strings.Add( "" );
  }

  if( GetNumCalculations() > 0 )
  {
    strings.Add( CString( MAKEINTRESOURCE( IDS_LENGTHSECTIONS ) ) + ":" );
    strings.Add( CString( MAKEINTRESOURCE( IDS_NAME ) ) + "\t" +
                 CString( MAKEINTRESOURCE( IDS_START ) ) + "\t" +
                 CString( MAKEINTRESOURCE( IDS_END ) ) + "\t" +
                 CString( MAKEINTRESOURCE( IDS_FILE ) ) );
    strings.Add( underscore );
    
    Calculation* calc = GetFirstCalculation();
    while( calc )
    {
      LengthSection* ls = calc->GetLengthSection();
      if( ls )
      {
        CString temp1 = ls->GetName();
        CString fileTitle = ls->GetFileTitle();
        CString fileExt = ls->GetFileExt();
        CString temp2 = "dath\\" + fileTitle + '.' + fileExt;
        CString str;
        str.Format("%s\t%.4f\t%.4f\t%s", temp1, ls->GetStartStation(), ls->GetEndStation(), temp2 );
        strings.Add( str );
      }
      
      calc = GetNextCalculation();
    }
    strings.Add( "" );
    strings.Add( "" );
    
    strings.Add( CString( MAKEINTRESOURCE( IDS_CALCULATIONS ) ) + ":" );
    strings.Add( CString( MAKEINTRESOURCE( IDS_NAME ) ) + "\t" + 
                 CString( MAKEINTRESOURCE( IDS_START ) ) + "\t" +
                 CString( MAKEINTRESOURCE( IDS_END ) ) + "\t" +
                 CString( MAKEINTRESOURCE( IDS_FILE ) ) );
    strings.Add( underscore );

    calc = GetFirstCalculation();
    while( calc )
    {
      CString temp1 = calc->GetName();
      CString fileTitle = calc->GetFileTitle();
      CString fileExt = calc->GetFileExt();
      CString temp2 = "prof\\" + fileTitle + '.' + fileExt;

      CString str;
      str.Format( "%s\t%.4f\t%.4f\t%s", temp1, calc->GetStartStation(), calc->GetEndStation(), temp2 );
      
      strings.Add( str );

      calc = GetNextCalculation();
    }
    strings.Add( "" );
    strings.Add( "" );
  }
  
  if( GetNumOutFlows() > 0 )
  {
    strings.Add( CString( MAKEINTRESOURCE( IDS_OUTFLOWS ) ) + ":" );
    strings.Add( CString( MAKEINTRESOURCE( IDS_NAME ) ) + "\t" +
                 CString( MAKEINTRESOURCE( IDS_WSPFIX ) ) + "\t\t" +
                 CString( MAKEINTRESOURCE( IDS_FILE ) ) );
    strings.Add( underscore );

    OutFlow* of = GetFirstOutFlow();
    while( of )
    {
      CString temp1 = of->GetName();

      CString temp2;
      if( of->WSPFIsDefined() )
			  temp2.LoadString( IDS_DEFINED );
      else
				temp2.LoadString( IDS_NOTDEFINED );
      
      CString fileTitle = GetFileTitle();
      CString temp3 = "prof\\" + fileTitle + ".qwt";
      CString str;
      str.Format( "%s\t%s\t\t%s", temp1, temp2, temp3 );
      
      strings.Add( str );
      
      of = GetNextOutFlow();
    }

    strings.Add( "" );
    strings.Add( "" );
  }
}
