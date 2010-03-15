/*! Time-stamp: <@(#)csection.cpp   29.08.02 - 10:54:03   Belger>
 *********************************************************************
 *  @file   : csection.cpp
 *
 *  Project : WSPWIN
 *
 *  Package : Projektmanager
 *
 *  Company : BCE
 *
 *  Author  : Belger                              Date: 29.08.02
 *
 *  Purpose : Implementation of methods for class CrossSectionArray
 *
 *********************************************************************
 */
#include "stdafx.h"

#include "dtypes.h"

#include "profil.h"
#include "project.h"
#include "datablck.h"
#include "coord.h"
#include "lSection.h"

#include "csection.h"

  ////////////////////////////
  //  Klasse  CrossSection
  ///////////////////////////

/* The Default Constructor */
CrossSection::CrossSection(Project* pProject) 
	: Section(CLASS_TYPE_CSECTION, pProject)
{
	m_dStation = 0;
	m_nVZK = 0;
	m_PK = _T("0");
}

CrossSection::~CrossSection()
{
}

CrossSection* CrossSection::Clone( Project* pProject, BOOL createNewFile )
// gibt eine Kopie dieser CrossSection zurück
// Parameter:
//          BOOL createNewFile: falls TRUE, wird ein neuer Dateiname erzeugt
// Rückgabewert:
//          Zeiger auf die Kopie
// Bemerkung:
{
  CrossSection* cs = new CrossSection( pProject );

  cs->SetFileName( m_fileTitle + "." + m_fileExt );

  // Profil clonen
  if ( !m_profil )
    LoadProfil();
  if ( m_profil )
    cs->SetProfil( m_profil->Clone( this ) );

  cs->SetProfilNr( m_nProfilNr ); // ist erst nach dem LoadProfil gesetzt
    
  POSITION pos = m_States.GetHeadPosition();
  while ( pos )
    cs->AddState( m_States.GetNext( pos ) );
    
  cs->SetStation( m_dStation );
  cs->SetVZK( m_nVZK );
  cs->SetPK( m_PK );
  cs->SetStateName( m_StateName );
  cs->SetWaterName( m_WaterName );

  // falls Datei kopiert werden soll, jetzt neuen Dateinamen erzeugen und CrossSection abspeichern
  if ( createNewFile )
  {
    cs->SetFileName( CString("") );
    cs->CreateFileName();
    cs->SaveProfil(); // das Profil muss gespeichert werden, das sonst beim nächsten CreateFileName der Dateiname nicht besetz ist
  };

  return cs;
}; // Clone

void CrossSection::SetWaterName( const CString& name )
{
	m_WaterName = name;
	SetStatesModified();

  Profil* profil = GetProfil();
  if( profil )
    profil->SetWaterName( name );
}; // SetWaterName

void CrossSection::SetStation( const double st )
{
	m_dStation = st;
	SetStatesModified();

  Profil* profil = GetProfil();
  if( profil )
    profil->SetStation( st );
}; // SetStation

void CrossSection::SetPK( const CString& pk )
{
	m_PK = pk;
	if (m_PK.Left(1) == "0")
		m_PK = "0";
	SetStatesModified();
}

void CrossSection::SetVZK( const int vzk )
{
	m_nVZK = vzk;
	SetStatesModified();
}

void CrossSection::SetStateName( const CString& name )
{
	m_StateName = name;
	SetStatesModified();

  Profil* profil = GetProfil();
  if( profil )
    profil->SetStateName( name );
}; // SetStateName

CString CrossSection::GetWaterName() const
{
	return m_WaterName;
}

double CrossSection::GetStation() const 
{
	return m_dStation;
}

CString CrossSection::GetPK() const
{
	return m_PK;
}

int CrossSection::GetPKNum() const
// gibt die PK als numerischen Wert zurück
// keine PK: Wert 0
// LL: Werte 100 - 199
// FF: Werte 200 - 299
// RR: Werte 300 - 399
{
  CString pk = GetPK();
  if( pk.GetLength() < 3 )
    return 0;

  CString numStr = pk.Mid( 2 );
  int num = atoi( numStr );
  if( num < 1 )
    return 0;

  switch( pk[0] )
  {
  case 'L':
    num += 100;
    break;

  case 'F':
    num += 200;
    break;

  case 'R':
    num += 300;
    break;

  default:
    num = 0;
  }; // switch pk[0]

  return num;
}; // GetPK

int CrossSection::GetVZK() const
{
	return m_nVZK;
}

CString CrossSection::GetStateName() const
{
	return m_StateName;
}

void CrossSection::CreateFileName( CStringList* forbiddenFiles /* = NULL */ )
// Erzeugt einen neue, unbelegten Dateinamen
// Parameter:
//        CStringList* forbiddenFiles: eine zusätzliche Liste von Dateinmamen, welche nicht belegt werden darf, kann NULL sein
// Bemerkung:
//        Funktioniert nur, falls noch kein Dateiname vergeben ist
{
	if( GetFileName().IsEmpty() )
	{
		int i = m_nProfilNr;

		CString filepath = m_pProject->GetDataDir();
		CString filename = m_WaterName.Left(2);
		CString str;
    str.Format("%06d", i);
		
    filename += str + ".prf";

		str = filepath + filename;
		
    CFileStatus rStatus;
    while( ( forbiddenFiles && forbiddenFiles->Find( filename ) ) || CFile::GetStatus( str, rStatus ) )
		{
			i++;
			filename = m_WaterName.Left( 2 );
			str.Format( "%06d", i );
			filename += str + ".prf";
			str = filepath + filename;
		}
		SetFileName( filename, TRUE );
	}; // if GetFileName == ""
}; // CreateFileName


/*!
 * Fügt einen Wasserspiegel ins Querprofil ein und trägt ggfls. auch den den Abfluss ein
 * Bemerkung:
 *        lädt das Profil
 *
 * @param const CString& wspKennung : 'Berechnungsvariante@Gewaesser@Zustand', wird ab Spalte 99 in die zweite Zeile eingetragen
 * @param double wspHohe : die Wasserspiegelhöhe
 * @param BOOL bDurchst : falls TRUE, wird auf die durchstr. Bereiche begrenzt, sonst WSP überall
 * @param const double abfluss : der einzutragende Abfluss, wird ein die zweite Zeile des DB geschrieben
 * @param const CString& strAbflussFormat : Mithilfe dieses Strings wird der Eintrag für den Abfluss formatiert. %1 ist der Platzhalter
 *
 * @return BOOL  : Erfolg der Operation
 */
BOOL CrossSection::InsertWsp( const CString& wspKennung, double wspHoehe, BOOL bDurchst, const double abfluss, const CString& strAbflussFormat )
{
  // zuerst das Profil laden
  if ( !LoadProfil() )
    return FALSE;
  
  // Geländehöhe muss vorhanden sein
  DataBlock* dbGel = m_profil->GetDataBlock( DST_GELAENDEHOEHE );
  if ( !dbGel )
    return FALSE;

  // suchen, ob dieser Wasserpiegel schon vorhanden ist, falls ja: ersetzen, sonst neuen Datenblock anlegen
  int index = 1;
  DataBlock* dbWsp = m_profil->GetDataBlock( DST_WSP_HOEHE, index++ );
  while ( dbWsp )
  {
    CString dbKennung = dbWsp->GetName( 1 );
    dbKennung = dbKennung.Mid( 99 );
    dbKennung.TrimRight();

    if ( dbKennung.CompareNoCase( wspKennung ) == 0 )
      break; // dieser Wasserpiegel ist schon da

    dbWsp = m_profil->GetDataBlock( DST_WSP_HOEHE, index++ );
  };

  if ( dbWsp ) // falls dieser Wsp schon da ist, löschen
  {
    m_profil->RemoveDataBlock( dbWsp );
    delete dbWsp;
    dbWsp = NULL;
  };

  // neuenDatenblock erzeugen
  dbWsp = new DataBlock( m_profil );
  dbWsp->SetType( DST_WSP_HOEHE );

  CString strAbfluss;
  if( strAbflussFormat.Find( TEXT("%1") )  != -1)
  {
    strAbfluss.Format( "%.1lf", abfluss );
    strAbfluss.FormatMessage( strAbflussFormat, strAbfluss );
  }
  else
    strAbfluss = strAbflussFormat;

  CString kennung;
  kennung.Format( "%-99.99s%s", strAbfluss, wspKennung );
  dbWsp->SetName( kennung, 1 );

  
  // jetzt die Wasserstände ausrechnen
  CTypedPtrArray<CPtrArray, Coord*> schnittpunkte; // die Schnittpunkte mit dem Gelände
  double maxH = -1e36;
  double minH = 1e36;  // minimale und maximale Geländehöhe
  double minY = 1e36;
  double maxY = -1e36;  // minimale und  maximale yKoordinate

  // zuerst alle Schnittpunkte der wagerechten mit Höhe wspHohe mit dem Gelände finden
  Coord* crd1 = dbGel->GetFirstCoord();
  minH = min( minH, crd1->dy );
  maxH = max( maxH, crd1->dy );
  minY = min( minY, crd1->dx );
  maxY = max( maxY, crd1->dx );
  Coord* crd2 = dbGel->GetNextCoord();
  while ( crd1 && crd2 )
  {
    // minimale und maximale Ausdehnungen ermitteln
    minH = min( minH, crd2->dy );
    maxH = max( maxH, crd2->dy );
    minY = min( minY, crd2->dx );
    maxY = max( maxY, crd2->dx );

    // erstmal schnelles Kriterium obs einen Schnitt gibt ( für schnelle Performance bei
    // vielen Profilpunkten )
    if ( ( crd1->dy <= wspHoehe && crd2->dy >= wspHoehe ) ||
         ( crd1->dy >= wspHoehe && crd2->dy <= wspHoehe ) )
    { // es gibt einen Schnittpunkt ( Zwischenwertsatz )
      // also können wir einfach mit der Geradegleichung ausrechnen: Gesucht x mit wspHoehe = a * x + b
      // es gilt: x = ( ( wspHoehe - y1 ) / a ) + x1
      // ( a, b sind die Parameter der Geraden durch crd1 und crd2 )
      double deltay = crd2->dy - crd1->dy;
      double deltax = crd2->dx - crd1->dx;

      if ( deltax != 0.0 )
      {
        double a = deltay / deltax; // a = ( y2- y1 ) / ( x2 - x1 )

        double x = ( ( wspHoehe - crd1->dy ) / a ) + crd1->dx;

        if ( crd2->dx < crd1->dx )
          a *= -1; // bei rücksprung ist's umgekehrt

        Coord* newCrd = new Coord( x, a );  // in a wird die Steigung zwischengespeichert, um zu wissen, ob's ein ein oder ein Austritt ist
        schnittpunkte.Add( newCrd );
      } // if deltax != 0.0
      else // senkrechte Wand
        schnittpunkte.Add( new Coord( crd1->dx, deltay ) ); // deltay hat das richtige Vorzeichen

    }; // falls schnittpunkt existiert

    crd1 = crd2;
    crd2 = dbGel->GetNextCoord();
  }; // while crd2

  // die äussersten Coordinaten auf jeden Fall als Schnittpunkte hinzufügen, falls sie unter Wasser liegen
  crd1 = dbGel->GetFirstCoord();
  crd2 = dbGel->GetLastCoord();
  if ( crd1->dy <= wspHoehe )
    schnittpunkte.InsertAt( 0, new Coord( crd1->dx, -1.0 ) );
  if ( crd2->dy <= wspHoehe )
    schnittpunkte.Add( new Coord( crd2->dx, +1.0 ) );

  // den erlaubten Bereich feststellen, falls gewünscht, falls durchst. Bereiche vorhanden sind, müssen die wsp's
  // innerhalb derselben liegen
  double yMinimal = minY;
  double yMaximal = maxY;  // im Zweifeslfall ist der ganze Bereich erlaubt

  if( bDurchst )
  {
    DataBlock* dbDurchst = m_profil->GetDataBlock( DST_DURCHST_BEREICH );
    if ( dbDurchst )
    {
      Coord* crdDurchst1 = dbDurchst->GetFirstCoord();
      Coord* crdDurchst2 = dbDurchst->GetLastCoord();
      
      if ( crdDurchst1 && crdDurchst2 && crdDurchst1 != crdDurchst2 )
      {
        yMinimal = crdDurchst1->dx;
        yMaximal = crdDurchst2->dx;
        
        // die Coordinaten auch als Schnittpunkte merken
        // die Reihenfolge ist kein Problem, weil die äußersten Geländecoordinaten
        // im nächsten Schritt eh gleich wieder rausfliegen
        if ( crdDurchst1->dy <= wspHoehe )
          schnittpunkte.InsertAt( 1, new Coord( yMinimal, -1.0  ) );
        if ( crdDurchst2->dy <= wspHoehe )
          schnittpunkte.Add( new Coord( yMaximal, +1.0 ) );
      };
    }; // if dbDurchst
  }; // if bDurchst

  // Schnittpunkte sind gefunden, jetzt die ausserhalb des Erlaubten Bereichs liegenden abfiltrieren
  if ( schnittpunkte.GetSize() > 1 )
  {
    for ( int i = 0; i < schnittpunkte.GetSize(); i++ )
    {
      crd1 = schnittpunkte[i];

      if ( crd1->dx < yMinimal || crd1->dx > yMaximal )
      { // Koordinate löschen, weil ausserhalb des erlaubten Bereichs
        delete crd1;
        crd1 = NULL;
        schnittpunkte.RemoveAt( i );
        i--;
        continue;
      };
    }; // for i
  }; // if schnittpunkte


  // jetzt Schnittpunkte paarweise dem Wasserpiegel hinzufügen
  // noch prüfen, ob zwei vieleicht auf dem gleichen Punkt liegen
  for ( int i = 0; i < schnittpunkte.GetSize() - 1; i++ )
  {
    Coord* wspCrd1 = schnittpunkte[i];
    Coord* wspCrd2 = schnittpunkte[i + 1];

    if( wspCrd1->dy <= 0 && wspCrd2->dy >= 0 && fabs( wspCrd1->dx - wspCrd2->dx ) > 0.00001 )
    {
      wspCrd1->dy = wspCrd2->dy = wspHoehe;
      dbWsp->AddCoord( wspCrd1 );
      dbWsp->AddCoord( wspCrd2 );
      schnittpunkte[i] = NULL;
      schnittpunkte[i + 1] = NULL;
      i++;
    }
    else
    {
      delete wspCrd1;
      schnittpunkte[i] = NULL;
      wspCrd1 = NULL;
    }; // 

  }; // for i

  // restliche schnittpunkte löschen
  for ( i = 0; i < schnittpunkte.GetSize(); i++ )
  {
    if ( schnittpunkte[i] )
      delete schnittpunkte[i];
  };
  schnittpunkte.RemoveAll();


  if ( dbWsp->GetNumCoords() > 1 )
    m_profil->AddDataBlock( dbWsp );
  else
  {
    delete dbWsp;
    return FALSE;
  };

  return TRUE;
}; // Insert Wsp

BOOL CrossSection::RemoveWsp( const CString& wspKennung )
// löscht einen Wasserspeiegel aus dem Querprofil
// Parameter:
//        const CString& wspKennung: Kennung des zu löschenden Wsp
// Rückgabewert:
//        TRUE / FALSE je nach Erfolg der Operation
{
  // zuerst das Profil laden
  if ( !LoadProfil() )
    return FALSE;

  // suchen, ob der gesuchte Wasserpiegel schon vorhanden ist
  int index = 1;
  DataBlock* dbWsp = m_profil->GetDataBlock( DST_WSP_HOEHE, index++ );
  while ( dbWsp )
  {
    CString dbKennung = dbWsp->GetName( 1 );
    dbKennung = dbKennung.Mid( 99 );
    dbKennung.TrimRight();

    if ( dbKennung.CompareNoCase( wspKennung ) == 0 )
    { // den Wasserpiegel löschen
      m_profil->RemoveDataBlock( dbWsp );
      delete dbWsp;
      return TRUE;
    };

    dbWsp = m_profil->GetDataBlock( DST_WSP_HOEHE, index++ );
  }; // while dbWsp

  return FALSE;
}; // RemoveWsp

////////////////
// Operatoren //
////////////////

BOOL CrossSection::operator==( const CrossSection& otherCs ) const
// zwei Querprofile werden als gleich angesehen, falls ihre Stationen und Verzweigungskennungen übereinstimmen
{
  return ( GetVZK() == otherCs.GetVZK() && fabs( GetStation() - otherCs.GetStation() ) < 0.00001 );
}; // operator==

BOOL CrossSection::operator<( const CrossSection& otherCs ) const
// Ordnung der Strangtabelle
// Sortiert wird nach PK, VZK und Station
// 
{
  double station = GetStation();
  double otherStation = otherCs.GetStation();

  int vzk = GetVZK();
  int otherVzk = otherCs.GetVZK();

  int pk = GetPKNum();
  int otherPk = otherCs.GetPKNum();

  // Fall 1: gleiche Station, gleiche VZK -> dann ist die PK ausschlaggebend
  if( fabs( station - otherStation ) < 0.00001 && vzk == otherVzk )
    return pk < otherPk;

  // Fall 2: VZK gleich oder eine von beiden 0: nur die Station entscheidet
  if( vzk == otherVzk || vzk == 0 || otherVzk == 0 )
    return station < otherStation;

  // Fall 3: VZKs unterschiedlich und beide ungleich 0: nur die VZK entscheidet
  return vzk > otherVzk;
}; // operator<


/////////////////////////////////////////////////
// Implementation der Klasse CrossSectionArray //
/////////////////////////////////////////////////

BOOL CrossSectionArray::ChangeDataBlocks( CTypedPtrMap<CMapWordToPtr, WORD, DataBlockArray*>& dataBlocks, CString& errorString )
// Ersetzt in diesen Querprofilen alle DatenBlöcke eines bestimmten Typs
// durch DatenBlöcke des gleichen Typs
// Die DatenBlöcke werden dabei auch noch inhaltlich überprüft
// Parameter:
//        CTypedPtrMap<CMapWordToPtr, WORD, DataBlockArray*>& dataBlocks: für jeden zu ersetzenden Typ ein DataBlockArray.
//                                                  Die Indizes jedes DataBlockArray stimmen mit den Indizes dieses Arrays überein
// Rückgabewert:
//        BOOL: falls FALSE gabs einen schweren Fehler oder Benutzer hat abgebrochen
//              auf jeden Fall sollten die Profile dann nicht gespeichert werden.
// Bemerkung:
//        diejenigen DatenBlöcke, die benutzt wurden ( in profile eingefügt ),
//        werden in den DataBlockArrays auf NULL gesetzt
//        Alle anderen werden nicht angetastet, d.h. sie müssen unter Umständen von
//        der aufrufenden Funktion zerstört werden
{
  // zuerst einmal die Map kopieren
  // das ganze ist nötig, da es sein kann, dass mehrere DatenBlockTypen auf einmal
  // bearbeitet werden
  CTypedPtrMap<CMapWordToPtr, WORD, DataBlockArray*> dbMap;

  // warum ham die MFCler bloß keinen Kopierkonstruktor für ihre Collections gemacht???
  POSITION pos = dataBlocks.GetStartPosition();
  while( pos )
  {
    WORD type;
    DataBlockArray* dbArray;
    dataBlocks.GetNextAssoc( pos, type, dbArray );

    dbMap.SetAt( type, dbArray );
  } // while pos

  // so jetzt solange DatenBlock Typen hinzufügen, bis keine mehr da sind
  pos = dbMap.GetStartPosition();
  while( pos )
  {
    WORD dbType;
    DataBlockArray* dummyArray = NULL;
    dbMap.GetNextAssoc( pos, dbType, dummyArray );

    // abhängig vom Typ passieren verschiedene Dinge
    CUIntArray newTypes; // die Typen der DatenBlöcke die ins Profil eingefügt werden
    CUIntArray oldTypes; // die Typen der DatenBlöcke, die aus dem profil entfernt werden
    CUIntArray finishedTypes; // die Typen, die in diesem Schleifenablauf abgearbeitet wurden

    BOOL bExcludes = FALSE; // ob die aufgeführten Typen sich gegenseitig ausschliessen oder ob sie gemeinsam behandelt werden müssen

    switch( dbType )
    {
        case DST_GELAENDEHOEHE:
        case DST_RECHTSWERT:
        case DST_HOCHWERT:
        case DST_TRENNFLAECHEN:
        case DST_DURCHST_BEREICH:
        case DST_BORDVOLL:
        case DST_MODELLGRENZEN:
          // bei diesen Typen alles einmal
          bExcludes = FALSE;

          newTypes.Add( dbType );
          oldTypes.Add( dbType );
          finishedTypes.Add( dbType );
          break;

        case DST_AXM:
        case DST_AYM:
        case DST_DPM:
          // hier werden alle drei auf einmal ersetzt oder gar nicht
          bExcludes = FALSE;

          finishedTypes.Add( DST_AXM );
          finishedTypes.Add( DST_AYM );
          finishedTypes.Add( DST_DPM );

          newTypes.Add( DST_AXM );
          newTypes.Add( DST_AYM );
          newTypes.Add( DST_DPM );
          
          oldTypes.Add( DST_AXM );
          oldTypes.Add( DST_AYM );
          oldTypes.Add( DST_DPM );
          break;

        case DST_RAUHIGKEIT:
        case DST_RAUHIGKEIT_KST:
          // diese beiden schliessen sich gegenseitig aus
          bExcludes = TRUE;

          finishedTypes.Add( DST_RAUHIGKEIT );
          finishedTypes.Add( DST_RAUHIGKEIT_KST );

          newTypes.Add( DST_RAUHIGKEIT );
          newTypes.Add( DST_RAUHIGKEIT_KST );

          oldTypes.Add( DST_RAUHIGKEIT );
          oldTypes.Add( DST_RAUHIGKEIT_KST );
          break;

        case DST_WSP_HOEHE:
          bExcludes = FALSE;
          newTypes.Add( dbType );
          oldTypes.Add( dbType );
          finishedTypes.Add( dbType );
          break;
    } // switch dbType

    // für die neuen Typen die entsprechenden DatenBlockArrays sammeln
    CTypedPtrArray<CPtrArray, DataBlockArray*> newDBArrays;
    for( int i = 0; i < newTypes.GetSize(); i++ )
    {
      int type = newTypes[i];
      DataBlockArray* dbArray;
      dbMap.Lookup( type, dbArray );
      ASSERT( dbArray != NULL );
      
      dbArray->SetSize( GetSize() ); // wir brauchen nur genau diese, und auch mindestens soviele

      newDBArrays.Add( dbArray );
    } // for i

    // alle Profile durchgehen
    BOOL bLoeschenAnswer = IDCANCEL; // Antwort der letzten Frage nach löschen etc.; IDYES, damit beim ersten mal auf jeden Fall gefragt wird
    BOOL bErsetzenAnswer = IDCANCEL; // dito fürs ersetzen
    BOOL bWelcherAnswer = IDCANCEL;  // dito für Nachfrage nach welchem DB
    CString welcherTyp  = DataBlock( NULL, DST_UNKNOWN ).GetDesc( 0 );

    for( i = 0; i < GetSize(); i++ )
    {
      // das eigentliche Profil holen
      CrossSection* cs = GetAt( i );
      if( !cs )
        continue;

      Profil* profil = cs->GetProfil();
      ASSERT( profil );

      // jetzt mal schauen, welche Daten wir in diesem Profil haben und
      // welche neu hinzugefügt werden sollen

      // jetzt die hinzuzufügenden Datenblöcke finden und überprüfen
      DataBlockArray newDBs;
      BOOL bCancelCS = FALSE; // falls Fehler in den neuen Daten entdeckt werden
                              // wird diese Flag gesetzt und das Profil übersprungen
      for( int d = 0; d < newTypes.GetSize(); d++ )
      {
        DataBlockArray* dbArray = newDBArrays[d];
        DataBlock* db = dbArray->GetAt( i );

        if( db == NULL )
          continue;

        CString error;
        if( !db->CheckData( error ) )
        {
          CString message;
          message.Format( IDS_CHECK_DATA_MESSAGE, cs->GetStation(), db->GetDesc( 0 ), error );
          errorString += message + "\n";

          bCancelCS = TRUE;
          continue; // wenn echte Fehler in den Daten sind, nichts tun
        } // if !db->CheckData

        newDBs.Add( db );
      } // for d
      
      if( bCancelCS == TRUE )
        continue;

      // ebenso für die alten DatenBlöcke
      DataBlockArray oldDBs;
      for( d = 0; d < oldTypes.GetSize(); d++ )
      {
        int type = oldTypes[d];
        DataBlock* db = profil->GetDataBlock( type );
        
        if( db != NULL )
          oldDBs.Add( db );
      } // for d

      // jetzt noch gegenseitige Abhängigkeiten testen
      
      // schwierigster Fall, gegenseitiges ausschliessen
      if( bExcludes )
      {
        // den (maximal) einzigen alten DatenBlock rausfinden
        DataBlock* oldDB = NULL;
        CString oldName;
        if( oldDBs.GetSize() > 0 ) // wenn mehr als ein DatenBlock sich auschliessender
          // Datenblöcke im Profil ist die restlichen löschen
        {
          oldDB = oldDBs[0];
          oldName = oldDB->GetDesc( 0 );
        }

        switch( newDBs.GetSize() )
        {
        case 2:
          if( bWelcherAnswer != IDALWAYS || bWelcherAnswer != IDNEVER )
          {
            CString title( MAKEINTRESOURCE(IDS_SAVE_DATA) );
            CString name0 = newDBs[0]->GetDesc( 0 );
            CString name1 = newDBs[1]->GetDesc( 0 );
            
            CString text;
            text.Format( IDS_SAVE_DATA_WELCHER_Q, cs->GetStation(), cs->GetVZK(), name0, name1 );
            text += "\n";
            if( oldDB == NULL )
              text += CString(MAKEINTRESOURCE(IDS_SAVE_DATA_NO_OLD_DATA));
            else
            {
              CString oldNameString;
              oldNameString.Format( IDS_SAVE_DATA_OLD_NAME, oldName );
              text += oldNameString;
            }

            text += "\n" + CString(MAKEINTRESOURCE(IDS_SAVE_DATA_CHOOSE));
              
            CMessageBoxWelcher dlg( title, text, name0, name1, NULL );
            bWelcherAnswer = dlg.DoModal();
            welcherTyp = dlg.GetWelcher();
          }; // if bWelcheAnswer
          
          switch( bWelcherAnswer )
          {
          case IDYES:
          case IDALWAYS:
            // den entsprechenden ersetzen
            if( welcherTyp.CompareNoCase( newDBs[0]->GetDesc( 0 ) ) == 0 )
              newDBs[1] = NULL;
            else
              newDBs[0] = NULL;
            break;
            
          case IDNO:
          case IDNEVER:
            continue; // nix ändern
            
          case IDCANCEL:
            return FALSE; // komplett abbrechen
          } // switch bWelcheAnswer
          break; // case 2
          
          case 1:
            // den neuen DatenBlock finden
            {
              DataBlock* newDB = newDBs[0];
              CString newName = newDB->GetDesc( 0 );
              
              // bei Auschluss überprüfen, ob der alte gleich dem neuen ist
              // falls ja, einfach ersetzen, falls nein nochmal nachfragen, ob ersetzt werden soll
              if( oldDB != NULL && oldDB->GetType() != newDB->GetType() )
              {
                if( bErsetzenAnswer != IDALWAYS && bErsetzenAnswer != IDNEVER )
                {
                  CString msgText;
                  msgText.Format( IDS_SAVE_DATA_CHANGE_QUESTION, cs->GetStation(), cs->GetVZK(), newName, oldName );
                  CMessageBox5 dlg( CString(MAKEINTRESOURCE(IDS_SAVE_DATA)), msgText, NULL );
                  bErsetzenAnswer = dlg.DoModal();
                }
                
                switch( bErsetzenAnswer )
                {
                case IDYES:
                case IDALWAYS:
                  // dann ist alles ok, oldDB wird durch newDB ersetzt
                  break;

                case IDNO:
                case IDNEVER:
                  // nichts tun
                  newDBs.RemoveAll();
                  oldDBs.RemoveAll();
                  break;

                default: // = IDCANCEL
                  continue;
                } // switch bErsetzenAnswer
              } // if oldDB
            } // case 1
            break;

          case 0:
            // falls keine Daten mehr da sind, aber vorher welche da sind, 
            // fragen ob gelöscht werden soll
            if( oldDB != NULL )
            {
              CString oldName = oldDB->GetDesc( 0 );
              
              // nur Fragen, wenn keine der "Immer" antworten war
              if( bLoeschenAnswer != IDALWAYS || bLoeschenAnswer == IDNEVER )
              {
                CString msgText;
                msgText.Format( IDS_SAVE_DATA_DELETE_QUESTION, cs->GetStation(), cs->GetVZK(), oldName );
                CMessageBox5 dlg( CString(MAKEINTRESOURCE(IDS_SAVE_DATA)), msgText, NULL );
                bLoeschenAnswer = dlg.DoModal();
              } // if bAnswer = IDYES || IDNO
              
              switch( bLoeschenAnswer )
              {
              case IDYES:
              case IDALWAYS:
                // nix tun, alle alten werden gelöscht
                break;
                
              case IDNO:
              case IDNEVER:
                // den alten DBs aus dem Array nehmen, damit sie nicht gelöscht weren
                oldDBs.RemoveAll();
                break;
                
              default: // IDCANCEL
                return FALSE;
              } // switch bLoeschenAnswer
            }; // if oldDB != NULL
            break; // case 0
            
            default: // mehr als zwei sich ausschliessende wird zur Zeit nicht unterstüzt
              break;
        } // switch newDBs.GetSize
      } // if bExcludes
      else
      {
        // Datenblöcke schliessen sich nicht aus

        // es müssen genauso viele neue DatenBlöcke da sein wie gefordert
        if( newDBs.GetSize() < newTypes.GetSize() )
          newDBs.RemoveAll(); // ansonsten sind alle neuen nicht gültig


        // wenn mehr alte Datenblöcke als neue da sind, fragen ob gelöscht werden darf
        // das kann nur sein wenn oldDBs.Size == newTypes.Size und newDBS.Size == 0
        if( oldDBs.GetSize() > newDBs.GetSize() )
        {
          // nur Fragen, wenn keine der "Immer" antworten war
          if( bLoeschenAnswer != IDALWAYS && bLoeschenAnswer != IDNEVER )
          {
            CString oldNames;
            for( int d = 0; d < oldDBs.GetSize(); d++ )
            {
              oldNames += oldDBs[d]->GetDesc( 0 );
              if( d < oldDBs.GetSize() - 1 )
                oldNames += ", ";
            } // for d

            CString msgText;
            msgText.Format( IDS_SAVE_DATA_DELETE_QUESTION, cs->GetStation(), cs->GetVZK(), oldNames );
            CMessageBox5 dlg( CString(MAKEINTRESOURCE(IDS_SAVE_DATA)), msgText, NULL );
            bLoeschenAnswer = dlg.DoModal();
          } // if bAnswer = IDYES || IDNO

          switch( bLoeschenAnswer )
          {
          case IDYES:
          case IDALWAYS:
            // nix tun, alle alten werden gelöscht
            break;
            
          case IDNO:
          case IDNEVER:
            // den alten DBs aus dem Array nehmen, damit sie nicht gelöscht weren
            oldDBs.RemoveAll();
            break;
            
          default: // IDCANCEL
            return FALSE;
          } // switch bLoeschenAnswer
        } // if oldDBs > newDBs
      } // if bExcludes
      
      // jetzt alle alten löschen und alle neuen zum Profil hinzufügen
      for( d = 0; d < oldDBs.GetSize(); d++ )
      {
        // extrawurst für Wasserspiegel: nur löschen, wenn die Kennung die gleiche ist
        DataBlock* oldDB = oldDBs[d];
        BOOL bFound = TRUE; // bleibt TRUE, wenn es einen neuen Datenblock gibt mit dem gleichen Namen wie der Alte

        if( oldDB->GetType() == DST_WSP_HOEHE )
        {
          CString oldName = oldDB->GetWSPName();
          bFound = FALSE;
        
          for( int nd = 0; nd < newDBs.GetSize(); nd++ )
          {
            DataBlock* newDB = newDBs[nd];
            if( newDB && oldName.CompareNoCase( newDB->GetWSPName() ) == 0 )
              bFound = TRUE;
          }; // for nd
        };
          
        // nur falls ein neuer den gleichen Namen will, wie ein Alter
        if( bFound == TRUE )
        {
          profil->RemoveDataBlock( oldDB );
          delete oldDB;
          oldDBs[d] = NULL;
        };
      } // for d
      oldDBs.RemoveAll(); //  damit später nicht mehr daruaf zugegriffen wird
      
      for( d = 0; d < newDBs.GetSize(); d++ )
      {
        profil->AddDataBlock( newDBs[d] );

        // die zugefügten aus den dbArrays rausnehmen
        for( int e = 0; e < newDBArrays.GetSize(); e++ )
        {
          DataBlockArray* dbArray = newDBArrays[e];
          if( dbArray && dbArray->GetAt( i ) == newDBs[d] )
            dbArray->SetAt( i, NULL );
        } // for e
      } // for d
      newDBs.RemoveAll(); // sicherheitshalber nicht mehr drauf zugreifen
    } // for i : jedes Profil

    // die behandelten Datenblocktypen löschen
    for( int d = 0; d < finishedTypes.GetSize(); d++ )
      dbMap.RemoveKey( finishedTypes[d] );
    
    pos = dbMap.GetStartPosition();
  } // while pos

  return TRUE; // alles OK ( hoffentlich zumindest )
} // ChangeDataBlocks


/*!
 * Löscht alle Datenblöcke gegebener Typen aus allen Querprofilen dieser List
 *
 * @param const CArray<int, int>& dbTypes : die zu löschenden Datenblocktypen
 *
 * @return UINT : Summe der gelöschten Datenblöcke
 */
UINT CrossSectionArray::DeleteDatablocks( const CArray<int, int>& dbTypes )
{
  UINT count = 0; // Summe der gelöschten Datenblöcke

  for( int i = 0; i < GetSize(); i++ )
  {
    CrossSection* cs = GetAt( i );
    if( cs != NULL )
    {
      if( cs->LoadProfil() == TRUE )
      {
        Profil* profil = cs->GetProfil();
        if( profil != NULL )
          count += profil->DeleteDataBlocks( dbTypes );
      }; // if cs->LoadProfil
    }; // if cs
  }; // for i

  return count;
} // DeleteDataBlocks

/*!
 * Erzeugt einen Laengsschnitt aus diesen CrossSections
 *
 * @return LengthSection*  : Gibt eine LengthSection zur³ck.
 */
LengthSection* CrossSectionArray::CreateLSection() const
{
  const double stationFaktor = -1000.0;

  // neue L-Section erzeugen
  LengthSection* ls = new LengthSection( NULL );
  Profil* lsProf = new Profil();
  ls->SetProfil( lsProf );
  
  //Profil-Attribute setzen
 	lsProf->SetProjectDesc( 1, CString("SPIEGELLINIEN-LAENGSSCHNITT") );
  lsProf->SetDate( COleDateTime::GetCurrentTime() );
  
  //Vector f³r die unterschiedlichen Datenbl÷cke
  CMap<int, int, DataBlock*, DataBlock*> dbMap; // Zuordnung Typ -> neuer DatenBlock
  CTypedPtrMap<CMapStringToPtr, CString, DataBlock*> wspDbMap; // Zuordnung Wsp-Kennung -> neuer DatenBlock

  int stationCount = 0; // wird für AddTextBlock gebraucht

  // Rückwärts durch die Crosssections
  for( int n = GetSize() - 1; n >= 0; n-- )
  {
    CrossSection* cs = GetAt( n );
    cs->LoadProfil();

    double station = stationFaktor * cs->GetStation();
    Profil* profil = cs->GetProfil();
    if( profil )
    {
      DataBlock* db = profil->GetFirstDataBlock();
      
      while( db )
      {
        CArray<int, int> typeArray;  // die Typen wo die Koordinaten hin sollen
        CArray<Coord*, Coord*> crdArray; // die neuen koordinaten jeweils

        int type = db->GetType();
        switch( type )
        {
        case DST_WSP_HOEHE:
          {
            // Sonderfall, arbeitet mit eigener HashTable
            CString key = db->GetWSPName();
            DataBlock* laengsDB = NULL;
            if( wspDbMap.Lookup( key, laengsDB ) == FALSE )
            {
              laengsDB = new DataBlock( lsProf, DST_WASSERSPIEGEL );
              laengsDB->SetWSPName( key );
              wspDbMap.SetAt( key, laengsDB );
            };
            
            Coord* crd = db->GetFirstCoord();
            if( crd != NULL )
            {
              Coord* newCrd = new Coord( *crd );
              newCrd->dx = stationFaktor * cs->GetStation();
              laengsDB->AddCoord( newCrd );
            } // if crd
          }//case DST_WSP_HOEHE
          break;
          
        case DST_GELAENDEHOEHE:
          {
            Coord* crd = profil->GetSohlHoehe( cs, false ); 
            // ohne Berücksichtigung der durchströmten Bereiche , ansonsten true !
            if( crd )
            {
              Coord* newCrd = new Coord( *crd );
              newCrd->dx = station;

              typeArray.Add( DST_SOHLHOEHE );
              crdArray.Add( newCrd );

              stationCount++;
            } // if crd

            // nicht nur die Sohlhöhe ausrechnen, sondern auch die Modellgrenzen
            // d.h. jeweils der erste Geländepunkt gibt ...
            //      jeweils der letzte Gelädepunkt gibt ...
            Coord* fstCrd = new Coord( db->GetFirstCoord() );
            Coord* lstCrd = new Coord( db->GetLastCoord() );

            fstCrd->dx = station;
            lstCrd->dx = station;

            typeArray.Add( DST_BOESCHUNG_LINKS_2 );
            crdArray.Add( fstCrd );

            typeArray.Add( DST_BOESCHUNG_RECHTS_2 );
            crdArray.Add( lstCrd );
          }//case DST_GELAENDEHOEHE
          break;
          
          //DKUK
        case DST_UK_BRUECKE:
          {
            Coord* crd = db->GetCoordAt( 0 );
            if( crd )
            {
              Coord* newCrd = new Coord( *crd );
              newCrd->dx = station;

              typeArray.Add( DST_DKUK );
              crdArray.Add( newCrd );
            } //if crd
          } // case DST_UK
          break;
          
          //DKOK
          
        case DST_OK_BRUECKE:
          {
            Coord* crd = db->GetCoordAt( 0 );
            if( crd != NULL )
            {
              Coord* newCrd = new Coord( *crd );
              newCrd->dx = station;

              typeArray.Add( DST_DKOK );
              crdArray.Add( newCrd );
            } // if crd
          }// case DST_OK
          break;
          //Bordvoll
          
        case DST_BORDVOLL:
          {
            for( int crdID = 0; crdID < 2; crdID++ )
            {
              Coord* crd = db->GetCoordAt( crdID );
              if( crd )
              {
                Coord* newCrd = new Coord( *crd );
                newCrd->dx = station;
                
                Coord* yCrd = profil->GetDataBlock( DST_GELAENDEHOEHE )->GetCoord( crd->dx, 0.0001 );
                if( yCrd )
                  newCrd->dy = yCrd->dy;
                
                if( crdID == 0 )
                  typeArray.Add( DST_BOESCHUNG_LINKS );
                else
                  typeArray.Add( DST_BOESCHUNG_RECHTS );
                crdArray.Add( newCrd );
              } //if crd1
            } // for crdID
          } //case DST_BV
          break;
          
        case DST_COMMENT:
          {
            int commentAnz = db->GetCommentCount();//Anzahl der Linien in einem Kommentar
            CStringArray strings;
            for( int z = 0; z < commentAnz; z++ )
              strings.Add( db->GetCommentLine( z ) );
            
            // Spezialfall, selbst den DatenBlock ausfindig machen, erzeugen
            DataBlock* dbComment = NULL;
            if( !dbMap.Lookup( DST_LP_TEXT, dbComment ) )
            {
              dbComment = new DataBlock( lsProf, DST_LP_TEXT );
              dbMap.SetAt( DST_LP_TEXT, dbComment );
            };

            dbComment->AddTextBlock( 1, stationCount, strings );
          }//case DST_COMMENT
          break;
        }//switch

        ASSERT( typeArray.GetSize() == crdArray.GetSize() );
        for( int i = 0; i < typeArray.GetSize(); i++ )
        {
          int typ = typeArray[i];

          DataBlock* lsDB = NULL;
          if( !dbMap.Lookup( typ, lsDB ) )
          {
            lsDB = new DataBlock( lsProf, typ );
            dbMap.SetAt( typ, lsDB );
          };

          lsDB->AddCoord( crdArray[i] );
        }; // for i
        
        db = profil->GetNextDataBlock();
      }// while db
    }// if profil
  }//for n

  
  //Hinzufügen der neuen Datenblöcke
  for( POSITION dbPos = dbMap.GetStartPosition(); dbPos; )
  {
    DataBlock* db;
    int typ;
    dbMap.GetNextAssoc( dbPos, typ, db );
    lsProf->AddDataBlock( db );
  } // for pos

  // wspDbMap auslesen
  for( POSITION wspPos = wspDbMap.GetStartPosition(); wspPos; )
  {
    DataBlock* db = NULL;
    CString key;
    wspDbMap.GetNextAssoc( wspPos, key, db );
    
    lsProf->AddDataBlock( db );
  } // for pos
  
  return ls;
}  // CreateLSection



/*!
 * Spiegelt alle Profile am Nullpunkt
 *
 */
void CrossSectionArray::FlipHorizontal( const bool bSave )
{
  for( int i = 0; i < GetSize(); i++ )
  {
    CrossSection* cs = GetAt( i );
    if( !cs->LoadProfil() )
      continue;

    Profil* profil = cs->GetProfil();
    if( !profil )
      continue;

    profil->FlipHorizontal();

    cs->SaveProfil();
  }
}


void CrossSection::CreatePrintSummary( CStringArray& strings )
{
  CString temp2 = "prof\\" + GetFileTitle() + '.' + GetFileExt();

  CString str;
  str.Format( "%.4f\t%s\t%d\t%s", GetStation(), GetPK(), GetVZK(), temp2 );
  
  strings.Add( str );
}
