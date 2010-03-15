#include "stdafx.h"

#include "profil.h"
#include "calc.h"
#include "calcData.h"

#include "lsection.h"

  ////////////////////////////
  //  Klasse  LengthSection
  ///////////////////////////

/* The Default Constructor */
LengthSection::LengthSection(Project* pProject)
	: Section(CLASS_TYPE_LSECTION, pProject)
{
}

LengthSection::~LengthSection()
{
  FlushProfil();
}

LengthSection* LengthSection::Clone( Project* pProject )
// gibt eine Kopie des Längsschnitts zurück
{
  LengthSection* ls = new LengthSection( pProject );

  ls->SetFileName( m_fileTitle + "." + m_fileExt );
  ls->SetProfilNr( m_nProfilNr );
  if ( !m_profil ) //m_profil: geerbt von section (Klasse Profil), wenn keins vrhanden, dann ein neues laden
    LoadProfil(); //Wo kommts her ??????
  if ( m_profil )	//wenn vorhanden, dann setze als erstes Profil das kopierte Profil von m_profil
    ls->SetProfil( m_profil->Clone( this ) );
  
  POSITION pos = m_States.GetHeadPosition();
  while ( pos )
    ls->AddState( m_States.GetNext( pos ) );

  ls->SetEndStation( m_dEndStation );		//setzt Anfangsstation
  ls->SetStartStation( m_dStartStation );	//setzt Endstation
  ls->SetName( m_name );					//Namen setzen
  ls->SetVZK( m_vzk );						//setzt VZK

  return ls;
}; // Clone

void LengthSection::CreateFileName( Calculation* calc )
// erzeuge einen zur Berechnungsvariante passenden Namen
// Parameter:
//        Calculation* calc: die zu diesem Längschnitt gehörende Berechnungsvariante
// Bemerkung:
//        Es wird sowohl der Dateiname ( m_fileExt, m_fileTitle, als auch m_vzk geädert
//          Aus irgendeinem Grund enthält m_vzk den Dateinamen...
//        Diese Funktion arbeitet nur, wenn der Dateiname vorher leer ist
{
  ASSERT( calc );

  CString fileName = calc->GetFileName();
  ASSERT( fileName.GetLength() > 2 );

  if ( calc->GetCalcData()->GetLWA() )
    fileName.SetAt( 2, 'p' );
  else
    fileName.SetAt( 2, 'w' );

  fileName.SetAt( 3, 'l' );

  if ( GetFileName().IsEmpty() )
  {
    SetFileName( fileName );
    SetVZK( fileName );
  };
}; // CreateFileName

void LengthSection::SetVZK(CString& vzk)
{
	m_vzk = vzk;
	SetStatesModified();		//Ist das der Modifier-FLAG ?????
}

void LengthSection::SetName(CString& name)
{
	m_name = name;
	SetStatesModified();
}

void LengthSection::SetStartStation(double value)
{
	m_dStartStation = value;
	SetStatesModified();
}

void LengthSection::SetEndStation(double value)
{
	m_dEndStation = value;
	SetStatesModified();
}

CString LengthSection::GetVZK()
{
  return m_vzk;
}

CString LengthSection::GetName()
{
	return m_name;
}

double LengthSection::GetStation()
{
	return m_dStartStation;
}

double LengthSection::GetStartStation()
{
	return m_dStartStation;
}

double LengthSection::GetEndStation()
{
	return m_dEndStation;
}
