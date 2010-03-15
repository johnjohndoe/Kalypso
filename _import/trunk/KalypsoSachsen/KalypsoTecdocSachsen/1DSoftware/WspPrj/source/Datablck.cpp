#include "stdafx.h"

#include "bce\include\linearequation.h"

#include "coord.h"
#include "profil.h"
#include "textBlock.h"

#include "datablck.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


#define N_NAMESETS		4

  ////////////////////////////
  //  Klasse  DataBlock
  ///////////////////////////

/* The Default Constructor */
DataBlock::DataBlock( Profil* pProfil /* = NULL */, int type /* = DST_UNKNOWN */ )
{
	m_pProfil = pProfil;
	m_nCoord = 0;
	for( int i = 0; i < 9; i++ )
    m_nControlData[i] = 0;
  
  m_nCoordIndex = 0;

  m_textBlock = 0;

  SetType( type );
}; // Standardkonstruktor

DataBlock::~DataBlock()
{
	for( int i = 0; i < m_Coords.GetSize(); i++ )
	{
		Coord* crd = m_Coords.GetAt(i);
		delete crd;
	}
	m_Coords.RemoveAll();

  delete m_textBlock;
}; // Destruktor

void DataBlock::SetCoordNum( int n )
{
	m_nCoord = n;
	if( m_pProfil )
    m_pProfil->SetModified();
}


/*!
 * Setzt den Type des Datenblocks.
 * sorgt auch dafür, dass die vom Typ abhängigen
 * Daten gesetzt werden.
 *
 * @todo : nicht alle Sonderbauwerke sind implementiert
 *
 * @param type : der neue Typ des Datenblocks
 *
 */
void DataBlock::SetType( int type )
{
	m_nType = type;
	m_name[0] = GetName(0, type);
	m_name[1] = GetName(1, type);

  // noch Sonderbauwerkskennung setzen
  int controlType = 0; // Standard
  switch( type )
  {
  case DST_COMMENT:
    controlType = 17;
    break;

  case DST_LP_TEXT:
    controlType = 12;
    break;
  }; // switch type
  m_nControlData[8] = controlType;
  
  if( m_pProfil )
    m_pProfil->SetModified();
}

int DataBlock::GetType() const
{
	return m_nType;
}

CString DataBlock::GetName( const int n, const int type )
{
	CString name[N_NAMESETS*2];
	
	switch(type)
	{
		case DST_COMMENT:
			name[0] = "Kommentar:"; // gross klein beachten!
      name[1] = "[-]";
			break;
		
		case DST_GELAENDEHOEHE:
			name[0] = "GELAENDE";
			name[1] = "HOEHE";
			break;
		
		case DST_TRENNFLAECHEN:
			name[0] = "TRENNFLAECHEN";
			name[1] = "[-]";
			name[2] = "TRENNFLAECHEN";
			break;
		
		case DST_RAUHIGKEIT_KST:
			name[0] = "RAUHEIT";
			name[1] = "KST";
			name[2] = "RAUHIGKEIT";
			name[3] = "KST";
			break;
		
		case DST_RAUHIGKEIT:
			name[0] = "RAUHEIT";
			name[1] = "KS";
			name[2] = "RAUHIGKEIT";
			name[3] = "KS";
			name[4] = "RAUHEIT";
			name[5] = "k-S";
			name[6] = "RAUHIGKEIT";
			name[7] = "k-S";
			break;
		
		case DST_DURCHST_BEREICH:
			name[0] = "DURCHSTROEMTE";
			name[1] = "BEREICHE";
			break;
		
		case DST_UK_BRUECKE:
			name[0] = "UK-BRUECKE";
			break;
		
		case DST_OK_BRUECKE:
			name[0] = "OK-BRUECKE";
			break;
		
		case DST_AXM:
			name[0] = "AX";
			break;
		
		case DST_AYM:
			name[0] = "AY";
			break;
		
		case DST_DPM:
			name[0] = "DP";
			break;
		
		case DST_OK_WEHRS:
			name[0] = "OK-WEHR";
			break;
		
		case DST_GELAENDE2:
			name[0] = "2.GELAENDE";
			break;
		
		case DST_FLAECHE:
			name[0] = "FLAECHE";
			break;
		
		case DST_KREISSEGM:
			name[0] = "KREISSEGMENT";
			break;
		
		case DST_SVA_WERT:
			name[0] = "SVA-WERT";
			break;
		
		case DST_OK_GELAENDE:
			name[0] = "OK-GELAENDE";
			break;
		
		case DST_KASTEN:
			name[0] = "KASTENPROFIL";
			break;
		
		case DST_LWA_FELDER:
			name[0] = "LWA-FELDER";
			break;
		
		case DST_GAUSSRUECK:
			name[0] = "GAUSSPROFIL";
			name[1] = "MIT";
			break;
		
		case DST_GAUSS:
			name[0] = "LAGE-KOORDINATEN";
			break;
		
		case DST_RECHTSWERT:
			name[0] = "RECHTSWERT";
			break;
		
		case DST_HOCHWERT:
			name[0] = "HOCHWERT";
			break;
		
		case DST_PUNKT_NR:
			name[0] = "PUNKT-NR";
			break;
		
		case DST_WSP_HOEHE:
			name[0] = "WSP-HOEHE";
			name[2] = "STATION";
			name[3] = "WSP-HOEHE";
			break;

		case DST_BVHOEHE://Dick 26.04.00
			name[0] = "Bordvoll-Hoehe";
			break;

        case DST_VMITTEL://Dick 26.04.00
			name[0] = "v-mittel";
			break;

        case DST_GEFAELLE://Dick 26.04.00
			name[0] = "Gefaelle";
			break;

        case DST_UNKNOWN://Dick 26.04.00
			name[0] = m_name[0];
			name[1] = m_name[1];
			break;

		case DST_BORDVOLL:
			name[0] = "BORDVOLL";
			break;
		
		case DST_TRENN_WEHR:
			name[0] = "TRENNLINIE";
			name[1] = "WEHR";
			break;
		
		case DST_MAUL:
			name[0] = "MAULPROFIL";
			break;
		
		case DST_EIPROFIL:
			name[0] = "EI";
			break;
		
		case DST_KREIS:
			name[0] = "KREIS";
			break;
		
		case DST_ARMCO84:
			name[0] = "ARMCO84";
			break;
		
		case DST_ARMCO71:
			name[0] = "ARMCO71";
			break;
		
		case DST_NWRINNE:
			name[0] = "NW-RINNE";
			break;
		
		case DST_FUELLHOEHE:
			name[0] = "FUELLHOEHEN-PROFIL";
			break;
		
		case DST_NAU_MED:
			name[0] = "NAUDASCHER-MEDLARZ";
			break;
		
		case DST_TRAPEZ:
			name[0] = "TRAPEZ";
			break;
		
		case DST_SOHLHOEHE:
			name[0] = "SOHLHOEHE";
			break;
		
		case DST_LAENGE:
			name[0] = "LAENGE";
			break;
		
		case DST_ABFLUSS:
			name[0] = "ABFLUSS";
			break;
		
		case DST_WASSERSPIEGEL:
			name[0] = "WASSERSPIEGEL";
			break;
		
		case DST_WSP_BREITE:
			name[0] = "WASSERSP.-BR";
			break;
		
		case DST_BOESCHUNG_LINKS:
			name[0] = "BOESCHUNG-LI";
			break;
		
		case DST_BOESCHUNG_RECHTS:
			name[0] = "BOESCHUNG-RE";
			break;
		
		case DST_PROFILART:
			name[0] = "PROFILART";
			break;
		
		case DST_VZKENNG:
			name[0] = "VERZWEIGUNG";
			break;
		
		case DST_PROFILKENNG:
			name[0] = "PROFILKENNUNG";
			break;
		
		case DST_DKUK:
			name[0] = "DECKENUNTERK";
			break;
		
		case DST_DKOK:
			name[0] = "DECKENOBERK";
			break;
		
		case DST_LP_TEXT:
			name[0] = "TEXT";
			break;
		
		case DST_BAUWERK:
			name[0] = "BAUWERK";
			break;
		
		case DST_SOHLHOEHE_2:
			name[0] = "2.SOHLHOEHE";
			break;
		
		case DST_WASSERSPIEGEL_2:
			name[0] = "2.WASSERSPIEGEL";
			break;
		
		case DST_BOESCHUNG_LINKS_2:
			name[0] = "2.BOESCHUNG-LI";
			break;
		
		case DST_BOESCHUNG_RECHTS_2:
			name[0] = "2.BOESCHUNG-RE";
			break;
		
		case DST_WSP_FIXIERUNG:
			name[0] = "Wasserspiegel fixierung";
			name[2] = "WSP-Fixierung";
			break;
		
		case DST_WSP_MESSUNG:
			name[0] = "WSP-Messung";
			break;
		
		case DST_RECHTSWERT_2:
			name[0] = "2.RECHTSWERT";
			break;
		
		case DST_HOCHWERT_2:
			name[0] = "2.HOCHWERT";
			break;
		
		case DST_RE_BOESCHUNG_RE:
			name[0] = "RE-BOESCHUNG-RE";
			break;

		case DST_HO_BOESCHUNG_RE:
			name[0] = "HO-BOESCHUNG-RE";
			break;

		case DST_RE_BOESCHUNG_LI:
			name[0] = "RE-BOESCHUNG-LI";
			break;

		case DST_HO_BOESCHUNG_LI:
			name[0] = "HO-BOESCHUNG-LI";
			break;

		case DST_RE_BOESCHUNG_RE_2:
			name[0] = "2.RE-BOESCHUNG-RE";
			break;

		case DST_HO_BOESCHUNG_RE_2:
			name[0] = "2.HO-BOESCHUNG-RE";
			break;

		case DST_RE_BOESCHUNG_LI_2:
			name[0] = "2.RE-BOESCHUNG-LI";
			break;

		case DST_HO_BOESCHUNG_LI_2:
			name[0] = "2.HO-BOESCHUNG-LI";
			break;

		case DST_DEICH_RECHTS:
			name[0] = "DEICH-RE";
			break;

		case DST_DEICH_LINKS:
			name[0] = "DEICH-LI";
			break;

		case DST_DEICH_RECHTS_2:
			name[0] = "2.DEICH-RE";
			break;

		case DST_DEICH_LINKS_2:
			name[0] = "2.DEICH-LI";
			break;

		case DST_RE_DEICH_RE:
			name[0] = "RE-DEICH-RE";
			break;

		case DST_HO_DEICH_RE:
			name[0] = "HO-DEICH-RE";
			break;

		case DST_RE_DEICH_LI:
			name[0] = "RE-DEICH-LI";
			break;

		case DST_HO_DEICH_LI:
			name[0] = "HO-DEICH-LI";
			break;

		case DST_RE_DEICH_RE_2:
			name[0] = "2.RE-DEICH-RE";
			break;

		case DST_HO_DEICH_RE_2:
			name[0] = "2.HO-DEICH-RE";
			break;

		case DST_RE_DEICH_LI_2:
			name[0] = "2.RE-DEICH-LI";
			break;

		case DST_HO_DEICH_LI_2:
			name[0] = "2.HO-DEICH-LI";
			break;

		case DST_SCHLEPPSPANN:
			name[0] = "SCHLEPPSPANNUNG";
			break;

		case DST_AUSUFERUNG_LI:
			name[0] = "AUSUFERUNG-Li.";
			break;

		case DST_AUSUFERUNG_RE:
			name[0] = "AUSUFERUNG-re.";
			break;

		case DST_ENERGIEHOEHE:
			name[0] = "ENERGIEHOEHE";
			break;

    case DST_BUHNEN:
      name[0] = "BUHNEN";
      break;

    case DST_BOESCHUNGSKANTEN:
      name[0] = "BOESCHUNGSKANTE";
      break;

    case DST_WSP_DIFFERENZ:
      name[0] = "WSP-DIFFERENZ NN+m";
      break;

    case DST_MODELLGRENZEN:
      name[0] = "MODELLGRENZEN";
      break;
		
    case DST_POL_GRENZEN:
      name[0] = "BEREICHE";
      break;
    
    default:
			break;
	}
  CString ergebnis;
	if (n >= 0 && n < N_NAMESETS * 2)
		ergebnis = name[n];
  return ergebnis;
  
}

void DataBlock::FindType()
{
	CString name[2];
	CString comp[N_NAMESETS*2];
	CString temp;
	int i, j;

	name[0] = m_name[0];
	name[0].MakeUpper();
	name[1] = m_name[1];
	name[1].MakeUpper();
	m_nType = DST_UNKNOWN;
	for ( i = DST_UNKNOWN + 1; i < N_DSTYPES; i++ ) // auch die negativen Datenblocktypen einlesen
	{
		for (j=0; j<N_NAMESETS*2; j++)
			comp[j] = GetName(j, i);
		switch (i)
		{
			case DST_SOHLHOEHE_2:
			case DST_WASSERSPIEGEL_2:
			case DST_BOESCHUNG_LINKS_2:
			case DST_BOESCHUNG_RECHTS_2:
			case DST_RECHTSWERT_2:
			case DST_HOCHWERT_2:
			case DST_RE_BOESCHUNG_RE_2:
			case DST_HO_BOESCHUNG_RE_2:
			case DST_RE_BOESCHUNG_LI_2:
			case DST_HO_BOESCHUNG_LI_2:
			case DST_DEICH_RECHTS_2:
			case DST_DEICH_LINKS_2:
			case DST_RE_DEICH_RE_2:
			case DST_HO_DEICH_RE_2:
			case DST_RE_DEICH_LI_2:
			case DST_HO_DEICH_LI_2:
        temp = name[0];
				j = name[0].Find(".");
				if (j!=-1)
				{
					name[0] = name[0].Right(name[0].GetLength()-j-1);
					comp[0] = comp[0].Right(comp[0].GetLength()-2);
				}
				break;

			default:
				break;
		}
		for (j=0; j<N_NAMESETS; j++)
		{
			if (!comp[j*2].IsEmpty() || !comp[j*2+1].IsEmpty())
			{
				BOOL bMatch = FALSE;
				if (!comp[j*2].IsEmpty())
					bMatch = name[0].Left(comp[j*2].GetLength()).CompareNoCase(comp[j*2])==0;
				if (bMatch && !comp[j*2+1].IsEmpty())
					bMatch = name[1].Left(comp[j*2+1].GetLength()).CompareNoCase(comp[j*2+1])==0;
        if(bMatch && (i==DST_BORDVOLL || i==DST_WASSERSP1))//Dick 26.04.00
          bMatch = name[0].CompareNoCase(comp[j*2])==0;
				if (bMatch)
				{
					m_nType = i;
					return;
				}
			}
		}
		switch (i)
		{
			case DST_SOHLHOEHE_2:
			case DST_WASSERSPIEGEL_2:
			case DST_BOESCHUNG_LINKS_2:
			case DST_BOESCHUNG_RECHTS_2:
			case DST_RECHTSWERT_2:
			case DST_HOCHWERT_2:
			case DST_RE_BOESCHUNG_RE_2:
			case DST_HO_BOESCHUNG_RE_2:
			case DST_RE_BOESCHUNG_LI_2:
			case DST_HO_BOESCHUNG_LI_2:
			case DST_DEICH_RECHTS_2:
			case DST_DEICH_LINKS_2:
			case DST_RE_DEICH_RE_2:
			case DST_HO_DEICH_RE_2:
			case DST_RE_DEICH_LI_2:
			case DST_HO_DEICH_LI_2:
				name[0] = temp;
				break;

			default:
				break;
		}
	}
}

DataBlock* DataBlock::Clone( Profil* pProfil )
{
	DataBlock *clone;
	int i;
	Coord *crd, *newCrd;

	clone = new DataBlock( pProfil );

	clone->m_nType = m_nType;
	clone->m_nCoord = m_nCoord;
	for (i=0; i<2; i++)
		clone->m_name[i] = m_name[i];
	for (i=0; i<9; i++)
		clone->m_nControlData[i] = m_nControlData[i];
	clone->m_Coords.SetSize(m_Coords.GetSize());
	for (i=0; i<m_Coords.GetSize(); i++)
	{
		crd = m_Coords.GetAt(i);
		newCrd = new Coord;
		newCrd->dx = crd->dx;
		newCrd->dy = crd->dy;
		clone->m_Coords.SetAt(i, newCrd);
	}
	clone->m_dVars.SetSize(m_dVars.GetSize());
	for (i=0; i<m_dVars.GetSize(); i++)
		clone->m_dVars[i] = m_dVars[i];
	clone->m_nVars.SetSize(m_nVars.GetSize());
	for (i=0; i<m_nVars.GetSize(); i++)
		clone->m_nVars[i] = m_nVars[i];
	clone->m_lines.SetSize(m_lines.GetSize());
	for (i=0; i<m_lines.GetSize(); i++)
		clone->m_lines[i] = m_lines[i];

	return clone;
}

void DataBlock::AddCoord(Coord* crd)
{
	m_Coords.SetAtGrow(m_Coords.GetSize(), crd);
	m_nCoord++;
	if (m_pProfil!=NULL)
		m_pProfil->SetModified();
}

void DataBlock::RemoveCoord(Coord* crd)
{
	for( int i = 0; i < m_Coords.GetSize(); i++ )
	{
		Coord* match = m_Coords.GetAt( i );
		if( match == crd )
		{
			delete crd;
			m_Coords.RemoveAt(i);
			m_nCoord--;
			break;
		}
	}
	if( m_pProfil != NULL )
		m_pProfil->SetModified();
}

int DataBlock::GetCurrentCoordIndex()
{
	return m_nCoordIndex;
}

int DataBlock::GetCoordIndex(double x, double toleranz /* = 0.0000001 */)
{
	
	
	for( int i = 0; i<m_Coords.GetSize(); i++ )
	{
		Coord* crd = m_Coords.GetAt(i);
		if( fabs( crd->dx - x ) < toleranz )
			return i;
	}
	return -1;
}

void DataBlock::InsertCoordAt(int n, Coord* crd)
{
	if (n >= 0)
	{
		m_Coords.InsertAt(n, crd);
		m_nCoord++;
	}
	if (m_pProfil!=NULL)
		m_pProfil->SetModified();
}

Coord* DataBlock::GetCoordAt(int n)
{
	if( n>= 0 && n < m_Coords.GetSize() )
		return m_Coords.GetAt( n );
	
  return NULL;
}


/*!
 * Gibt den y-Wert an einer bestimmten Stelle zurück.
 * Interpoliert zur Not linear oder extrapoliert konstant bzw. lineart
 *
 * @param dx : für diese x-Koordinate wird der y Wert gesucht
 * @param bExtrapolLinear: 
 *
 * @return double  : der dy-Wert an stelle dx
 */
double DataBlock::GetDYAt( double dx, bool bExtrapolLinear )
{
  // zuerst gucken, ob es diese Kooridnate schon gibt
  Coord* pCrd = GetCoord( dx, 0.0001 );
  if( pCrd )
     return pCrd->dy;

  // erstmal raufinden, wie wir interpolieren müssen

  // extrapolieren?
  Coord* pCrdLinFst = NULL;
  Coord* pCrdLinLst = NULL;

  // dx liegt vor dem Profil
  Coord* pFstCrd = GetFirstCoord();
  if( pFstCrd && dx < pFstCrd->dx )
  {
    if( !bExtrapolLinear )
       return pFstCrd->dy;

    pCrdLinFst = pFstCrd;
    pCrdLinLst = GetCoordAt( 1 );
  }

  // dx liegt nach dem Profil
  Coord* pLstCrd = GetLastCoord();
  if( pLstCrd && pLstCrd->dx < dx )
  {
    if( !bExtrapolLinear )
      return pLstCrd->dy;

    pCrdLinFst = GetCoordAt( GetNumCoords() - 2 );
    pCrdLinLst = pLstCrd;
  }

  // wenn noch keine gefunden, liegt die Koordinate im Profil
  if( pCrdLinFst != NULL ||  pCrdLinLst != NULL )
  {
    for( int i = 0; i < GetNumCoords(); i++ )
    {
      Coord* pCrd = GetCoordAt( i );
      if( pCrd->dx < dx )
      {
        pCrdLinFst = pCrd;
        pCrdLinLst = GetCoordAt( i + 1 );
        break;
      }
    }
  }

  // jetzt linear interpolieren
  if( pCrdLinFst != NULL && pCrdLinLst != NULL )
  {
    BCE::Math::LinearEquation le( pCrdLinFst->dx, pCrdLinFst->dy, pCrdLinLst->dx, pCrdLinLst->dy );
    return le.computeY( dx );
  }

  TRACE( "dy für x=%lf nicht gefunden\n", dx );
  return 0.0;
}


Coord* DataBlock::GetFirstCoord()
{
	m_nCoordIndex = 0;
	return GetNextCoord();
}

Coord* DataBlock::GetNextCoord()
{
	if (m_nCoordIndex<0 || m_nCoordIndex>=m_Coords.GetSize())
		return NULL;
	else
		return m_Coords.GetAt(m_nCoordIndex++);
}

Coord* DataBlock::GetLastCoord()
{
	m_nCoordIndex = m_Coords.GetSize()-1;
	return GetPrevCoord();
}

Coord* DataBlock::GetPrevCoord()
{
	if (m_nCoordIndex<0 || m_nCoordIndex>=m_Coords.GetSize())
		return NULL;
	else
		return m_Coords.GetAt(m_nCoordIndex--);
}

int DataBlock::GetNumCoords() const
{
	return m_Coords.GetSize();
}

int DataBlock::GetLineCount() const
{
  return m_lines.GetSize();
}; // GetLineCount

CString DataBlock::GetLine( const int n ) const
{
	if (n>=0 && n<m_lines.GetSize())
		return m_lines[n];
	else
		return TEXT("");
}

double DataBlock::GetDVar(int n)
{
	if (n>=0 && n<m_dVars.GetSize())
		return m_dVars[n];
	return 0;
}

int DataBlock::GetNVar(int n)
{
	if (n>=0 && n<m_nVars.GetSize())
		return m_nVars[n];
	return 0;
}

CString DataBlock::GetName( const int n ) const
{
	if( n >= 0 && n < 2 )
		return m_name[n];
  else
    return CString();
}
void DataBlock::SetName( const CString& name, const int n )
{
	if( n >= 0 && n < 2 )
		m_name[n] = name;
	if( m_pProfil )
    m_pProfil->SetModified();
}

CString DataBlock::GetDesc( int n )
{
	int nID1, nID2;

  switch( m_nType )
  {
  case DST_COMMENT:
    nID1 = IDS_DESC_COMMENT_1;
    nID2 = IDS_DESC_COMMENT_2;
    break;
    
  case DST_GELAENDEHOEHE:
    nID1 = IDS_DESC_GELAENDEHOEHE_1;
    nID2 = IDS_DESC_GELAENDEHOEHE_2;
    break;
    
  case DST_TRENNFLAECHEN:
    nID1 = IDS_DESC_TRENNFLAECHEN_1;
    nID2 = IDS_DESC_TRENNFLAECHEN_2;
    break;
    
  case DST_RAUHIGKEIT_KST:
    nID1 = IDS_DESC_RAUHIGKEIT_KST_1;
    nID2 = IDS_DESC_RAUHIGKEIT_KST_2;
    break;
    
  case DST_RAUHIGKEIT:
    nID1 = IDS_DESC_RAUHIGKEIT_1;
    nID2 = IDS_DESC_RAUHIGKEIT_2;
    break;
    
  case DST_DURCHST_BEREICH:
    nID1 = IDS_DESC_DURCHST_BEREICH_1;
    nID2 = IDS_DESC_DURCHST_BEREICH_2;
    break;
    
  case DST_UK_BRUECKE:
    nID1 = IDS_DESC_UK_BRUECKE_1;
    nID2 = IDS_DESC_UK_BRUECKE_2;
    break;
    
  case DST_OK_BRUECKE:
    nID1 = IDS_DESC_OK_BRUECKE_1;
    nID2 = IDS_DESC_OK_BRUECKE_2;
    break;
    
  case DST_AXM:
    nID1 = IDS_DESC_AXM_1;
    nID2 = IDS_DESC_AXM_2;
    break;
    
  case DST_AYM:
    nID1 = IDS_DESC_AYM_1;
    nID2 = IDS_DESC_AYM_2;
    break;
    
  case DST_DPM:
    nID1 = IDS_DESC_DPM_1;
    nID2 = IDS_DESC_DPM_2;
    break;
    
  case DST_OK_WEHRS:
    nID1 = IDS_DESC_OK_WEHRS_1;
    nID2 = IDS_DESC_OK_WEHRS_2;
    break;
    
  case DST_GELAENDE2:
    nID1 = IDS_DESC_GELAENDE2_1;
    nID2 = IDS_DESC_GELAENDE2_2;
    break;
    
  case DST_FLAECHE:
    nID1 = IDS_DESC_FLAECHE_1;
    nID2 = IDS_DESC_FLAECHE_2;
    break;
    
  case DST_KREISSEGM:
    nID1 = IDS_DESC_KREISSEGM_1;
    nID2 = IDS_DESC_KREISSEGM_2;
    break;
    
  case DST_SVA_WERT:
    nID1 = IDS_DESC_SVA_WERT_1;
    nID2 = IDS_DESC_SVA_WERT_2;
    break;
    
  case DST_OK_GELAENDE:
    nID1 = IDS_DESC_OK_GELAENDE_1;
    nID2 = IDS_DESC_OK_GELAENDE_2;
    break;
    
  case DST_KASTEN:
    nID1 = IDS_DESC_KASTEN_1;
    nID2 = IDS_DESC_KASTEN_2;
    break;
    
  case DST_LWA_FELDER:
    nID1 = IDS_DESC_LWA_FELDER_1;
    nID2 = IDS_DESC_LWA_FELDER_2;
    break;
    
  case DST_GAUSS:
    nID1 = IDS_DESC_GAUSS_1;
    nID2 = IDS_DESC_GAUSS_2;
    break;
    
  case DST_RECHTSWERT:
    nID1 = IDS_DESC_RECHTSWERT_1;
    nID2 = IDS_DESC_RECHTSWERT_2;
    break;
    
  case DST_HOCHWERT:
    nID1 = IDS_DESC_HOCHWERT_1;
    nID2 = IDS_DESC_HOCHWERT_2;
    break;
    
  case DST_PUNKT_NR:
    nID1 = IDS_DESC_PUNKT_NR_1;
    nID2 = IDS_DESC_PUNKT_NR_2;
    break;
    
  case DST_WSP_HOEHE:
    nID1 = IDS_DESC_WSP_HOEHE_1;
    nID2 = IDS_DESC_WSP_HOEHE_2;
    break;
    
  case DST_WASSERSP1:
    nID1 = IDS_DESC_WASSERSP1_1;
    nID2 = IDS_DESC_WASSERSP1_2;
    break;
    
  case DST_WASSERSP100:
    nID1 = IDS_DESC_WASSERSP100_1;
    nID2 = IDS_DESC_WASSERSP100_2;
    break;
    
  case DST_WASSERSP5:
    nID1 = IDS_DESC_WASSERSP5_1;
    nID2 = IDS_DESC_WASSERSP5_2;
    break;
    
  case DST_BORDVOLL:
    nID1 = IDS_DESC_BORDVOLL_1;
    nID2 = IDS_DESC_BORDVOLL_2;
    break;
    
  case DST_TRENN_WEHR:
    nID1 = IDS_DESC_TRENN_WEHR_1;
    nID2 = IDS_DESC_TRENN_WEHR_2;
    break;
    
  case DST_MAUL:
    nID1 = IDS_DESC_MAUL_1;
    nID2 = IDS_DESC_MAUL_2;
    break;
    
  case DST_EIPROFIL:
    nID1 = IDS_DESC_EIPROFIL_1;
    nID2 = IDS_DESC_EIPROFIL_2;
    break;
    
  case DST_KREIS:
    nID1 = IDS_DESC_KREIS_1;
    nID2 = IDS_DESC_KREIS_2;
    break;
    
  case DST_ARMCO84:
    nID1 = IDS_DESC_ARMCO84_1;
    nID2 = IDS_DESC_ARMCO84_2;
    break;
    
  case DST_ARMCO71:
    nID1 = IDS_DESC_ARMCO71_1;
    nID2 = IDS_DESC_ARMCO71_2;
    break;
    
  case DST_NWRINNE:
    nID1 = IDS_DESC_NWRINNE_1;
    nID2 = IDS_DESC_NWRINNE_2;
    break;
    
  case DST_FUELLHOEHE:
    nID1 = IDS_DESC_FUELLHOEHE_1;
    nID2 = IDS_DESC_FUELLHOEHE_2;
    break;
    
  case DST_NAU_MED:
    nID1 = IDS_DESC_NAU_MED_1;
    nID2 = IDS_DESC_NAU_MED_2;
    break;
    
  case DST_TRAPEZ:
    nID1 = IDS_DESC_TRAPEZ_1;
    nID2 = IDS_DESC_TRAPEZ_2;
    break;
    
  case DST_SOHLHOEHE:
    nID1 = IDS_DESC_SOHLHOEHE_1;
    nID2 = IDS_DESC_SOHLHOEHE_2;
    break;
    
  case DST_LAENGE:
    nID1 = IDS_DESC_LAENGE_1;
    nID2 = IDS_DESC_LAENGE_2;
    break;
    
  case DST_ABFLUSS:
    nID1 = IDS_DESC_ABFLUSS_1;
    nID2 = IDS_DESC_ABFLUSS_2;
    break;
    
  case DST_WASSERSPIEGEL:
    nID1 = IDS_DESC_WASSERSPIEGEL_1;
    nID2 = IDS_DESC_WASSERSPIEGEL_2;
    break;
    
  case DST_WSP_BREITE:
    nID1 = IDS_DESC_WSP_BREITE_1;
    nID2 = IDS_DESC_WSP_BREITE_2;
    break;
    
  case DST_BOESCHUNG_LINKS:
    nID1 = IDS_DESC_BOESCHUNG_LINKS_1;
    nID2 = IDS_DESC_BOESCHUNG_LINKS_2;
    break;
    
  case DST_BOESCHUNG_RECHTS:
    nID1 = IDS_DESC_BOESCHUNG_RECHTS_1;
    nID2 = IDS_DESC_BOESCHUNG_RECHTS_2;
    break;
    
  case DST_PROFILART:
    nID1 = IDS_DESC_PROFILART_1;
    nID2 = IDS_DESC_PROFILART_2;
    break;
    
  case DST_VZKENNG:
    nID1 = IDS_DESC_VZKENNG_1;
    nID2 = IDS_DESC_VZKENNG_2;
    break;
    
  case DST_PROFILKENNG:
    nID1 = IDS_DESC_PROFILKENNG_1;
    nID2 = IDS_DESC_PROFILKENNG_2;
    break;
    
  case DST_DKUK:
    nID1 = IDS_DESC_DKUK_1;
    nID2 = IDS_DESC_DKUK_2;
    break;
    
  case DST_DKOK:
    nID1 = IDS_DESC_DKOK_1;
    nID2 = IDS_DESC_DKOK_2;
    break;
    
  case DST_LP_TEXT:
    nID1 = IDS_DESC_LP_TEXT_1;
    nID2 = IDS_DESC_LP_TEXT_2;
    break;
    
  case DST_BAUWERK:
    nID1 = IDS_DESC_BAUWERK_1;
    nID2 = IDS_DESC_BAUWERK_2;
    break;
    
  case DST_GEFAELLE:
    nID1 = IDS_DESC_GEFAELLE_1;
    nID2 = IDS_DESC_GEFAELLE_2;
    break;
    
  case DST_VMITTEL:
    nID1 = IDS_DESC_VMITTEL_1;
    nID2 = IDS_DESC_VMITTEL_2;
    break;
    
  case DST_BVHOEHE:
    nID1 = IDS_DESC_BVHOEHE_1;
    nID2 = IDS_DESC_BVHOEHE_2;
    break;
    
  case DST_SOHLHOEHE_2:
    nID1 = IDS_DESC_SOHLHOEHE_2_1;
    nID2 = IDS_DESC_SOHLHOEHE_2_2;
    break;
    
  case DST_WASSERSPIEGEL_2:
    nID1 = IDS_DESC_WASSERSPIEGEL_2_1;
    nID2 = IDS_DESC_WASSERSPIEGEL_2_2;
    break;
    
  case DST_BOESCHUNG_LINKS_2:
    nID1 = IDS_DESC_BOESCHUNG_LINKS_2_1;
    nID2 = IDS_DESC_BOESCHUNG_LINKS_2_2;
    break;
    
  case DST_BOESCHUNG_RECHTS_2:
    nID1 = IDS_DESC_BOESCHUNG_RECHTS_2_1;
    nID2 = IDS_DESC_BOESCHUNG_RECHTS_2_2;
    break;
    
  case DST_STATION:
    nID1 = IDS_DESC_STATION_1;
    nID2 = IDS_DESC_STATION_2;
    break;
    
  case DST_WSP_FIXIERUNG:
    nID1 = IDS_DESC_WSP_FIXIERUNG_1;
    nID2 = IDS_DESC_WSP_FIXIERUNG_2;
    break;
    
  case DST_WSP_MESSUNG:
    nID1 = IDS_DESC_WSP_MESSUNG_1;
    nID2 = IDS_DESC_WSP_MESSUNG_2;
    break;
    
  case DST_RECHTSWERT_2:
    nID1 = IDS_DESC_RECHTSWERT_2_1;
    nID2 = IDS_DESC_RECHTSWERT_2_2;
    break;
    
  case DST_HOCHWERT_2:
    nID1 = IDS_DESC_HOCHWERT_2_1;
    nID2 = IDS_DESC_HOCHWERT_2_2;
    break;
    
  case DST_RE_BOESCHUNG_RE:
    nID1 = IDS_DESC_RE_BOESCHUNG_RE_1;
    nID2 = IDS_DESC_RE_BOESCHUNG_RE_2;
    break;
    
  case DST_HO_BOESCHUNG_RE:
    nID1 = IDS_DESC_HO_BOESCHUNG_RE_1;
    nID2 = IDS_DESC_HO_BOESCHUNG_RE_2;
    break;
    
  case DST_RE_BOESCHUNG_LI:
    nID1 = IDS_DESC_RE_BOESCHUNG_LI_1;
    nID2 = IDS_DESC_RE_BOESCHUNG_LI_2;
    break;
    
  case DST_HO_BOESCHUNG_LI:
    nID1 = IDS_DESC_HO_BOESCHUNG_LI_1;
    nID2 = IDS_DESC_HO_BOESCHUNG_LI_2;
    break;
    
  case DST_RE_BOESCHUNG_RE_2:
    nID1 = IDS_DESC_RE_BOESCHUNG_RE_2_1;
    nID2 = IDS_DESC_RE_BOESCHUNG_RE_2_2;
    break;
    
  case DST_HO_BOESCHUNG_RE_2:
    nID1 = IDS_DESC_HO_BOESCHUNG_RE_2_1;
    nID2 = IDS_DESC_HO_BOESCHUNG_RE_2_2;
    break;
    
  case DST_RE_BOESCHUNG_LI_2:
    nID1 = IDS_DESC_RE_BOESCHUNG_LI_2_1;
    nID2 = IDS_DESC_RE_BOESCHUNG_LI_2_2;
    break;
    
  case DST_HO_BOESCHUNG_LI_2:
    nID1 = IDS_DESC_HO_BOESCHUNG_LI_2_1;
    nID2 = IDS_DESC_HO_BOESCHUNG_LI_2_2;
    break;
    
  case DST_DEICH_RECHTS:
    nID1 = IDS_DESC_DEICH_RECHTS_1;
    nID2 = IDS_DESC_DEICH_RECHTS_2;
    break;
    
  case DST_DEICH_LINKS:
    nID1 = IDS_DESC_DEICH_LINKS_1;
    nID2 = IDS_DESC_DEICH_LINKS_2;
    break;
    
  case DST_DEICH_RECHTS_2:
    nID1 = IDS_DESC_DEICH_RECHTS_2_1;
    nID2 = IDS_DESC_DEICH_RECHTS_2_2;
    break;
    
  case DST_DEICH_LINKS_2:
    nID1 = IDS_DESC_DEICH_LINKS_2_1;
    nID2 = IDS_DESC_DEICH_LINKS_2_2;
    break;
    
  case DST_RE_DEICH_RE:
    nID1 = IDS_DESC_RE_DEICH_RE_1;
    nID2 = IDS_DESC_RE_DEICH_RE_2;
    break;
    
  case DST_HO_DEICH_RE:
    nID1 = IDS_DESC_HO_DEICH_RE_1;
    nID2 = IDS_DESC_HO_DEICH_RE_2;
    break;
    
  case DST_RE_DEICH_LI:
    nID1 = IDS_DESC_RE_DEICH_LI_1;
    nID2 = IDS_DESC_RE_DEICH_LI_2;
    break;
    
  case DST_HO_DEICH_LI:
    nID1 = IDS_DESC_HO_DEICH_LI_1;
    nID2 = IDS_DESC_HO_DEICH_LI_2;
    break;
    
  case DST_RE_DEICH_RE_2:
    nID1 = IDS_DESC_RE_DEICH_RE_2_1;
    nID2 = IDS_DESC_RE_DEICH_RE_2_2;
    break;
    
  case DST_HO_DEICH_RE_2:
    nID1 = IDS_DESC_HO_DEICH_RE_2_1;
    nID2 = IDS_DESC_HO_DEICH_RE_2_2;
    break;
    
  case DST_RE_DEICH_LI_2:
    nID1 = IDS_DESC_RE_DEICH_LI_2_1;
    nID2 = IDS_DESC_RE_DEICH_LI_2_2;
    break;
    
  case DST_HO_DEICH_LI_2:
    nID1 = IDS_DESC_HO_DEICH_LI_2_1;
    nID2 = IDS_DESC_HO_DEICH_LI_2_2;
    break;
    
  case DST_SCHLEPPSPANN:
    nID1 = IDS_DESC_SCHLEPPSPANN_1;
    nID2 = IDS_DESC_SCHLEPPSPANN_2;
    break;
    
  case DST_AUSUFERUNG_LI:
    nID1 = IDS_DESC_AUSUFERUNG_LI_1;
    nID2 = IDS_DESC_AUSUFERUNG_LI_2;
    break;
    
  case DST_AUSUFERUNG_RE:
    nID1 = IDS_DESC_AUSUFERUNG_RE_1;
    nID2 = IDS_DESC_AUSUFERUNG_RE_2;
    break;
    
  case DST_ENERGIEHOEHE:
    nID1 = IDS_DESC_ENERGIEHOEHE_1;
    nID2 = IDS_DESC_ENERGIEHOEHE_2;
    break;
    
  case DST_BUHNEN:
    nID1 = IDS_DESC_BUHNEN_1;
    nID2 = IDS_DESC_BUHNEN_2;
    break;
    
  case DST_BOESCHUNGSKANTEN:
    nID1 = IDS_DESC_BOESCHUNGSKANTEN_1;
    nID2 = IDS_DESC_BOESCHUNGSKANTEN_2;
    break;
    
  case DST_WSP_DIFFERENZ:
    nID1 = IDS_DESC_WSP_DIFFERENZ_1;
    nID2 = IDS_DESC_WSP_DIFFERENZ_2;
    break;
    
  case DST_MODELLGRENZEN:
    nID1 = IDS_DESC_MODELLGRENZEN_1;
    nID2 = IDS_DESC_MODELLGRENZEN_2;
    break;
    
  case DST_POL_GRENZEN:
    nID1 = IDS_DESC_POL_GRENZEN_1;
    nID2 = IDS_DESC_POL_GRENZEN_2;
    
  default:
    return CString( "?" );
  }

  switch( n )
  {
  case 0:
    return CString( MAKEINTRESOURCE( nID1 ) );
    
  case 1:
    return CString( MAKEINTRESOURCE( nID2 ) );
    
  default:
    return CString( "?" );
  }
  
  return CString( "?" );
}

istream& operator>>( istream& is, DataBlock &dat )
{
	char buffer[LINE_SIZE];
	
	// 1. Zeile
	is.getline( buffer, LINE_SIZE, '\n' );
	dat.m_name[0] = buffer;
	dat.m_name[0].TrimLeft();
	dat.m_name[0].TrimRight();
	// 2. Zeile
	is.getline( buffer, LINE_SIZE, '\n' );
	dat.m_name[1] = buffer;
	dat.FindType();
	
  switch( dat.m_nType )
	{
		case DST_WSP_HOEHE:
		case DST_SOHLHOEHE_2:
    case DST_WASSERSPIEGEL:
		case DST_WASSERSPIEGEL_2:
		case DST_BOESCHUNG_LINKS_2:
		case DST_BOESCHUNG_RECHTS_2:
		case DST_RECHTSWERT_2:
		case DST_HOCHWERT_2:
		case DST_RE_BOESCHUNG_RE_2:
		case DST_HO_BOESCHUNG_RE_2:
		case DST_RE_BOESCHUNG_LI_2:
		case DST_HO_BOESCHUNG_LI_2:
		case DST_DEICH_LINKS_2:
		case DST_DEICH_RECHTS_2:
		case DST_RE_DEICH_RE_2:
		case DST_HO_DEICH_RE_2:
		case DST_RE_DEICH_LI_2:
		case DST_HO_DEICH_LI_2:
		case DST_WSP_MESSUNG:
		case DST_WSP_FIXIERUNG:
    case DST_ABFLUSS: //12.04.2000 Dick
			dat.m_name[1] = buffer;
			dat.m_name[1].TrimRight();
			break;

		default:
			break;
	}
	
  // 3. Zeile
  is.getline( buffer, LINE_SIZE, '\n' );

  // Problem: wir haben zwei Datenformat: 
  // einmal sind die Zahlen einfach Leerzeichen separiert, einmal sind
  // sie immer genau drei zeichen lang (Problem mit Profilnummern > 999 !!! )
  // deswegen wird versucht auf zwei verschiedene Arten zu lesen
  if( sscanf( buffer, "%d %d %d %d %d %d %d %d %d", &dat.m_nControlData[0], &dat.m_nControlData[1], &dat.m_nControlData[2], &dat.m_nControlData[3], 
	  &dat.m_nControlData[4], &dat.m_nControlData[5], &dat.m_nControlData[6], &dat.m_nControlData[7], &dat.m_nControlData[8] ) != 9 )
  {
	  // Leerzeichensepariert hat nicht geklappt, jetzt mit fester Breite lesen
	  
	  // 3. Zeile
	  CString str = buffer;
	  for( int i = 0; i < 9 && !str.IsEmpty(); i++ )
	  {
		  if( str.GetLength() < 3 )
		  {
			  AfxMessageBox( "Error in Datablock Line 3", MB_OK | MB_ICONSTOP );
			  dat.m_nControlData[8] = 0; // als 'nomrales' Profil lesen
			  break;
		  }
		  
		  CString parseString = str.Left( 3 );
		  parseString.TrimLeft();
		  parseString.TrimRight();
		  
		  dat.m_nControlData[i] = atoi( parseString );
		  str = str.Right( str.GetLength() - 3 );
	  }
  }
  
  int i;
  CString str;
  switch( dat.m_nControlData[8] )
	{
		case 0:		// coordinates (z.B. Gelaendehoehe)
		case 15:	// Kreissegment
			dat.m_Coords.SetSize( dat.m_nCoord );
			for( i = 0; i < dat.m_nCoord; i++ )
			{
				Coord* crd = new Coord;
        short steuer;
        double value;
				is >> steuer >> value;
				crd->xs = steuer;
				crd->dx = value;
				dat.m_Coords.SetAt( i, crd );
			}
			for( i = 0; i < dat.m_nCoord; i++ )
			{
				Coord* crd = dat.m_Coords.GetAt( i );
        short steuer;
        double value;
				is >> steuer >> value;
				crd->ys = steuer;
				crd->dy = value;
				dat.m_Coords.SetAt( i, crd );
			}
			is.getline( buffer, LINE_SIZE, '\n' );	// remove trash and end of line
			break;

		case 1:		// DATPLT
			is.getline(buffer, LINE_SIZE, '\n');
			str = buffer;
			str.TrimLeft();
			str.TrimRight();
			dat.m_lines.SetAtGrow(dat.m_lines.GetSize(), str);
			while( is.good() && str.SpanIncluding("END").CompareNoCase("END") != 0 )
			{
				is.getline(buffer, LINE_SIZE, '\n');
				str = buffer;
				str.TrimLeft();
				str.TrimRight();
				dat.m_lines.SetAtGrow(dat.m_lines.GetSize(), str);

				if( is.eof() )
				{
					// should do some kind of error handling
				}
			}
			break;

		case 2:		// Kanalsymbol
			is.getline(buffer, LINE_SIZE, '\n');
			str = buffer;
			str.TrimRight();
			str += " ";
			str.TrimLeft();
			for (i=0; i<6; i++)
			{
				dat.m_nVars.SetAtGrow(dat.m_nVars.GetSize(), atoi(str.Left(str.Find(" "))));
				str = str.Right(str.GetLength()-str.Find(" "));
				str.TrimLeft();
			}
			if (dat.m_nVars[4]!=0)
			{
				if (dat.m_nVars[5]==0)
				{
					for (i=0; i<2; i++)
					{
						is.getline(buffer, LINE_SIZE, '\n');
						str = buffer;
						str.TrimRight();
						str += " ";
						str.TrimLeft();
						for( int j = 0; j < dat.m_nVars[4]; j++ )
						{
							dat.m_nVars.SetAtGrow(dat.m_nVars.GetSize(), atoi(str.Left(str.Find(" "))));
							str = str.Right(str.GetLength()-str.Find(" "));
							str.TrimLeft();
						}
					}
				}
				else
				{
					is.getline(buffer, LINE_SIZE, '\n');
					str = buffer;
					str.TrimRight();
					str += " ";
					str.TrimLeft();
					for( int j = 0; j < dat.m_nVars[4]; j++ )
					{
						dat.m_dVars.SetAtGrow(dat.m_dVars.GetSize(), atof(str.Left(str.Find(" "))));
						str = str.Right(str.GetLength()-str.Find(" "));
						str.TrimLeft();
					}
				}
			}
			break;

		case 3:		// Rohre-entleerung,-entluftung
			is.getline(buffer, LINE_SIZE, '\n');
			str = buffer;
			str.TrimRight();
			str += " ";
			str.TrimLeft();
			for (i=0; i<2; i++)
			{
				dat.m_nVars.SetAtGrow(dat.m_nVars.GetSize(), atoi(str.Left(str.Find(" "))));
				str = str.Right(str.GetLength()-str.Find(" "));
				str.TrimLeft();
			}
			for (i=0; i<dat.m_nVars[1]; i++)
			{
        int j;
				is >> j;
				str.Format("%d", j);
				dat.m_nVars.SetAtGrow(dat.m_nVars.GetSize(), atoi(str.Left(1)));
				dat.m_nVars.SetAtGrow(dat.m_nVars.GetSize(), atoi(str.Right(str.GetLength()-1)));
			}
			is.getline(buffer, LINE_SIZE, '\n');
			break;

		case 4:		// Bauwerk
			is.getline(buffer, LINE_SIZE, '\n');
			str = buffer;
			str.TrimRight();
			str += " ";
			str.TrimLeft();
			for (i=0; i<2; i++)
			{
				dat.m_nVars.SetAtGrow(dat.m_nVars.GetSize(), atoi(str.Left(str.Find(" "))));
				str = str.Right(str.GetLength()-str.Find(" "));
				str.TrimLeft();
			}
			for (i=0; i<dat.m_nVars[1]; i++)
			{
				is.getline(buffer, LINE_SIZE, '\n');
				str = buffer;
				str.TrimRight();
				str += " ";
				str.TrimLeft();
				dat.m_nVars.SetAtGrow(dat.m_nVars.GetSize(), atoi(str.Left(str.Find(" "))));
				str = str.Right(str.GetLength()-str.Find(" "));
				str.TrimLeft();
				for( int j = 0; j < 3; j++ )
				{
					dat.m_dVars.SetAtGrow(dat.m_dVars.GetSize(), atof(str.Left(str.Find(" "))));
					str = str.Right(str.GetLength()-str.Find(" "));
					str.TrimLeft();
				}
			}
			break;

		case 5:		// Bemassung
			is.getline(buffer, LINE_SIZE, '\n');
			str = buffer;
			str.TrimRight();
			str += " ";
			str.TrimLeft();
			for (i=0; i<2; i++)
			{
				dat.m_nVars.SetAtGrow(dat.m_nVars.GetSize(), atoi(str.Left(str.Find(" "))));
				str = str.Right(str.GetLength()-str.Find(" "));
				str.TrimLeft();
			}
			for (i=0; i<dat.m_nVars[1]; i++)
			{
				is.getline(buffer, LINE_SIZE, '\n');
				str = buffer;
				str.TrimRight();
				str += " ";
				str.TrimLeft();
				for( int j = 0; j < 2; j++ )
				{
					dat.m_nVars.SetAtGrow(dat.m_nVars.GetSize(), atoi(str.Left(str.Find(" "))));
					str = str.Right(str.GetLength()-str.Find(" "));
					str.TrimLeft();
				}
				dat.m_dVars.SetAtGrow(dat.m_dVars.GetSize(), atof(str.Left(str.Find(" "))));
				str = str.Right(str.GetLength()-str.Find(" "));
				str.TrimLeft();
				for (j=0; j<3; j++)
				{
					is.getline(buffer, LINE_SIZE, '\n');
					str = buffer;
					str.TrimLeft();
					str.TrimRight();
					dat.m_lines.SetAtGrow(dat.m_lines.GetSize(), str);
				}
			}
			break;

		case 6:		// Trapezprofil
			is.getline(buffer, LINE_SIZE, '\n');
			str = buffer;
			str.TrimRight();
			str += " ";
			str.TrimLeft();
			for (i=0; i<6 && !str.IsEmpty(); i++)
			{
				dat.m_dVars.SetAtGrow(dat.m_dVars.GetSize(), atof(str.Left(str.Find(" "))));
				str = str.Right(str.GetLength()-str.Find(" "));
				str.TrimLeft();
			}
			break;

		case 7:		// Kreis
			is.getline(buffer, LINE_SIZE, '\n');
			str = buffer;
			str.TrimRight();
			str += " ";
			str.TrimLeft();
			for (i=0; i<4 && !str.IsEmpty(); i++)
			{
				dat.m_dVars.SetAtGrow(dat.m_dVars.GetSize(), atof(str.Left(str.Find(" "))));
				str = str.Right(str.GetLength()-str.Find(" "));
				str.TrimLeft();
			}
			break;

		case 8:		// Eiprofil
		case 9:		// Maulprofil
			is.getline(buffer, LINE_SIZE, '\n');
			str = buffer;
			str.TrimRight();
			str += " ";
			str.TrimLeft();
			for (i=0; i<5 && !str.IsEmpty(); i++)
			{
				dat.m_dVars.SetAtGrow(dat.m_dVars.GetSize(), atof(str.Left(str.Find(" "))));
				str = str.Right(str.GetLength()-str.Find(" "));
				str.TrimLeft();
			}
			break;

		case 10:	// Text auf ein Profillinie
			is.getline(buffer, LINE_SIZE, '\n');
			str = buffer;
			str.TrimRight();
			str += " ";
			str.TrimLeft();
			for (i=0; i<3; i++)
			{
				dat.m_nVars.SetAtGrow(dat.m_nVars.GetSize(), atoi(str.Left(str.Find(" "))));
				str = str.Right(str.GetLength()-str.Find(" "));
				str.TrimLeft();
			}
			for (i=0; i<dat.m_nVars[1]; i++)
			{
				is.getline(buffer, LINE_SIZE, '\n');
				str = buffer;
				str.TrimRight();
				str += " ";
				str.TrimLeft();
				dat.m_nVars.SetAtGrow(dat.m_nVars.GetSize(), atoi(str.Left(str.Find(" "))));
				str = str.Right(str.GetLength()-str.Find(" "));
				str.TrimLeft();
				str.TrimRight();
				dat.m_lines.SetAtGrow(dat.m_lines.GetSize(), str);
			}
			break;

		case 11:	// Darstellung von Hoehenwerten
			is.getline(buffer, LINE_SIZE, '\n');
			str = buffer;
			str.TrimRight();
			str += " ";
			str.TrimLeft();
			for (i=0; i<2; i++)
			{
				dat.m_nVars.SetAtGrow(dat.m_nVars.GetSize(), atoi(str.Left(str.Find(" "))));
				str = str.Right(str.GetLength()-str.Find(" "));
				str.TrimLeft();
			}
			is.getline(buffer, LINE_SIZE, '\n');
			str = buffer;
			str.TrimRight();
			str += " ";
			str.TrimLeft();
			for (i=0; i<dat.m_nVars[1]; i++)
			{
				dat.m_nVars.SetAtGrow(dat.m_nVars.GetSize(), atoi(str.Left(str.Find(" "))));
				str = str.Right(str.GetLength()-str.Find(" "));
				str.TrimLeft();
			}
			break;

		case 12:	// Sondertexte
      {
        is.getline( buffer, LINE_SIZE, '\n' );

        int refData = 0;
        int tsCount = 0;
        double hoehenBezug = 0.0;

        sscanf( buffer, "%d %d %lf", &refData, &tsCount, &hoehenBezug );

        DataBlock* refDB = dat.m_pProfil ? dat.m_pProfil->GetDataBlockByIndex( refData - 1 ) : 0;

        dat.m_textBlock = new TextBlock( refDB, hoehenBezug );
        for( int t = 0; t < tsCount; t++ )
        {
          is.getline( buffer, LINE_SIZE, '\n' );

          int lineCount = 0;
          int rahmenArt = 0;
          int rahmenJustierung = 0;
          int textJustierung = 0;
          int stationsNummer = 0;
          int textGroesse = 0;
    
          sscanf( buffer, "%d %d %d %d %d %d", &lineCount, &rahmenArt, &rahmenJustierung, &textJustierung, &stationsNummer, &textGroesse );

          TextBlock::TextSatz ts( rahmenArt, rahmenJustierung, textJustierung, stationsNummer, textGroesse );
          for( int l = 0; l < lineCount; l++ )
          {
            is.getline( buffer, LINE_SIZE, '\n' );
            ts.strings.Add( buffer );
          } // for l

          dat.m_textBlock->AddTextSatz( ts );
        } // for t

/*
        str = buffer;
        str.TrimRight();
        str += " ";
        str.TrimLeft();
        for (i=0; i<2; i++)
        {
          dat.m_nVars.SetAtGrow(i, atoi(str.Left(str.Find(" "))));
          str = str.Right(str.GetLength()-str.Find(" "));
          str.TrimLeft();
        }
        dat.m_dVars.SetAtGrow(0, atof(str.Left(str.Find(" "))));
        int count = 0;
        for (i=0; i<dat.m_nVars[1]; i++)
        {
          is.getline(buffer, LINE_SIZE, '\n');
          str = buffer;
          str.TrimRight();
          str += " ";
          str.TrimLeft();
          for( int j = i * 6 + 3; j < ( i + 1 ) * 6 + 3; j++ )
          {
            dat.m_nVars.SetAtGrow(j, atoi(str.Left(str.Find(" "))));
            str = str.Right(str.GetLength()-str.Find(" "));
            str.TrimLeft();
          }
          for (j=count; j<count+dat.m_nVars[i*6+3]; j++)
          {
            is.getline(buffer, LINE_SIZE, '\n');
            str = buffer;
            str.TrimRight();
            str.TrimLeft();
            dat.m_lines.SetAtGrow(j, str);
          }
          count += dat.m_nVars[i*6+3];
        }
*/
      }
      break;

		case 13:	// Bebauungsgrenzen
			is.getline(buffer, LINE_SIZE, '\n');
			str = buffer;
			dat.m_nVars.SetAtGrow(dat.m_nVars.GetSize(), atoi(str));
			for (i=0; i<dat.m_nVars[0]; i++)
			{
				is.getline(buffer, LINE_SIZE, '\n');
				str = buffer;
				str.TrimRight();
				str += " ";
				str.TrimLeft();
				for( int j = 0; j < 4; j++ )
				{
					dat.m_dVars.SetAtGrow(dat.m_dVars.GetSize(), atof(str.Left(str.Find(" "))));
					str = str.Right(str.GetLength()-str.Find(" "));
					str.TrimLeft();
				}
				is.getline(buffer, LINE_SIZE, '\n');
				str = buffer;
				str.TrimLeft();
				str.TrimRight();
				dat.m_lines.SetAtGrow(dat.m_lines.GetSize(), str);
			}
			break;

		case 14:	// Doppellinie
			is.getline(buffer, LINE_SIZE, '\n');
			str = buffer;
			str.TrimRight();
			str += " ";
			str.TrimLeft();
			for (i=0; i<2; i++)
			{
				dat.m_nVars.SetAtGrow(dat.m_nVars.GetSize(), atoi(str.Left(str.Find(" "))));
				str = str.Right(str.GetLength()-str.Find(" "));
				str.TrimLeft();
			}
			for (i=0; i<dat.m_nVars[1]; i++)
			{
				is.getline(buffer, LINE_SIZE, '\n');
				str = buffer;
				str.TrimRight();
				str += " ";
				str.TrimLeft();
				for( int j = 0; j < 3; j++ )
				{
					dat.m_dVars.SetAtGrow(dat.m_dVars.GetSize(), atof(str.Left(str.Find(" "))));
					str = str.Right(str.GetLength()-str.Find(" "));
					str.TrimLeft();
				}
			}
			break;

		case 16:	// NW-Rinne
			is.getline(buffer, LINE_SIZE, '\n');
			str = buffer;
			str.TrimRight();
			str += " ";
			str.TrimLeft();
			for (i=0; i<3; i++)
			{
				dat.m_dVars.SetAtGrow(dat.m_dVars.GetSize(), atof(str.Left(str.Find(" "))));
				str = str.Right(str.GetLength()-str.Find(" "));
				str.TrimLeft();
			}
			break;

		case 17:	// Kommentarzeilen
			for( i = 0; i < dat.m_nCoord; i++ ) // Anzahl Kommentarzeilen = Anzahl Koordinaten
			{
				is.getline( buffer, LINE_SIZE, '\n' );
				str = buffer;
				str.TrimLeft();
				str.TrimRight();
				dat.m_lines.SetAtGrow( dat.m_lines.GetSize(), str );
			}
			break;

		case 19:	// Armco71
		case 21:	// HAMCO Super-Span-Bogen Profil
		case 22:	// HAMCO Elliptische Profil
			is.getline(buffer, LINE_SIZE, '\n');
			str = buffer;
			str.TrimRight();
			str += " ";
			str.TrimLeft();
			for (i=0; i<5; i++)
			{
				dat.m_dVars.SetAtGrow(dat.m_dVars.GetSize(), atof(str.Left(str.Find(" "))));
				str = str.Right(str.GetLength()-str.Find(" "));
				str.TrimLeft();
			}
			is.getline(buffer, LINE_SIZE, '\n');
			str = buffer;
			str.TrimRight();
			str += " ";
			str.TrimLeft();
			for (i=0; i<4; i++)
			{
				dat.m_dVars.SetAtGrow(dat.m_dVars.GetSize(), atof(str.Left(str.Find(" "))));
				str = str.Right(str.GetLength()-str.Find(" "));
				str.TrimLeft();
			}
			break;

		case 20:	// HAMCO Maulprofil
			is.getline(buffer, LINE_SIZE, '\n');
			str = buffer;
			str.TrimRight();
			str += " ";
			str.TrimLeft();
			for (i=0; i<5; i++)
			{
				dat.m_dVars.SetAtGrow(dat.m_dVars.GetSize(), atof(str.Left(str.Find(" "))));
				str = str.Right(str.GetLength()-str.Find(" "));
				str.TrimLeft();
			}
			is.getline(buffer, LINE_SIZE, '\n');
			str = buffer;
			str.TrimRight();
			str += " ";
			str.TrimLeft();
			for (i=0; i<6; i++)
			{
				dat.m_dVars.SetAtGrow(dat.m_dVars.GetSize(), atof(str.Left(str.Find(" "))));
				str = str.Right(str.GetLength()-str.Find(" "));
				str.TrimLeft();
			}
			break;

    case 23: // pol. Grenzen
      // Format: Anzahl der Zeilen = Anzahl der Kooridnaten
      // jede Zeile: [d8.4] Station von; [d8.4] Station bis  [hex] color [int] style [int] hatch [String] Text
			for( i = 0; i < dat.m_nCoord; i++ ) // Anzahl Grenzen = Anzahl Koordinaten
			{
        DataBlock::Bereich b;

        is >> b.from;
        is >> b.to;
        is >> hex >> b.color;
        is >> b.style;
        is >> b.hatch;

        is.getline( buffer, LINE_SIZE, '\n' );
        b.text = CString( buffer );
        b.text.TrimRight();
        b.text.TrimLeft();

        dat.m_bereiche.Add( b );
			}
      break;

		default:
			break;
	}
	return is;
}

ostream& operator<<(ostream& os, DataBlock &dat)
{
	Coord *crd;
	int count, i, j;
	
	os.flags(ios::right);
	os.setf(ios::fixed);
	os.precision(4);
	// 1. Zeile
	os << dat.m_name[0] << endl;
	// 2. Zeile
	os << dat.m_name[1] << endl;
	// 3. Zeile
	for (i=0; i<9; i++)
		os << setw(3) << dat.m_nControlData[i];
	os << endl;
	// folgende Zeile
	switch (dat.m_nControlData[8])
	{
		case 0:		// coordinates (z.B. Gelaendehoehe)
		case 15:	// Kreissegment
			count = 0;
			for (i=0; i<dat.m_Coords.GetSize(); i++)
			{
				crd = dat.m_Coords.GetAt(i);
				os << setw(2) << crd->xs << " " << setw(12) << crd->dx << " ";
				count++;
				if (div(count, 8).rem==0 && count!=dat.m_Coords.GetSize())
					os << endl;
			}
			os << endl;
			count = 0;
			for (i=0; i<dat.m_Coords.GetSize(); i++)
			{
				crd = dat.m_Coords.GetAt(i);
				os << setw(2) << crd->ys << " " << setw(12) << crd->dy << " ";
				count++;
				if (div(count, 8).rem==0 && count!=dat.m_Coords.GetSize())
					os << endl;
			}
			os << endl;
			break;

		case 1:		// DATPLT
			for (i=0; i<dat.m_lines.GetSize(); i++)
				os << dat.m_lines[i] << endl;
			break;

		case 2:		// Kanalsymbol
			for (i=0; i<6; i++)
				os << dat.m_nVars[i] << " ";
			os << endl;
			if (dat.m_nVars[4]!=0)
			{
				if (dat.m_nVars[5]==0)
				{
					for (i=0; i<2; i++)
					{
						for (j=0; j<dat.m_nVars[4]; j++)
						{
							os << dat.m_nVars[6+j+i*dat.m_nVars[4]] << " ";
						}
						os << endl;
					}
				}
				else
				{
					for (j=0; j<dat.m_nVars[4]; j++)
						os << dat.m_nVars[6+j] << " ";
					os << endl;
				}
			}
			break;

		case 3:		// Rohre-entleerung,-entluftung
			for (i=0; i<2; i++)
			{
				os << dat.m_nVars[i] << " ";
			}
			os << endl;
			for (i=0; i<dat.m_nVars[1]; i++)
			{
				os << dat.m_nVars[2+i*2] << dat.m_nVars[2+i*2+1] << " ";
			}
			os << endl;
			break;

		case 4:		// Bauwerk
			for (i=0; i<2; i++)
			{
				os << dat.m_nVars[i] << " ";
			}
			os << endl;
			for (i=0; i<dat.m_nVars[1]; i++)
			{
				os << dat.m_nVars[2+i] << " ";
				for (j=0; j<3; j++)
					os << dat.m_dVars[i*3+j] << " ";
				os << endl;
			}
			break;

		case 5:		// Bemassung
			for (i=0; i<2; i++)
			{
				os << dat.m_nVars[i] << " ";
			}
			os << endl;
			for (i=0; i<dat.m_nVars[1]; i++)
			{
				for (j=0; j<2; j++)
				{
					os << dat.m_nVars[2+j+i*2] << " ";
				}
				os << dat.m_dVars[i] << endl;
				for (j=0; j<3; j++)
				{
					os << dat.m_lines[j+i*3] << endl;
				}
			}
			break;

		case 6:		// Trapezprofil
			for (i=0; i<dat.m_dVars.GetSize(); i++)
			{
				os << dat.m_dVars[i] << " ";
			}
			os << endl;
			break;

		case 7:		// Kreis
			for (i=0; i<dat.m_dVars.GetSize(); i++)
			{
				os << dat.m_dVars[i] << " ";
			}
			os << endl;
			break;

		case 8:		// Eiprofil
		case 9:		// Maulprofil
			for (i=0; i<dat.m_dVars.GetSize(); i++)
			{
				os << dat.m_dVars[i] << " ";
			}
			os << endl;
			break;

		case 10:	// Text auf ein Profillinie
			for (i=0; i<3; i++)
			{
				os << dat.m_nVars[i] << " ";
			}
			os << endl;
			for (i=0; i<dat.m_nVars[1]; i++)
			{
				os << dat.m_nVars[3+i] << " ";
				os << dat.m_lines[i] << endl;
			}
			break;

		case 11:	// Darstellung von Hoehenwerten
			for (i=0; i<2; i++)
			{
				os << dat.m_nVars[i] << " ";
			}
			os << endl;
			for (i=0; i<dat.m_nVars[1]; i++)
			{
				os << dat.m_nVars[2+i] << " ";
			}
			os << endl;
			break;

		case 12:	// Sondertexte
      {
        const TextBlock* tb = dat.m_textBlock;
        if( !tb )
           os << "0 0 0.0" << endl;
        else
        {
          DataBlock* bezugDB = tb->GetBezugsdatensatz();
          int dbRef = ( bezugDB && dat.m_pProfil ) ? dbRef = dat.m_pProfil->GetDataBlockIndex( bezugDB ) : -1;

          os << dbRef + 1 << ' ';
          os << tb->GetTextSatzCount() << ' ';
          os << tb->GetHoehenbezugspunkt() << endl;

          for( int t = 0; t < tb->GetTextSatzCount(); t++ )
          {
            const TextBlock::TextSatz& ts = tb->GetTextSatz( t );
            os << ts.strings.GetSize() << ' ';
            os << ts.rahmenArt << ' ';
            os << ts.rahmenJustierung << ' ';
            os << ts.textJustierung << ' ';
            os << ts.stationsNummer << ' ';
            os << ts.textGroesse << endl;
    
            for( int l = 0; l < ts.strings.GetSize(); l++ )
              os << ts.strings[l] << endl;
          } // for t
        }
      }
			break;

		case 13:	// Bebauungsgrenzen
			os << dat.m_nVars[0] << endl;
			for (i=0; i<dat.m_nVars[0]; i++)
			{
				for (j=0; j<4; j++)
				{
					os << dat.m_dVars[j+i*4] << " ";
				}
				os << endl;
				os << dat.m_lines[i] << endl;
			}
			break;

		case 14:	// Doppellinie
			for (i=0; i<2; i++)
			{
				os << dat.m_nVars[i] << " ";
			}
			os << endl;
			for (i=0; i<dat.m_nVars[1]; i++)
			{
				for (j=0; j<3; j++)
				{
					os << dat.m_dVars[j+i*3] << " ";
				}
				os << endl;
			}
			break;

		case 16:	// NW-Rinne
			for (i=0; i<3; i++)
			{
				os << dat.m_dVars[i] << " ";
			}
			os << endl;
			break;

		case 17:	// Kommentarzeilen
			for( i = 0; i < dat.m_lines.GetSize(); i++ )
				os << dat.m_lines[i] << endl;
			break;

		case 19:	// Armco71
		case 21:	// HAMCO Super-Span-Bogen Profil
		case 22:	// HAMCO Elliptische Profil
			for (i=0; i<5; i++)
			{
				os << dat.m_dVars[i] << " ";
			}
			os << endl;
			for (i=0; i<4; i++)
			{
				os << dat.m_dVars[5+i] << " ";
			}
			os << endl;
			break;

		case 20:	// HAMCO Maulprofil
			for (i=0; i<5; i++)
			{
				os << dat.m_dVars[i] << " ";
			}
			os << endl;
			for (i=0; i<6; i++)
			{
				os << dat.m_dVars[5+i] << " ";
			}
			os << endl;
			break;

    case 23: // pol. Grenzen
      // Format: Anzahl der Zeilen = Anzahl der Kooridnaten
      // jede Zeile: [d8.4] Station von; [d8.4] Station bis  [hex] color [int] style [int] hatch [String] Text
			for( i = 0; i < dat.GetBereichCount(); i++ ) // Anzahl Grenzen = Anzahl Koordinaten
			{
        DataBlock::Bereich b = dat.GetBereich( i );
        os << b.from << ' ' << b.to << ' ' << hex << b.color << dec << ' ' << b.style << ' ' << b.hatch << ' ' << b.text << endl;
			}
      break;


		default:
			break;
	}

	return os;
}

Coord* DataBlock::GetCoord(double ycrd, double dy)
// findet eine Coordinate im Datenblock
// Parameter:
//        double ycrd: die gesuchte Koordinate soll diesen y-Wert haben
//        double dy:    maximale toleranz der Suche
// Rückgabewert:
//          Coord*: zeigt auf die gefundene koordinate, NULL bei Misserfolg
{
  Coord *pCrd;

	m_nCoordIndex = 0;
  while ((pCrd = GetNextCoord()) != NULL)
  {
    if ((fabs(pCrd->dx - ycrd)) <= dy)
      return pCrd;
  };
  return NULL;
}

void DataBlock::SetCoordAt(int n, Coord *crd)
{
	if (n>=0 && n<m_Coords.GetSize())
		m_Coords.SetAt(n, crd);
	if (m_pProfil!=NULL)
		m_pProfil->SetModified();
}

CRect DataBlock::GetExtent()
// gibt die Ausdehnung des Datenblocks in mm zurück
// Rückgabewert:
//          die Ausdehnung in mm
{
  CRect rect( 1000000000, -1000000000, -1000000000, 1000000000 );
  switch( GetType() )
  {
  case DST_GELAENDEHOEHE:
    {
      for ( int i = 0; i < GetNumCoords(); i++ )
      {
        Coord* crd = GetCoordAt( i );
        CPoint point( (int)(crd->dx * 1000), (int)(crd->dy * 1000) );

        rect.left = min( rect.left, point.x );
        rect.right = max( rect.right, point.x );
        rect.top = max( rect.top, point.y );
        rect.bottom = min( rect.bottom, point.y );
      }; //for i
    }; // DST_GELAENDEHOEHE
    break;

  case DST_WSP_HOEHE:
    {
      for( int i = 0; i < GetNumCoords(); i++ )
      {
        Coord* crd = GetCoordAt( i );
        int hoehe = (int)(crd->dy * 1000);

        rect.top = max( rect.top, hoehe );
        rect.bottom = min( rect.bottom, hoehe );
      }; // for i
    };
    break; // DST_WSP_HOEHE

  case DST_KREIS:
    {
      int y = (int)( GetKreisY() * 1000.0 ); // y/z: tiefster Scheitelpunkt
      int z = (int)( GetKreisZ() * 1000.0 );
      int radius = (int)( GetKreisDurchmesser() / 2 * 1000 );
      rect.left = min( rect.left, y - radius );
      rect.right = max( rect.right, y + radius );
      rect.top = max( rect.top, z + 2 * radius );
      rect.bottom = min( rect.bottom, z );
    }; // DST_KREIS
    break;

  default:
    {
    }; // default

  }; // switch type

  return rect;
}; // GetExtent

void DataBlock::Paint( CDC* dc, CRect* extent )
// zeichnet das Profil in einen DeviceKontext
// Parameter:
//        CDC* dc: der Device Kontext: der Mapping mode etc. müssen so gewählt sein, dass
//                  - das Profil seine Koordinaten als logische Koordinaten benutzen kann
//                  - die clipBox komplett bezeichnet werden darf
//        CRect* exten: die Aussmasse des Profils ( wird übergeben, damit nicht jedesmal neu ausgerechent werden muss )
{
  COLORREF color = RGB( 0, 0, 0 );
  COLORREF fillColor = RGB( 255, 255, 255 );
  UINT lineSize = 1;
  
  switch ( GetType() )
  {
  case DST_GELAENDEHOEHE:
    color = RGB( 255, 0, 128 );
    lineSize = 2;
    break;

  case DST_WSP_HOEHE:
    color = RGB( 0, 0, 255 );
    lineSize = 2;
    break;

  case DST_KREIS:
    color = RGB( 0, 0, 0 );
    fillColor = RGB( 255, 255, 6 ); // GELB
    lineSize = 2;
    break;
  }; // switch type
  
  CBrush* oldBrush = NULL;
  CPen* oldPen = NULL;

  CPen linePen, fillPen;
  if ( !linePen.CreatePen( PS_SOLID | PS_JOIN_ROUND, lineSize, color ) || 
       !fillPen.CreatePen( PS_SOLID, 1, RGB( 128, 128, 128 ) ) )
    return;
  CBrush fillBrush( HS_CROSS, fillColor );

  switch( GetType() )
  {
  case DST_GELAENDEHOEHE:
    {
      LPPOINT points = (LPPOINT)malloc( ( GetNumCoords() ) * sizeof( POINT ) );
      Coord* crd = GetFirstCoord();
      UINT count = 0;
      while( crd )
      {
        points[count].x = (int)(crd->dx * 1000);
        points[count].y = (int)(crd->dy * 1000);
        count++;
        crd = GetNextCoord();
      }; // while crd

      // Füllung zeichnen
      oldPen = (CPen*)dc->SelectObject( &fillPen );
      for( UINT i = 0; i < count; i++ )
      {
        dc->MoveTo( points[i] );
        dc->LineTo( points[i].x, extent->bottom );
      }; // for i
      
      // Gelände zeichnen
      dc->SelectObject( &linePen );
      dc->Polyline( points, count );
      
      free( points );
    }; // DST_GELAENDEHOHE
    break;


  case DST_WSP_HOEHE:
    {
      oldPen = (CPen*)dc->SelectObject( &linePen );
      Coord* crd = GetFirstCoord();
      while( crd )
      {
        dc->MoveTo( (int)(crd->dx * 1000), (int)(crd->dy * 1000) );
        crd = GetNextCoord();
        if ( crd )
        {
          dc->LineTo( (int)(crd->dx * 1000), (int)(crd->dy * 1000) );
          crd = GetNextCoord();
        }; // if coord
        
      }; // while crd
    }; // DST_WSP_HOEHE
    break;

  case DST_KREIS:
    {
      oldPen = (CPen*)dc->SelectObject( &linePen );
      oldBrush = (CBrush*)dc->SelectObject( &fillBrush );

      int y = (int)( GetKreisY() * 1000.0 ) ;
      int z = (int)( GetKreisZ() * 1000.0 );
      int radius = (int)( GetKreisDurchmesser() / 2 * 1000 );

      dc->Ellipse( y - radius, z + radius * 2, y + radius, z );
    }; // case DST_KREIS
    break;

  default:
    break;
  }; // switch type

  if ( oldPen )
    dc->SelectObject( oldPen );
  if ( oldBrush )
    dc->SelectObject( oldBrush );
}; // Paint

void DataBlock::SetProfil( Profil* profil )
{
  m_pProfil = profil;
}; // SetProfil

Profil* DataBlock::GetProfil()
{
  return m_pProfil;
}; // GetProfil

void DataBlock::SortCoordsByXs( BOOL bDeleteXs /* = TRUE */ )
// sortiert die Koordinaten nach Xs
// Parameter:
//          BOOL bDeleteXs: falls TRUE, werden alle Xs nach der Operation auf 0 gesetzt
{
  // die brutale Methode: alle Coordinaten in einen Vector kopieren, den Vector sortieren 
  // und danach wieder zurückholen
  std::vector<Coord> CrdVector;
  CrdVector.reserve( m_Coords.GetSize() );
  
  for( int i = 0; i < m_Coords.GetSize(); i++ )
  {
    CrdVector.push_back( Coord( *m_Coords[i] ) );
    delete m_Coords[i];
  };
  m_Coords.RemoveAll();

  std::sort( CrdVector.begin(), CrdVector.end() );

  for( i = 0; i < CrdVector.size(); i++ )
  {
    Coord* crd = new Coord( CrdVector[i] );
    if( bDeleteXs )
      crd->xs = 0;
    m_Coords.Add( crd );
  }; // for i
};

BOOL DataBlock::CheckData( CString& errorString )
// Überprüft die Daten des Datenblocks und bereinigt sie notfalls falls möglich
// Parameter:
//        CString& errorString: falls der Datenblock nicht bereinigt werden konnte wird hier eine Fehlermeldung abgelegt
// Rückgabewert:
//        BOOL: FALSE, falls der Datenblock nicht in Ordnung ist
// Bemerkung:
//        die Koordinaten müssen in der richtigen Reihenfolge vorliegen
//
//        folgende Datenblöcke werden überprüft, und so gegbenenfalls bereinigt:
//  DST_DURCHST_BEREICHE: muss genau zwei Koordinaten haben
//  DST_TRENNFLAECHEN: muss genau zwei Koordinaten haben; dy der ersten muss 1 oder 3, dy der zweiten 2 oder 4 sein
//                      dies wird im zweifelsfall einfach auf 1 bzw 2 gesetzt
{
  // zuerst Koordinatenanzahlen ueberpruefen: KO - Kriterium
  switch( GetType() )
  {
  case DST_DURCHST_BEREICH:
  case DST_TRENNFLAECHEN:
    if( GetNumCoords() != 2 )
    {
      errorString.Format( IDS_DATA_ERROR_NUM_COORD, 2 );
      return FALSE;
    };
    break;
  }; // switch type

  // jetzt noch ein paar weitere Checks: reparable Fehler
  switch( GetType() )
  {
  case DST_TRENNFLAECHEN:
    {
      // dy der beiden Coordinaten auf Werte zwischen 1 und 4 setzen: die erste 1 oder 3, die zweite 2 oder 4
      int count = 0;

      for( int i = 0; i < GetNumCoords(); i++ )
      {
        Coord* crd = GetCoordAt( i );
        if( crd )
        {
          if( crd->dy < 3 )
            crd->dy = 1 + i;
          else
            crd->dy = 3 + i;
        }; // if crd
      }; // for i
    }; // DST_TRENNFLAECHEN
    break;
  }; // switch type

  return TRUE;
}; // CheckData

BOOL DataBlock::CheckAllYZero()
// Prüft, ob alle Koordinaten im Profil 0.0 als y-Wert haben
// Rückgabewert:
//        TRUE, wenn alle 0 sind, sonst FALSE
{
  for( int i = 0; i < m_Coords.GetSize(); i++ )
  {
    Coord* crd = m_Coords[i];
    if( crd->dy != 0.0 )
      return FALSE;
  } // for i

  return TRUE;
} // CheckAllXZero




/*!
 * Gibt eine Kommentarzeile zurück
 *
 * @param index : der Index der Zeile
 *
 * @return CString  : die kommentarzeile, "", falls index nicht ok
 */
CString DataBlock::GetCommentLine( const int index ) const
{
  if( 0 <= index && index < GetCommentCount() )
  {
    CString text = GetLine( index );
    if( text.GetLength() > 3 )
      return text.Right( text.GetLength() - 3 );
  };

  return ""; // Fehler
}; // GetCommentLine

/* Fügt eine neue Kommentarlinie ein
 * Parameter:
 *			CString commentLine
 *
 * Rückgabewerte:
 *			void
 *
 * Erklärung:
 * 
 * 
 * 
 * 
*/
void DataBlock::SetCommentLine( const CString& commentLine )
{
  m_lines.Add( "CC " + commentLine );
  int anz = m_lines.GetSize();
  SetControlData( 7, anz );
}

void DataBlock::AddTextBlock( const int dbIndex, const int stationIndex, const CStringArray& strings )
{
  m_nVars.SetAtGrow( 0, dbIndex );

  int count = 0;
  if( m_nVars.GetSize() > 1 )
    count = m_nVars[1];
  m_nVars.SetAtGrow( 1, count + 1 );
  m_dVars.SetAtGrow( 2, 0.0 );

  m_nVars.SetAtGrow( count * 6 + 3, strings.GetSize() );
  m_nVars.SetAtGrow( count * 6 + 4, 0 );
  m_nVars.SetAtGrow( count * 6 + 5, 2 );
  m_nVars.SetAtGrow( count * 6 + 6, 2 );
  m_nVars.SetAtGrow( count * 6 + 7, stationIndex );
  m_nVars.SetAtGrow( count * 6 + 8, 2 );
  
  for( int i = 0; i < strings.GetSize(); i++ )
    m_lines.Add( strings[i] );
}

/* Verändert die Kontrollparameter eines Datenblocks
 * Parameter:
 *			int position, int value
 *
 * Rückgabewerte:
 *			void
 *
 * Erklärung:
 * Verändert die Kontrollparameter eines Datenblocks (z.B. Anzahl der Textzeilen[0], zugehöriges Profil [4]) 
 * 
 * 
 * 
*/
void DataBlock::SetControlData(int position, int value) { m_nControlData[position] = value; }



/*!
 * Gibt die bildnummer des Bilders in der Bilderliste passend zu diesem Datenblock zurück
 *
 * @param none
 *
 * @return int  : die Nummer des Bildes
 */
int DataBlock::GetImageType()
{
  return GetImageType( m_nType );
}; // GetImageType

/*!
 * Gibt zu einem bestimmten Datenblocktyp die Nummer des Bildes in der bilderliste zurück
 *
 * @param nType : Datenblocktyp
 *
 * @return int : Nummer des entsprechenden Bildes
 */
/* static */ int DataBlock::GetImageType( int nType )
{
	switch( nType )
	{
		case DST_GELAENDEHOEHE:
		case DST_GELAENDE2:
		case DST_FLAECHE:
			return IMAGE_GELAENDEHOEHE;

		case DST_TRENNFLAECHEN:
		case DST_TRENN_WEHR:
			return IMAGE_TRENNFLAECHEN;

		case DST_RAUHIGKEIT:
		case DST_RAUHIGKEIT_KST:
			return IMAGE_RAUHIGKEIT;

		case DST_DURCHST_BEREICH:
			return IMAGE_DURCHST_BEREICH;

		case DST_UK_BRUECKE:
		case DST_DKUK:
			return IMAGE_UK_BRUECKE;

		case DST_OK_BRUECKE:
		case DST_DKOK:
			return IMAGE_OK_BRUECKE;

		case DST_AXM:
		case DST_AYM:
		case DST_DPM:
			return IMAGE_BEWUCHS;

		case DST_OK_WEHRS:
			return IMAGE_OK_WEHRS;

		case DST_KREISSEGM:
			return IMAGE_KREISSEGM;

		case DST_OK_GELAENDE:
			return IMAGE_OK_GELAENDE;

		case DST_KASTEN:
			return IMAGE_KASTEN;

		case DST_GAUSS:
			return IMAGE_GAUSS;

		case DST_RECHTSWERT:
		case DST_RECHTSWERT_2:
		case DST_RE_BOESCHUNG_RE:
		case DST_RE_BOESCHUNG_LI:
		case DST_RE_BOESCHUNG_RE_2:
		case DST_RE_BOESCHUNG_LI_2:
		case DST_RE_DEICH_RE:
		case DST_RE_DEICH_LI:
		case DST_RE_DEICH_RE_2:
		case DST_RE_DEICH_LI_2:
			return IMAGE_RECHTSWERT;

		case DST_HOCHWERT:
		case DST_HOCHWERT_2:
		case DST_HO_BOESCHUNG_RE:
		case DST_HO_BOESCHUNG_LI:
		case DST_HO_BOESCHUNG_RE_2:
		case DST_HO_BOESCHUNG_LI_2:
		case DST_HO_DEICH_RE:
		case DST_HO_DEICH_LI:
		case DST_HO_DEICH_RE_2:
		case DST_HO_DEICH_LI_2:
			return IMAGE_HOCHWERT;

		case DST_PUNKT_NR:
			return IMAGE_PUNKT_NR;

		case DST_WSP_HOEHE:
			return IMAGE_WSP_HOEHE;

		case DST_WASSERSP1:
		case DST_WASSERSP100:
		case DST_WASSERSP5:
		case DST_WASSERSPIEGEL:
		case DST_WASSERSPIEGEL_2:
		case DST_WSP_FIXIERUNG:
			return IMAGE_WASSERSPIEGEL;

		case DST_BORDVOLL:
			return IMAGE_BORDVOLL;

		case DST_MAUL:
			return IMAGE_MAUL;

		case DST_EIPROFIL:
		case DST_ARMCO84:
		case DST_ARMCO71:
			return IMAGE_EIPROFIL;

		case DST_KREIS:
			return IMAGE_KREIS;

		case DST_TRAPEZ:
			return IMAGE_TRAPEZ;

		case DST_SOHLHOEHE:
		case DST_SOHLHOEHE_2:
			return IMAGE_SOHLHOEHE;

		case DST_LAENGE:
			return IMAGE_LAENGE;

		case DST_ABFLUSS:
			return IMAGE_ABFLUSS;

    case DST_WSP_BREITE:
			return IMAGE_WSP_BREITE;

		case DST_BOESCHUNG_LINKS:
		case DST_BOESCHUNG_LINKS_2:
			return IMAGE_BOESCHUNG_LINKS;

		case DST_BOESCHUNG_RECHTS:
		case DST_BOESCHUNG_RECHTS_2:
			return IMAGE_BOESCHUNG_RECHTS;

		case DST_BAUWERK:
			return IMAGE_BAUWERK;

		case DST_WSP_MESSUNG:
			return IMAGE_WSP_MESSUNG;

		case DST_DEICH_LINKS:
		case DST_DEICH_LINKS_2:
		case DST_AUSUFERUNG_LI:
			return IMAGE_DEICH_LINKS;

		case DST_DEICH_RECHTS:
		case DST_DEICH_RECHTS_2:
		case DST_AUSUFERUNG_RE:
			return IMAGE_DEICH_RECHTS;

		case DST_STATION:
			return IMAGE_STATIONIERUNG;

		default:
			return IMAGE_TEXT;
	}
} // GetImageType


/*!
 * Gibt den Namen des Wasserspiegels zurück
 *
 * Sollte nur bei DatenBlöcken benutzt werden,
 * die tatsächlich ein WSP sind.
 * Gibt Right( 100 ) der zweiten Zeile der
 * Datenblockbeschriftung zurück.
 *
 * @return CString  : 
 */
CString DataBlock::GetWSPName() const
{
  ASSERT( GetType() == DST_WSP_HOEHE || GetType() == DST_WASSERSPIEGEL );

  CString name1 = GetName( 1 );
  if( name1.GetLength() > 100 )
    return name1.Mid( 99 );
  else
    return CString();
}; // GetWSPName


/*!
 * Setzt den Namen des Wasserspiegels.
 * 
 * Sollte nur bei Datenblöcken benutzt werden, die ein
 * WSP sind. Setzt den Namen in die zweite Zeile der
 * Beschriftung ab Spalte 100
 *
 * @param name : der neue Name
 *
 */
void DataBlock::SetWSPName( const CString& name )
{
  ASSERT( GetType() == DST_WSP_HOEHE || GetType() == DST_WASSERSPIEGEL );
  
  CString name1 = GetName( 1 ).Left( 99 );
  int missing = 99 - name1.GetLength();
  SetName( name1 + CString( ' ', missing ) + name, 1 );
}; // SetWSPName



/*!
 * Spiegelt den Datenblock am Nullpunkt.
 *
 * @todo : restliche Sonderprofiltypen implementieren
 */
void DataBlock::FlipHorizontal()
{
  switch( GetSonderProfilType() )
  {
  case 0:
  case 15:	// Kreissegment
    {
      // Sonderfall für BV und Trefnnflächen: merken, welche Kennung die haben
      bool bSetLage = false;
      int lage1, lage2;
      if( ( m_nType == DST_TRENNFLAECHEN || m_nType == DST_BORDVOLL ) && m_Coords.GetSize() == 2 )
      {
        lage1 = int( ( m_Coords[0]->dy - 1 ) / 2 );
        lage2 = int( ( m_Coords[1]->dy - 2 ) / 2 );

        bSetLage = true;
      }

      // alles was nicht Sonderprofil ist: einfach die Koordinaten spiegeln
      for( int i = 0; i < m_Coords.GetSize(); i++ )
      {
        Coord* crd = m_Coords[i];
        crd->dx *= -1;
        
        // Markierung für Sortierung
        crd->xs = -i;
      }

      SortCoordsByXs();


      // Sonderfall für TF und BV
      if( bSetLage )
      {
        m_Coords[0]->dy = 1 + 2 * lage1;
        m_Coords[1]->dy = 2 + 2 * lage2;
      }
    }
    break;

		case 6:		// Trapezprofil
      m_dVars[4] *= -1;
			break;

		case 7:		// Kreis
			m_dVars[2] *= -1;
			break;

		case 8:		// Eiprofil
		case 9:		// Maulprofil
			m_dVars[3] *= -1;
			break;
    
  default:
    break;
  }
}
