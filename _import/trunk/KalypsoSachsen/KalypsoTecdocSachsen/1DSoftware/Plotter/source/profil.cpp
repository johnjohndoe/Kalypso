/*! Time-stamp: <@(#)profil.cpp   27.08.02 - 14:32:43   Belger>
 *********************************************************************
 *  @file   : profil.cpp
 *
 *  Project : WSPWIN
 *
 *  Package : WSPWIN Plotter
 *
 *  Company : BCE
 *
 *  Author  : Belger                              Date: 27.08.02
 *
 *  Purpose : Implementation der Klasse CProfil
 *
 *********************************************************************
 */
// Profil.cpp: Implementierung der Klasse CProfil.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "..\..\commonMfc\commonMfc.h"
#include "..\..\wspprj\wspprj.h"

#include "draw.h"
#include "Profil.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

////////////////
// Konstanten //
////////////////

/* static */ UINT CProfil::LCOMMENT_DIST = 50;

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CProfil::CProfil()
{
  m_rangeDbType = DST_UNKNOWN;
  m_dFrom = m_dTo = 0;
  m_dLowPoint = 0;
  m_bAutoHeight = TRUE;
  m_dHeight = 0;
  m_bAutoAnfang = TRUE;
  m_bAutoEnde = TRUE;
  m_dMaxHeight = 0;
  m_nProfilFormat = maxStations;
  m_align = CPlotArranger::SW;
}

///////////////
// Attribute //
///////////////

CSize CProfil::GetRealSize( double xScale, double yScale, CView* pView )
// gibt die tatsächlichen Aussmasse des Profils Millimetern ( Papiergrösse ) zurück
// Die Members m_rectVisible und m_lCommentHeight müssen richtig gesetzt sein
{
  UINT x = 1000 * MM_FACTOR * m_rectVisible.Width() / xScale;
  UINT y = 1000 * MM_FACTOR * m_rectVisible.Height() / yScale;

  // die LComments anpassen
  int maxSize = 0;
  POSITION pos = GetHeadPosition();
  while( pos )
  {
    CDrawObj* pObj = GetNextObject( pos );
    if( pObj->GetType() == DST_LP_TEXT && !pObj->IsInvisible() )
    {
      CSize size = ((CDrawRect*)pObj)->GetOutputTextSize( pView );

      double logSize = size.cy * yScale / ( 1000 * MM_FACTOR ); // die Grösse in Logischen Koordinaten
      double logTop = pObj->m_dPosition.bottom + logSize;

      double overSize = logTop - m_rectVisible.top;
      if( overSize > 0 )
      {
        int overHeight = 1000 * MM_FACTOR * overSize / yScale;
        maxSize = max( maxSize, overHeight );
      }
    };
  }; // while pos

  return CSize( x, y + maxSize );
}; // GetRealSize

/*!
 * Sucht aus dem Profil raus, welche Datenblöcke vorhanden sind
 *
 * @param dbArray : dieses Array wird gefüllt
 */
void CProfil::GetDataBlocks( CArray<int, int>& dbArray ) const
{
  CArray<int, int> helpArray; // Hilfsarray: zum zusammensuchen der DB-Typen: helpArray[i] == 1 genau dann, wenn der Datenblocktyp i da ist
  POSITION pos = GetHeadPosition();
  while( pos != NULL )
  {
    CDrawObj* pObj = GetNextObject( pos );
    if( pObj != NULL )
    {
      int type = pObj->GetType();
      if( type >= 0 ) // die User-Objekte haben DST_UNKNOWN == -4
        helpArray.SetAtGrow( type, 1 );
    }
  } // while pos

  // jetzt zu einem Array zusammentragen
  dbArray.RemoveAll();
  for( int i = 0; i < helpArray.GetSize(); i++ )
  {
    if( helpArray[i] == 1 )
      dbArray.Add( i );
  } // for i
} // GetDataBlocks

/*!
 * Rechnet aus, welchen Bereich ein bestimmter DatenBlockTyp einnimmt
 *
 * @param dbType : Typ des Datenblocks
 * @param from : hier wird die kleinste y-Koordinate des DB abgelegt
 * @param to : hier die Grösste
 */
void CProfil::GetDbRange( int dbType, double& from, double& to ) const
{
  double newFrom = DBL_MAX; // sehr gross
  double newTo = -DBL_MAX; // sehr klein

  CMapDouble mins, maxs; // werden für CalcBounds benötigt, sonst nicht
  
  POSITION pos = GetHeadPosition();
  while( pos != NULL )
  {
    CDrawObj* pObj = GetNextObject( pos );
    if( pObj != NULL && pObj->GetType() == dbType )
    {
      CDoubleIRect dPosition( pObj->CalcBounds( mins, maxs ) );
      CDoubleIRect extent( dPosition );
      newFrom = min( newFrom, extent.left );
      newTo = max( newTo, extent.right );
    } // if pObj
  }; // while pos

  if( newFrom < DBL_MAX && newTo > -DBL_MAX )
  {
    from = newFrom;
    to = newTo;
  };
} // GetDbRange

/////////////////
// Operationen //
/////////////////

void CProfil::AddObject( CDrawObj* pObj )
{
  AddTailObject( pObj );
}

BOOL CProfil::RemoveObject( CDrawObj* pObj )
{
  // Find and remove from document
  POSITION pos = FindObject( pObj );
  if( pos != NULL )
  {
    RemoveObjectAt( pos );
    return TRUE;
  }
  else
    return FALSE;
} // Remove

CDoubleIRect CProfil::CalcProfilSizes( CView* pView )
// Rechnet die Grösse des neu Profils aus 
// Passt Grössen der Trennflächen etc. an die errechnete Höhe an
{
  CMapDouble maxStations;		// maps stationierung to highest point
  CMapDouble minStations; // maps stationierung to lowest point
  CMapDouble stations; // Die Höhe der jeweiligen Station
  
  BOOL bGelaendeVisible = FALSE;
  
  BOOL bXCalculated = FALSE,
    bYCalculated = FALSE;
  
  // Set profil size (so that default scales can be correctly calculated)
  CDoubleIRect rect( 1e36, -1e36, -1e36, 1e36 );
  POSITION pos = GetHeadPosition();
  while( pos!=NULL )
  {
    CDrawObj* pObj = GetNextObject( pos );

    // nur sichtbare Objekte zählen
    if( !pObj->IsInvisible() )
    {
      BOOL bX = FALSE; // ob x-Werte berücksichtigt werden sollen
      BOOL bY = FALSE; // ob y-Werte berücksichtigt werden sollen
      BOOL bStations = FALSE; // sollen bei diesem Datensatz die y-Werte adaptiert werden

      int type = pObj->GetType();

      switch( type )
      {
      case DST_GELAENDEHOEHE:		// polylines are drawn
      case DST_UK_BRUECKE:
      case DST_OK_BRUECKE:
      case DST_OK_WEHRS:
      case DST_GELAENDE2:
      case DST_OK_GELAENDE:
      case DST_KASTEN:
      case DST_WSP_HOEHE:
      case DST_WASSERSP1:
      case DST_WASSERSP100:
      case DST_WASSERSP5:
      case DST_SOHLHOEHE:
      case DST_WASSERSPIEGEL:
      case DST_BOESCHUNG_LINKS:
      case DST_BOESCHUNG_RECHTS:
      case DST_BVHOEHE:
      case DST_SOHLHOEHE_2:
      case DST_WASSERSPIEGEL_2:
      case DST_BOESCHUNG_LINKS_2:
      case DST_BOESCHUNG_RECHTS_2:
      case DST_WSP_FIXIERUNG:
      case DST_WSP_MESSUNG:
      case DST_DEICH_LINKS:
      case DST_DEICH_RECHTS:
      case DST_DEICH_LINKS_2:
      case DST_DEICH_RECHTS_2:
      case DST_AUSUFERUNG_LI:
      case DST_AUSUFERUNG_RE:
      case DST_ENERGIEHOEHE:
      case DST_ABFLUSS: //12.04.2000 Dick
        {
          if( pObj->IsKindOf(RUNTIME_CLASS(CDrawPoly)) && ((CDrawPoly*)pObj)->GetShape()==CDrawPoly::polygon)
            break;
          
          if( type == DST_GELAENDEHOEHE || pObj->GetType() == DST_SOHLHOEHE )
            bGelaendeVisible = TRUE;

          if( type != DST_ABFLUSS && type != DST_ENERGIEHOEHE )
            bStations = TRUE;

          bX = bY = TRUE;
        }
        break;

      case DST_TRENNFLAECHEN:
      case DST_DURCHST_BEREICH:
      case DST_BOESCHUNGSKANTEN:
      case DST_MODELLGRENZEN:
      case DST_BORDVOLL:
      case DST_TRENN_WEHR:
      case DST_LP_TEXT:
        bX = TRUE;
        break;
        
      case DST_KREISSEGM:
      case DST_KREIS:
      case DST_MAUL:
      case DST_EIPROFIL:
      case DST_TRAPEZ:
      case DST_DKUK:
      case DST_DKOK:
      case DST_BAUWERK:
      case DST_UNKNOWN: // im Profil fixierte User-Objekte
        bX = bY = TRUE;
        break;

      default:
        break;
      }; // switch type


      // jetzt Abhängig von der ObjektArt die Grössen setzen
      CMapDouble mins, maxs; // Hilfsmaps
      CDoubleIRect objBounds( pObj->CalcBounds( mins, maxs ) );

      // je nachdem ob x und oder y Werte berücksichtigt werden sollen, jetzt Grössen anpassen
      if( bX )
      {
        bXCalculated = TRUE;

        rect.left = min( rect.left, objBounds.left );
        rect.right = max( rect.right, objBounds.right );
      } // if bX
      
      if( bY )
      {
        bYCalculated = TRUE;

        rect.bottom = min( rect.bottom, objBounds.bottom );
        rect.top = max( rect.top, objBounds.top );

        // die gemerkten Minimas und Maximas anpassen
        minStations.SetMins( mins );
        maxStations.SetMaxs( maxs );
      } // if bY

      // die Stationswerte anpassen
      if( bStations )
      {
        switch( m_nProfilFormat )
        {
        case CProfil::minStations:
          stations.SetMins( mins );
          break;
          
        case CProfil::stationsToGround:
          if( pObj->GetType() == DST_GELAENDEHOEHE || pObj->GetType() == DST_SOHLHOEHE )
            stations.AddMap( maxs );
          break;
          
        case CProfil::maxStations:
          stations.SetMaxs( maxs );
          break;
        } // switch m_nProfilFormat
      }; // if bStations

    } // if !pObj->IsInvisible
  } // while position


  if (!bXCalculated)
    rect.left = rect.right = 0;
  if (!bYCalculated)
    rect.top = rect.bottom = 0;

  SetTotalRect( rect );

  // linke / rechte Ausdehnung festlegen
  if( m_dFrom >= m_dTo )
  {
    m_dFrom = rect.left;
    m_dTo = rect.right;
  }


  if( m_rangeDbType == DST_UNKNOWN )
  {
    if( m_bAutoAnfang )
      m_dFrom = rect.left;
    if( m_bAutoEnde )
      m_dTo = rect.right;
  }
  else
    GetDbRange( m_rangeDbType, m_dFrom, m_dTo );
  
  if( m_dFrom < rect.left || m_dFrom >= rect.right )
    m_dFrom = rect.left;
  if( m_dTo <= rect.left || m_dTo > rect.right )
    m_dTo = rect.right;

  // die Breite des tatsächlich angezeigten Profils setzen
  if( bXCalculated )
  {
    if( rect.left < m_dFrom )
      rect.left = m_dFrom;

    if( rect.right > m_dTo )
      rect.right = m_dTo;
  }


  // obere / untere Ausdehnung bestimmen

  // die Höhe des tatsächlich angezeigten Profils setzen
  // dazu werden die Höhen der Stationen berücksichtigt die Innerhalb des gewählten
  // Bereichs liegen, sowie die erste und letzte ausserhalb
  if( bYCalculated )
  {
    // zuerst rausfinden, welches die beiden Stationen sind, die gerade ausserhalb des Bereichs liegen
    // d.h. dFrom ist das Infimum von m_dFrom und dTo das Supremum von m_dTo
    double dFrom = -1e36;
    double dTo = 1e36;

    POSITION sPos = maxStations.GetStartPosition();
    while( sPos != NULL )
    {
      double xPos, yPos;
      maxStations.GetNextAssoc( sPos, xPos, yPos );

      // dFrom = das grösste, welches noch kleiner als m_dFrom ist
      if( xPos <= m_dFrom && xPos > dFrom )
        dFrom = xPos;

      // dTo = das kleinste, welches grösser als m_dTo ist
      if( xPos >= m_dTo && xPos < dTo )
        dTo = xPos;
    } // while sPos

    // Top und bottom neu festlegen
    double newTop = -1e36;
    POSITION hPos = maxStations.GetStartPosition();
    double lastY = -1e36;

    while( hPos != NULL )
    {
      double xPos, yPos;
      maxStations.GetNextAssoc( hPos, xPos, yPos );
      if( xPos >= dFrom && xPos <= dTo )
        newTop = max( newTop, yPos );
    }

    double newBottom = 1e36;
    POSITION dPos = minStations.GetStartPosition();
    while( dPos != NULL )
    {
      double xPos, yPos;
      minStations.GetNextAssoc( dPos, xPos, yPos );
      if( xPos >= dFrom && xPos <= dTo )
        newBottom = min( newBottom, yPos );
    }

    if( newTop > -1e36 )
      rect.top = newTop;
    if( newBottom < 1e36 )
      rect.bottom = newBottom;
  } // if bYCalculated

  // offset bottom of profil size to the next lowest whole number
  double bottom = floor( rect.bottom );
  if( bottom == rect.bottom )
    rect.bottom -= 1;
  else
    rect.bottom = bottom;

  m_dMaxHeight = rect.bottom;
  if( m_bAutoHeight )
    m_dHeight = rect.bottom;
  else
  {
    if( m_dHeight <= rect.top )
      rect.bottom = m_dHeight;
    else
      m_dHeight = m_dMaxHeight;
  }

  // das wirklich sichtbare Rechteck ist jetzt bekannt
  SetVisibleRect( rect );

  // now update Stationierung, Trennflaechen, and Durchst. Bereich
  // we must also update all fillings
  pos = GetHeadPosition();
  while( pos )
  {
    CDrawObj* pObj = GetNextObject( pos );
    pObj->SetClipRange( m_dFrom, m_dTo, rect.bottom );
    if( !pObj->IsInvisible() )
    {
      switch(pObj->GetType())
      {
      case DST_STATION:
        {
          double height;
          if( stations.Lookup( pObj->m_dPosition.left, height ) )
            pObj->m_dPosition.top = height;
          else
            pObj->m_dPosition.top = rect.bottom;
          pObj->m_dPosition.bottom = rect.bottom;
        }
        break;
        
      case DST_TRENNFLAECHEN:
      case DST_DURCHST_BEREICH:
      case DST_BOESCHUNGSKANTEN:
      case DST_MODELLGRENZEN:
      case DST_BORDVOLL:
      case DST_TRENN_WEHR:
        pObj->m_dPosition.top = rect.top;
        pObj->m_dPosition.bottom = rect.bottom;
        break;
        
      case DST_GELAENDEHOEHE:
      case DST_SOHLHOEHE:
        // adjust the bottom of the fillings
        if( pObj->IsKindOf(RUNTIME_CLASS(CDrawPoly)) && ((CDrawPoly*)pObj)->GetShape()==CDrawPoly::polygon )
        {
          CDoublePoint pt;
          double ymin = 1e36;
          
          for( int i=0; i < ((CDrawPoly*)pObj)->GetNumPoints(); i++ )
          {
            pt = ((CDrawPoly*)pObj)->GetPoint(i);
            ymin = min(ymin, pt.y);
          }
          for( i = 0; i<((CDrawPoly*)pObj)->GetNumPoints(); i++)
          {
            pt = ((CDrawPoly*)pObj)->GetPoint(i);
            if (pt.y==ymin)
            {
              pt.y = rect.bottom;
              ((CDrawPoly*)pObj)->SetPoint(i, pt);
            }
          }
        }
        break;

      case DST_UNKNOWN:

        break;
        
      default:
        // check all other fillings: make invisible if Gelaendehoehe
        // or Sohlhoehe is not visible!
        if (pObj->IsKindOf(RUNTIME_CLASS(CDrawPoly)) && ((CDrawPoly*)pObj)->GetShape()==CDrawPoly::polygon)
        {
          if( !bGelaendeVisible )
          {
            CString str, msg;
            DataBlock temp(NULL);
            
            pObj->SetFlags(CDrawObj::invisible, TRUE);
            temp.SetType(pObj->GetType());
            str = temp.GetDesc( 0 );
            msg.FormatMessage( IDS_CANT_SHOW_FILLING, str );
            AfxMessageBox( msg, MB_OK | MB_ICONINFORMATION );
          }
        }
        break;
      }
    }
  }; // while pObj

  return GetVisibleRect();
}; // CalcProfilSizes
