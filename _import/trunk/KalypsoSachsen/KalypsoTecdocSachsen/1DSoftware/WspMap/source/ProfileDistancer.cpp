// ProfileDistancer.cpp: Implementierung der Klasse CProfileDistancer.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include <ostream>
#include <iomanip>

#include "ProfileDistancer.h"
#include "mapObject.h"
#include "mapHelper.h"
#include "profilModel.h"
#include "mapdocdata.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CProfileDistancer::CProfileDistancer( std::ostream& logStream ) : m_logStream( logStream )
{
}

CProfileDistancer::~CProfileDistancer()
{
	for( ConnMap::iterator connIt = m_connMap.begin(); connIt != m_connMap.end(); connIt++ )
		delete connIt->second;
}

void CProfileDistancer::AddDistances( Connection* conn, const long lineID, LPDISPATCH lineDisp, CMapObject* anfProfil, CMapObject* endProfil, CProfilModel* anfModel, CProfilModel* endModel, CLayerArray* layers )
{
	LPDISPATCH anfDisp = anfProfil->GetAt( MO2_FIELD_SHAPE ).pdispVal;
	LPDISPATCH endDisp = endProfil->GetAt( MO2_FIELD_SHAPE ).pdispVal;

	long anfID = anfProfil->GetAt( MO2_FIELD_FEATUREID ).lVal;
	long endID = endProfil->GetAt( MO2_FIELD_FEATUREID ).lVal;

	double anfStation = anfProfil->GetAt( MO2_FIELD_STATION ).dblVal;
	double endStation = endProfil->GetAt( MO2_FIELD_STATION ).dblVal;
	CString tmpAnf;
	tmpAnf.Format( "km %.4lf", anfStation );
	CString tmpEnd;
	tmpEnd.Format( "km %.4lf", endStation );
	LPCTSTR anfProfileName = (LPCSTR)tmpAnf;
	LPCTSTR endProfileName = LPCTSTR( tmpEnd );

	
	CMoLine crossLine;
	crossLine.AttachDispatch( lineDisp, FALSE );
	lineDisp->AddRef();
	

	CMoPoints endPoints = crossLine.GetCrossings( endDisp );
	if( !LPDISPATCH( endPoints ) || endPoints.GetCount() == 0 )
		return;
	
	// Begin Verifikation!
	if( endPoints.GetCount() != 1 )
	{
		m_logStream << "Linie FeatureID = " << lineID;
		m_logStream << " schneidet Profil " << endProfileName << " mehrfach." << std::endl;

//		for( int i = 0; i < endPoints.GetCount(); i++ )
//		{
//			CMoPoint point( endPoints.Item( CComVariant( i ) ) );
//			m_logStream << std::fixed << std::setw( 10 ) << point.GetX() << " / " << point.GetY() << std::endl;
//		}

		return;
	}
	CMoPoint endCrossPoint( endPoints.Item( CComVariant( 0 ) ) );
	
	CMoPoints anfPoints = crossLine.GetCrossings( anfDisp );
	if( !LPDISPATCH( anfPoints ) || anfPoints.GetCount() == 0 )
	{
		// DEBUG
		m_logStream << "Profil " << anfProfileName << " schneidet Linie mit FeatureID = ";
		m_logStream << lineID << " nicht." << std::endl;
		return;
	}
	if( anfPoints.GetCount() != 1 )
	{
		// DEBUG
		m_logStream << "Profil " << anfProfileName << " wird von Linie mit FeatureID = ";
		m_logStream << lineID << " mehrfach (" << anfPoints.GetCount() << ") geschnitten." << std::endl;
		return;
	}
	CMoPoint anfCrossPoint( anfPoints.Item( CComVariant( 0 ) ) );
	
	// die Linie Segmentweise durchforsten, mutlilines werden nicht unterstützt
	double distance;
	try
	{
		distance = FindDistance( lineDisp, anfDisp, endDisp );
	}
	catch( distance_not_found d )
	{
		// DEBUG
//		m_logStream << d.CTSTR() << std::endl;
		try
		{
			distance = FindDistance( lineDisp, endDisp, anfDisp );
		}
		catch( distance_not_found e )
		{
			// DEBUG
//			m_logStream << e.CTSTR() << std::endl;
//			m_logStream << "Kein Abstand gefunden für " << anfProfileName;
//			m_logStream << " - " << endProfileName << " bei Linie mit ID = ";
//			m_logStream << lineID << std::endl;
			return;
		}
	}

	m_logStream << "Abstand gefunden für ";
	m_logStream << anfProfileName << " - " << endProfileName;
	m_logStream << " bei Linie mit FeatureID = " << lineID << ": " << distance << std::endl;

	const double anfY = layers->ProjectToProfilLine( anfCrossPoint, anfID );
	const double endY = layers->ProjectToProfilLine( endCrossPoint, endID );

	CProfilModel::Zone anfZone = anfModel->GetZone( anfY );
	CProfilModel::Zone endZone = endModel->GetZone( endY );

	if( anfZone != endZone )
	{
		m_logStream << "Linie mit FeatureID = " << lineID;
		m_logStream << " trifft die Profile in zwei verschiedenen Fliesszonen." << std::endl;
		return;
	}
	if( anfZone == CProfilModel::Zone::out )
	{
		m_logStream << "Linie mit FeatureID = " << lineID;
		m_logStream << " trifft beide Profile ausserhalb der durchströmten Bereiche." << std::endl;
		return;
	}


	AddDistance( conn, anfZone, distance );
}

void CProfileDistancer::AddDistance( Connection* conn, const CProfilModel::Zone& zone, double distance )
{
	ConnMap::iterator connIt = m_connMap.find( conn );
	DistMap* distMap;
	if( connIt == m_connMap.end() )
	{
		distMap = new DistMap();
		m_connMap.insert( ConnMap::value_type( conn, distMap ) );
	}
	else
		distMap = connIt->second;

	distMap->insert( DistMap::value_type( zone, distance ) );
}

double CProfileDistancer::FindDistance( LPDISPATCH lineDisp, LPDISPATCH anfDisp, LPDISPATCH endDisp )
{
	CMoLine line;
	line.AttachDispatch( lineDisp, FALSE );


	CMoParts lineParts = line.GetParts();
	int partsCount = lineParts.GetCount();
	if( partsCount == 0 )
		throw distance_not_found( "FindDistance mit leerer Linie aufgerufen" ); // darf eigentlich nie passieren

	if( partsCount > 1 )
		throw distance_not_found( "Multi-Linien nicht unterstüzt" );

	CMoPoints linePoints( lineParts.Item( CComVariant( 0 ) ) );

	// ersten Schnitt finden
	double distance = 0.0;
	boolean firstFound = false;
	for( int i = 0; i < linePoints.GetCount() - 1; i++ )
	{
		CMoPoint point1( linePoints.Item( CComVariant( i ) ) );
		CMoPoint point2( linePoints.Item( CComVariant( i + 1 ) ) );
		

		CMoLine segment = CMapHelper::CreateSegment( point1, point2 );
		
		CMoPoints anfCrossPoints( segment.GetCrossings( anfDisp ) );
		int anfCrossCount = anfCrossPoints.GetCount();
		if( anfCrossCount > 1 )
			throw distance_not_found( "FindDistance aufgerufen mit Linie, die mehrfach schneidet." );
		
		CMoPoints endCrossPoints( segment.GetCrossings( endDisp ) );
		int endCrossCount = endCrossPoints.GetCount();
		if( endCrossCount > 1 )
			throw distance_not_found( "FindDistance aufgerufen mit Linie, die mehrfach schneidet." );

		// Sonderfall: beide Linien schneiden im gleichen segment
		if( anfCrossCount == 1 && endCrossCount == 1 )
		{
			CMoPoint anfCrossPoint( anfCrossPoints.Item( CComVariant( 0 ) ) );
			CMoPoint endCrossPoint( endCrossPoints.Item( CComVariant( 0 ) ) );
			return anfCrossPoint.DistanceTo( endCrossPoint );
		}

		CMoPoints crossPoints = firstFound ? anfCrossPoints : endCrossPoints;
		int crossCount = crossPoints.GetCount();
		CMoPoint crossPoint;
		if( crossCount == 1 )
		{	
			CMoPoint crossPoint( crossPoints.Item( CComVariant( 0 ) ) );
			if( firstFound )
			{
				distance += point1.DistanceTo( crossPoint );
				return distance;
			}
			else
			{
				distance += crossPoint.DistanceTo( point2 );
				firstFound = true;
				continue;
			}
		}

		// falls es keinen Schnitt gibt, der erste aber bereits gefunden wurde, 
		// einfach die länge des segments aufaddieren
		if( firstFound )
			distance += point1.DistanceTo( point2 );
	}


	// todo: release dispatch?

	throw distance_not_found( "Ende der Linie erreicht" );
}
