// profilmodel.cpp: Implementierung der Klasse CProfilModel.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"


#include "bce/include/LinearEquation.h"
#include "commonMfc\include\point.h"
#include "commonMfc\include\rect.h"

#include "profilmodel.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

CProfilModel::ProfilPoint CProfilModel::NO_POINT( -1.0, -1.0, -1.0, -1.0 );

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CProfilModel::CProfilModel()
{
  m_extent = CDoubleRect( 999999999.99, -999999999.99, -999999999.99, 99999999.99 );
  m_station = 0.0;
}

void CProfilModel::Clear()
{
  m_grenzen.clear();
  m_points.clear();
  m_wsps.RemoveAll();

  m_extent = CDoubleRect( 999999999.99, -999999999.99, -999999999.99, 99999999.99 );
}

void CProfilModel::AddPoint( const double x, const double y, const double rw, const double hw )
{
  AddPoint( ProfilPoint( x, y, rw, hw ) );
};

void CProfilModel::AddPoint( const ProfilPoint& point )
{
  m_points.push_back( point );

  m_extent.left = min( m_extent.left, point.x );
  m_extent.right = max( m_extent.right, point.x );
  m_extent.top = max( m_extent.top, point.y );
  m_extent.bottom = min( m_extent.bottom, point.y );
};

unsigned long CProfilModel::GetTypeColor( const CProfilModel::Type type ) const
{
  unsigned long color = 0;
  m_typeColors.Lookup( type, color );

  return color;
}

void CProfilModel::AddGrenze( const double x, const Type type ) 
{
  m_grenzen.push_back( Grenze( x, type ) ); 

  // TODO: jedesmal sortieren ist vielleicht etwas übertrieben, vielleicht
  // gibts ja noch was effektiveres
  // andererseits wird ja nie wirklich viele Grenzen geben
  m_grenzen.sort();
}

CProfilModel::ProfilPoint CProfilModel::GetInterpolPointAt( const double x ) const
{
  PointIterator pIt = GetPointBegin();
  if( pIt != GetPointEnd() )
  {
    pIt++;

    while( pIt != GetPointEnd() )
    {
      if( ( *(pIt - 1) ).x <= x && x < (*pIt).x )
      {
        double x1 = (*(pIt - 1)).x;
        double y1 = (*(pIt - 1)).y;
        double rw1 = (*(pIt - 1)).rw;
        double hw1 = (*(pIt - 1)).hw;
        
        double x2 = (*pIt).x;
        double y2 = (*pIt).y;
        double rw2 = (*pIt).rw;
        double hw2 = (*pIt).hw;

        BCE::Math::LinearEquation le_y( x1, y1, x2, y2 );
        BCE::Math::LinearEquation le_rw( x1, rw1, x2, rw2 );
        BCE::Math::LinearEquation le_hw( x1, hw1, x2, hw2 );

        return ProfilPoint( x, le_y.computeY( x ), le_rw.computeY( x ), le_hw.computeY( x ) );
      }

      pIt++;
    }
  }

  throw CProfilModel::InterpolException();
}

CProfilModel::Zone CProfilModel::GetZone( const double x )
{
	// die Grenzen sind ja zum Glück immer aufsteigend sortiert!
	const Grenze* left = 0;
	for( GrenzenIterator gIt = m_grenzen.begin(); gIt != m_grenzen.end(); gIt++ )
	{
		const Grenze* right = &(*gIt);

		// TODO: Distances benutzen!
		if( left && left->x <= x && x < right->x )
		{
			// gefunden, anhand des typs von rechts und links die zone entscheiden
			if( left->typ == db && right->typ == db )
				return out;
			if( left->typ == db && right->typ == bv )
				return VL;
			if( left->typ == db && right->typ == tf )
				return VL;
			if( left->typ == bv && right->typ == db )
				return VR;
			if( left->typ == bv && right->typ == bv )
				return out;
			if( left->typ == bv && right->typ == tf )
				return VL;
			if( left->typ == tf && right->typ == db )
				return VR;
			if( left->typ == tf && right->typ == bv )
				return VR;
			if( left->typ == tf && right->typ == tf )
				return HF;
		}

		left = right;
	}

	return Zone::out;
}