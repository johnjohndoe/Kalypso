#include "stdafx.h"

#include <vector>

#include "bce/include/LinearEquation.h"

#include "triple.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

void TripleArray::Paint( CDC* dc, CRect* extent ) const
{
  COLORREF color = RGB( 0, 255, 128 );
  UINT lineSize = 2;

  CBrush* oldBrush = NULL;
  CPen* oldPen = NULL;

  CPen linePen, fillPen;
  if( !linePen.CreatePen( PS_SOLID | PS_JOIN_ROUND, lineSize, color ) || 
      !fillPen.CreatePen( PS_SOLID, 1, RGB( 128, 128, 128 ) ) )
    return;

  LPPOINT points = (LPPOINT)malloc( size() * sizeof( POINT ) );
  int i = 0;
  for( TripleArray::const_iterator tIt = begin(); tIt != end(); tIt++ )
  {
    Triple* trip = *tIt;

    points[i].x = (int)( trip->breite * 1000 );
    points[i].y = (int)( trip->hoehe * 1000 );

    i++;
  }; // while crd
  
  // Gelände zeichnen
  oldPen = dc->SelectObject( &linePen );
  dc->Polyline( points, i );
  
  free( points );
  
  if ( oldPen )
    dc->SelectObject( oldPen );
  if ( oldBrush )
    dc->SelectObject( oldBrush );
}

int TripleArray::Smooth( const double param, const bool bDelete )
{
  std::vector<bool> bitset = BCE::Geometry::DouglasPeucker( *this, param );

  int count = 0;
  int index = 0;
  for( int i = 0; i < bitset.size(); i++ )
  {
    if( bitset[i] == false )
    {
      RemoveAt( index, bDelete );
      count++;
    }
    else
      index++;
  }

  return count;
}

void TripleArray::RemoveAt( const int index, const bool bDelete )
{
  if( bDelete )
    delete at( index );

  erase( begin() + index );
}