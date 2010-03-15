#include <math.h>

#include <fstream>

#include "LinearEquation.h"

#include "vectorHelper.h"

#include "PolyLine.h"


/**
 * Implementation von D-P, rekursiv
 * 
 *   
 *
*/
void _Douglas_Peucker_Impl( const BCE::Geometry::IPolyLine& polyline, const int start, const int stop, const double distance, std::vector<bool>& bitset )
{
  if( stop - start <= 1 )
     return;

  // Linie start - stop ausrechnen
  BCE::Math::LinearEquation le( polyline.xAt( start ), polyline.yAt( start ), polyline.xAt( stop ), polyline.yAt( stop ) );

  double maxDist = 0.0;
  int maxIndex = -1;
  for( int i = start + 1; i < stop; i++ )
  {
    const double x = polyline.xAt( i );
    const double y = polyline.yAt( i );

    const double y_ = le.computeY( x );

    const double dist = fabs( y - y_ );

    if( maxDist < dist )
    {
      maxDist = dist;
      maxIndex = i;
    }
  }

  if( maxIndex == -1 )
    return;

  if( maxDist > distance )
  {
    bitset[maxIndex] = true;

    _Douglas_Peucker_Impl( polyline, start, maxIndex, distance, bitset );
    _Douglas_Peucker_Impl( polyline, maxIndex, stop, distance, bitset );
  }
}

std::vector<bool> BCE::Geometry::DouglasPeucker( const IPolyLine& polyline, const double distance )
{
  const int size = polyline.pointCount();

  std::vector<bool> bitset( size );
  bitset.resize( size );

  bitset[0] = true;
  bitset[size - 1] = true;

  _Douglas_Peucker_Impl( polyline, 0, size - 1, distance, bitset );

  return bitset;
}



