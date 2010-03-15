#include "stdafx.h"

#include "coord.h"

  ////////////////////////////
  //  Klasse  Coord
  ///////////////////////////

/* The Default Constructor */
Coord::Coord()
{
  dx = 0;
	dy = 0;
	xs = 0;
	ys = 0;
}

// the set Constructor
Coord::Coord( const double dx, const double dy, const long xs /* = 0 */, const long ys /* = 0.0 */ ) : dx( dx ), dy( dy ), xs( xs ), ys( ys )
{};

/* The Copy Constructor */
Coord::Coord( const Coord& crd )
{
	*this = crd;
}

Coord::Coord( const Coord* pCrd )
{
  *this = *pCrd;
};

Coord::~Coord()
{
}

void Coord::Set( const double x, const double y )
{
	dx = x;
	dy = y;
}

Coord& Coord::operator=( const Coord &other )
{
	if( &other == this )
		return *this;
	xs = other.xs;
	ys = other.ys;
	dx = other.dx;
	dy = other.dy;

	return *this;
}

BOOL Coord::operator==( const Coord& crd ) const
{
	if( dx == crd.dx && dy == crd.dy )
		return TRUE;
	else
		return FALSE;
}

BOOL Coord::operator!=( const Coord& crd ) const
{
	if( dx != crd.dx || dy != crd.dy )
		return TRUE;
	else
		return FALSE;
}
bool Coord::operator<( const Coord& other ) const
// der Vergleichsoperator: die Coordinaten werden anhand von xs verglichen
// ( für DataBlock::SortCoordsByXs )
{
  if( xs < other.xs )
    return true;
  else
    return false;
}; // operator<