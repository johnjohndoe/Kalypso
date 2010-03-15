#include "stdafx.h"

#include "3dcoord.h"

////////////////////////////
//  Klasse  C3DCoord
///////////////////////////

/* The Default Constructor */
C3DCoord::C3DCoord()
{
	dx = 0;
	dy = 0;
  dz = std::numeric_limits<double>::infinity();
}

/* The Copy Constructor */
C3DCoord::C3DCoord(const C3DCoord& crd)
{
	*this = crd;
}

C3DCoord::~C3DCoord()
{
}

void C3DCoord::Set(double x, double y, double z)
{
	dx = x;
	dy = y;
	dz = z;
}

C3DCoord& C3DCoord::operator=(const C3DCoord &other)
{
	if (&other==this)
		return *this;
	dx = other.dx;
	dy = other.dy;
	dz = other.dz;

	return *this;
}

BOOL C3DCoord::operator==(C3DCoord crd) const
{
	if (dx == crd.dx && dy == crd.dy && dz == crd.dz)
		return TRUE;
	else
		return FALSE;
}

BOOL C3DCoord::operator!=(C3DCoord crd) const
{
	if (dx != crd.dx || dy != crd.dy || dz != crd.dz)
		return TRUE;
	else
		return FALSE;
}