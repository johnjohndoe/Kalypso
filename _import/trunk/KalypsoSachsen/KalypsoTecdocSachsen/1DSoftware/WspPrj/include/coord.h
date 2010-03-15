// Coord.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef COORD_H
#define COORD_H

class Coord : public CObject
{
public:
	 Coord();
   Coord( const double dx, const double dy, const long xs = 0, const long ys = 0 );
   Coord( const Coord& crd );
   Coord( const Coord* pCrd );
   ~Coord();
   
   Coord& operator=( const Coord &other );
   BOOL operator==( const Coord& crd ) const;
   BOOL operator!=( const Coord& crd ) const;
   
   void Set( const double x, const double y );
   
   bool operator<( const Coord& other ) const;
   
   long xs, ys;
   double dx;
   double dy;
};

#endif // COORD_H