// Point.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef POINT_H
#define POINT_H

#ifdef _DEBUG
#include "log4cpp/category.hh"
#endif _DEBUG

///////////////////////////////////////////////////////////////////////////////
// Vorlagenklasse Point
// Rerpäsentiert einen 2-dimensionalen Punkt mit Coordinaten belibigen Typs
///////////////////////////////////////////////////////////////////////////////
// Die Klasse T muss folgende operatoren können:
//      - T operator=;
//      - bool operator==
//      - 
template<class T>
class Point : public CObject
{
  /////////////
  // Members //
  /////////////
public:
  T x;
  T y;

  ////////////////////////////
  //  Konstruktion
  ///////////////////////////
public:
  Point()
  {
    x = y = (T)0;
  }

  Point( const Point<T>& pt )
  {
    *this = pt;
  }

  Point( const T& x, const T& y )
  {
    this->x = x;
    this->y = y;
  }

  Point( const POINT& point )
  {
    this->x = (T)point.x;
    this->y = (T)point.y;
  }

  ////////////////
  // Operatoren //
  ////////////////

  Point<T>& operator=( const Point<T> &other )
  {
    x = other.x;
    y = other.y;
    
    return *this;
  }

  bool operator==( const Point<T>& pt ) const
  {
    if( x == pt.x && y == pt.y )
      return true;
    else
      return false;
  }

  bool operator!=( const Point<T>& pt ) const
  {
    if( x != pt.x || y != pt.y )
      return true;
    else
      return false;
  }

  Point<T>& operator-=( const Point<T>& point )
  {
    x -= point.x;
    y -= point.y;
    
    return *this;
  }
  
  Point<T>& operator+=( const Point<T>& point )
  {
    x += point.x;
    y += point.y;
    
    return *this;
  }
  
  Point<T> operator-( const Point<T>& point ) const
  {
    return Point<T>( x - point.x, y - point.y );
  }

  Point<T> operator+( const Point<T>& point ) const
  {
    return Point<T>( x + point.x, y + point.y );
  }
  
  operator POINT()
  {
    POINT pt = { x, y };
    return pt;
  }

  void SetPoint( const T& x, const T& y )
  {
    this->x = x;
    this->y = y;
  }

#ifdef _DEBUG
  /*!
  * Loggt sich selbst mit Prorität debug in eine Kategorie
  */
  void debug( log4cpp::Category& cat ) const
  {
    using namespace log4cpp;
    
    DEBUG_ONLY( cat.debug( "A Point with dx = %lf, dy = %lf", (double)x, (double)y ) );
  }; // debug
#endif _DEBUG

}; // class Point

template<class T> CArchive& AFXAPI operator<<( CArchive& ar, const Point<T>& pt )
{
  ar << pt.x;
  ar << pt.y;
  
  return ar;
}

template<class T> CArchive& AFXAPI operator>>( CArchive& ar, Point<T>& pt )
{
	ar >> pt.x;
	ar >> pt.y;

	return ar;
}


// noch ein paar übliche Typedefs

typedef Point<int> CIntPoint;
typedef Point<double> CDoublePoint;

#endif // POINT_H