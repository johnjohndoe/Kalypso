// Rect.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef RECT_H
#define RECT_H

#include "log4cpp/category.hh"

#include "point.h"

/////////////////////////////////////////////////////////////
// Klassenvorlage Rect
// Diese Klasse repräsentiert ein Rechteck mit Kooridnaten
// eines beliebigen ( numerischen ) Typs
// Zusätzlich kann der Benutzer wählen, ob
// die y-Achse positiv ( d.h. top < bottom; bInv = false) oder
// negativ ( bottom < top, bInv = true ) sein soll
/////////////////////////////////////////////////////////////
template<class T, bool bInv = false >
// Parameter:
//    typedef T: der Typ der Koordinaten. Es müssen folgende Operatioren definiert sein
//                  - T::operator<
//                  - T::operator=
//                  - T::operator==
//                  - T::operator-
//                  - CArchive& operator<<( CArchive& ar, const T& t )
//                  - CArchive& operator>>( CArchive& ar, const T& t )
//
class Rect : public CObject
{
  /////////////
  // Members //
  /////////////
public:
  T left;
  T right;
  T top;
  T bottom;

  ///////////////////
  // Konstruktoren //
  ///////////////////
public:
  Rect()
  {
    left = 0;
    right = 0;
    top = 0;
    bottom = 0;
  };

  Rect( const T& left, const T& top, const T& right, const T& bottom )
  {
    this->left = left;
    this->right = right;
    this->top = top;
    this->bottom = bottom;
  };

  Rect( const Rect<T, bInv>* pOther )
  {
    *this = *pOther;
  };


  Rect( const CRect& other )
  {
    *this = other;
  };

  template<class S, bool bOtherInv> explicit Rect( const Rect<S, bOtherInv>& other )
  {
    // jetzt die einzelnen Members kopieren
    this->left = (T)other.left;
    this->right = (T)other.right;

    if( bInv == bOtherInv )
    {
      this->top = (T)other.top;
      this->bottom = (T)other.bottom;
    }
    else
    {
      this->top = (T)other.bottom;
      this->bottom = (T)other.top;
    }
  } // Template-Copy-Contructor

  ////////////////
  // Operatoren //
  ////////////////

	template<class S, bool bOtherInv> Rect<T, bInv>& operator= ( const Rect<S, bOtherInv>& other )
  {
    if( &other == this )
      return *this;
	
    left = (T)other.left;
    right = (T)other.right;
    top = (T)other.top;
    bottom = (T)other.bottom;
    
    return *this;
  };

  Rect<T, bInv>& operator=( const CRect& other )
  {
    left = (T)other.left;
    right = (T)other.right;
    top = (T)other.top;
    bottom = (T)other.bottom;

    return *this;
  };

  bool operator!=( const Rect<T, bInv>& other ) const
  {
    if( &other == this )
      return false;
    if( left != other.left )
      return true;
    if( right != other.right )
      return true;
    if( top != other.top )
      return true;
    if( bottom != other.bottom )
      return true;
    
    return FALSE;
  };

  bool operator==( const Rect<T, bInv>& other ) const
  {
    if( left != other.left || right != other.right || top != other.top || bottom != other.bottom )
      return false;
    else
      return true;
  };


  Rect<T, bInv>& operator-=( const Point<T>& point )
  {
    left -= point.x;
    top -= point.y;
    right -= point.x;
    bottom -= point.y;
    
    return *this;
  };

  Rect<T, bInv>& operator-=( const SIZE& size )
  {
    left -= size.cx;
    top -= size.cy;
    right -= size.cx;
    bottom -= size.cy;
    
    return *this;
  }

	Rect<T, bInv>& operator+=( const Point<T>& point )
  {
    left += point.x;
    top += point.y;
    right += point.x;
    bottom += point.y;
    
    return *this;
  };

  Rect<T, bInv>& operator+=( const CPoint& point )
  {
    left += point.x;
    top += point.y;
    right += point.x;
    bottom += point.y;
    
    return *this;
  }

  Rect<T, bInv>& operator|=( const Rect<T, bInv>& other )
  {
    left = min( left, other.left );
    right = max( right, other.right );
    
    if( bInv )
    {
      top = max( top, other.top );
      bottom = min( top, other.top );
    }
    else
    {
      top = min( top, other.top );
      bottom = max( bottom, other.bottom );
    }

    return *this;
  };

  Rect<T, bInv>& operator|=( const Point<T>& point )
  {
    left = min( left, point.dx );
    right = max( right, point.dx );
    
    if( bInv )
    {
      top = max( top, point.dy );
      bottom = min( top, point.dy );
    }
    else
    {
      top = min( top, point.dy );
      bottom = max( top, point.dy );
    }
    
    return *this;
  };

  Rect<T, bInv>& operator&=( const Rect<T, bInv>& other )
  {
    // nicht schneidende Rechtecke ausschliessen
    if( left > other.right || right < other.left ||
      ( bInv && ( top < other.bottom || bottom > other.top ) ) ||
      ( !bInv && ( top > other.bottom || bottom < other.top ) ) )
    {
      SetRect( 0, 0, 0, 0 );
      return *this;
    }

    // ansonsten den Schnitt ausrechnen
    left = max( left, other.left );
    right = min( right, other.right );

    if( bInv )
    {
      top = max( top, other.top );
      bottom = min( bottom, other.bottom );
    }
    else
    {
      top = min( top, other.top );
      bottom = max( bottom, other.bottom );
    }

    return *this;
  }

	Rect<T, bInv> operator-( const Point<T>& point ) const
  {
    return Rect<T, bInv>( *this ) -= point;
  };

	Rect<T, bInv> operator+( const Point<T>& point ) const
  {
    return Rect<T, bInv>( *this ) += point;
  };
  
  Rect<T, bInv> operator|( const Rect<T, bInv>& other ) const
  {
    return Rect<T, bInv>( *this ) |= other;
  };

  Rect<T, bInv> operator|( const Point<T>& point ) const
  {
    return Rect<T, bInv>( *this ) |= point;
  };

  Rect<T, bInv> operator&( const Rect<T, bInv>& other ) const
  {
    return Rect<T, bInv>( *this ) &= other;
  }

  operator RECT()
  {
    RECT rect = { (LONG)left, (LONG)top, (LONG)right, (LONG)bottom };
    return rect;
  }

  /////////////////
  // Operationen //
  /////////////////

	void SetRect( const Point<T>& tl, const Point<T>& br )
  {
    left = tl.dx;
    right = br.dx;
    top = tl.dy;
    bottom = br.dy;
  };

	void SetRect( const T& left, const T& top, const T& right, const T& bottom )
  {
    this->left = left;
    this->right = right;
    this->top = top;
    this->bottom = bottom;
  };

  void OffsetRect( const T& x, const T& y )
  {
    left += x;
    right += x;
    top += y;
    bottom += y;
  }

  void OffsetRect( const Point<T>& pt )
  {
    OffsetRect( pt.x, pt.y );
  }

  void OffsetRect( SIZE size )
  {
    OffsetRect( size.cx, size.cy );
  }
	
  void CompareAndExpand( const Rect<T, bInv>& rect )
  {
    if( rect.left < left )
      left = rect.left;
    if( rect.right > right )
      right = rect.right;

    if( bInv )
    {
      if( rect.top > top )
        top = rect.top;
      if( rect.bottom < bottom ) 
        bottom = rect.bottom;
    }
    else
    {
      if( rect.top < top )
        top = rect.top;
      if( rect.bottom > bottom ) 
        bottom = rect.bottom;
    }
  } // CompareAndExpand

  void CompareAndExpand( const Point<T>& pt )
  {
    if( pt.x < left )
      left = pt.x;
    if( pt.y > top )
      top = pt.y;
    if( pt.x > right )
      right = pt.x;
    if( pt.y < bottom )
      bottom = pt.y;
  };

	void CompareAndExpand( const CSize& pt )
  {
    if( pt.cx < left )
      left = pt.cx;
    if( pt.cy > top )
      top = pt.cy;
    if( pt.cx > right )
      right = pt.cx;
    if( pt.cy < bottom )
      bottom = pt.cy;
  };

  bool Contains( const Rect<T, bInv>& other ) const
  {
    if( other.IsNullRect() )
      return false;

    Rect<T, bInv> expanded( this );
    Rect<T, bInv> expander( other );
    expander.NormalizeRect();

    expanded.CompareAndExpand( expander );

    return expanded == *this;
  };
	
  bool NormalizeRect()
  {
    bool bChange = false;

    if( left > right )
    {
      T tmp = left;
      left = right;
      right = tmp;

      bChange = true;
    }
    
    if( bInv )
    {
      if( top < bottom )
      {
        T tmp = top;
        top = bottom;
        bottom = tmp;

        return true;
      }
    }
    else
    {
      if( top > bottom )
      {
        T tmp = top;
        top = bottom;
        bottom = tmp;

        return true;
      }
    }

    return bChange;
  };

	T Height() const
  {
    return (T)( fabs( (double)top - (double)bottom ) );
  };

	T Width() const
  {
    return (T)( fabs( (double)right - (double)left ) );
  };

  Point<T> TopLeft() const
  {
    return Point<T>( left, top );
  }

  Point<T> Center() const
    // Gibt den Mittelpunkt ( Schnittpunkt der Diagonalen )
    // zurück, unabhängig davon, ob das Rechteck normalisiert ist
    // oder nicht
  {
    return Point<T>( ( left + right ) / 2, ( top + bottom ) / 2 );
  }


	bool IsRectEmpty() const
  {
    if( top == bottom )
      return true;
    if( left == right )
      return true;
    
    return false;
  };

  bool IsNullRect() const
  {
    return ( left == T( 0 ) && top == T( 0 ) && right == T( 0 ) && bottom == T( 0 ) );
  };


  /*!
   * Loggt sich selbst mit Prorität debug in eine Kategorie
   */
  void debug( log4cpp::Category& cat ) const
  {
    using namespace log4cpp;
    
    DEBUG_ONLY( cat.debug( "A Rect with left = %lf, top = %lf, right = %lf, bottom = %lf", (double)left, (double)top, (double)right, (double)bottom ) );
  }; // Dump
  ;

};

template<class T, bool bInv> CArchive& AFXAPI operator<<( CArchive& ar, const Rect<T, bInv>& rect )
{
  ar << rect.left;
  ar << rect.right;
  ar << rect.top;
  ar << rect.bottom;

  return ar;
};

template<class T, bool bInv> CArchive& AFXAPI operator>>( CArchive& ar, Rect<T, bInv >& rect )
{
	ar >> rect.left;
	ar >> rect.right;
	ar >> rect.top;
	ar >> rect.bottom;

	return ar;
};

// noch ein paar übliche typedefs

typedef Rect<double, false> CDoubleRect; // das normale Double Rect
typedef Rect<double, true> CDoubleIRect; // das Inverse DoubleRect
typedef Rect<int, false> CIntRect; // das normale Int Rect
typedef Rect<int, true> CIntIRect; // das Inverse IntRect

#endif // RECT_H