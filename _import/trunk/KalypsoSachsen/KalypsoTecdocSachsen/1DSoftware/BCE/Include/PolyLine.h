// PolyLine.h: Schnittstelle für die Klasse IPolyLine.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_POLYLINE_H__131B3370_5343_11D8_B4BC_00104BB3E525__INCLUDED_)
#define AFX_POLYLINE_H__131B3370_5343_11D8_B4BC_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <vector>

namespace BCE
{
  namespace Geometry
  {
    /**
     * PolyLine - Interface
     */
    class IPolyLine  
    {
    protected:
      IPolyLine() {};
      virtual ~IPolyLine() {};

    public:
      /** Number of points */
      virtual int pointCount() const = 0;

      /** x-Value at given index */
      virtual double xAt( const int index ) const = 0;

      /** y-Value at given index */
      virtual double yAt( const int index ) const = 0;
    };

    /**
     * Implementation of DouglasPeucker Algoryth for Polygons implementing the IPolyLine Interface.
     *
     * @param distance max. allowed distance for line-smoothing
     *
     * @return Returns Bitvector of Important points, e.g. each bit corresponding to an index in
     *  polyline indicates if point can be removed (1: point is important, 0: point can be removed)
    */
    std::vector<bool> DouglasPeucker( const IPolyLine& polyline, const double distance );
  }
};

#endif // !defined(AFX_POLYLINE_H__131B3370_5343_11D8_B4BC_00104BB3E525__INCLUDED_)
