// LinearEquation.h: Schnittstelle für die Klasse LinearEquation.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_LINEAREQUATION_H__D7084911_70D0_11D7_B3C5_00104BB3E525__INCLUDED_)
#define AFX_LINEAREQUATION_H__D7084911_70D0_11D7_B3C5_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000


namespace BCE
{
  namespace Math
  {
  /**
  * @author schlienger (created 04.04.2003)
  * represents a linear equation such as Y = aX + b
    */
    class LinearEquation
    {
      private:
        double m_a;	// direction coeff.
        double m_b;	// y coordinate at the origin
      
      public:
      LinearEquation()
      {
        m_a = 1;
        m_b = 0;
      }
      
      LinearEquation( double x1, double y1, double x2, double y2 )
      {
        setPoints(x1, y1, x2, y2);
      }
      
      void setPoints( double x1, double y1, double x2, double y2 )
      {
//        if( x2 == x1 )
//          throw new IllegalArgumentException("x1 = x2 !");
        
        m_a = (y2 - y1) / (x2 - x1);
        
        m_b = y1 - m_a*x1;
      }
      
      double computeX( double y )
      {
        return (y - m_b) / m_a;
      }
      
      double computeY( double x )
      {
        return m_a * x + m_b;
      }
      
    };
  }
}
#endif // !defined(AFX_LINEAREQUATION_H__D7084911_70D0_11D7_B3C5_00104BB3E525__INCLUDED_)
