/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.commons.math;

import java.awt.geom.Point2D;
import java.util.NoSuchElementException;

/**
 * represents a linear equation such as Y = aX + b
 * 
 * @author schlienger
 */
public class LinearEquation
{
  /** static exception for Speed-Up of Method solve */
  private final static NoSuchElementException SOLVE_NO_SOLVE_EXCEPTION = new NoSuchElementException(
      "lines do not intersect" );

  /** static exception for better speed when dealing with this class */
  private final static SameXValuesException SAME_X_EXCEPTION = new SameXValuesException();

  /** direction coeff. or slope */
  private double m_a;

  /** y coordinate at the origin */
  private double m_b;

  /**
   * Creates a new LinearEquation object. Sets a default slope of 1 and an Y-coord of 0
   */
  public LinearEquation()
  {
    m_a = 1;
    m_b = 0;
  }

  /**
   * Creates a new LinearEquation object.
   * 
   * @param x
   *          a point's X-coordinate
   * @param y
   *          a point's Y-coordinate
   * @param slope
   *          directional coefficient
   */
  public LinearEquation( double x, double y, double slope )
  {
    m_a = slope;

    m_b = y - ( m_a * x );
  }

  /**
   * Creates a new LinearEquation object.
   * 
   * @param x1
   *          first point's x
   * @param y1
   *          first point's y
   * @param x2
   *          second point's x
   * @param y2
   *          second point's y
   * 
   * @throws SameXValuesException
   *           when x1 == x2
   */
  public LinearEquation( double x1, double y1, double x2, double y2 ) throws SameXValuesException
  {
    setPoints( x1, y1, x2, y2 );
  }

  /**
   * Creates a new LinearEquation object with two points.
   * 
   * @param p1
   *          first point
   * @param p2
   *          second point
   * 
   * @throws SameXValuesException
   *           when x1 == x2
   */
  public LinearEquation( Point2D p1, Point2D p2 ) throws SameXValuesException
  {
    setPoints( p1.getX(), p1.getY(), p2.getX(), p2.getY() );
  }

  /**
   * Redefine the LinearEquation with two points. Computes the slope and the Y-coordinate at the origin.
   * 
   * @param x1
   *          first point's x
   * @param y1
   *          first point's y
   * @param x2
   *          second point's x
   * @param y2
   *          second point's y
   * 
   * @throws SameXValuesException
   *           when x1 == x2
   */
  public void setPoints( double x1, double y1, double x2, double y2 ) throws SameXValuesException
  {
    if( Double.compare( x2, x1 ) == 0 )
      throw SAME_X_EXCEPTION;

    m_a = ( y2 - y1 ) / ( x2 - x1 );

    m_b = y1 - ( m_a * x1 );
  }

  /**
   * Returns the X for a given Y
   * 
   * @param y
   * 
   * @return X
   */
  public double computeX( double y )
  {
    return ( y - m_b ) / m_a;
  }

  /**
   * Returns the Y for a given X
   * 
   * @param x
   * 
   * @return Y
   */
  public double computeY( double x )
  {
    return ( m_a * x ) + m_b;
  }

  /**
   * Returns the Point that is located at the middle of the segment represented by the given points.
   * 
   * @param p1
   * @param p2
   * @return point
   * @throws SameXValuesException
   */
  public static Point2D segmentMiddle( Point2D p1, Point2D p2 ) throws SameXValuesException
  {
    double x = p1.getX() < p2.getX() ? p1.getX() : p2.getX();
    x += Math.abs( p2.getX() - p1.getX() ) / 2;

    LinearEquation le = new LinearEquation( p1, p2 );

    double y = le.computeY( x );

    return new Point2D.Double( x, y );
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    return "Y = " + String.valueOf( m_a ) + "*X + " + String.valueOf( m_b );
  }

  /**
   * L�st diese Lineare Gleichung durch gleichsetzten mit einer anderen.
   * 
   * @param le
   *          Die andere Lineare Gleichung
   * 
   * @return die x-Koordinate der L�sungsmenge
   * 
   * @throws NoSuchElementException
   *           wenn keine L�sung
   */
  public double solve( LinearEquation le ) throws NoSuchElementException
  {
    double divisor = le.m_a - m_a;

    if( Double.compare( divisor, 0.0 ) == 0 )
      throw SOLVE_NO_SOLVE_EXCEPTION;

    return ( m_b - le.m_b ) / divisor;
  }

  /**
   * Computes the (orthogonal) distance between the Point p and the line represented by this LinearEquation.
   * 
   * @param p
   * 
   * @return distance
   */
  public double distance( Point2D p )
  {
    return Math.abs( p.getY() - ( m_a * p.getX() ) - m_b ) / Math.sqrt( 1 + ( m_b * m_b ) );
  }

  /**
   * The X-coordinates are identical, cannot compute a LinerEquation based on these X-values.
   * 
   * @author schlienger
   */
  public static class SameXValuesException extends Exception
  {
    public SameXValuesException()
    {
      super( "X-Values are identical. Cannot create LinearEquation" );
    }

    public SameXValuesException( String message )
    {
      super( message );
    }

    public SameXValuesException( String message, Throwable cause )
    {
      super( message, cause );
    }

    public SameXValuesException( Throwable cause )
    {
      super( cause );
    }
  }
}