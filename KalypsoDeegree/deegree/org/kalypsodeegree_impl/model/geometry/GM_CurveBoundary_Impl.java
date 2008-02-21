/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

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
 
 
 history:
 
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
 
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.model.geometry;

import java.io.Serializable;

import org.deegree.crs.transformations.CRSTransformation;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_CurveBoundary;
import org.kalypsodeegree.model.geometry.GM_MultiPrimitive;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;

/**
 * default implementation of the GM_CurveBoundary interface from package jago.model.
 * <p>
 * ------------------------------------------------------------
 * </p>
 * 
 * @version 10.6.2001
 * @author Andreas Poth
 */
class GM_CurveBoundary_Impl extends GM_PrimitiveBoundary_Impl implements GM_CurveBoundary, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = 4226497939552424434L;

  private GM_Position m_ep = null;

  private GM_Position m_sp = null;

  /**
   * constructor of curve_boundary with CS_CoordinateSystem and startpoint and endpoint
   */
  public GM_CurveBoundary_Impl( final String crs, final GM_Position sp, final GM_Position ep )
  {
    super( crs );

    m_sp = sp;
    m_ep = ep;

    setValid( false );
  }

  /**
   * The operation "dimension" shall return the inherent dimension of this GM_Object, which shall be less than or equal
   * to the coordinate dimension. The dimension of a collection of geometric objects shall be the largest dimension of
   * any of its pieces. Points are 0-dimensional, curves are 1-dimensional, surfaces are 2-dimensional, and solids are
   * 3-dimensional.
   */
  public int getDimension( )
  {
    return 1;
  }

  /**
   * The operation "coordinateDimension" shall return the dimension of the coordinates that define this GM_Object, which
   * must be the same as the coordinate dimension of the coordinate reference system for this GM_Object.
   */
  public int getCoordinateDimension( )
  {
    return getStartPoint().getAsArray().length;
  }

  /**
   * returns a shallow copy of the geometry
   */
  @Override
  public Object clone( )
  {
    // kuch
    final String system = getCoordinateSystem();

    final GM_Position[] sp = GeometryFactory.cloneGM_Position( new GM_Position[] { m_sp } );
    final GM_Position[] ep = GeometryFactory.cloneGM_Position( new GM_Position[] { m_ep } );

    return new GM_CurveBoundary_Impl( system, sp[0], ep[0] );
  }

  /**
   * returns the StartPoint of the boundary
   */
  public GM_Position getStartPoint( )
  {
    return m_sp;
  }

  /**
   * returns the EndPoint of the boundary
   */
  public GM_Position getEndPoint( )
  {
    return m_ep;
  }

  /**
   * checks if this curve is completly equal to the submitted geometry
   * 
   * @param other
   *            object to compare to
   */
  @Override
  public boolean equals( final Object other )
  {
    if( !super.equals( other ) || !(other instanceof GM_CurveBoundary_Impl) )
    {
      return false;
    }

    if( !m_ep.equals( ((GM_CurveBoundary) other).getEndPoint() ) || !m_sp.equals( ((GM_CurveBoundary) other).getStartPoint() ) )
    {
      return false;
    }

    return true;
  }

  /**
   * The Boolean valued operation "intersects" shall return TRUE if this GM_Object intersects another GM_Object. Within
   * a GM_Complex, the GM_Primitives do not intersect one another. In general, topologically structured data uses shared
   * geometric objects to capture intersection information.
   */
  @Override
  public boolean intersects( final GM_Object gmo )
  {
    boolean inter = false;
    final GM_Point p1 = new GM_Point_Impl( m_sp, getCoordinateSystem() );
    final GM_Point p2 = new GM_Point_Impl( m_ep, getCoordinateSystem() );

    try
    {
      if( gmo instanceof GM_Point )
      {
        inter = LinearIntersects.intersects( p1, (GM_Point) gmo );

        if( !inter )
        {
          inter = LinearIntersects.intersects( p2, (GM_Point) gmo );
        }
      }
      else if( gmo instanceof GM_Curve )
      {
        inter = LinearIntersects.intersects( p1, (GM_Curve) gmo );

        if( !inter )
        {
          inter = LinearIntersects.intersects( p2, (GM_Curve) gmo );
        }
      }
      else if( gmo instanceof GM_Surface )
      {
        inter = LinearIntersects.intersects( p1, (GM_Surface) gmo );

        if( !inter )
        {
          inter = LinearIntersects.intersects( p2, (GM_Surface) gmo );
        }
      }
      else if( gmo instanceof GM_MultiPrimitive )
      {
        inter = intersectsMultiPrimitive( (GM_MultiPrimitive) gmo );
      }
    }
    catch( final Exception e )
    {
    }

    return inter;
  }

  /**
   * the operations returns true if the submitted multi primitive intersects with the curve segment
   */
  private boolean intersectsMultiPrimitive( final GM_MultiPrimitive mprim ) throws Exception
  {
    boolean inter = false;

    final int cnt = mprim.getSize();

    for( int i = 0; i < cnt; i++ )
    {
      if( intersects( mprim.getPrimitiveAt( i ) ) )
      {
        inter = true;
        break;
      }
    }

    return inter;
  }

  /**
   * calculates the envelope of the curve boundary
   */
  private void calculateEnvelope( )
  {
    final double[] min = m_sp.getAsArray().clone();
    final double[] max = m_ep.getAsArray().clone();

    for( int i = 0; i < min.length; i++ )
    {
      if( min[i] > max[i] )
      {
        final double d = min[i];
        min[i] = max[i];
        max[i] = d;
      }
    }

    setEnvelope( new GM_Envelope_Impl( new GM_Position_Impl( min ), new GM_Position_Impl( max ) ) );
  }

  /**
   * calculates the envelope of the curve boundary
   */
  @Override
  protected void calculateParam( )
  {
    calculateEnvelope();
    setValid( true );
  }

  @Override
  public String toString( )
  {
    return "point1: [" + m_sp + "] - point2: [" + m_ep + "]";
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#transform(org.deegree.crs.transformations.CRSTransformation,
   *      java.lang.String)
   */
  public GM_Object transform( final CRSTransformation trans, final String targetOGCCS ) throws Exception
  {
    /* If the target is the same coordinate system, do not transform. */
    String coordinateSystem = getCoordinateSystem();
    if( coordinateSystem == null || coordinateSystem.equalsIgnoreCase( targetOGCCS ) )
      return this;

    final GM_Position transStartPos = getStartPoint().transform( trans );
    final GM_Position transEndPos = getEndPoint().transform( trans );

    return new GM_CurveBoundary_Impl( targetOGCCS, transStartPos, transEndPos );
  }

}