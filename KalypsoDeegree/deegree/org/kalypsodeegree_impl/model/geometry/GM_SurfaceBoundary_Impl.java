/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

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

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.model.geometry;

import java.io.Serializable;
import java.util.Arrays;

import org.deegree.model.geometry.GM_Boundary;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Ring;
import org.deegree.model.geometry.GM_SurfaceBoundary;

/**
 * default implementation of the GM_SurfaceBoundary interface.
 * 
 * ------------------------------------------------------------
 * 
 * @version 11.6.2001
 * @author Andreas Poth href="mailto:poth@lat-lon.de"
 */
class GM_SurfaceBoundary_Impl extends GM_PrimitiveBoundary_Impl implements GM_SurfaceBoundary,
    Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = 1399131144729310956L;

  public GM_Ring exterior = null;

  public GM_Ring[] interior = null;

  /**
   * constructor
   */
  public GM_SurfaceBoundary_Impl( GM_Ring exterior, GM_Ring[] interior ) throws GM_Exception
  {
    super( exterior.getCoordinateSystem() );
    this.exterior = exterior;
    this.interior = interior;
    setValid( false );
  }

  /**
   * gets the exterior ring
   */
  public GM_Ring getExteriorRing()
  {
    return exterior;
  }

  /**
   * gets the interior ring(s)
   */
  public GM_Ring[] getInteriorRings()
  {
    return interior;
  }

  /**
   * returns the boundary of the boundary
   */
  public GM_Boundary getBoundary()
  {
    return null;
  }

  /**
   * checks if this curve is completly equal to the submitted geometry
   * 
   * @param other
   *          object to compare to
   */
  public boolean equals( Object other )
  {
    if( !super.equals( other ) || !( other instanceof GM_SurfaceBoundary_Impl ) )
    {
      return false;
    }

    if( !exterior.equals( ( (GM_SurfaceBoundary)other ).getExteriorRing() ) )
    {
      return false;
    }

    if( interior != null )
    {
      GM_Ring[] r1 = getInteriorRings();
      GM_Ring[] r2 = ( (GM_SurfaceBoundary)other ).getInteriorRings();

      if( !Arrays.equals( r1, r2 ) )
      {
        return false;
      }
    }
    else
    {
      if( ( (GM_SurfaceBoundary)other ).getInteriorRings() != null )
      {
        return false;
      }
    }

    return true;
  }

  /**
   * The operation "dimension" shall return the inherent dimension of this
   * GM_Object, which shall be less than or equal to the coordinate dimension.
   * The dimension of a collection of geometric objects shall be the largest
   * dimension of any of its pieces. Points are 0-dimensional, curves are
   * 1-dimensional, surfaces are 2-dimensional, and solids are 3-dimensional.
   */
  public int getDimension()
  {
    return 1;
  }

  /**
   * The operation "coordinateDimension" shall return the dimension of the
   * coordinates that define this GM_Object, which must be the same as the
   * coordinate dimension of the coordinate reference system for this GM_Object.
   */
  public int getCoordinateDimension()
  {
    return exterior.getPositions()[0].getAsArray().length;
  }

  /**
   * returns a copy of the geometry
   */
  public Object clone()
  {
    GM_SurfaceBoundary sb = null;

    try
    {
      GM_Ring ext = (GM_Ring)( (GM_Ring_Impl)getExteriorRing() ).clone();
      GM_Ring[] inn = new GM_Ring[interior.length];

      for( int i = 0; i < inn.length; i++ )
      {
        inn[i] = (GM_Ring)( (GM_Ring_Impl)interior[i] ).clone();
      }

      sb = new GM_SurfaceBoundary_Impl( ext, inn );
    }
    catch( Exception ex )
    {
      System.out.println( "GM_SurfaceBoundary_Impl.clone: " + ex );
    }

    return sb;
  }

  /**
   * The Boolean valued operation "intersects" shall return TRUE if this
   * GM_Object intersects another GM_Object. Within a GM_Complex, the
   * GM_Primitives do not intersect one another. In general, topologically
   * structured data uses shared geometric objects to capture intersection
   * information.
   */
  public boolean intersects( GM_Object gmo )
  {
    boolean inter = exterior.intersects( gmo );

    if( !inter )
    {
      if( interior != null )
      {
        for( int i = 0; i < interior.length; i++ )
        {
          if( interior[i].intersects( gmo ) )
          {
            inter = true;
            break;
          }
        }
      }
    }

    return inter;
  }

  /**
   * The Boolean valued operation "contains" shall return TRUE if this GM_Object
   * contains another GM_Object.
   * <p>
   * </p>
   * At the moment the operation just works with point geometries
   */
  public boolean contains( GM_Object gmo )
  {
    boolean con = false;

    con = exterior.contains( gmo );

    if( con )
    {
      if( interior != null )
      {
        for( int i = 0; i < interior.length; i++ )
        {
          if( interior[i].intersects( gmo ) )
          {
            con = false;
            break;
          }
        }
      }
    }

    return con;
  }

  /**
   * The Boolean valued operation "contains" shall return TRUE if this GM_Object
   * contains a single point given by a coordinate.
   * <p>
   * </p>
   * dummy implementation
   */
  public boolean contains( GM_Position position )
  {
    return contains( new GM_Point_Impl( position, null ) );
  }

  /**
   * calculates the envelope of the surface boundary
   */
  private void calculateEnvelope()
  {
    envelope = (GM_Envelope)( (GM_Envelope_Impl)exterior.getEnvelope() ).clone();
  }

  /**
   * calculates the centroid of the surface boundary
   */
  private void calculateCentroid()
  {
    try
    {
      double[] cen = (double[])exterior.getCentroid().getAsArray().clone();
      double cnt = exterior.getAsCurveSegment().getNumberOfPoints();

      for( int i = 0; i < cen.length; i++ )
      {
        cen[i] *= cnt;
      }

      if( interior != null )
      {
        for( int i = 0; i < interior.length; i++ )
        {
          double[] pos = interior[i].getCentroid().getAsArray();
          cnt += interior[i].getAsCurveSegment().getNumberOfPoints();

          for( int j = 0; j < pos.length; j++ )
          {
            cen[j] += ( pos[j] * interior[i].getAsCurveSegment().getNumberOfPoints() );
          }
        }
      }

      for( int j = 0; j < cen.length; j++ )
      {
        cen[j] /= cnt;
      }

      centroid = new GM_Point_Impl( new GM_Position_Impl( cen ), crs );
    }
    catch( Exception ex )
    {
      System.out.println( ex );
    }
  }

  /**
   * calculates the centroid and the envelope of the surface boundary
   */
  protected void calculateParam()
  {
    calculateEnvelope();
    calculateCentroid();
    setValid( true );
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = null;
    ret = "interior = " + interior + "\n";
    ret += ( "exterior = " + exterior + "\n" );
    return ret;
  }
}