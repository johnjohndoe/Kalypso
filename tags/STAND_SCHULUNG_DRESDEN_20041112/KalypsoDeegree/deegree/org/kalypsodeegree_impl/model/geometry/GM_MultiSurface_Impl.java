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

import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_MultiSurface;
import org.deegree.model.geometry.GM_Surface;
import org.deegree.model.geometry.GM_SurfacePatch;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * default implementation of the GM_MultiSurface interface from package
 * jago.model.
 * 
 * <p>
 * ------------------------------------------------------------
 * </p>
 * 
 * @version 12.6.2001
 * @author Andreas Poth
 *         <p>
 */
final class GM_MultiSurface_Impl extends GM_MultiPrimitive_Impl implements GM_MultiSurface,
    Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = -6471121873087659850L;

  private double area = 0;

  /**
   * Creates a new GM_MultiSurface_Impl object.
   * 
   * @param crs
   */
  public GM_MultiSurface_Impl( CS_CoordinateSystem crs )
  {
    super( crs );
  }

  /**
   * Creates a new GM_MultiSurface_Impl object.
   * 
   * @param surface
   */
  public GM_MultiSurface_Impl( GM_Surface[] surface )
  {
    super( null );

    for( int i = 0; i < surface.length; i++ )
    {
      aggregate.add( surface[i] );
    }

    setValid( false );
  }

  /**
   * Creates a new GM_MultiSurface_Impl object.
   * 
   * @param surface
   * @param crs
   */
  public GM_MultiSurface_Impl( GM_Surface[] surface, CS_CoordinateSystem crs )
  {
    super( crs );

    for( int i = 0; i < surface.length; i++ )
    {
      aggregate.add( surface[i] );
    }

    setValid( false );
  }

  /**
   * adds an GM_Surface to the aggregation
   */
  public void addSurface( GM_Surface gms )
  {
    super.add( gms );
  }

  /**
   * inserts a GM_Surface in the aggregation. all elements with an index equal
   * or larger index will be moved. if index is larger then getSize() - 1 or
   * smaller then 0 or gms equals null an exception will be thrown.
   * 
   * @param gms
   *          GM_Surface to insert.
   * @param index
   *          position where to insert the new GM_Surface
   */
  public void insertSurfaceAt( GM_Surface gms, int index ) throws GM_Exception
  {
    super.insertObjectAt( gms, index );
  }

  /**
   * sets the submitted GM_Surface at the submitted index. the element at the
   * position <code>index</code> will be removed. if index is larger then
   * getSize() - 1 or smaller then 0 or gms equals null an exception will be
   * thrown.
   * 
   * @param gms
   *          GM_Surface to set.
   * @param index
   *          position where to set the new GM_Surface
   */
  public void setSurfaceAt( GM_Surface gms, int index ) throws GM_Exception
  {
    setObjectAt( gms, index );
  }

  /**
   * removes the submitted GM_Surface from the aggregation
   * 
   * @return the removed GM_Surface
   */
  public GM_Surface removeSurface( GM_Surface gms )
  {
    return (GM_Surface)super.removeObject( gms );
  }

  /**
   * removes the GM_Surface at the submitted index from the aggregation. if
   * index is larger then getSize() - 1 or smaller then 0 an exception will be
   * thrown.
   * 
   * @return the removed GM_Surface
   */
  public GM_Surface removeSurfaceAt( int index ) throws GM_Exception
  {
    return (GM_Surface)super.removeObjectAt( index );
  }

  /**
   * returns the GM_Surface at the submitted index.
   */
  public GM_Surface getSurfaceAt( int index )
  {
    return (GM_Surface)super.getPrimitiveAt( index );
  }

  /**
   * returns all GM_Surfaces as array
   */
  public GM_Surface[] getAllSurfaces()
  {
    return (GM_Surface[])aggregate.toArray( new GM_Surface[getSize()] );
  }

  /**
   * calculates the bounding box / envelope of the aggregation
   */
  private void calculateEnvelope()
  {
    GM_Envelope bb = getSurfaceAt( 0 ).getEnvelope();

    double[] min = (double[])bb.getMin().getAsArray().clone();
    double[] max = (double[])bb.getMax().getAsArray().clone();

    for( int i = 1; i < getSize(); i++ )
    {
      double[] pos1 = getSurfaceAt( i ).getEnvelope().getMin().getAsArray();
      double[] pos2 = getSurfaceAt( i ).getEnvelope().getMax().getAsArray();

      for( int j = 0; j < pos1.length; j++ )
      {
        if( pos1[j] < min[j] )
        {
          min[j] = pos1[j];
        }
        else if( pos1[j] > max[j] )
        {
          max[j] = pos1[j];
        }

        if( pos2[j] < min[j] )
        {
          min[j] = pos2[j];
        }
        else if( pos2[j] > max[j] )
        {
          max[j] = pos2[j];
        }
      }
    }

    envelope = new GM_Envelope_Impl( new GM_Position_Impl( min ), new GM_Position_Impl( max ) );
  }

  /**
   * calculates the centroid and area of the aggregation
   */
  private void calculateCentroidArea()
  {

    area = 0;
    int cnt = getCoordinateDimension();
    try
    {
      double[] cen = new double[cnt];

      for( int i = 0; i < getSize(); i++ )
      {
        double a = getSurfaceAt( i ).getArea();
        area = area + a;

        double[] pos = getSurfaceAt( i ).getCentroid().getAsArray();

        for( int j = 0; j < cnt; j++ )
        {
          cen[j] = cen[j] + ( pos[j] * a );
        }
      }

      for( int j = 0; j < cnt; j++ )
      {
        cen[j] = cen[j] / area;
      }

      centroid = new GM_Point_Impl( new GM_Position_Impl( cen ), null );
    }
    catch( Exception e )
    {
      System.out.println( e );
    }
  }

  /**
   * calculates the centroid, area and envelope of the aggregation
   */
  protected void calculateParam()
  {
    calculateEnvelope();
    calculateCentroidArea();
    setValid( true );
  }

  /**
   * returns the area of the multi surface. this is calculate as the sum of all
   * containing surface areas.
   */
  public double getArea()
  {
    if( !isValid() )
    {
      calculateParam();
    }
    return area;
  }

  /**
   * returns a shallow copy of the geometry
   */
  public Object clone()
  {
    GM_MultiSurface ms = null;

    try
    {
      ms = new GM_MultiSurface_Impl( getCoordinateSystem() );

      for( int i = 0; i < this.getSize(); i++ )
      {
        GM_Surface_Impl si = (GM_Surface_Impl)getSurfaceAt( i );
        ms.add( (GM_Surface)si.clone() );
      }
    }
    catch( Exception ex )
    {
      System.out.println( "GM_MultiSurface_Impl.clone: " + ex );
    }

    return ms;
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
    return 2;
  }

  /**
   * The operation "coordinateDimension" shall return the dimension of the
   * coordinates that define this GM_Object, which must be the same as the
   * coordinate dimension of the coordinate reference system for this GM_Object.
   */
  public int getCoordinateDimension()
  {
    GM_SurfacePatch sp = null;

    try
    {
      sp = getSurfaceAt( 0 ).getSurfacePatchAt( 0 );
    }
    catch( Exception ex )
    {}

    return sp.getExteriorRing()[0].getAsArray().length;
  }
}