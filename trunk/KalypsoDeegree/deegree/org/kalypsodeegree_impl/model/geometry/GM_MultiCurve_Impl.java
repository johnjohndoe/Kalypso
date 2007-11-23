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
import java.util.LinkedList;
import java.util.List;

import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_CurveSegment;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * default implementation of the GM_MultiCurve interface from package jago.model.
 * ------------------------------------------------------------
 * 
 * @version 12.6.2001
 * @author Andreas Poth
 */
final class GM_MultiCurve_Impl extends GM_MultiPrimitive_Impl implements GM_MultiCurve, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = 2730942874409216686L;

  /**
   * Creates a new GM_MultiCurve_Impl object.
   * 
   * @param crs
   */
  public GM_MultiCurve_Impl( final CS_CoordinateSystem crs )
  {
    super( crs );
  }

  /**
   * Creates a new GM_MultiCurve_Impl object.
   * 
   * @param gmc
   */
  public GM_MultiCurve_Impl( final GM_Curve[] gmc )
  {
    super( null );

    for( final GM_Curve element : gmc )
    {
      m_aggregate.add( element );
    }
  }

  /**
   * Creates a new GM_MultiCurve_Impl object.
   * 
   * @param gmc
   * @param crs
   */
  public GM_MultiCurve_Impl( final GM_Curve[] gmc, final CS_CoordinateSystem crs )
  {
    super( crs );

    for( final GM_Curve element : gmc )
    {
      m_aggregate.add( element );
    }
  }

  /**
   * adds a GM_Curve to the aggregation
   */
  public void addCurve( final GM_Curve gmc )
  {
    super.add( gmc );
  }

  /**
   * inserts a GM_Curve in the aggregation. all elements with an index equal or larger index will be moved. if index is
   * larger then getSize() - 1 or smaller then 0 or gmc equals null an exception will be thrown.
   * 
   * @param gmc
   *            GM_Curve to insert.
   * @param index
   *            position where to insert the new GM_Curve
   */
  public void insertCurveAt( final GM_Curve gmc, final int index ) throws GM_Exception
  {
    super.insertObjectAt( gmc, index );
  }

  /**
   * sets the submitted GM_Curve at the submitted index. the element at the position <code>index</code> will be
   * removed. if index is larger then getSize() - 1 or smaller then 0 or gmc equals null an exception will be thrown.
   * 
   * @param gmc
   *            GM_Curve to set.
   * @param index
   *            position where to set the new GM_Curve
   */
  public void setCurveAt( final GM_Curve gmc, final int index ) throws GM_Exception
  {
    setObjectAt( gmc, index );
  }

  /**
   * removes the submitted GM_Curve from the aggregation
   * 
   * @return the removed GM_Curve
   */
  public GM_Curve removeCurve( final GM_Curve gmc )
  {
    return (GM_Curve) super.removeObject( gmc );
  }

  /**
   * removes the GM_Curve at the submitted index from the aggregation. if index is larger then getSize() - 1 or smaller
   * then 0 an exception will be thrown.
   * 
   * @return the removed GM_Curve
   */
  public GM_Curve removeCurveAt( final int index ) throws GM_Exception
  {
    return (GM_Curve) super.removeObjectAt( index );
  }

  /**
   * removes all GM_Curve from the aggregation.
   */
  @Override
  public void removeAll( )
  {
    super.removeAll();
  }

  /**
   * returns the GM_Curve at the submitted index.
   */
  public GM_Curve getCurveAt( final int index )
  {
    return (GM_Curve) super.getPrimitiveAt( index );
  }

  /**
   * returns all GM_Curves as array
   */
  public GM_Curve[] getAllCurves( )
  {
    return m_aggregate.toArray( new GM_Curve[getSize()] );
  }

  /**
   * returns true if the submitted GM_Curve is within the aggregation
   */
  public boolean isMember( final GM_Curve gmc )
  {
    return super.isMember( gmc );
  }

  /**
   * calculates the bounding box / envelope of the aggregation
   */
  protected void calculateEnvelope( )
  {
    final int size = getSize();
    if( size == 0 )
      return;

    final GM_Envelope bb = getCurveAt( 0 ).getEnvelope();

    final double[] min = bb.getMin().getAsArray().clone();
    final double[] max = bb.getMax().getAsArray().clone();

    for( int i = 1; i < size; i++ )
    {
      final double[] pos1 = getCurveAt( i ).getEnvelope().getMin().getAsArray();
      final double[] pos2 = getCurveAt( i ).getEnvelope().getMax().getAsArray();

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

    setEnvelope( new GM_Envelope_Impl( new GM_Position_Impl( min ), new GM_Position_Impl( max ) ) );
  }

  /**
   * calculates the centroid of the aggregation
   */
  protected void calculateCentroid( )
  {
    double cnt = 0;

    final int size = getSize();
    // If we are empty we dont have a centroid
    if( size == 0 )
      return;

    final GM_Point gmp = getCurveAt( 0 ).getCentroid();

    final double[] cen = new double[gmp.getAsArray().length];
    
    final int dimSize = Math.min( cen.length, getCoordinateDimension() );

    for( int i = 0; i < size; i++ )
    {
      cnt += getCurveAt( i ).getNumberOfCurveSegments();

      final double[] pos = getCurveAt( i ).getCentroid().getAsArray();
      
      // pos.length is always 2, so we have ArrayIndexOutOfBoundsException always when dimSize > 2
      // TODO consider this
      
      final int dimSize2 = Math.min( pos.length, dimSize );
      for( int j = 0; j < dimSize2; j++ )
//      for( int j = 0; j < dimSize; j++ )
      {
        cen[j] += pos[j];
      }
    }

    for( int j = 0; j < dimSize; j++ )
    {
      cen[j] = cen[j] / cnt / size;
    }

    setCentroid( new GM_Point_Impl( new GM_Position_Impl( cen ), null ) );
  }

  /**
   * calculates the centroid and envelope of the aggregation
   */
  @Override
  protected void calculateParam( )
  {
    calculateCentroid();
    calculateEnvelope();
    setValid( true );
  }

  /**
   * The operation "dimension" shall return the inherent dimension of this GM_Object, which shall be less than or equal
   * to the coordinate dimension. The dimension of a collection of geometric objects shall be the largest dimension of
   * any of its pieces. Points are 0-dimensional, curves are 1-dimensional, surfaces are 2-dimensional, and solids are
   * 3-dimensional.
   */
  @Override
  public int getDimension( )
  {
    return 1;
  }

  /**
   * The operation "coordinateDimension" shall return the dimension of the coordinates that define this GM_Object, which
   * must be the same as the coordinate dimension of the coordinate reference system for this GM_Object.
   */
  @Override
  public int getCoordinateDimension( )
  {
    GM_CurveSegment sp = null;

    try
    {
      sp = getCurveAt( 0 ).getCurveSegmentAt( 0 );
    }
    catch( final Exception ex )
    {
    }

    return sp.getPositionAt( 0 ).getAsArray().length;
  }

  /**
   * @see org.kalypsodeegree_impl.model.geometry.GM_Object_Impl#clone()
   */
  @Override
  public GM_Object clone( ) throws CloneNotSupportedException
  {
    // kuch
    final GM_Curve[] curves = getAllCurves();
    final List<GM_Curve> myCurves = new LinkedList<GM_Curve>();
    for( final GM_Curve curve : curves )
    {
      myCurves.add( (GM_Curve) curve.clone() );
    }

    return new GM_MultiCurve_Impl( myCurves.toArray( new GM_Curve[] {} ) );
  }
}