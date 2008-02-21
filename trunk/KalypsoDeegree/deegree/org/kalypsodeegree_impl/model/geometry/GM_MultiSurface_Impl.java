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

import org.deegree.crs.transformations.CRSTransformation;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * default implementation of the GM_MultiSurface interface from package jago.model.
 * <p>
 * ------------------------------------------------------------
 * </p>
 * 
 * @version 12.6.2001
 * @author Andreas Poth
 *         <p>
 */
final class GM_MultiSurface_Impl extends GM_MultiPrimitive_Impl implements GM_MultiSurface, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = -6471121873087659850L;

  private double area = 0;

  /**
   * Creates a new GM_MultiSurface_Impl object.
   * 
   * @param crs
   */
  public GM_MultiSurface_Impl( final String crs )
  {
    super( crs );
  }

  /**
   * Creates a new GM_MultiSurface_Impl object.
   * 
   * @param surface
   */
  public GM_MultiSurface_Impl( final GM_Surface< ? >[] surface )
  {
    super( null );

    for( final GM_Surface< ? > element : surface )
    {
      m_aggregate.add( element );
    }

    setValid( false );
  }

  /**
   * Creates a new GM_MultiSurface_Impl object.
   * 
   * @param surface
   * @param crs
   */
  public GM_MultiSurface_Impl( final GM_Surface< ? >[] surface, final String crs )
  {
    super( crs );

    for( final GM_Surface< ? > element : surface )
    {
      m_aggregate.add( element );
    }

    setValid( false );
  }

  /**
   * adds an GM_Surface to the aggregation
   */
  public void addSurface( final GM_Surface< ? > gms )
  {
    super.add( gms );
  }

  /**
   * inserts a GM_Surface in the aggregation. all elements with an index equal or larger index will be moved. if index
   * is larger then getSize() - 1 or smaller then 0 or gms equals null an exception will be thrown.
   * 
   * @param gms
   *            GM_Surface to insert.
   * @param index
   *            position where to insert the new GM_Surface
   */
  public void insertSurfaceAt( final GM_Surface< ? > gms, final int index ) throws GM_Exception
  {
    super.insertObjectAt( gms, index );
  }

  /**
   * sets the submitted GM_Surface at the submitted index. the element at the position <code>index</code> will be
   * removed. if index is larger then getSize() - 1 or smaller then 0 or gms equals null an exception will be thrown.
   * 
   * @param gms
   *            GM_Surface to set.
   * @param index
   *            position where to set the new GM_Surface
   */
  public void setSurfaceAt( final GM_Surface< ? > gms, final int index ) throws GM_Exception
  {
    setObjectAt( gms, index );
  }

  /**
   * removes the submitted GM_Surface from the aggregation
   * 
   * @return the removed GM_Surface
   */
  public GM_Surface< ? > removeSurface( final GM_Surface< ? > gms )
  {
    return (GM_Surface< ? >) super.removeObject( gms );
  }

  /**
   * removes the GM_Surface at the submitted index from the aggregation. if index is larger then getSize() - 1 or
   * smaller then 0 an exception will be thrown.
   * 
   * @return the removed GM_Surface
   */
  public GM_Surface< ? > removeSurfaceAt( final int index ) throws GM_Exception
  {
    return (GM_Surface< ? >) super.removeObjectAt( index );
  }

  /**
   * returns the GM_Surface at the submitted index.
   */
  public GM_Surface< ? > getSurfaceAt( final int index )
  {
    return (GM_Surface< ? >) super.getPrimitiveAt( index );
  }

  /**
   * returns all GM_Surfaces as array
   */
  public GM_Surface< ? >[] getAllSurfaces( )
  {
    return m_aggregate.toArray( new GM_Surface[getSize()] );
  }

  /**
   * @see org.kalypsodeegree_impl.model.geometry.GM_Primitive_Impl#getAdapter(java.lang.Class)
   */
  @SuppressWarnings("unchecked")
  @Override
  public Object getAdapter( final Class adapter )
  {
    if( adapter == GM_SurfacePatch[].class )
    {
      final List<GM_SurfacePatch> patchList = new LinkedList<GM_SurfacePatch>();

      final GM_Surface< ? >[] surfaces = getAllSurfaces();

      for( final GM_Surface< ? > surface : surfaces )
      {
        final GM_SurfacePatch[] surfacePatches = (GM_SurfacePatch[]) surface.getAdapter( GM_SurfacePatch[].class );
        for( final GM_SurfacePatch surfacePatch : surfacePatches )
        {
          patchList.add( surfacePatch );
        }
      }
      return patchList.toArray( new GM_SurfacePatch[patchList.size()] );
    }

    if( adapter == GM_Curve.class )
    {
      final List<GM_Curve> curveList = new LinkedList<GM_Curve>();

      final GM_Surface< ? >[] surfaces = getAllSurfaces();

      for( final GM_Surface< ? > surface : surfaces )
      {
        final GM_SurfacePatch[] surfacePatches = (GM_SurfacePatch[]) surface.getAdapter( GM_SurfacePatch[].class );
        for( final GM_SurfacePatch surfacePatch : surfacePatches )
        {
          final GM_Position[] exteriorRing = surfacePatch.getExteriorRing();
          try
          {
            curveList.add( GeometryFactory.createGM_Curve( exteriorRing, getCoordinateSystem() ) );
          }
          catch( final GM_Exception e )
          {
            final IStatus status = StatusUtilities.statusFromThrowable( e );
            KalypsoDeegreePlugin.getDefault().getLog().log( status );
            return null;
          }
        }
      }
      return curveList.toArray( new GM_Curve[curveList.size()] );

    }

    return super.getAdapter( adapter );
  }

  /**
   * calculates the bounding box / envelope of the aggregation
   */
  private void calculateEnvelope( )
  {
    final GM_Envelope bb = getSurfaceAt( 0 ).getEnvelope();

    final double[] min = bb.getMin().getAsArray().clone();
    final double[] max = bb.getMax().getAsArray().clone();

    final int size = getSize();
    for( int i = 1; i < size; i++ )
    {
      final double[] pos1 = getSurfaceAt( i ).getEnvelope().getMin().getAsArray();
      final double[] pos2 = getSurfaceAt( i ).getEnvelope().getMax().getAsArray();

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
   * calculates the centroid and area of the aggregation
   */
  private void calculateCentroidArea( )
  {
    area = 0;
    // REMARK: we reduce to dimension 2 here, because everyone else (GM_Surface, GM_Curve)
    // always only produce 2-dim centroids, causing an ArrayOutOfBoundsException here...
    // Maybe it would be nice to always have a 3-dim centroid if possible
    final int cnt = Math.min( 2, getCoordinateDimension() );
    try
    {
      final double[] cen = new double[cnt];

      for( int i = 0; i < getSize(); i++ )
      {
        final double a = getSurfaceAt( i ).getArea();
        area = area + a;

        final double[] pos = getSurfaceAt( i ).getCentroid().getAsArray();

        for( int j = 0; j < cnt; j++ )
        {
          cen[j] = cen[j] + (pos[j] * a);
        }
      }

      for( int j = 0; j < cnt; j++ )
      {
        cen[j] = cen[j] / area;
      }

      setCentroid( new GM_Point_Impl( new GM_Position_Impl( cen ), null ) );
    }
    catch( final Exception e )
    {
      System.out.println( e );
    }
  }

  /**
   * calculates the centroid, area and envelope of the aggregation
   */
  @Override
  protected void calculateParam( )
  {
    calculateEnvelope();
    calculateCentroidArea();
    setValid( true );
  }

  /**
   * returns the area of the multi surface. this is calculate as the sum of all containing surface areas.
   */
  public double getArea( )
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
  @Override
  public Object clone( ) throws CloneNotSupportedException
  {
    // kuch
    final GM_Surface< ? >[] surfaces = getAllSurfaces();
    final List<GM_Surface< ? >> mySurfaces = new LinkedList<GM_Surface< ? >>();

    for( final GM_Surface< ? > surface : surfaces )
    {
      mySurfaces.add( (GM_Surface< ? >) surface.clone() );
    }

    return new GM_MultiSurface_Impl( mySurfaces.toArray( new GM_Surface[] {} ) );
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
    return 2;
  }

  /**
   * The operation "coordinateDimension" shall return the dimension of the coordinates that define this GM_Object, which
   * must be the same as the coordinate dimension of the coordinate reference system for this GM_Object.
   */
  @Override
  public int getCoordinateDimension( )
  {
    final GM_SurfacePatch sp = getSurfaceAt( 0 ).get( 0 );

    return sp.getExteriorRing()[0].getAsArray().length;
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_Object#transform(org.kalypsodeegree_impl.model.ct.MathTransform,
   *      org.opengis.cs.CS_CoordinateSystem)
   */
  @Override
  public GM_Object transform( CRSTransformation trans, String targetOGCCS ) throws Exception
  {
    /* If the target is the same coordinate system, do not transform. */
    String coordinateSystem = getCoordinateSystem();
    if( coordinateSystem == null || coordinateSystem.equalsIgnoreCase( targetOGCCS ) )
      return this;

    Debug.debugMethodBegin( this, "transformMultiSurface" );

    final GM_Surface[] surfaces = new GM_Surface[getSize()];

    for( int i = 0; i < getSize(); i++ )
    {
      surfaces[i] = (GM_Surface) getSurfaceAt( i ).transform( trans, targetOGCCS );
    }
    Debug.debugMethodEnd();
    return GeometryFactory.createGM_MultiSurface( surfaces, targetOGCCS );
  }
}