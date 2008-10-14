/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.model.geometry;

import java.io.Serializable;

import org.deegree.crs.transformations.CRSTransformation;
import org.kalypsodeegree.model.geometry.GM_Aggregate;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Ring;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfaceBoundary;
import org.kalypsodeegree.model.geometry.GM_SurfaceInterpolation;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;

/**
 * default implementierung of the GM_Polygon interface from package jago.model.
 * ------------------------------------------------------------
 * 
 * @version 11.6.2001
 * @author Andreas Poth
 */
class GM_Polygon_Impl extends GM_SurfacePatch_Impl implements GM_Polygon, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = -1293845886457211088L;

  private GM_SurfaceBoundary boundary = null;

  /**
   * Creates a new GM_Polygon_Impl object.
   * 
   * @param interpolation
   * @param exteriorRing
   * @param interiorRings
   * @param crs
   * @throws GM_Exception
   */
  public GM_Polygon_Impl( final GM_SurfaceInterpolation interpolation, final GM_Position[] exteriorRing, final GM_Position[][] interiorRings, final String crs ) throws GM_Exception
  {
    super( interpolation, exteriorRing, interiorRings, crs );

    final GM_Ring outer = new GM_Ring_Impl( exteriorRing, crs );
    GM_Ring[] inner = null;

    if( interiorRings != null )
    {
      inner = new GM_Ring[interiorRings.length];

      for( int i = 0; i < inner.length; i++ )
      {
        inner[i] = new GM_Ring_Impl( interiorRings[i], crs );
      }
    }

    boundary = new GM_SurfaceBoundary_Impl( outer, inner );
  }

  /**
   * The operation "boundary" shall return the boundary of this GM_SurfacePatch represented as a collection of GM_Curves
   * organized as a GM_SurfaceBoundary, consisting of GM_Curve instances along the boundary of the aggregate GM_Surface,
   * and interior to the GM_Surface where GM_SurfacePatches are adjacent.
   */
  public GM_SurfaceBoundary getBoundary( )
  {
    return boundary;
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
    if( !super.equals( other ) || !(other instanceof GM_Polygon_Impl) )
    {
      return false;
    }

    return true;
  }

  @Override
  public String toString( )
  {
    String ret = "GM_SurfacePatch: ";
    ret = "interpolation = " + m_interpolation + "\n";
    ret += "exteriorRing = \n";

    for( final GM_Position element : m_exteriorRing )
    {
      ret += (element + "\n");
    }

    ret += ("interiorRings = " + m_interiorRings + "\n");
    ret += ("envelope = " + m_envelope + "\n");
    return ret;
  }

  /**
   * returns a deep copy of the geometry
   */
  @Override
  public Object clone( )
  {
    // kuch
    final GM_Position[] clonedExteriorRing = GeometryFactory.cloneGM_Position( getExteriorRing() );

    final GM_Position[][] interiorRings = getInteriorRings();
    final GM_Position[][] clonedInterior = new GM_Position[interiorRings.length][];

    for( int i = 0; i < interiorRings.length; i++ )
    {
      clonedInterior[i] = GeometryFactory.cloneGM_Position( interiorRings[i] );
    }

    try
    {
      int value = GM_SurfaceInterpolation.NONE;
      GM_SurfaceInterpolation interpolation = getInterpolation();
      if( interpolation != null )
        value = interpolation.getValue();

      return new GM_Polygon_Impl( new GM_SurfaceInterpolation_Impl( value ), clonedExteriorRing, clonedInterior, m_crs );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }

    throw (new IllegalStateException());
  }

  /**
   * The Boolean valued operation "intersects" shall return TRUE if this GM_Object intersects another GM_Object. Within
   * a GM_Complex, the GM_Primitives do not intersect one another. In general, topologically structured data uses shared
   * geometric objects to capture intersection information.
   */
  public boolean intersects( final GM_Object gmo )
  {
    boolean inter = false;

    try
    {
      if( gmo instanceof GM_Point )
      {
        inter = LinearIntersects.intersects( ((GM_Point) gmo).getPosition(), this );
      }
      else if( gmo instanceof GM_Curve )
      {
        inter = LinearIntersects.intersects( (GM_Curve) gmo, new GM_Surface_Impl( this ) );
      }
      else if( gmo instanceof GM_Surface )
      {
        inter = LinearIntersects.intersects( (GM_Surface) gmo, new GM_Surface_Impl( this ) );
      }
      else if( gmo instanceof GM_Aggregate )
      {
        inter = intersectsMultiObject( (GM_Aggregate) gmo );
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
  private boolean intersectsMultiObject( final GM_Aggregate mprim ) throws Exception
  {
    boolean inter = false;

    final int cnt = mprim.getSize();

    for( int i = 0; i < cnt; i++ )
    {
      if( intersects( mprim.getObjectAt( i ) ) )
      {
        inter = true;
        break;
      }
    }

    return inter;
  }

  /**
   * The Boolean valued operation "contains" shall return TRUE if this GM_Object contains another GM_Object.
   * <p>
   * </p>
   */
  public boolean contains( final GM_Object gmo )
  {
    boolean contain = false;

    try
    {
      if( gmo instanceof GM_Point )
      {
        contain = LinearContains.contains( this, ((GM_Point) gmo).getPosition() );
      }
      else if( gmo instanceof GM_Curve )
      {
        // contain = contain_.contains ( new GM_Surface_Impl ( this ),
        // (GM_Curve)gmo );
        contain = LinearContains.contains( this, ((GM_Curve) gmo).getAsLineString() );
      }
      else if( gmo instanceof GM_Surface )
      {
        contain = LinearContains.contains( new GM_Surface_Impl( this ), (GM_Surface) gmo );
      }
      else if( gmo instanceof GM_Aggregate )
      {
        contain = containsMultiObject( (GM_Aggregate) gmo );
      }
    }
    catch( final Exception e )
    {
    }

    return contain;
  }

  private boolean containsMultiObject( final GM_Aggregate gmo )
  {
    try
    {
      for( int i = 0; i < gmo.getSize(); i++ )
      {
        if( !contains( gmo.getObjectAt( i ) ) )
        {
          return false;
        }
      }
    }
    catch( final Exception e )
    {
    }

    return true;
  }

  /**
   * @see org.kalypsodeegree_impl.model.geometry.GM_SurfacePatch_Impl#invalidate()
   */
  @Override
  public void invalidate( )
  {
    boundary.invalidate();

    super.invalidate();
  }

  /**
   * @see org.kalypsodeegree.model.geometry.GM_SurfacePatch#transform(org.deegree.crs.transformations.CRSTransformation,
   *      java.lang.String)
   */
  public GM_SurfacePatch transform( final CRSTransformation trans, final String targetOGCCS ) throws Exception
  {
    /* If the target is the same coordinate system, do not transform. */
    String coordinateSystem = getCoordinateSystem();
    if( coordinateSystem == null || coordinateSystem.equalsIgnoreCase( targetOGCCS ) )
      return this;

    /* exterior ring */
    final GM_Ring exRing = GeometryFactory.createGM_Ring( getExteriorRing(), getCoordinateSystem() );
    final GM_Ring transExRing = (GM_Ring) exRing.transform( trans, targetOGCCS );

    /* interior rings */
    final GM_Position[][] interiors = getInteriorRings();
    if( interiors == null )
      return new GM_Polygon_Impl( m_interpolation, transExRing.getPositions(), null, targetOGCCS );

    final GM_Position[][] transInRings = new GM_Position[interiors.length][];

    for( int j = 0; j < interiors.length; j++ )
    {
      final GM_Ring inRing = GeometryFactory.createGM_Ring( interiors[j], getCoordinateSystem() );
      transInRings[j] = ((GM_Ring) inRing.transform( trans, targetOGCCS )).getPositions();
    }

    return new GM_Polygon_Impl( m_interpolation, transExRing.getPositions(), transInRings, targetOGCCS );
  }
}