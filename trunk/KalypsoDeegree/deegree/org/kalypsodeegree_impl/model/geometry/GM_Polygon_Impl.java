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

import org.deegree.model.geometry.GM_Aggregate;
import org.deegree.model.geometry.GM_Curve;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Polygon;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Ring;
import org.deegree.model.geometry.GM_Surface;
import org.deegree.model.geometry.GM_SurfaceBoundary;
import org.deegree.model.geometry.GM_SurfaceInterpolation;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * default implementierung of the GM_Polygon interface from package jago.model.
 * 
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
   * 
   * @throws GM_Exception
   */
  public GM_Polygon_Impl( GM_SurfaceInterpolation interpolation, GM_Position[] exteriorRing,
      GM_Position[][] interiorRings, CS_CoordinateSystem crs ) throws GM_Exception
  {
    super( interpolation, exteriorRing, interiorRings, crs );

    GM_Ring outer = new GM_Ring_Impl( exteriorRing, crs );
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
   * The operation "boundary" shall return the boundary of this GM_SurfacePatch
   * represented as a collection of GM_Curves organized as a GM_SurfaceBoundary,
   * consisting of GM_Curve instances along the boundary of the aggregate
   * GM_Surface, and interior to the GM_Surface where GM_SurfacePatches are
   * adjacent.
   */
  public GM_SurfaceBoundary getBoundary()
  {
    return boundary;
  }

  /**
   * checks if this curve is completly equal to the submitted geometry
   * 
   * @param other
   *          object to compare to
   */
  public boolean equals( Object other )
  {
    if( !super.equals( other ) || !( other instanceof GM_Polygon_Impl ) )
    {
      return false;
    }

    return true;
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = "GM_SurfacePatch: ";
    ret = "interpolation = " + interpolation + "\n";
    ret += "exteriorRing = \n";

    for( int i = 0; i < exteriorRing.length; i++ )
    {
      ret += ( exteriorRing[i] + "\n" );
    }

    ret += ( "interiorRings = " + interiorRings + "\n" );
    ret += ( "envelope = " + envelope + "\n" );
    return ret;
  }

  /**
   * returns a shallow copy of the geometry
   */
  public Object clone()
  {
    GM_Polygon p = null;

    try
    {
      p = new GM_Polygon_Impl( new GM_SurfaceInterpolation_Impl( getInterpolation().getValue() ),
          getExteriorRing(), getInteriorRings(), this.crs );
    }
    catch( Exception ex )
    {
      System.out.println( "GM_Polygon_Impl.clone: " + ex );
    }

    return p;
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
    boolean inter = false;

    try
    {
      if( gmo instanceof GM_Point )
      {
        inter = LinearIntersects.intersects( ( (GM_Point)gmo ).getPosition(), this );
      }
      else if( gmo instanceof GM_Curve )
      {
        inter = LinearIntersects.intersects( (GM_Curve)gmo, new GM_Surface_Impl( this ) );
      }
      else if( gmo instanceof GM_Surface )
      {
        inter = LinearIntersects.intersects( (GM_Surface)gmo, new GM_Surface_Impl( this ) );
      }
      else if( gmo instanceof GM_Aggregate )
      {
        inter = intersectsMultiObject( (GM_Aggregate)gmo );
      }
    }
    catch( Exception e )
    {}

    return inter;
  }

  /**
   * the operations returns true if the submitted multi primitive intersects
   * with the curve segment
   */
  private boolean intersectsMultiObject( GM_Aggregate mprim ) throws Exception
  {
    boolean inter = false;

    int cnt = mprim.getSize();

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
   * The Boolean valued operation "contains" shall return TRUE if this GM_Object
   * contains another GM_Object.
   * <p>
   * </p>
   */
  public boolean contains( GM_Object gmo )
  {
    boolean contain = false;

    try
    {
      if( gmo instanceof GM_Point )
      {
        contain = LinearContains.contains( this, ( (GM_Point)gmo ).getPosition() );
      }
      else if( gmo instanceof GM_Curve )
      {
        //                    contain = contain_.contains ( new GM_Surface_Impl ( this ),
        //                                                 (GM_Curve)gmo );
        contain = LinearContains.contains( this, ( (GM_Curve)gmo ).getAsLineString() );
      }
      else if( gmo instanceof GM_Surface )
      {
        contain = LinearContains.contains( new GM_Surface_Impl( this ), (GM_Surface)gmo );
      }
      else if( gmo instanceof GM_Aggregate )
      {
        contain = containsMultiObject( (GM_Aggregate)gmo );
      }
    }
    catch( Exception e )
    {}

    return contain;
  }

  /**
   * 
   * 
   * @param gmo
   * 
   * @return
   */
  private boolean containsMultiObject( GM_Aggregate gmo )
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
    catch( Exception e )
    {}

    return true;
  }
}