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

import org.deegree.model.geometry.GM_Curve;
import org.deegree.model.geometry.GM_CurveSegment;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_LineString;
import org.deegree.model.geometry.GM_MultiPrimitive;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Surface;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * default implementation of the GM_LineString interface of package jago.model.
 * 
 * ------------------------------------------------------------
 * 
 * @version 10.6.2001
 * @author Andreas Poth
 */
class GM_LineString_Impl extends GM_CurveSegment_Impl implements GM_LineString, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = 8093549521711824076L;

  /**
   * Creates a new GM_LineString_Impl object.
   * 
   * @param gmps
   * @param cs
   * 
   * @throws GM_Exception
   */
  public GM_LineString_Impl( GM_Position[] gmps, CS_CoordinateSystem cs ) throws GM_Exception
  {
    super( gmps, cs );
  }

  /**
   * returns a shallow copy of the geometry
   */
  public Object clone()
  {
    GM_CurveSegment cs = null;

    try
    {
      cs = new GM_LineString_Impl( getPositions(), getCoordinateSystem() );
    }
    catch( Exception ex )
    {
      System.out.println( "GM_LineString_Impl.clone: " + ex );
    }

    return cs;
  }

  /**
   * returns the length of the curve in units of the related spatial reference
   * system
   */
  public double getLength()
  {
    return -1;
  }

  /**
   * returns a reference to itself
   */
  public GM_LineString getAsLineString() throws GM_Exception
  {
    return this;
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
        GM_CurveSegment[] cs = new GM_CurveSegment[]
        { this };
        LinearIntersects inter_ = new LinearIntersects();
        inter = LinearIntersects.intersects( (GM_Curve)gmo, new GM_Curve_Impl( cs ) );
      }
      else if( gmo instanceof GM_Surface )
      {
        GM_CurveSegment[] cs = new GM_CurveSegment[]
        { this };
        LinearIntersects inter_ = new LinearIntersects();
        inter = LinearIntersects.intersects( new GM_Curve_Impl( cs ), (GM_Surface)gmo );
      }
      else if( gmo instanceof GM_MultiPrimitive )
      {
        inter = intersectsMultiPrimitive( (GM_MultiPrimitive)gmo );
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
  private boolean intersectsMultiPrimitive( GM_MultiPrimitive mprim ) throws Exception
  {
    boolean inter = false;

    int cnt = mprim.getSize();

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
   * The Boolean valued operation "contains" shall return TRUE if this GM_Object
   * contains another GM_Object.
   */
  public boolean contains( GM_Object gmo )
  {
    return false;
  }
}