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

 Contact :

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

import org.deegree.model.geometry.GM_Boundary;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Position;
import org.deegree_impl.tools.Debug;
import org.opengis.cs.CS_CoordinateSystem;

import com.vividsolutions.jts.geom.Geometry;

/**
 * Default implementation of the GM_Object interface from package deegree.model.
 * The implementation is abstract because only the management of the spatial
 * reference system is unique for all geometries.
 * <p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public abstract class GM_Object_Impl implements GM_Object, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = 130728662284673112L;

  protected static double mute = 0.000000001;

  protected CS_CoordinateSystem crs = null;

  protected GM_Boundary boundary = null;

  protected GM_Envelope envelope = null;

  protected GM_Object convexHull = null;

  protected GM_Point centroid = null;

  protected boolean empty = true;

  protected boolean valid = false;

  /**
   * constructor that sets the spatial reference system
   * 
   * @param crs
   *          new spatial reference system
   */
  protected GM_Object_Impl( CS_CoordinateSystem crs )
  {
    setCoordinateSystem( crs );
  }

  /**
   * returns the spatial reference system of a geometry
   */
  public CS_CoordinateSystem getCoordinateSystem()
  {
    return crs;
  }

  /**
   * sets the spatial reference system
   * 
   * @param crs
   *          new spatial reference system
   */
  public void setCoordinateSystem( CS_CoordinateSystem crs )
  {
    this.crs = crs;
  }

  /**
   * returns a shallow copy of the geometry. this isn't realized at this level
   * so a CloneNotSupportedException will be thrown.
   */
  public Object clone() throws CloneNotSupportedException
  {
    throw new CloneNotSupportedException();
  }

  /**
   * returns true if no geometry values resp. points stored within the geometry.
   */
  public boolean isEmpty()
  {
    return empty;
  }

  /**
   * indicates the geometry as empty
   */
  public void setEmpty( boolean empty )
  {
    this.empty = empty;
  }

  /**
   * returns the boundary of the surface as general boundary
   */
  public GM_Boundary getBoundary()
  {
    if( !isValid() )
    {
      calculateParam();
    }
    return boundary;
  }

  /**
   * dummy implementation of this method
   */
  public void translate( double[] d )
  {
    setValid( false );
  }

  /**
   * The operation "distance" shall return the distance between this GM_Object
   * and another GM_Object. This distance is defined to be the greatest lower
   * bound of the set of distances between all pairs of points that include one
   * each from each of the two GM_Objects. A "distance" value shall be a
   * positive number associated to distance units such as meters or standard
   * foot. If necessary, the second geometric object shall be transformed into
   * the same coordinate reference system as the first before the distance is
   * calculated.
   * <p>
   * </p>
   * If the geometric objects overlap, or touch, then their distance apart shall
   * be zero. Some current implementations use a "negative" distance for such
   * cases, but the approach is neither consistent between implementations, nor
   * theoretically viable.
   * <p>
   * </p>
   * dummy implementation
   */
  public double distance( GM_Object gmo )
  {
    // ziemlicher hack, um die distance zu ermitteln, vermutlich sehr teuer (=langsam)
    
    try
    {
      final Geometry otherGmo = JTSAdapter.export( gmo );
      final Geometry thisGmo = JTSAdapter.export( this );
      
      return otherGmo.distance( thisGmo );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }
    
    return -9999;
  }

  /**
   * The operation "centroid" shall return the mathematical centroid for this
   * GM_Object. The result is not guaranteed to be on the object. For
   * heterogeneous collections of primitives, the centroid only takes into
   * account those of the largest dimension. For example, when calculating the
   * centroid of surfaces, an average is taken weighted by area. Since curves
   * have no area they do not contribute to the average.
   * <p>
   * </p>
   */
  public GM_Point getCentroid()
  {
    if( !isValid() )
    {
      calculateParam();
    }
    return centroid;
  }

  /**
   * returns the bounding box / envelope of a geometry
   */
  public GM_Envelope getEnvelope()
  {
    if( !isValid() )
    {
      calculateParam();
    }
    return envelope;
  }

  /**
   * The operation "convexHull" shall return a GM_Object that represents the
   * convex hull of this GM_Object.
   * <p>
   * </p>
   * dummy implementation
   */
  public GM_Object getConvexHull()
  {
    if( !isValid() )
    {
      calculateParam();
    }
    return null;
  }

  /**
   * The operation "buffer" shall return a GM_Object containing all points whose
   * distance from this GM_Object is less than or equal to the "distance" passed
   * as a parameter. The GM_Object returned is in the same reference system as
   * this original GM_Object. The dimension of the returned GM_Object is
   * normally the same as the coordinate dimension - a collection of GM_Surfaces
   * in 2D space and a collection of GM_Solids in 3D space, but this may be
   * application defined.
   * <p>
   * </p>
   * dummy implementation
   */
  public GM_Object getBuffer( double distance )
  {
    return null;
  }

  /**
   * The Boolean valued operation "contains" shall return TRUE if this GM_Object
   * contains another GM_Object.
   * <p>
   * 
   * @param that
   *          the GM_Object to test (whether is is contained)
   * @return true if the given object is contained, else false
   */
  public boolean contains( GM_Object that )
  {
    try
    {
      // let JTS do the hard work
      Geometry jtsThis = JTSAdapter.export( this );
      Geometry jtsThat = JTSAdapter.export( that );
      return jtsThis.contains( jtsThat );

    }
    catch( GM_Exception e )
    {
      System.out.println( e );
      return false;
    }
  }

  /**
   * The Boolean valued operation "contains" shall return TRUE if this GM_Object
   * contains a single point given by a coordinate.
   * <p>
   * 
   * @param position
   *          GM_Position to test (whether is is contained)
   * @return true if the given object is contained, else false
   */
  public boolean contains( GM_Position position )
  {
    return contains( new GM_Point_Impl( position, null ) );
  }

  /**
   * The Boolean valued operation "intersects" shall return TRUE if this
   * GM_Object intersects another GM_Object. Within a GM_Complex, the
   * GM_Primitives do not intersect one another. In general, topologically
   * structured data uses shared geometric objects to capture intersection
   * information.
   * <p>
   * 
   * @param that
   *          the GM_Object to intersect with
   * @return true if the objects intersects, else false
   */
  public boolean intersects( GM_Object that )
  {
    try
    {
      // let JTS do the hard work
      Geometry jtsThis = JTSAdapter.export( this );
      Geometry jtsThat = JTSAdapter.export( that );
      return jtsThis.intersects( jtsThat );

    }
    catch( GM_Exception e )
    {
      System.out.println( e );
      return false;
    }
  }

  /**
   * The "union" operation shall return the set theoretic union of this
   * GM_Object and the passed GM_Object.
   * <p>
   * 
   * @param that
   *          the GM_Object to unify
   * @return intersection or null, if computation failed
   */
  public GM_Object union( GM_Object that )
  {
    GM_Object union = null;

    try
    {
      // let JTS do the hard work
      Geometry jtsThis = JTSAdapter.export( this );
      Geometry jtsThat = JTSAdapter.export( that );
      Geometry jtsUnion = jtsThis.union( jtsThat );

      if( !jtsUnion.isEmpty() )
      {
        union = JTSAdapter.wrap( jtsUnion );
        ( (GM_Object_Impl)union ).setCoordinateSystem( getCoordinateSystem() );
      }
    }
    catch( GM_Exception e )
    {
      System.out.println( e );
    }
    return union;
  }

  /**
   * The "intersection" operation shall return the set theoretic intersection of
   * this <tt>GM_Object</tt> and the passed <tt>GM_Object</tt>.
   * <p>
   * 
   * @param that
   *          the GM_Object to intersect with
   * @return intersection or null, if it is empty (or computation failed)
   */
  public GM_Object intersection( GM_Object that )
  {

    GM_Object intersection = null;

    try
    {
      // let JTS do the hard work
      Geometry jtsThis = JTSAdapter.export( this );
      Geometry jtsThat = JTSAdapter.export( that );
      Geometry jtsIntersection = jtsThis.intersection( jtsThat );

      if( !jtsIntersection.isEmpty() )
      {
        intersection = JTSAdapter.wrap( jtsIntersection );
        ( (GM_Object_Impl)intersection ).setCoordinateSystem( getCoordinateSystem() );
      }
    }
    catch( GM_Exception e )
    {
      System.out.println( e );
    }
    return intersection;
  }

  /**
   * The "difference" operation shall return the set theoretic difference of
   * this GM_Object and the passed GM_Object.
   * <p>
   * 
   * @param that
   *          the GM_Object to calculate the difference with
   * @return difference or null, if it is empty (or computation failed)
   */
  public GM_Object difference( GM_Object that )
  {
    GM_Object difference = null;

    try
    {
      // let JTS do the hard work
      Geometry jtsThis = JTSAdapter.export( this );
      Geometry jtsThat = JTSAdapter.export( that );
      Geometry jtsDifference = jtsThis.difference( jtsThat );

      if( !jtsDifference.isEmpty() )
      {
        difference = JTSAdapter.wrap( jtsDifference );
        ( (GM_Object_Impl)difference ).setCoordinateSystem( getCoordinateSystem() );
      }
    }
    catch( GM_Exception e )
    {
      System.out.println( e );
    }
    return difference;
  }

  /**
   * Compares the GM_Object to be equal to another GM_Object.
   * <p>
   * 
   * @param that
   *          the GM_Object to test for equality
   * @return true if the objects are equal, else false
   */
  public boolean equals( Object that )
  {
    if( ( that == null ) || !( that instanceof GM_Object_Impl ) )
    {
      return false;
    }

    if( crs != null )
    {
      if( !crs.equals( ( (GM_Object)that ).getCoordinateSystem() ) )
      {
        return false;
      }
    }
    else
    {
      if( ( (GM_Object)that ).getCoordinateSystem() != null )
      {
        return false;
      }
    }

    try
    {
      // let JTS do the hard work
      Geometry jtsThis = JTSAdapter.export( this );
      Geometry jtsThat = JTSAdapter.export( (GM_Object)that );
      return jtsThis.equals( jtsThat );
    }
    catch( GM_Exception e )
    {
      System.out.println( e );
      return false;
    }
  }

  /*
   * provide optimized proximity queries within for a distance . calvin added on
   * 10/21/2003
   */
  public boolean isWithinDistance( GM_Object that, double distance )
  {
    if( that == null )
      return false;
    try
    {
      // let JTS do the hard work
      Geometry jtsThis = JTSAdapter.export( this );
      Geometry jtsThat = JTSAdapter.export( that );
      return jtsThis.isWithinDistance( jtsThat, distance );
    }
    catch( GM_Exception e )
    {
      Debug.debugException( e, "" );
      return false;
    }

  }

  /**
   * invalidates the calculated parameters of the GM_Object
   */
  protected void setValid( boolean valid )
  {
    this.valid = valid;
  }

  /**
   * returns true if the calculated parameters of the GM_Object are valid and
   * false if they must be recalculated
   */
  protected boolean isValid()
  {
    return valid;
  }

  /**
   * recalculates internal parameters
   */
  protected abstract void calculateParam();

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = null;
    ret = "CoordinateSystem = " + crs + "\n";
    ret += ( "empty = " + empty + "\n" );
    ret += ( "mute = " + mute + "\n" );
    return ret;
  }
}