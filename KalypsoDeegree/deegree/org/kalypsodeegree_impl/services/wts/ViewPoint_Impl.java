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
package org.deegree_impl.services.wts;

import javax.media.j3d.Transform3D;
import javax.vecmath.Point3d;
import javax.vecmath.Vector3d;

import org.deegree.services.wts.ViewPoint;
import org.deegree_impl.tools.Debug;

/**
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:lupp@lat-lon.de">Katharina Lupp </a>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class ViewPoint_Impl implements ViewPoint
{
  private static double rad0 = Math.toRadians( 0 );

  private static double rad90 = Math.toRadians( 90 );

  private static double rad180 = Math.toRadians( 180 );

  private static double rad270 = Math.toRadians( 270 );

  private static double rad360 = Math.toRadians( 360 );

  private Point3d observerPosition = null;

  private Point3d targetPoint = null;

  private Point3d[] footprint = null;

  private double aov = 0;

  private double hdir = 0;

  private double vdir = 0;

  /** Creates a new instance of ViewPoint_Impl */
  public ViewPoint_Impl( double hdir, double vdir, double distance, Point3d targetPoint, double aov )
  {
    setHDirection( hdir );
    setVDirection( -vdir );
    setAoV( aov );
    setTarget( targetPoint );
    observerPosition = calcObserverPosition( distance );
    footprint = calcFootprint( targetPoint.y );

  }

  /** Creates a new instance of ViewPoint_Impl */
  public ViewPoint_Impl( Point3d observerPosition, Point3d targetPoint, double aov )
  {
    setAoV( aov );
    setObserverPosition( observerPosition );
    setTarget( targetPoint );

    double[] dir = calcDirections();
    setHDirection( dir[0] );
    setVDirection( dir[1] );
    footprint = calcFootprint( targetPoint.y );
  }

  /** Creates a new instance of ViewPoint_Impl */
  public ViewPoint_Impl( Point3d observerPosition, double hdir, double vdir, double aov )
      throws InvalidArcException
  {
    this( observerPosition, hdir, vdir, 100, aov );
  }

  /** Creates a new instance of ViewPoint_Impl */
  public ViewPoint_Impl( Point3d observerPosition, double hdir, double vdir, double height,
      double aov ) throws InvalidArcException
  {
    //        if ( hdir > rad360 || hdir < rad0 || vdir > rad90 || vdir < rad0 ) {
    //            throw new InvalidArcException();
    //        }
    setAoV( aov );
    setHDirection( hdir );
    setVDirection( -vdir );
    setObserverPosition( observerPosition );
    targetPoint = calcTargetPoint( height );
    footprint = calcFootprint( targetPoint.y );
  }

  /**
   * calculates the observers position for a given target point, distance and
   * view directions.
   * 
   * <pre>
   * VP
   * | \
   * |  \
   * |   \
   * |    \
   * |_____\_____________ 
   *      TP
   * </pre>
   */
  private Point3d calcObserverPosition( double distance )
  {
    Debug.debugMethodBegin( this, "calcObserverPosition" );

    double height = Math.tan( -vdir ) * distance;

    height += targetPoint.y;

    double x = -Math.sin( hdir ) * distance;
    double y = -Math.cos( hdir ) * distance;

    Debug.debugMethodEnd();
    return new Point3d( x + targetPoint.x, height, y + targetPoint.z );
  }

  /**
   * calculates the target point of the view for the submitted height
   * 
   * <pre>
   * VP
   * | \
   * |  \
   * |   \______________ height = XX
   * |   |\
   * |___|_\_____________ height = 0
   *   TP1  TP2
   * </pre>
   */
  private Point3d calcTargetPoint( double height )
  {
    Debug.debugMethodBegin( this, "calcTargetPoint" );

    double y = observerPosition.y - height;
    double c = y / Math.tan( vdir );
    double a = Math.sin( hdir ) * c;
    double b = Math.sqrt( ( c * c ) - ( a * a ) );

    Point3d tp = null;

    if( ( hdir >= rad0 ) && ( hdir <= rad90 ) )
    {
      tp = new Point3d( observerPosition.x + a, height, observerPosition.z + b );
    }
    else if( ( hdir >= rad90 ) && ( hdir <= rad180 ) )
    {
      tp = new Point3d( observerPosition.x + a, height, observerPosition.z - b );
    }
    else if( ( hdir >= rad180 ) && ( hdir <= rad270 ) )
    {
      tp = new Point3d( observerPosition.x + a, height, observerPosition.z - b );
    }
    else if( ( hdir >= rad270 ) && ( hdir <= rad360 ) )
    {
      tp = new Point3d( observerPosition.x + a, height, observerPosition.z + b );
    }

    Debug.debugMethodEnd();
    return tp;
  }

  /**
   * calculates the vertical and horizontal direction of the observer's view
   */
  private double[] calcDirections()
  {
    Debug.debugMethodBegin( this, "calcDirections" );

    //VDIR: calculation of vertical direction of the observer's view
    double b = observerPosition.y - targetPoint.y;
    double a = Math
        .sqrt( ( ( observerPosition.x - targetPoint.x ) * ( observerPosition.x - targetPoint.x ) )
            + ( ( observerPosition.z - targetPoint.z ) * ( observerPosition.z - targetPoint.z ) ) );
    double c = Math.sqrt( ( a * a ) + ( b * b ) );
    vdir = rad90 - Math.acos( b / c );

    //HDIR: calculation of horizontal direction of the observer's view
    double x = targetPoint.x - observerPosition.x;
    double z = targetPoint.z - observerPosition.z;
    hdir = Math.atan( x / z );

    if( ( x >= 0 ) && ( z <= 0 ) )
    {
      hdir += rad180;
    }
    else if( ( x <= 0 ) && ( z <= 0 ) )
    {
      hdir += rad180;
    }
    else if( ( x <= 0 ) && ( z >= 0 ) )
    {
      hdir += rad360;
    }

    Debug.debugMethodEnd();
    return new double[]
    { hdir, vdir };
  }

  /**
   * 
   * 
   * @param height
   * @param dist
   * 
   * @return
   */
  private double calcDir( double height, double dist )
  {
    double dir = Math.atan( height / dist );
    return dir;
  }

  /**
   * calculates footprint
   */
  private Point3d[] calcFootprint( double height )
  {
    Debug.debugMethodBegin( this, "calcFootprint" );

    double maxDist = -15000;

    Point3d[] fp = new Point3d[4];

    double h = observerPosition.y - height;
    double dir1 = calcDir( h, maxDist );
    double dir2 = vdir - ( aov / 2d );

    // footprint back border distance
    double b1 = h / Math.tan( dir1 );

    // footprint front border distance
    double b2 = 0;

    if( Math.toDegrees( dir2 ) > 90 )
    {
      b2 = -h / Math.tan( rad90 - dir2 );
    }
    else if( Math.toDegrees( dir2 ) < 0 )
    {
      b2 = h / Math.tan( dir2 );
    }
    else
    {
      b2 = 0;
    }

    // calc corners of the footprint back border
    double l = Math.sqrt( ( h * h ) + ( b1 * b1 ) );
    double bc = Math.tan( aov / 2d ) * l;
    fp[0] = new Point3d( bc, 0, -b1 );
    fp[1] = new Point3d( -bc, 0, -b1 );

    l = Math.sqrt( ( h * h ) + ( b2 * b2 ) );

    double fc = Math.tan( aov / 2d ) * l;
    fp[2] = new Point3d( fc, 0, -b2 );
    fp[3] = new Point3d( -fc, 0, -b2 );

    Transform3D rotTransform = new Transform3D();
    rotTransform.rotY( hdir );

    Transform3D translatTransform = new Transform3D();
    Vector3d vec = new Vector3d( observerPosition.x, observerPosition.y - height,
        observerPosition.z );
    translatTransform.setTranslation( vec );

    for( int i = 0; i < fp.length; i++ )
    {
      rotTransform.transform( fp[i] );
      translatTransform.transform( fp[i] );
    }

    Debug.debugMethodEnd();
    return fp;
  }

  /**
   * gets the field of view of the observer in radians
   *  
   */
  public double getAoV()
  {
    return aov;
  }

  /**
   * defines the field of view of the observer in radians
   *  
   */
  public void setAoV( double aov )
  {
    this.aov = aov;
  }

  /**
   * gets the horizontal direction in radians the observer looks
   *  
   */
  public double getHDirection()
  {
    return hdir;
  }

  /**
   * defines the horizontal direction in radians the observer looks
   *  
   */
  public void setHDirection( double hdir )
  {
    this.hdir = hdir;

    if( targetPoint != null )
    {
      targetPoint = calcTargetPoint( targetPoint.y );
    }
  }

  /**
   * gets the directions the observer looks and his field of view in radians
   */
  public Point3d getTarget()
  {
    return targetPoint;
  }

  /**
   * defines the directions the observer looks and his field of view in radians
   *  
   */
  public void setTarget( Point3d targetPoint )
  {
    this.targetPoint = targetPoint;
  }

  /**
   * gets vertical direction in radians the observer looks
   *  
   */
  public double getVDirection()
  {
    return vdir;
  }

  /**
   * defines vertical direction in radians the observer looks
   *  
   */
  public void setVDirection( double vdir )
  {
    this.vdir = vdir;

    if( targetPoint != null )
    {
      targetPoint = calcTargetPoint( targetPoint.y );
    }
  }

  /**
   * gets the position of the observer, the directions he looks and his field of
   * view in radians
   *  
   */
  public Point3d getObserverPosition()
  {
    return observerPosition;
  }

  /**
   * defines the position of the observer, the directions he looks and his field
   * of view in radians
   *  
   */
  public void setObserverPosition( Point3d observerPosition )
  {
    this.observerPosition = observerPosition;
  }

  /**
   * get footprint or rather the field of view
   */
  public Point3d[] getFootprint()
  {
    return footprint;
  }

  /**
   * get footprint or rather the field of view
   */
  public Point3d[] getFootprint( double height )
  {
    return calcFootprint( height );
  }

  public String toString()
  {
    StringBuffer sb = new StringBuffer();
    sb.append( "observerPosition: " + observerPosition + "\n" );
    sb.append( "targetPoint: " + targetPoint + "\n" );
    sb.append( "footprint: " + "\n" );
    sb.append( footprint[0] + "\n" );
    sb.append( footprint[1] + "\n" );
    sb.append( footprint[2] + "\n" );
    sb.append( footprint[3] + "\n" );
    sb.append( "aov: " + Math.toDegrees( aov ) + "\n" );
    sb.append( "hdir: " + Math.toDegrees( hdir ) + "\n" );
    sb.append( "hdir: " + Math.toDegrees( vdir ) + "\n" );
    return sb.toString();
  }
}