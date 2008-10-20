/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.transformation;

import java.util.ArrayList;

import javax.vecmath.Point3d;

import org.deegree.crs.coordinatesystems.CoordinateSystem;
import org.deegree.crs.exceptions.TransformationException;
import org.deegree.crs.transformations.CRSTransformation;
import org.eclipse.osgi.framework.internal.core.FrameworkProperties;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * This class provides some functions for the GeoTransformer.
 *
 * @author Holger Albert
 */
@SuppressWarnings("restriction")
public class TransformUtilities
{
  /**
   * The constructor.
   */
  private TransformUtilities( )
  {
  }

  /**
   * This function will check a SystemProperty, if a transformation of geographic data should be done.<br>
   * The default value is always true (e.g. in the case, the property is not set or set to true).<br>
   * Only if the property is set to false, this function returns false.
   *
   * @return True, if a transformation should be done, false otherwise.
   */
  public static boolean shouldTransform( )
  {
    String transform = getTransformProperty();
    if( (transform != null) && (transform.equalsIgnoreCase( "false" )) )
    {
      Debug.TRANSFORM.printf( "Should not transform ...\n" );
      return false;
    }

    Debug.TRANSFORM.printf( "Should transform ...\n" );
    return true;
  }

  /**
   * This function will return the string of the SystemProperty 'org.kalypso.kalypsodegree.transform'.
   *
   * @return The value property or null.
   */
  private static String getTransformProperty( )
  {
    return FrameworkProperties.getProperty( "org.kalypso.kalypsodegree.transform" );
  }

  /**
   * This function transformes a point.
   *
   * @param geo
   *            The point, which should be transformed.
   * @param trans
   *            The transformation.
   * @return The transformed point.
   */
  public static GM_Point transform( GM_Point geo, CRSTransformation trans ) throws TransformationException
  {
    /* If the flag is set to false, no transformation is allowed. */
    if( shouldTransform() == false )
      return geo;

    /* Get the coordinate systems. */
    CoordinateSystem sourceCRS = trans.getSourceCRS();
    CoordinateSystem targetCRS = trans.getTargetCRS();

    /* If the coordinate systems are the same, do not transform. */
    if( sourceCRS.getIdAndName().equals( targetCRS.getIdAndName() ) )
      return geo;

    Debug.TRANSFORM.printf( "POINT: %s to %s\n", sourceCRS.getIdentifier(), targetCRS.getIdentifier() );

    /* Transform. */
    Point3d coords = new Point3d( geo.getX(), geo.getY(), geo.getZ() );
    Point3d newCoords = trans.doTransform( coords );

    return GeometryFactory.createGM_Point( newCoords.x, newCoords.y, (targetCRS.getDimension() == 3) ? newCoords.z : Double.NaN, targetCRS.getIdentifier() );
  }

  /**
   * This function transforms a position.
   *
   * @param pos
   *            The position, which should be transformed.
   * @param trans
   *            The transformation.
   * @return The transformed position.
   */
  public static GM_Position transform( GM_Position pos, CRSTransformation trans ) throws TransformationException
  {
    /* If the flag is set to false, no transformation is allowed. */
    if( shouldTransform() == false )
      return pos;

    /* Get the coordinate systems. */
    CoordinateSystem sourceCRS = trans.getSourceCRS();
    CoordinateSystem targetCRS = trans.getTargetCRS();

    /* If the coordinate systems are the same, do not transform. */
    if( sourceCRS.getIdAndName().equals( targetCRS.getIdAndName() ) )
      return pos;

    Debug.TRANSFORM.printf( "POS: %s to %s\n", sourceCRS.getIdentifier(), targetCRS.getIdentifier() );

    Point3d coords = new Point3d( pos.getX(), pos.getY(), pos.getZ() );
    Point3d newCoords = trans.doTransform( coords );

    return GeometryFactory.createGM_Position( newCoords.x, newCoords.y, newCoords.z );
  }

  /**
   * This function transforms an array of positions.<br>
   * It uses the function {@link #transform(GM_Position, CRSTransformation)} for each position.
   *
   * @param pos
   *            The array of positions.
   * @param trans
   *            The transformation.
   * @return The array of transformed positions.
   */
  public static GM_Position[] transform( GM_Position[] pos, CRSTransformation trans ) throws TransformationException
  {
    ArrayList<GM_Position> newPos = new ArrayList<GM_Position>();

    for( int i = 0; i < pos.length; i++ )
      newPos.add( transform( pos[i], trans ) );

    return newPos.toArray( new GM_Position[] {} );
  }
}