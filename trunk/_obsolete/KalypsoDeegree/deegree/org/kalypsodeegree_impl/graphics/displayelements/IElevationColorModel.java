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
package org.kalypsodeegree_impl.graphics.displayelements;

import java.awt.Color;

import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree_impl.graphics.sld.awt.FillPainter;
import org.kalypsodeegree_impl.graphics.sld.awt.StrokePainter;

/**
 * Interface for color model used in the elevation visualization
 * 
 * @author Madanagopal
 * @author Patrice Congo
 */
public interface IElevationColorModel
{
  /**
   * To get the color for the given elevation
   * 
   * @return an {@link Color} for the given elevation
   */
  public Color getColor( double elevation );

  /**
   * To set a new min and max elevation for the color model
   * 
   * @return an {@link Color} for the given elevation
   */
  public void setElevationMinMax( double min, double max );

  /**
   * To get the min and max elevation for the color model
   * 
   * @return an {@link Color} for the given elevation
   */
  public double[] getElevationMinMax( );

  public double getDiscretisationInterval( );

  public int getNumOfClasses( );

  public double getFrom( int currentClass );

  public double getTo( int currentClass );

  public StrokePainter getLinePainter( int currentClass );

  public FillPainter getFillPolygonPainter( int currentClass );

  public void setProjection( GeoTransform projection );
}
