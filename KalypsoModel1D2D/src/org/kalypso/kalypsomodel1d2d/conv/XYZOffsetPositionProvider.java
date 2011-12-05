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
package org.kalypso.kalypsomodel1d2d.conv;

import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * An offset based position provider with the following translation scheme:
 * <ul>
 * <li/>target.x=native.x+offsetX <li/>target.y=native.y+offsetY <li/>target.z=native.z+offsetZ
 * </ul>
 * This implementaion ignores the coordinate system issues.
 * 
 * @author Patrice Congo
 */
public class XYZOffsetPositionProvider implements IPositionProvider
{
  /**
   * The x offset to add to the native x-coordinate to get the target coordinate
   */
  private double xOffset;

  /**
   * The y offset to add to the native y-coordinate to get the target coordinate
   */
  private double yOffset;

  /**
   * The z offset to add to the native z-coordinate to get the target coordinate
   */
  private double zOffset;

  /**
   * coordinate reference system of the target points
   */
  private String crs;

  public XYZOffsetPositionProvider( double xoffset, double yoffset, String coordinateSystem )
  {
    Assert.throwIAEOnNullParam( coordinateSystem, "coordinateSystem" ); //$NON-NLS-1$
    xOffset = xoffset;
    yOffset = yoffset;
    zOffset = 0.0;
    this.crs = coordinateSystem;
  }

  /**
   * Creates a native coordinate system with the specified target coordinate system and the given coordinate offsets
   * 
   * @param xOffset
   *            the x offset
   * @param yOffset
   *            the y offset
   * @param zOffset
   *            the z offset
   * @param crs
   *            the target coordinate system
   */
  public XYZOffsetPositionProvider( String crs, double xOffset, double yOffset, double zOffset ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNullParam( crs, "crs" ); //$NON-NLS-1$

    this.xOffset = xOffset;
    this.yOffset = yOffset;
    this.zOffset = zOffset;
    this.crs = crs;

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IPositionProvider#getCoordinateSystem()
   */
  @Override
  public String getCoordinateSystem( )
  {
    return crs;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IPositionProvider#getGMPoint(double, double, double)
   */
  @Override
  public GM_Point getGMPoint( double nativeX, double nativeY, double nativeZ )
  {

    return GeometryFactory.createGM_Point( nativeX + xOffset, nativeY + yOffset, nativeZ + zOffset, crs );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IPositionProvider#getNativeX(org.kalypsodeegree.model.geometry.GM_Point)
   */
  @Override
  public double getNativeX( GM_Point point ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNullParam( point, "point" ); //$NON-NLS-1$
    return point.getX() - xOffset;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IPositionProvider#getNativeY(org.kalypsodeegree.model.geometry.GM_Point)
   */
  @Override
  public double getNativeY( GM_Point point ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNullParam( point, "point" ); //$NON-NLS-1$
    return point.getY() - yOffset;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IPositionProvider#getNativeZ(org.kalypsodeegree.model.geometry.GM_Point)
   */
  @Override
  public double getNativeZ( GM_Point point ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNullParam( point, "point" ); //$NON-NLS-1$
    return point.getZ() - zOffset;
  }
}