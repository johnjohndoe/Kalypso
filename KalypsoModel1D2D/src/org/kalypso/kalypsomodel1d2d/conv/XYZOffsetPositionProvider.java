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
 * <li/>target.x=native.x+offsetX
 * <li/>target.y=native.y+offsetY
 * <li/>target.z=native.z+offsetZ
 * </ul>
 * This implementation ignores the coordinate system issues.
 * 
 * @author Patrice Congo
 */
public class XYZOffsetPositionProvider implements IPositionProvider
{
  /**
   * The x offset to add to the native x-coordinate to get the target coordinate
   */
  private final double m_xOffset;

  /**
   * The y offset to add to the native y-coordinate to get the target coordinate
   */
  private final double m_yOffset;

  /**
   * The z offset to add to the native z-coordinate to get the target coordinate
   */
  private final double m_zOffset;

  /**
   * coordinate reference system of the target points
   */
  private final String m_crs;

  public XYZOffsetPositionProvider( final double xoffset, final double yoffset, final String coordinateSystem )
  {
    Assert.throwIAEOnNullParam( coordinateSystem, "coordinateSystem" ); //$NON-NLS-1$
    m_xOffset = xoffset;
    m_yOffset = yoffset;
    m_zOffset = 0.0;
    m_crs = coordinateSystem;
  }

  /**
   * Creates a native coordinate system with the specified target coordinate system and the given coordinate offsets
   * 
   * @param xOffset
   *          the x offset
   * @param yOffset
   *          the y offset
   * @param zOffset
   *          the z offset
   * @param crs
   *          the target coordinate system
   */
  public XYZOffsetPositionProvider( final String crs, final double xOffset, final double yOffset, final double zOffset ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNullParam( crs, "crs" ); //$NON-NLS-1$

    m_xOffset = xOffset;
    m_yOffset = yOffset;
    m_zOffset = zOffset;
    m_crs = crs;

  }

  @Override
  public String getCoordinateSystem( )
  {
    return m_crs;
  }

  @Override
  public GM_Point getGMPoint( final double nativeX, final double nativeY, final double nativeZ )
  {
    return GeometryFactory.createGM_Point( nativeX + m_xOffset, nativeY + m_yOffset, nativeZ + m_zOffset, m_crs );
  }

  @Override
  public double getNativeX( final GM_Point point ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNullParam( point, "point" ); //$NON-NLS-1$
    return point.getX() - m_xOffset;
  }

  @Override
  public double getNativeY( final GM_Point point ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNullParam( point, "point" ); //$NON-NLS-1$
    return point.getY() - m_yOffset;
  }

  @Override
  public double getNativeZ( final GM_Point point ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNullParam( point, "point" ); //$NON-NLS-1$
    return point.getZ() - m_zOffset;
  }
}