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
package org.kalypso.ogc.gml.map.layer;

import java.awt.Graphics2D;
import java.awt.Point;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.contribs.eclipse.jobs.BufferPaintJob.IPaintable;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypsodeegree.graphics.transformation.GeoTransform;

/**
 *{@link org.kalypso.contribs.eclipse.jobs.BufferPaintJob.IPaintable} implementation that renders a map layer.
 *
 * @author Gernot Belger
 */
public class ThemePaintable implements IPaintable
{
  private final GeoTransform m_world2screen;
  private final IKalypsoTheme m_theme;

  public ThemePaintable( final IKalypsoTheme theme, final GeoTransform world2screen )
  {
    m_theme = theme;
    m_world2screen = world2screen;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return "Painting " + m_theme.getLabel();
  }

  /**
   * @see org.kalypso.contribs.eclipse.jobs.BufferPaintJob.IPaintable#getSize()
   */
  @Override
  public Point getSize( )
  {
    return new Point( (int) m_world2screen.getDestWidth(), (int) m_world2screen.getDestHeight() );
  }

  /**
   * @see org.kalypso.contribs.eclipse.jobs.BufferPaintJob.IPaintable#paint(java.awt.Graphics2D,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public void paint( final Graphics2D g, final IProgressMonitor monitor ) throws CoreException
  {
    m_theme.paint( g, m_world2screen, null, monitor );
  }

}
