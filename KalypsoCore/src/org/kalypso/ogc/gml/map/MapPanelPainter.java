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
package org.kalypso.ogc.gml.map;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Point;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.jobs.BufferPaintJob.IPaintable;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.graphics.transformation.GeoTransform;

/**
 * Paints a {@link org.kalypso.ogc.gml.mapmodel.IMapModell}.
 * <p>
 * The painter draws a buffered image which gets created in a background job.
 * </p>
 * In order to create the image, schedule this painter as a job. </p>
 * 
 * @author Gernot Belger
 */
public class MapPanelPainter implements IPaintable
{
  private final GeoTransform m_world2screen;

  private final IMapModell m_modell;

  private final IMapLayer[] m_layers;

  private final Color m_bgColor;

  /**
   * Creates this painter. Call {@link #schedule()} immediately after creation in order to create the buffered image.
   * 
   * @param bgColor
   *          If set, the buffer image is filled with this color before any layer will be painted.
   */
  public MapPanelPainter( final IMapLayer[] layers, final IMapModell modell, final GeoTransform world2screen, final Color bgColor )
  {
    m_layers = layers;
    m_modell = modell;
    m_world2screen = world2screen;
    m_bgColor = bgColor;
  }

  public GeoTransform getWorld2screen( )
  {
    return m_world2screen;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return "Painting map: " + m_modell.getLabel( m_modell );
  }

  /**
   * @see org.kalypso.contribs.eclipse.jobs.BufferPaintJob.IPaintable#getSize()
   */
  @Override
  public Point getSize( )
  {
    if( m_world2screen == null )
      return new Point( 0, 0 );

    return new Point( (int) m_world2screen.getDestWidth(), (int) m_world2screen.getDestHeight() );
  }

  /**
   * @see org.kalypso.contribs.eclipse.jobs.BufferPaintJob.IPaintable#paint(java.awt.Graphics2D,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public void paint( final Graphics2D g, final IProgressMonitor monitor ) throws CoreException
  {
    /* Draw background */
    final int screenWidth = (int) m_world2screen.getDestWidth();
    final int screenHeight = (int) m_world2screen.getDestHeight();
    // setClip, necessary, as some display element use the clip bounds to determine the screen-size
    g.setClip( 0, 0, screenWidth, screenHeight );
    if( m_bgColor != null )
    {
      g.setColor( m_bgColor );
      g.fillRect( 0, 0, screenWidth, screenHeight );
    }

    monitor.beginTask( "Painting themes", m_layers.length );

    for( final IMapLayer layer : m_layers )
    {
      monitor.subTask( layer.getLabel() );

      // Force repaint after and before layer

      layer.paint( g, m_world2screen, new SubProgressMonitor( monitor, 1 ) );

      final IMapPanel mapPanel = layer.getMapPanel();
// mapPanel.repaintMap();

      // Check for cancel
      ProgressUtilities.worked( monitor, 0 );
    }
  }

}
