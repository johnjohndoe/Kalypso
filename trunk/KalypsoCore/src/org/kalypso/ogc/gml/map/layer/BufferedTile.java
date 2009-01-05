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

import java.awt.Graphics;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.kalypso.contribs.eclipse.jobs.BufferPaintJob;
import org.kalypso.ogc.gml.map.MapPanelUtilities;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * A paint job that just paints a paintable for the given world2screen transformation.<br>
 * Also represents a rendered 'tile' showing a portion of a theme.
 * 
 * @author Gernot Belger
 */
public class BufferedTile extends BufferPaintJob
{
  private final IJobChangeListener m_listener = new JobChangeAdapter()
  {
    /**
     * @see org.eclipse.core.runtime.jobs.JobChangeAdapter#done(org.eclipse.core.runtime.jobs.IJobChangeEvent)
     */
    @Override
    public void done( final IJobChangeEvent event )
    {
      handleJobDone( event.getResult() );
    }
  };

  private final GeoTransform m_world2screen;

  private final BufferedRescaleMapLayer m_layer;

  public BufferedTile( final IPaintable paintable, final BufferedRescaleMapLayer layer, final GeoTransform world2screen )
  {
    super( paintable );
    m_layer = layer;
    m_world2screen = world2screen;
    addJobChangeListener( m_listener );
  }

  public GeoTransform getWorld2Screen( )
  {
    return m_world2screen;
  }

  public boolean intersects( final GeoTransform world2screen )
  {
    return m_world2screen.getSourceRect().intersects( world2screen.getSourceRect() );
  }

  public boolean intersects( final GM_Envelope extent )
  {
    return m_world2screen.getSourceRect().intersects( extent );
  }

  /**
   * Paint my image regarding the given world2screen; rescaling it if necessary.
   */
  public void paint( final Graphics g, final GeoTransform world2screen )
  {
    MapPanelUtilities.paintIntoExtent( g, world2screen, getImage(), m_world2screen.getSourceRect(), null );
  }

  protected void handleJobDone( final IStatus result )
  {
    removeJobChangeListener( m_listener );

    if( result.isOK() )
      m_layer.applyTile( this );
  }


}