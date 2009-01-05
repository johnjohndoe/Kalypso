/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.kalypso.contribs.eclipse.core.runtime.jobs.MutexRule;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * Renders theme in background, but always keeps the last rendered tile.<br>
 * As long as painting is in progress, the last tile will be drawn (resized to fit its position).<br>
 * The map is only redrawn (via invalidateMap) after rendering has completely finished, so the theme appears suddenly.
 * 
 * @author Gernot Belger
 */
public class BufferedRescaleMapLayer extends AbstractMapLayer
{
  /** One static mutex-rule, so painting jobs for different themes run one after another. */
  private static final ISchedulingRule MUTEX_RULE = new MutexRule();

  private BufferedTile m_runningTile = null;

  /** The last correctly finished tile. Will be painted as long es the runningTile is about to be painted. */
  private BufferedTile m_tile = null;

  public BufferedRescaleMapLayer( final IMapPanel panel, final IKalypsoTheme theme )
  {
    super( panel, theme );
  }

  /**
   * @see org.kalypso.ogc.gml.map.IMapLayer#dispose()
   */
  @Override
  public void dispose( )
  {
    super.dispose();

    if( m_runningTile != null )
      m_runningTile.dispose();

    if( m_tile != null )
      m_tile.dispose();
  }

  /**
   * @see org.kalypso.ogc.gml.map.IMapLayer#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public void paint( final Graphics g, final GeoTransform world2screen, final IProgressMonitor monitor )
  {
    // If m_tile fits to world2screen, just paint it
    if( !checkTile( m_tile, world2screen ) )
    {
      // If m_running tile does not fit or already was finished, reschedule it
      if( !checkTile( m_runningTile, world2screen ) || m_runningTile.getResult() != null )
        rescheduleJob( world2screen );
      // else, we wait for it to finish; then m_tile will be good
    }

    // If we have a good tile, paint it
    if( m_tile != null && m_tile.intersects( world2screen ) )
      m_tile.paint( g, world2screen );
    // only we have no good tile, paint the running tile
    else if( m_runningTile != null && m_runningTile.intersects( world2screen ) )
      m_runningTile.paint( g, world2screen );
  }

  /** Check if the tile fits to the given world2screen */
  private boolean checkTile( final BufferedTile tile, final GeoTransform world2screen )
  {
    if( tile == null )
      return false;

    final GeoTransform m_world2screen = tile.getWorld2Screen();

    if( !m_world2screen.getSourceRect().equals( world2screen.getSourceRect() ) )
      return false;

    if( m_world2screen.getDestWidth() != world2screen.getDestWidth() )
      return false;

    if( m_world2screen.getDestHeight() != world2screen.getDestHeight() )
      return false;

    return true;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return "Painting theme: " + getLabel();
  }

  /**
   * @see org.kalypso.ogc.gml.map.layer.AbstractMapLayer#invalidate(org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  @Override
  protected void invalidate( final GM_Envelope extent )
  {
    // Force repaint: reschedule, will eventually replace the current tile
    if( m_tile != null && m_tile.intersects( extent ) )
      rescheduleJob( m_tile.getWorld2Screen() );
  }

  private synchronized void rescheduleJob( final GeoTransform world2screen )
  {
    if( m_runningTile != null )
    {
      m_runningTile.dispose();
      m_runningTile = null;
    }

    final ThemePaintable paintable = new ThemePaintable( getTheme(), world2screen );
    final BufferedTile runningTile = new BufferedTile( paintable, this, world2screen );
    runningTile.setUser( false );
    // TODO: decide depending on theme-type, if mutex-shall be used;
    // Or: one mutex per theme?
// runningTile.setRule( MUTEX_RULE );
    runningTile.schedule( 250 );

    m_runningTile = runningTile;
  }

  public void applyTile( final BufferedTile tile )
  {
    m_tile = tile;
    m_runningTile = null;

    getMapPanel().invalidateMap();
  }

}
