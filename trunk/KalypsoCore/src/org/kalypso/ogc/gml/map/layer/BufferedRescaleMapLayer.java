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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.contribs.eclipse.jobs.JobObserverJob;
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
  private BufferedTile m_runningTile = null;

  /** The last correctly finished tile. Will be painted as long es the runningTile is about to be painted. */
  private BufferedTile m_tile = null;

  /** Rule that is set to all paint jobs of this layer. */
  private final ISchedulingRule m_rule;

  private final long m_repaintMillis;

  private final boolean m_paintRunningTile;

  /**
   * When constructed with this constructed, no repaint happens during painting of the theme.<br>
   * Same as {@link #BufferedRescaleMapLayer(IMapPanel, IKalypsoTheme, ISchedulingRule, Long.MAX_Value)}
   */
  public BufferedRescaleMapLayer( final IMapPanel panel, final IKalypsoTheme theme, final ISchedulingRule rule, final boolean paintRunningTile )
  {
    this( panel, theme, rule, paintRunningTile, Long.MAX_VALUE );
  }

  /**
   * @param rule
   *          {@link ISchedulingRule} set to all jobs used to render this layer.
   * @param paintRunningTile
   *          If <code>true</code>, an already scheduled buffer will be painted; else, only finished tiles will be
   *          drawn.
   */
  public BufferedRescaleMapLayer( final IMapPanel panel, final IKalypsoTheme theme, final ISchedulingRule rule, final boolean paintRunningTile, final long repaintMillis )
  {
    super( panel, theme );
    
    m_paintRunningTile = paintRunningTile;
    m_repaintMillis = repaintMillis;
    m_rule = rule;
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
    // Fetch current state here (avoid synchronised blocks)
    final BufferedTile tile = m_tile;
    final BufferedTile runningTile = m_runningTile;

    // If m_tile fits to world2screen, just paint it
    if( !checkTile( tile, world2screen ) )
    {
      // If m_running tile does not fit or already was finished, reschedule it
      if( !checkTile( runningTile, world2screen ) || runningTile.getResult() != null )
        rescheduleJob( world2screen );
      // else, we wait for it to finish; then m_tile will be good
    }

    // If we have a running tile, that already has started, paint it if this is requested
    if( runningTile != null && runningTile.intersects( world2screen ) && runningTile.getState() == Job.RUNNING && m_paintRunningTile )
      runningTile.paint( g, world2screen );
    // if we have a good tile, paint it
    else if( tile != null && tile.intersects( world2screen ) )
      tile.paint( g, world2screen );
    // only we have no good tile, paint the running tile
    else if( runningTile != null && runningTile.intersects( world2screen ) )
      runningTile.paint( g, world2screen );
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
    final BufferedTile runningTile = new BufferedTile( paintable, world2screen );

    final JobObserverJob repaintJob = new JobObserverJob( "Repaint map observer", runningTile, m_repaintMillis )
    {
      @Override
      protected void jobRunning( )
      {
        getMapPanel().invalidateMap();
      }

      /**
       * @see org.kalypso.contribs.eclipse.jobs.JobObserverJob#jobDone(org.eclipse.core.runtime.IStatus)
       */
      @Override
      protected void jobDone( final IStatus result )
      {
        applyTile( runningTile, result );
      }
    };
    repaintJob.setSystem( true );
    repaintJob.schedule();

    runningTile.setUser( false );
    runningTile.setRule( m_rule );
    runningTile.schedule( 250 );

    m_runningTile = runningTile;
  }

  public void applyTile( final BufferedTile tile, final IStatus result )
  {
    if( result.isOK() )
    {
      m_tile = tile;
      m_runningTile = null;

      getMapPanel().invalidateMap();
    }
    else if( result.matches( IStatus.CANCEL ) )
      return;
    else
    {
      // TODO: do something with the status, so it gets seen in the outline!
      // Other idea: paint status into image, when this tile gets painted
      final Throwable exception = result.getException();
      if( exception != null )
        exception.printStackTrace();
    }
  }

}
