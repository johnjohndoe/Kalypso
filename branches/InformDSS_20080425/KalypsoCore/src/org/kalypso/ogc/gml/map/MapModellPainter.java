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

import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.core.runtime.jobs.MutexRule;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.awt.ComponentRepaintJob;
import org.kalypso.contribs.java.awt.HighlightGraphics;
import org.kalypso.ogc.gml.mapmodel.IMapModell;

/**
 * Paints a {@link org.kalypso.ogc.gml.mapmodel.IMapModell}.
 * <p>
 * The painter draw really draw a buffered image which gets created in a background job.
 * </p>
 * In order to create the image, schedule this painter as a job.
 * </p>
 * 
 * @author Gernot Belger
 */
public class MapModellPainter extends Job implements IPainter
{
  /**
   * Maximum delay by wich repaints to the map are produced.
   * 
   * @see java.awt.Component#repaint(long)
   */
  private static final int MAP_REPAINT_MILLIS = 500;

  private final MapPanel m_mapPanel;

  private Image m_mapImage = null;

  private final ThemePainter m_themePainter;

  /** One mutex-rule per panel, so painting jobs for one panel run one after another. */
  private final ISchedulingRule m_painterMutex = new MutexRule();

  /**
   * Creates this painter. Call {@link #schedule()} immediately after creation in order to create the buffered image.
   * </p>
   */
  public MapModellPainter( final MapPanel mapPanel )
  {
    super( "" );
    m_mapPanel = mapPanel;
    setRule( m_painterMutex );

    m_themePainter = new ThemePainter( m_mapPanel );
  }

  /**
   * @see org.kalypso.ogc.gml.map.IPainter#paint(java.awt.Graphics2D)
   */
  public void paint( final Graphics2D g )
  {
    // draw map:
    if( m_mapImage != null )
      g.drawImage( m_mapImage, 0, 0, null );
  }

  /**
   * @see org.kalypso.ogc.gml.map.IPainter#dispose()
   */
  public void dispose( )
  {
    if( m_mapImage != null )
      m_mapImage.flush();
  }

  /**
   * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    final IMapModell mapModell = m_mapPanel.getMapModell();
    if( mapModell != null )
    {
      setName( "Karte zeichnen: " + mapModell.getName() );
    }
    Graphics2D gr = null;
    Job repaintJob = null;
    try
    {
      repaintJob = new ComponentRepaintJob( m_mapPanel, MapModellPainter.MAP_REPAINT_MILLIS );
      repaintJob.setSystem( true );
      repaintJob.setPriority( Job.LONG );
      repaintJob.schedule( 500 );

      final SubMonitor progress = SubMonitor.convert( monitor, "Karte wird gezeichnet", 100 );

      ProgressUtilities.worked( progress, 0 );

      final int width = m_mapPanel.getWidth();
      final int height = m_mapPanel.getHeight();
      if( (width > 0) && (height > 0) )
      {
        gr = createImage( progress.newChild( 10 ), width, height );
        // if image is null, wokbench is probably shutting down,
        // just return without comment
        if( gr == null )
          return Status.OK_STATUS;

        gr.setClip( m_mapPanel.getBounds() );
        m_themePainter.paintThemes( gr, false, progress.newChild( 45 ) );

        // TODO: replace highlightGraphics concept by highlight-style
        final HighlightGraphics highlightGraphics = new HighlightGraphics( gr );
        m_themePainter.paintThemes( highlightGraphics, true, progress.newChild( 45 ) );
      }
      else
        progress.setWorkRemaining( 0 );
    }
    catch( final CoreException ce )
    {
      return ce.getStatus();
    }
    catch( final Throwable t )
    {
      return StatusUtilities.statusFromThrowable( t, "Fehler beim Zeichnen der Karte." );
    }
    finally
    {
      repaintJob.cancel();

      if( gr != null )
        gr.dispose();

      monitor.done();

      /* Force at least one repaint at the end of this operation. */
      m_mapPanel.repaint( MapModellPainter.MAP_REPAINT_MILLIS );
    }

    return Status.OK_STATUS;
  }

  /**
   * Creates the buffer image and returns a graphics context onto the freshly created image.
   * <p>
   * The caller is responsible for disposing the graphics.
   * </p>
   */
  private Graphics2D createImage( final SubMonitor monitor, final int width, final int height ) throws CoreException
  {
    monitor.beginTask( "initalisiere Buffer", 100 );

    m_mapImage = m_mapPanel.createImage( width, height );
    if( m_mapImage == null )
      return null;

    final Graphics2D gr = (Graphics2D) m_mapImage.getGraphics();
    gr.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON );
    gr.setRenderingHint( RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON );

    ProgressUtilities.worked( monitor, 0 );
    monitor.done();

    return gr;
  }

}
