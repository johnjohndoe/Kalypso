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
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.image.BufferedImage;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.jobs.BufferPaintJob;
import org.kalypso.contribs.eclipse.jobs.BufferPaintJob.IPaintable;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * This implementation keeps the rendered theme in an image-buffer and automatically refreshes the buffer if needed.
 *
 * @author Gernot Belger
 */
public class BufferedMapLayer extends AbstractMapLayer implements IPaintable
{
  // We do not use it as job yet, just as image container
  private final BufferPaintJob m_paintJob = new BufferPaintJob( this );

  private GeoTransform m_world2screen;

  public BufferedMapLayer( final IMapPanel panel, final IKalypsoTheme theme )
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

    m_paintJob.dispose();
  }

  /**
   * @see org.kalypso.ogc.gml.map.IMapLayer#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public void paint( final Graphics g, final GeoTransform world2screen, final IProgressMonitor monitor )
  {
    // CHECK IMAGE/POSITION
    if( screenSizeChanged( world2screen ) )
      m_paintJob.dispose();
    else if( extentChanged( world2screen ) )
      m_paintJob.dispose();
    // REMARK: If only the extent changed, we could instead reset the image (saves recreation of the buffer image); but
    // is it really quicker? (And how to do it? must be marked as completely transparent)

    m_world2screen = world2screen;

    // Paint, if necessary
    if( m_paintJob.getImage() == null )
    {
      final IStatus status = m_paintJob.run( monitor );
      // If canceled, dispose current image, so that next paint will redraw
      if( status.matches( IStatus.CANCEL ) )
        m_paintJob.dispose();
    }

    final BufferedImage image = m_paintJob.getImage();
    g.drawImage( image, 0, 0, null );
  }

  private boolean extentChanged( final GeoTransform world2screen )
  {
    if( m_world2screen == null )
      return true;

    if( !m_world2screen.getSourceRect().equals( world2screen.getSourceRect() ) )
      return true;

    return false;
  }

  private boolean screenSizeChanged( final GeoTransform world2screen )
  {
    // Screen size changed?
    if( m_world2screen == null )
      return true;

    if( m_world2screen.getDestWidth() != world2screen.getDestWidth() )
      return true;
    if( m_world2screen.getDestHeight() != world2screen.getDestHeight() )
      return true;

    return false;
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
    getTheme().paint( g, m_world2screen, null, monitor );
  }

  /**
   * @see org.kalypso.ogc.gml.map.layer.AbstractMapLayer#invalidate(org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  @Override
  protected void invalidate( final GM_Envelope extent )
  {
    if( extent == null || m_world2screen != null && m_world2screen.getSourceRect().intersects( extent ) )
      m_paintJob.dispose();
  }

}
