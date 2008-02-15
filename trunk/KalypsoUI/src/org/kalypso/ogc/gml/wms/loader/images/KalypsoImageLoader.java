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
package org.kalypso.ogc.gml.wms.loader.images;

import java.awt.Image;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.ogc.gml.wms.provider.images.IKalypsoImageProvider;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * This class loads a theme with a provider.
 * 
 * @author Holger Albert
 */
public class KalypsoImageLoader extends Job
{
  /**
   * This variable stores the image provider, whoch will retrieve the image.
   */
  private IKalypsoImageProvider m_provider;

  /**
   * This variable stores the requested width.
   */
  private int m_width;

  /**
   * This variable stores the requested height.
   */
  private int m_height;

  /**
   * This variable stores the requested bounding box.
   */
  private GM_Envelope m_bbox;

  /**
   * This variable stores the result of the loading.
   */
  private Image m_buffer;

  /**
   * The constructor.
   * 
   * @param name
   *            The name of the job.
   * @param provider
   *            The image provider, which will retrieve the image.
   * @param width
   *            The requested width.
   * @param height
   *            The requested height.
   * @param bbox
   *            The requested bounding box.
   */
  public KalypsoImageLoader( final String name, final IKalypsoImageProvider provider, final int width, final int height, final GM_Envelope bbox )
  {
    super( name );

    m_provider = provider;
    m_width = width;
    m_height = height;
    m_bbox = bbox;

    m_buffer = null;
  }

  /**
   * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    /* Start the task. */
    monitor.beginTask( "Loading image ...", 1000 );

    try
    {
      /* If the provider is missing, tell the user. */
      if( m_provider == null )
        return StatusUtilities.createErrorStatus( "There is no image provider given ..." );

      /* Load the image. This could take a while. */
      m_buffer = m_provider.getImage( m_width, m_height, m_bbox );

      return StatusUtilities.createOkStatus( "The theme was successfully loaded." );
    }
    catch( final CoreException e )
    {
      return e.getStatus();
    }
    catch( final Throwable throwable )
    {
      return StatusUtilities.statusFromThrowable( throwable );
    }
    finally
    {
      monitor.done();
    }
  }

  /**
   * This function returns the result of the loading process. This could be null, if loading has not finished yet.
   * 
   * @return The result image or null.
   */
  public Image getBuffer( )
  {
    return m_buffer;
  }

  /**
   * This function returns the full extent, if available.
   * 
   * @return The full extent.
   */
  public GM_Envelope getFullExtent( )
  {
    if( m_provider != null )
      return m_provider.getFullExtent();

    return null;
  }

  /**
   * This function disposes this loader.
   */
  public void dispose( )
  {
    m_provider = null;
    m_width = 0;
    m_height = 0;
    m_bbox = null;

    if( m_buffer != null )
    {
      m_buffer.flush();
      m_buffer = null;
    }

    /* Cancels this job. */
    cancel();
  }
}