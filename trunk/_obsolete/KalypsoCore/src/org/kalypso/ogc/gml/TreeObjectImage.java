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
package org.kalypso.ogc.gml;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.widgets.Display;

/**
 * Helper class to paint an legend icon as offscreen image.<br>
 * Useful if we want to draw the images on the fly. Should only be used, if only a few such images will be produced.
 *
 * @author Gernot Belger
 */
public class TreeObjectImage
{
  private final int m_width;

  private final int m_height;

  private final Image m_image;

  private GC m_gc = null;

  public TreeObjectImage( final int width, final int height )
  {
    m_width = width;
    m_height = height;

    final Display display = Display.getCurrent();
    m_image = new Image( display, width, height );
  }


  public void dispose( )
  {
    if( m_gc != null )
    m_gc.dispose();
    m_image.dispose();
  }

  /**
   * @see java.lang.Object#finalize()
   */
  @Override
  protected void finalize( ) throws Throwable
  {
    dispose();

    super.finalize();
  }

  public synchronized GC getGC( )
  {
    if( m_gc == null )
    {
      m_gc = new GC( m_image );
      m_gc.setAntialias( SWT.ON );
    }

    return m_gc;
  }

  public ImageDescriptor getImageDescriptor( )
  {
    /* No need to resize. */
    if( m_width == 16 && m_height == 16 )
    {
      final ImageData imageData = m_image.getImageData();
      return ImageDescriptor.createFromImageData( imageData );
    }

    /* Resize, if the image is not 16 / 16. */
    final ImageData imageData = resize( m_image, 16, 16 );
    return ImageDescriptor.createFromImageData( imageData );
  }

  /**
   * This function resizes the given image.
   *
   * @param image
   *          The old image.
   * @param witdth
   *          The new width.
   * @param height
   *          The new height.
   */
  private static ImageData resize( final Image image, final int width, final int height )
  {
    final Image scaled = new Image( image.getDevice(), width, height );
    try
    {
      final GC gc = new GC( scaled );
      gc.setAntialias( SWT.ON );
      gc.setInterpolation( SWT.HIGH );
      gc.drawImage( image, 0, 0, image.getBounds().width, image.getBounds().height, 0, 0, width, height );
      gc.dispose();
      return scaled.getImageData();
    }
    finally
    {
      scaled.dispose();
    }
  }

}
