/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wbprivate final ExportMapOptionsPage 

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.editor.mapeditor;

import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.OutputStream;

import javax.imageio.ImageIO;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.metadoc.IExportableObject;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * @author belger
 */
public class ExportableMap implements IExportableObject
{
  private final int m_width;

  private final int m_height;

  private final String m_format;

  private final MapPanel m_panel;

  public ExportableMap( final MapPanel panel, final int width, final int height, final String format )
  {
    m_panel = panel;
    m_width = width;
    m_height = height;
    m_format = format;
  }

  /**
   * @see org.kalypso.metadoc.IExportableObject#getPreferredDocumentName()
   */
  public String getPreferredDocumentName()
  {
    return  "Karte." + m_format;
  }

  /**
   * @see org.kalypso.metadoc.IExportableObject#exportObject(java.io.OutputStream,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  public IStatus exportObject( final OutputStream output, final IProgressMonitor monitor )
  {
    try
    {
      monitor.beginTask( "Bildexport", 1000 );
      
      final IMapModell mapModell = m_panel.getMapModell();

      final GeoTransform transform = m_panel.getProjection();
      final GM_Envelope boundingBox = m_panel.getBoundingBox();

      final Rectangle bounds = new Rectangle( m_width, m_height );
      final BufferedImage image = MapModellHelper.createImageFromModell( transform, boundingBox, bounds, bounds.width,
          bounds.height, mapModell );
      final boolean result = ImageIO.write( image, m_format, output );
      if( !result )
        return new Status( IStatus.WARNING, KalypsoGisPlugin.getId(), 0,
            "Ungültiges Format: " + m_format, null );
    }
    catch( final IOException e )
    {
      e.printStackTrace();

      return new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0, "Fehler beim Export", e );
    }
    finally
    {
      monitor.done();
    }

    return Status.OK_STATUS;
  }
}