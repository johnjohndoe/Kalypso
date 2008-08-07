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

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.OutputStream;

import javax.imageio.ImageIO;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.i18n.Messages;
import org.kalypso.metadoc.IExportableObject;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.ui.KalypsoGisPlugin;

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
  public String getPreferredDocumentName( )
  {
    // TODO besserer Name?
    return FileUtilities.validateName( Messages.getString( "org.kalypso.ui.editor.mapeditor.ExportableMap.0" ) + m_format, "_" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.metadoc.IExportableObject#exportObject(java.io.OutputStream,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  public IStatus exportObject( final OutputStream output, final IProgressMonitor monitor )
  {
    try
    {
      monitor.beginTask( Messages.getString( "org.kalypso.ui.editor.mapeditor.ExportableMap.1" ), 1000 ); //$NON-NLS-1$

      final BufferedImage image = MapModellHelper.createWellFormedImageFromModel( m_panel, m_width, m_height );

      final boolean result = ImageIO.write( image, m_format, output );
      if( !result )
        return new Status( IStatus.WARNING, KalypsoGisPlugin.getId(), 0, Messages.getString( "org.kalypso.ui.editor.mapeditor.ExportableMap.2" ) + m_format, null ); //$NON-NLS-1$
    }
    catch( final IOException e )
    {
      e.printStackTrace();

      return new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0, Messages.getString( "org.kalypso.ui.editor.mapeditor.ExportableMap.3" ), e ); //$NON-NLS-1$
    }
    finally
    {
      monitor.done();
    }

    return Status.OK_STATUS;
  }

  /**
   * @see org.kalypso.metadoc.IExportableObject#getIdentifier()
   */
  public String getIdentifier( )
  {
    // TODO bessere Id?
    return getPreferredDocumentName();
  }

  /**
   * @see org.kalypso.metadoc.IExportableObject#getCategory()
   */
  public String getCategory( )
  {
    // TODO bessere category
    return Messages.getString( "org.kalypso.ui.editor.mapeditor.ExportableMap.4" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.metadoc.IExportableObject#getStationIDs()
   */
  public String getStationIDs( )
  {
    return "";
  }
}