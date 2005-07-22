/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

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
package org.kalypso.ogc.sensor.diagview.jfreechart;

import java.io.IOException;
import java.io.OutputStream;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.kalypso.metadoc.IExportableObject;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * ExportableChart
 * 
 * @author schlienger
 */
public class ExportableChart implements IExportableObject
{
  public final static String EXT_JPEG = ".jpg";

  public final static String EXT_PNG = ".png";

  private final JFreeChart m_chart;

  private final String m_fileExt;

  private final int m_width;

  private final int m_height;

  public ExportableChart( final JFreeChart chart, final String fileExt, final int width, final int height )
  {
    m_chart = chart;
    m_fileExt = fileExt;
    m_width = width;
    m_height = height;
  }

  /**
   * @see org.kalypso.metadoc.IExportableObject#getPreferredDocumentName()
   */
  public String getPreferredDocumentName()
  {
    return m_chart.getTitle() + m_fileExt;
  }

  /**
   * @see org.kalypso.metadoc.IExportableObject#exportObject(java.io.OutputStream, org.eclipse.core.runtime.IProgressMonitor)
   */
  public IStatus exportObject( final OutputStream outs, final IProgressMonitor monitor )
  {
    try
    {
      if( m_fileExt.equalsIgnoreCase( EXT_JPEG ) )
        ChartUtilities.writeChartAsJPEG( outs, m_chart, m_width, m_height );
      else if( m_fileExt.equalsIgnoreCase( EXT_PNG ) )
        ChartUtilities.writeChartAsPNG( outs, m_chart, m_width, m_height );
      else
        return KalypsoGisPlugin.createErrorStatus( "File extension not supported: " + m_fileExt, null );
    }
    catch( final IOException e )
    {
      return KalypsoGisPlugin.createErrorStatus( "Diagramm konnte nicht als Bild exportiert werden", e );
    }

    return Status.OK_STATUS;
  }
}
