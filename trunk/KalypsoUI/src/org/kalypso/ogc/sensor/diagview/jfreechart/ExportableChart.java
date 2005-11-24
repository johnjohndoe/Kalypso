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

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.OutputStream;

import org.apache.commons.configuration.Configuration;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.jfree.chart.encoders.EncoderUtil;
import org.jfree.chart.title.TextTitle;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.metadoc.IExportableObject;
import org.kalypso.ogc.sensor.MetadataExtenderWithObservation;

/**
 * ExportableChart based on an existing chart
 * 
 * @author schlienger
 */
public class ExportableChart implements IExportableObject
{
  public final static String DEFAULT_FORMAT = "png";
  public final static int DEFAULT_WIDTH = 400;
  public final static int DEFAULT_HEIGHT = 300;

  private final ObservationChart m_chart;

  private final String m_format;

  private final int m_width;
  private final int m_height;

  private final String m_identifierPrefix;
  private final String m_category;

  public ExportableChart( final ObservationChart chart, final String format, final int width, final int height,
      final String identifierPrefix, final String category )
  {
    m_chart = chart;
    m_format = format;
    m_width = width;
    m_height = height;
    m_identifierPrefix = identifierPrefix;
    m_category = category;
  }

  /**
   * @see org.kalypso.metadoc.IExportableObject#getPreferredDocumentName()
   */
  public String getPreferredDocumentName()
  {
    final TextTitle title = m_chart.getTitle();

    String name = "Diagramm";
    if( title != null && title.getText().length() > 0 )
      name = title.getText();

    return name + "." + m_format;
  }

  /**
   * @see org.kalypso.metadoc.IExportableObject#exportObject(java.io.OutputStream,
   *      org.eclipse.core.runtime.IProgressMonitor, org.apache.commons.configuration.Configuration)
   */
  public IStatus exportObject( final OutputStream outs, final IProgressMonitor monitor,
      final Configuration metadataExtensions )
  {
    monitor.beginTask( "Diagramm-Export", IProgressMonitor.UNKNOWN );

    try
    {
      // let update the metadata with the information we have
      MetadataExtenderWithObservation.extendMetadata( metadataExtensions, m_chart.getTemplate().getItems() );

      final BufferedImage image = m_chart.createBufferedImage( m_width, m_height, null );
      EncoderUtil.writeBufferedImage( image, m_format, outs );
    }
    catch( final IOException e )
    {
      return StatusUtilities.statusFromThrowable( e, "Diagramm konnte nicht als Bild exportiert werden" );
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
  public String getIdentifier()
  {
    return m_identifierPrefix + getPreferredDocumentName();
  }

  /**
   * @see org.kalypso.metadoc.IExportableObject#getCategory()
   */
  public String getCategory()
  {
    return m_category;
  }
}
