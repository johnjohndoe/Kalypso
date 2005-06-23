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
package org.kalypso.lhwsachsenanhalt.saale;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.logging.Logger;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.obslink.TimeseriesLink;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;

/**
 * Ein FeatureVisitor, um alle Zeitreihen eines bestimmten typs aus dem GML rauszuschreiben
 * 
 * @author belger
 */
public class HWVorZMLWriterVisitor implements FeatureVisitor
{
  private final Logger m_logger = Logger.getLogger( getClass().getName() );

  private final HWVOR00Converter m_converter = new HWVOR00Converter();

  private final URL m_context;

  private final IUrlResolver m_resolver;

  private String m_dataAxis;

  private final String m_linkProperty;

  public HWVorZMLWriterVisitor( final String linkProperty, final String dataAxis, final IUrlResolver resolver,
      final URL context )
  {
    m_linkProperty = linkProperty;
    m_dataAxis = dataAxis;
    m_resolver = resolver;
    m_context = context;

  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    final TimeseriesLink link = (TimeseriesLink)f.getProperty( m_linkProperty );
    try
    {
      final IObservation observation = ZmlFactory.parseXML( m_resolver.resolveURL( m_context, link.getHref() ), "id" );
      m_converter.addObservation( observation, TimeserieConstants.TYPE_DATE, m_dataAxis );
    }
    catch( final MalformedURLException e )
    {
      // sollte nie pasieren
      e.printStackTrace();
    }
    catch( final SensorException e )
    {
      e.printStackTrace();

      m_logger.warning( "Zeitreihe konnte aus Modell nicht gelesen werden: " + link.getHref() );

    }

    return true;
  }

  public void writeObservations( final File file ) throws IOException
  {
    Writer writer = null;
    try
    {
      writer = new BufferedWriter( new FileWriter( file ) );
      m_converter.toHWVOR00( writer );
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }
}
