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
package org.kalypso.ogc.util;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Date;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.commons.java.net.UrlResolver;
import org.kalypso.commons.resources.FolderUtilities;
import org.kalypso.commons.resources.SetContentHelper;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.status.KalypsoProtocolWriter;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.forecast.ForecastFilter;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.ogc.util.CopyObservationHandler.Source;
import org.kalypso.zml.ObservationType;
import org.kalypso.zml.obslink.TimeseriesLink;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;

/**
 * @author belger
 */
public class CopyObservationFeatureVisitor implements FeatureVisitor
{
  private final Source[] m_sources;

  private final URL m_context;

  private final String m_targetobservation;

  private final IUrlResolver m_urlResolver;

  private final PrintWriter m_logWriter;

  private static final String SUMM_INFO = "*** ";

  private final Date m_forecastFrom;

  private final Date m_forecastTo;

  public CopyObservationFeatureVisitor( final URL context, final IUrlResolver urlResolver, final String targetobservation, final Source[] sources,
      final Date forecastFrom, final Date forecastTo, final PrintWriter logWriter )
  {
    m_context = context;
    m_urlResolver = urlResolver;
    m_targetobservation = targetobservation;
    m_sources = sources;
    m_forecastFrom = forecastFrom;
    m_forecastTo = forecastTo;
    m_logWriter = logWriter;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    try
    {
      final IObservation[] sourceObses = getObservations( f );

      final TimeseriesLink targetlink = (TimeseriesLink)f.getProperty( m_targetobservation );
      if( targetlink == null )
      {
        m_logWriter.println( SUMM_INFO + "Keine Ziel-Verkn�pfung gefunden f�r Feature mit ID: " + f.getId() );
        return true;
      }

      if( sourceObses.length == 0 || sourceObses[0] == null )
      {
        m_logWriter.println( SUMM_INFO + "Keine Quell-Verkn�pfung(en) gefunden f�r Feature mit ID: " + f.getId() );
        return true;
      }

      final IObservation obs;

      // only do ForeCastFilter if we have mor than one obs
      if( sourceObses.length < 2 || sourceObses[1] == null )
        obs = sourceObses[0];
      else
      {
        // NOTE for ForecastFilter: the order is important:
        // obs( i ) has a higher priority than obs( i + 1 )
        // with 'i' the index in the observations array...
        final ForecastFilter fc = new ForecastFilter();
        fc.initFilter( new IObservation[]
        {
            sourceObses[0],
            sourceObses[1] }, sourceObses[0], null ); // TODO check if null
                                                      // context is ok here
        obs = fc;
      }

      // set forecast metadata, might be used in diagram for instance
      // to mark the forecast range
      TimeserieUtils.setForecast( obs, m_forecastFrom, m_forecastTo );

      // protocol the observations here and inform the user
      KalypsoProtocolWriter.analyseValues( obs, obs.getValues( null ), m_logWriter, SUMM_INFO );

      // remove query part if present, href is also used as file name here!
      final String href = ZmlURL.getIdentifierPart( targetlink.getHref() );

      final IFile targetfile = ResourceUtilities.findFileFromURL( m_urlResolver.resolveURL( m_context, href ) );
      // TODO: check if valid
      FolderUtilities.mkdirs( targetfile.getParent() );

      final SetContentHelper thread = new SetContentHelper()
      {
        protected void write( final OutputStreamWriter w ) throws Throwable
        {
          final ObservationType type = ZmlFactory.createXML( obs, null );
          ZmlFactory.getMarshaller().marshal( type, w );
        }
      };
      thread.setFileContents( targetfile, false, true, new NullProgressMonitor() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      m_logWriter.println( "Fehler beim Kopieren der Zeitreihen f�r Feature: " + f.getId() );
      m_logWriter.println( e.getLocalizedMessage() );
    }

    return true;
  }

  private IObservation[] getObservations( final Feature f ) throws SensorException, IOException
  {
    final IObservation[] obses = new IObservation[m_sources.length];
    for( int i = 0; i < m_sources.length; i++ )
    {
      final Source source = m_sources[i];
      obses[i] = getObservation( f, source.getProperty(), source.getFrom(), source.getTo() );

      //      m_summaryWriter.write( "Zeitreihe unbekannt: " + (
      // (TimeseriesLink)f.getProperty( source.getProperty() ) ).getHref() );
      // TODO: catch exception and log unknown obs
      //      write( "Zeitreihe m�glicherweise unbekannt: "
      //          + ( (TimeseriesLink)feature.getProperty( prop ) ).getHref(),
      // e.getLocalizedMessage(),
      //          msgWriter, logWriter );
    }

    return obses;
  }

  private IObservation getObservation( final Feature feature, final String sourceProperty, final Date from, final Date to )
      throws MalformedURLException, SensorException
  {
    if( sourceProperty == null )
      return null;

    final TimeseriesLink sourcelink = (TimeseriesLink)feature.getProperty( sourceProperty );
    if( sourcelink == null )
      return null;
    // keine Zeitreihe verlink, z.B. kein Pegel am
    // Knoten in KalypsoNA
    String href = sourcelink.getHref();
    final String sourceref = ZmlURL.insertRequest( href, new ObservationRequest( from, to ) );

    final URL sourceURL = new UrlResolver().resolveURL( m_context, sourceref );

    return ZmlFactory.parseXML( sourceURL, feature.getId() );
  }
}
