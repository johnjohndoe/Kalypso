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

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Date;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.eclipse.core.resources.FolderUtilities;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.eclipse.util.SetContentHelper;
import org.kalypso.java.net.IUrlResolver;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.status.KalypsoProcolWriter;
import org.kalypso.ogc.sensor.timeseries.forecast.ForecastFilter;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.ogc.util.CopyObservationHandler.Source;
import org.kalypso.util.runtime.args.DateRangeArgument;
import org.kalypso.util.url.UrlResolver;
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

  private final BufferedWriter m_summaryWriter;

  private final BufferedWriter m_detailWriter;

  public CopyObservationFeatureVisitor( final URL context, final IUrlResolver urlResolver,
      final String targetobservation, final Source[] sources, final BufferedWriter summaryWriter, final BufferedWriter detailWriter )
  {
    m_context = context;
    m_urlResolver = urlResolver;
    m_targetobservation = targetobservation;
    m_sources = sources;
    m_summaryWriter = summaryWriter;
    m_detailWriter = detailWriter;
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
        m_summaryWriter.write( "Keine Ziel-Zeitreihe gefunden für Feature mit ID: " + f.getId() );
        return true;
      }
      
      if( sourceObses.length == 0 || sourceObses[0] == null )
      {
        m_summaryWriter.write( "Keine Quell-Zeitreihe(n) gefunden für Feature mit ID: " + f.getId() );
        return true;
      }

      final IObservation obs;

      // only do ForeCastFilter if we have mor than one obs
      if( sourceObses.length < 2 || sourceObses[1] == null )
        obs = sourceObses[1];
      else
      {
        // NOTE for ForecastFilter: the order is important:
        // obs( i ) has a higher priority than obs( i + 1 )
        // with 'i' the index in the observations array...
        final ForecastFilter fc = new ForecastFilter();
        fc.initFilter( new IObservation[]
        {
            sourceObses[0],
            sourceObses[1] }, sourceObses[0] );
        obs = fc;
      }

      // set forecast metadata, might be used in diagram for instance
      // to mark the forecast range
      // TODO: extra argumente?
      //      TimeserieUtils.setForecast( obs, from2, to2 );

      // protocol the observations here and inform the user
      KalypsoProcolWriter.analyseValues( obs, obs.getValues( null ),  m_summaryWriter, m_detailWriter );

      // remove query part if present, href is also used as file name here!
      final String href = ZmlURL.getIdentifierPart( targetlink.getHref() );

      final IFile targetfile = ResourceUtilities.findFileFromURL( m_urlResolver.resolveURL(
          m_context, href ) );
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
      
      try
      {
        m_summaryWriter.write( "Fehler beim Kopieren der Zeitreihen für Feature: " + f.getId() );
        m_summaryWriter.write( e.getLocalizedMessage() );
      }
      catch( IOException e1 )
      {
        // should never happen
        e1.printStackTrace();
      }
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

//      m_summaryWriter.write( "Zeitreihe unbekannt: " + ( (TimeseriesLink)f.getProperty( source.getProperty() ) ).getHref() );
      // TODO: catch exception and log unknown obs
      //      write( "Zeitreihe möglicherweise unbekannt: "
      //          + ( (TimeseriesLink)feature.getProperty( prop ) ).getHref(),
      // e.getLocalizedMessage(),
      //          msgWriter, logWriter );
    }

    return obses;
  }

  private IObservation getObservation( final Feature feature, final String sourceProperty,
      final Date from, final Date to ) throws MalformedURLException, SensorException
  {
    if( sourceProperty == null )
      return null;

    final TimeseriesLink sourcelink = (TimeseriesLink)feature.getProperty( sourceProperty );
    if( sourcelink == null ) // keine Zeitreihe verlink, z.B. kein Pegel am
      // Knoten in KalypsoNA
      return null;
    final String sourceref = ZmlURL.insertDateRange( sourcelink.getHref(), new DateRangeArgument(
        from, to ) );

    final URL sourceURL = new UrlResolver().resolveURL( m_context, sourceref );

    return ZmlFactory.parseXML( sourceURL, feature.getId() );
  }

}
