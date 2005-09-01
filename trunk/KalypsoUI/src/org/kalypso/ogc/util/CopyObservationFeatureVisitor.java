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

import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Properties;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.kalypso.commons.java.net.UrlResolver;
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

  private final Properties m_metadata;

  /**
   * @param context
   *          context to resolve relative url
   * @param urlResolver
   *          resolver for urls
   * @param forecastFrom
   * @param forecastTo
   * @param logWriter
   * @param sources
   * @param metadata
   *          All entries will be added to the target observation
   * @param targetobservation
   */
  public CopyObservationFeatureVisitor( final URL context, final IUrlResolver urlResolver,
      final String targetobservation, final Source[] sources, final Properties metadata, final Date forecastFrom,
      final Date forecastTo, final PrintWriter logWriter )
  {
    m_context = context;
    m_urlResolver = urlResolver;
    m_targetobservation = targetobservation;
    m_sources = sources;
    m_metadata = metadata;
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
        m_logWriter.println( SUMM_INFO + "Keine Ziel-Verknüpfung gefunden für Feature mit ID: " + f.getId() );
        return true;
      }

      if( sourceObses.length == 0 || sourceObses[0] == null )
      {
        m_logWriter.println( SUMM_INFO + "Keine Quell-Verknüpfung(en) gefunden für Feature mit ID: " + f.getId() );
        return true;
      }

      final IObservation resultObs;

      // only do ForeCastFilter if we have more than one obs
      if( sourceObses.length < 2 || sourceObses[1] == null )
        resultObs = sourceObses[0];
      else
      {
        final ForecastFilter fc = new ForecastFilter();
        fc.initFilter( sourceObses, sourceObses[0], null );
        // TODO check if null context is ok here
        resultObs = fc;
      }

      // set forecast metadata, might be used in diagram for instance
      // to mark the forecast range
      TimeserieUtils.setForecast( resultObs, m_forecastFrom, m_forecastTo );

      // put additional metadata that we got from outside
      resultObs.getMetadataList().putAll( m_metadata );

      // protocol the observations here and inform the user
      KalypsoProtocolWriter.analyseValues( resultObs, resultObs.getValues( null ), m_logWriter, SUMM_INFO );

      // remove query part if present, href is also used as file name here!
      final String href = ZmlURL.getIdentifierPart( targetlink.getHref() );

      final IFile targetfile = ResourceUtilities.findFileFromURL( m_urlResolver.resolveURL( m_context, href ) );

      final File file = targetfile.getLocation().toFile();
      FileOutputStream stream = null;
      try
      {
        if( !file.getParentFile().exists() )
          file.getParentFile().mkdirs();
        stream = new FileOutputStream( file );
        final ObservationType type = ZmlFactory.createXML( resultObs, null );
        ZmlFactory.getMarshaller().marshal( type, stream );
      }
      finally
      {
        IOUtils.closeQuietly( stream );
      }

      //      FolderUtilities.mkdirs( targetfile.getParent() );
      //      final SetContentHelper thread = new SetContentHelper()
      //      {
      //        protected void write( final OutputStreamWriter w ) throws Throwable
      //        {
      //          final ObservationType type = ZmlFactory.createXML( resultObs, null );
      //          ZmlFactory.getMarshaller().marshal( type, w );
      //        }
      //      };
      //      thread.setFileContents( targetfile, true, true, new NullProgressMonitor() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      m_logWriter.println( "Fehler beim Kopieren der Zeitreihen für Feature: " + f.getId() );
      m_logWriter.println( e.getLocalizedMessage() );
    }

    return true;
  }

  private IObservation[] getObservations( final Feature f ) throws SensorException
  {
    List result = new ArrayList();
    //    final IObservation[] obses = new IObservation[m_sources.length];
    for( int i = 0; i < m_sources.length; i++ )
    {
      final Source source = m_sources[i];
      try
      {
        result.add( getObservation( f, source.getProperty(), source.getFrom(), source.getTo(), source.getFilter() ) );
      }
      catch( Exception e )
      {
        // it is possible to use the target also as input, e.g. if you want to update just a part of the zml.
        // if this source==target is unreachable it should be ignored, if it is not the target throw an exception
        if( m_targetobservation.equals( source.getProperty() ) )
          m_logWriter
              .println( "Hinweis: Zielzeitreihe konnte nicht gleichzeitig als Quelle verwendet werden und wird ignoriert. (kein Fehler)" );
        else
          throw new SensorException( e );
      }
      //      m_summaryWriter.write( "Zeitreihe unbekannt: " + (
      // (TimeseriesLink)f.getProperty( source.getProperty() ) ).getHref() );
      // TODO: catch exception and log unknown obs
      //      write( "Zeitreihe möglicherweise unbekannt: "
      //          + ( (TimeseriesLink)feature.getProperty( prop ) ).getHref(),
      // e.getLocalizedMessage(),
      //          msgWriter, logWriter );
    }

    return (IObservation[])result.toArray( new IObservation[result.size()] );
  }

  private IObservation getObservation( final Feature feature, final String sourceProperty, final Date from,
      final Date to, String filter ) throws MalformedURLException, SensorException
  {
    if( sourceProperty == null )
      return null;

    final TimeseriesLink sourcelink = (TimeseriesLink)feature.getProperty( sourceProperty );
    if( sourcelink == null )
      return null;
    // keine Zeitreihe verlink, z.B. kein Pegel am
    // Knoten in KalypsoNA
    final String href;
    if( filter == null )
      href = sourcelink.getHref();
    else
      href = ZmlURL.insertQueryPart( sourcelink.getHref(), filter ); // use insertQueryPart, not insertFilter, because
    // filter variable might also contain request spec
    final String sourceref = ZmlURL.insertRequest( href, new ObservationRequest( from, to ) );

    final URL sourceURL = new UrlResolver().resolveURL( m_context, sourceref );

    try
    {
      return ZmlFactory.parseXML( sourceURL, feature.getId() );
    }
    catch( final SensorException e )
    {
      // tricky: wrap the exception with timeserie-link as text to have a better error message
      throw new SensorException( "Konnte Zeitreihe nicht laden: " + sourceref, e );
    }
  }

  public final static class Source
  {
    private final String property;

    private final Date from;

    private final Date to;

    private final String filter;

    public Source( final String prop, final Date dfrom, final Date dto, final String filt )
    {
      this.property = prop;
      this.from = dfrom;
      this.to = dto;
      this.filter = filt;
    }

    public final Date getFrom()
    {
      return from;
    }

    public final String getProperty()
    {
      return property;
    }

    public final Date getTo()
    {
      return to;
    }

    public String getFilter()
    {
      return filter;
    }
  }

}
