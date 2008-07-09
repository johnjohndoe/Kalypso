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

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.JAXBException;
import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.net.UrlResolver;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.contribs.java.util.logging.ILogger;
import org.kalypso.contribs.java.util.logging.LoggerUtilities;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationConstants;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.status.KalypsoProtocolWriter;
import org.kalypso.ogc.sensor.template.ObsViewUtils;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.forecast.ForecastFilter;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.zml.ObservationType;
import org.kalypso.zml.obslink.ObjectFactory;
import org.kalypso.zml.obslink.TimeseriesLink;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author belger
 */
public class CopyObservationFeatureVisitor implements FeatureVisitor
{
  /** Used to search/replace metadata content with properties of the visited feature */
  private static Pattern PATTERN_FEATURE_PROPERTY = Pattern.compile( "\\Q${property;\\E([^;]*)\\Q;\\E([^}]*)\\Q}\\E" );

  private final Source[] m_sources;

  private final URL m_context;

  private final String m_targetobservation;

  private final IUrlResolver m_urlResolver;

  private final ILogger m_logger;

  private final Date m_forecastFrom;

  private final Date m_forecastTo;

  private final Properties m_metadata;

  /**
   * Die Liste der Tokens und deren Ersetzung in der Form:
   * <p>
   * tokenName-featurePropertyName;tokenName-featurePropertyName;...
   * <p>
   * Die werden benutzt um token-replace im Zml-Href durchzuführen (z.B. um automatisch der Name der Feature als
   * Request-Name zu setzen)
   */
  private final String m_tokens;

  private final File m_targetobservationDir;

  /**
   * @param context
   *          context to resolve relative url
   * @param urlResolver
   *          resolver for urls
   * @param metadata
   *          All entries will be added to the target observation
   * @param targetobservation
   */
  public CopyObservationFeatureVisitor( final URL context, final IUrlResolver urlResolver,
      final String targetobservation, final Source[] sources, final Properties metadata, final Date forecastFrom,
      final Date forecastTo, final ILogger logger, final String tokens )
  {
    m_context = context;
    m_urlResolver = urlResolver;
    m_targetobservation = targetobservation;
    m_targetobservationDir = null;
    m_sources = sources;
    m_metadata = metadata;
    m_forecastFrom = forecastFrom;
    m_forecastTo = forecastTo;
    m_logger = logger;
    m_tokens = tokens;
  }

  /**
   * @param context
   *          context to resolve relative url
   * @param urlResolver
   *          resolver for urls
   * @param metadata
   *          All entries will be added to the target observation
   */
  public CopyObservationFeatureVisitor( final URL context, final IUrlResolver urlResolver,
      final File targetobservationDir, final Source[] sources, final Properties metadata, final Date forecastFrom,
      final Date forecastTo, final ILogger logger, final String tokens )
  {
    m_context = context;
    m_urlResolver = urlResolver;
    m_targetobservationDir = targetobservationDir;
    m_targetobservation = null;
    m_sources = sources;
    m_metadata = metadata;
    m_forecastFrom = forecastFrom;
    m_forecastTo = forecastTo;
    m_logger = logger;
    m_tokens = tokens;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    try
    {
      final IObservation[] sourceObses = getObservations( f );

      final TimeseriesLink targetlink = getTargetLink( f );

      if( targetlink == null )
      {
        m_logger.log( Level.WARNING, LoggerUtilities.CODE_SHOW_MSGBOX,
            "Keine Ziel-Verknüpfung gefunden für Feature mit ID: " + f.getId() );
        return true;
      }

      if( sourceObses.length == 0 || sourceObses[0] == null )
      {
        m_logger.log( Level.WARNING, LoggerUtilities.CODE_SHOW_MSGBOX,
            "Keine Quell-Verknüpfung(en) gefunden für Feature mit ID: " + f.getId() );
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
      final MetadataList resultMetadata = resultObs.getMetadataList();
      String metaName = null;
      for( final Iterator it = m_metadata.entrySet().iterator(); it.hasNext(); )
      {
        final Map.Entry entry = (Entry)it.next();
        final String metaValue = replaceMetadata( f, (String)entry.getValue() );
        final String metaKey = (String)entry.getKey();
        resultMetadata.put( metaKey, metaValue );

        if( ObservationConstants.MD_NAME.equals( metaKey ) )
          metaName = metaValue;
      }

      // protocol the observations here and inform the user
      KalypsoProtocolWriter.analyseValues( resultObs, resultObs.getValues( null ), m_logger );

      // remove query part if present, href is also used as file name here!
      final String href = ZmlURL.getIdentifierPart( targetlink.getHref() );

      final IFile targetfile = ResourceUtilities.findFileFromURL( m_urlResolver.resolveURL( m_context, href ) );

      final IPath location = targetfile.getLocation();
      final File file = location.toFile();
      OutputStream stream = null;
      try
      {
        if( !file.getParentFile().exists() )
          file.getParentFile().mkdirs();
        stream = new BufferedOutputStream( new FileOutputStream( file ) );
        final ObservationType type = ZmlFactory.createXML( resultObs, null );
        // Overwrite name of obs if metadata name was changed.
        if( metaName != null )
          type.setName( metaName );
        ZmlFactory.getMarshaller().marshal( type, stream );
      }
      finally
      {
        IOUtils.closeQuietly( stream );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      m_logger.log( Level.WARNING, LoggerUtilities.CODE_SHOW_DETAILS,
          "Fehler beim Kopieren der Zeitreihen für Feature: " + f.getId() + "\t" + e.getLocalizedMessage() );
    }

    return true;
  }

  /**
   * Search/replace metadata values with properties from the current feature. <br>
   * REMARK: The search/replace mechanism is similar to the one used in Kalypso 2.0 and this code should be replaced as
   * soon as we change to the new version of Kalypso.
   */
  private String replaceMetadata( final Feature feature, final String metaValue )
  {
    if( metaValue == null )
      return null;

    final Matcher matcher = PATTERN_FEATURE_PROPERTY.matcher( metaValue );
    if( matcher.matches() )
    {
      final String propertyName = matcher.group( 1 );
      final String defaultValue = matcher.group( 2 );
      final QName propertyQName = createQName( propertyName );

      final Object property = feature.getProperty( propertyQName.getLocalPart() );
      if( property == null )
        return defaultValue;

      return property.toString();
    }

    return metaValue;
  }

  /**
   * REMARK: this is another backport from Kalypso 2.0 (QNameUtilities) remove as soon as we go to thsat version. syntax
   * of fragmentedFullQName :
   * 
   * <pre>
   *         &lt;namespace&gt;#&lt;localpart&gt;
   * </pre>
   * 
   * example: fragmentedFullQName = www.w3c.org#index.html <br/>If no '#' is given, a qname with only a localPart is
   * created.
   * 
   * @return qname from fragmentedFullQName
   */
  public static QName createQName( final String fragmentedFullQName )
  {
    final String[] parts = fragmentedFullQName.split( "#" );
    if( parts.length == 2 )
      return new QName( parts[0], parts[1] );

    return QName.valueOf( fragmentedFullQName );
  }

  /**
   * 
   * @param f
   */
  private TimeseriesLink getTargetLink( Feature f )
  {
    if( m_targetobservationDir != null )
    {
      final ObjectFactory factory = new ObjectFactory();
      String name = (String)f.getProperty( "name" );
      if( name == null || name.length() < 1 )
        name = f.getId();
      if( name == null || name.length() < 1 )
        name = "generated";
      final File file = getValidFile( name, 0 );
      final TimeseriesLink link;
      try
      {
        link = factory.createTimeseriesLink();
        final IFile contextIFile = ResourceUtilities.findFileFromURL( m_context );
        final File contextFile = contextIFile.getLocation().toFile();
        final String relativePathTo = FileUtilities.getRelativePathTo( contextFile, file );
        link.setHref( relativePathTo );
        return link;
      }
      catch( JAXBException e )
      {
        e.printStackTrace();
        return null;
      }
    }
    return (TimeseriesLink)f.getProperty( m_targetobservation );
  }

  private File getValidFile( final String name, int index )
  {
    String newName = name;
    if( index > 0 )
      newName = newName + "_" + Integer.toString( index );
    final String newName2 = org.kalypso.contribs.java.io.FileUtilities.validateName( newName, "_" );
    final File file = new File( m_targetobservationDir, newName2 + ".zml" );
    if( file.exists() )
    {
      index++;
      return getValidFile( name, index );
    }
    return file;
  }

  private IObservation[] getObservations( final Feature f ) throws SensorException
  {
    List result = new ArrayList();
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
          m_logger
              .log( Level.WARNING, LoggerUtilities.CODE_NONE,
                  "Hinweis: Zielzeitreihe konnte nicht gleichzeitig als Quelle verwendet werden und wird ignoriert. (kein Fehler)" );
        else
          throw new SensorException( e );
      }
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
    String sourceref = ZmlURL.insertRequest( href, new ObservationRequest( from, to ) );

    // token replacement
    if( m_tokens != null && m_tokens.length() > 0 )
    {
      final Properties properties = FeatureHelper.createReplaceTokens( feature, m_tokens );

      sourceref = ObsViewUtils.replaceTokens( sourceref, properties );
    }

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
