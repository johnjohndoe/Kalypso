package org.kalypso.ogc.sensor.filter;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.io.IOUtils;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.util.factory.ConfigurableCachableObjectFactory;

/**
 * Handles filter information found within an URL and creates the corresponding
 * filter.
 * 
 * @author schlienger
 */
public class FilterFactory
{
  /**
   * regular expression representing the syntax of the filter specification in
   * the fragment part of the URL. Here is the syntax:
   * 
   * <pre>
   *      file://path/resource.ext#filter(ID*CONF)
   *           
   *      ID: Alphanum
   *          id of the filter to use
   *           
   *      CONF: Alphanum and following chars: - _ . ! &tilde; '
   *            configuration string for the filter
   * </pre>
   */
  private final static String REGEXP_FILTER = "filter\\((\\p{Alpha}+?)*([\\p{Alpha}-_.!~']+?)\\)";

  private final Pattern m_pattern;

  private final ConfigurableCachableObjectFactory m_fact;
  
  private static FilterFactory m_instance = null;

  /**
   * Constructor. Reads the properties and creates the factory.
   */
  protected FilterFactory( )
  {
    m_pattern = Pattern.compile( REGEXP_FILTER );

    final InputStream ins = getClass().getResourceAsStream(
        "resource/filters.props" );
    final Properties props = new Properties();
    try
    {
      props.load( ins );
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( ins );
    }

    m_fact = new ConfigurableCachableObjectFactory( props, false, getClass()
        .getClassLoader() );
  }

  /**
   * Creates a filtered observation if the specified url contains the
   * specification for creating a filter. Otherwise directly returns the given
   * observation.
   * 
   * @param u
   * @param obs
   * @return IObservation
   * @throws SensorException
   */
  protected IObservation internalCreateFilterFrom( final URL u, final IObservation obs )
      throws SensorException
  {
    String strUrl = u.toExternalForm();

    final int i1 = strUrl.indexOf( "filter(" );
    if( i1 == -1 )
      return obs;

    final int i2 = strUrl.indexOf( ")", i1 );
    if( i2 == -1 )
      throw new SensorException(
          "URL-fragment does not contain a valid filter specification. URL: "
              + strUrl );

    final String strFrag = strUrl.substring( i1, strUrl.length() - i2 );

    final Matcher matcher = m_pattern.matcher( strFrag );

    if( !matcher.matches() )
      throw new SensorException(
          "URL-fragment does not contain a valid filter specification. Fragment: "
              + strFrag );

    final String filterId = matcher.group( 0 );
    final String filterConf = matcher.group( 1 );

    try
    {
      final IObservationFilter filter = (IObservationFilter) m_fact
          .getObjectInstance( filterId, IObservationFilter.class );

      filter.initFilter( filterConf, obs );

      return filter;
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      throw new SensorException( "URL could not be resolved", e );
    }
  }
  
  /**
   * Creates a filtered observation if the specified url contains the
   * specification for creating a filter. Otherwise directly returns the given
   * observation.
   * 
   * @param u
   * @param obs
   * @return IObservation
   * @throws SensorException
   */
  public static IObservation createFilterFrom( final URL u, final IObservation obs ) throws SensorException
  {
    if( m_instance == null )
      m_instance = new FilterFactory();
    
    return m_instance.internalCreateFilterFrom( u, obs );
  }
}