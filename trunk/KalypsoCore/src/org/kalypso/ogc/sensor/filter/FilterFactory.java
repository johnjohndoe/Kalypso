package org.kalypso.ogc.sensor.filter;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.net.URL;
import java.util.Properties;

import org.apache.commons.io.IOUtils;
import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.util.factory.ConfigurableCachableObjectFactory;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.zml.filters.AbstractFilterType;
import org.kalypso.zml.filters.ObjectFactory;
import org.xml.sax.InputSource;

/**
 * Handles filter information found within an URL and creates the corresponding
 * filter.
 * <p>
 * This class uses the Singleton Pattern.
 * 
 * @author schlienger
 */
public class FilterFactory
{
  /** start tag for filters */
  private static final String TAG_FILTER1 = "<filter>";

  /** stop tag for filters */
  private static final String TAG_FILTER2 = "</filter>";

  private final ConfigurableCachableObjectFactory m_fact;

  private static FilterFactory m_instance = null;

  /** jaxb object factory for filter stuff */
  private static final ObjectFactory OF_FILTER = new ObjectFactory();

  /**
   * Constructor. Reads the properties and creates the factory.
   */
  protected FilterFactory( )
  {
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
   * Singleton access
   * 
   * @return singleton instance
   */
  private static FilterFactory getInstance( )
  {
    if( m_instance == null )
      m_instance = new FilterFactory();

    return m_instance;
  }

  /**
   * Returns a creator instance for the given filter type
   * 
   * @param aft
   * @return creator instance
   * @throws FactoryException
   */
  public static IFilterCreator getCreatorInstance( final AbstractFilterType aft )
      throws FactoryException
  {
    final String className = ClassUtilities.getOnlyClassName( aft.getClass() );

    final IFilterCreator creator = (IFilterCreator) getInstance().m_fact
        .getObjectInstance( className, IFilterCreator.class );

    return creator;
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
  public static IObservation createFilterFrom( final URL u,
      final IObservation obs ) throws SensorException
  {
    final String strUrl = u.toExternalForm();

    final int i1 = strUrl.indexOf( TAG_FILTER1 );
    if( i1 == -1 )
      return obs;

    final int i2 = strUrl.indexOf( TAG_FILTER2, i1 );
    if( i2 == -1 )
      throw new SensorException(
          "URL-fragment does not contain a valid filter specification. URL: "
              + strUrl );

    final String strFilterXml = strUrl
        .substring( i1 + TAG_FILTER1.length(), i2 );

    final StringReader sr = new StringReader( strFilterXml );

    final IObservation obsFilter;
    try
    {
      final AbstractFilterType af = (AbstractFilterType) OF_FILTER.createUnmarshaller().unmarshal( new InputSource( sr ) );

      final IFilterCreator creator = getCreatorInstance( af );
      obsFilter = creator.createFilter( af, obs );
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      throw new SensorException( e );
    }
    finally
    {
      IOUtils.closeQuietly( sr );
    }

    return obsFilter;
  }

  /**
   * Creates a filter hierarchy
   * 
   * @param ins
   * @return
   * @throws SensorException
   */
  public static IObservation createFilter( final InputSource ins )
      throws SensorException
  {
    try
    {
      final AbstractFilterType af = (AbstractFilterType) OF_FILTER.createUnmarshaller().unmarshal( ins );

      final IFilterCreator creator = getCreatorInstance( af );
      return creator.createFilter( af, null );
    }
    catch( Exception e )
    {
      throw new SensorException( e );
    }
  }
  
  /**
   * Creates a filter hierarchy
   * 
   * @param ins
   * @return
   * @throws SensorException
   */
  public static IObservation createFilter( final InputStream ins )
      throws SensorException
  {
    try
    {
      final AbstractFilterType af = (AbstractFilterType) OF_FILTER.createUnmarshaller().unmarshal( ins );

      final IFilterCreator creator = getCreatorInstance( af );
      return creator.createFilter( af, null );
    }
    catch( Exception e )
    {
      throw new SensorException( e );
    }
  }
}