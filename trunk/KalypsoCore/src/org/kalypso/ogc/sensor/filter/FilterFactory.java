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
package org.kalypso.ogc.sensor.filter;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.util.Properties;

import org.apache.commons.io.IOUtils;
import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.ZmlURLConstants;
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
   * @return creator instance
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
   * @return IObservation
   */
  public static IObservation createFilterFrom( final String href,
      final IObservation obs ) throws SensorException
  {
    if( href == null || href.length() == 0 )
      return obs;
    
    final int i1 = href.indexOf( ZmlURLConstants.TAG_FILTER1 );
    if( i1 == -1 )
      return obs;

    final int i2 = href.indexOf( ZmlURLConstants.TAG_FILTER2, i1 );
    if( i2 == -1 )
      throw new SensorException(
          "URL-fragment does not contain a valid filter specification. URL: "
              + href );

    final String strFilterXml = href
        .substring( i1 + ZmlURLConstants.TAG_FILTER1.length(), i2 );

    final StringReader sr = new StringReader( strFilterXml );

    final IObservation obsFilter;
    try
    {
      final AbstractFilterType af = (AbstractFilterType) OF_FILTER.createUnmarshaller().unmarshal( new InputSource( sr ) );
      sr.close();
      
      final IFilterCreator creator = getCreatorInstance( af );
      obsFilter = creator.createFilter( af, obs );
    }
    catch( final Exception e ) // generic exception caught for simplicity
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
   * Creates a filter hierarchy using the InputSource.
   * 
   * @param ins
   * @return filter
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
   * Creates a filter hierarchy using the InputStream.
   * 
   * @param ins
   * @return filter
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