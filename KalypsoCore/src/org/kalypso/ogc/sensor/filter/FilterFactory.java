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
import java.net.URL;
import java.util.Properties;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Unmarshaller;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.factory.ConfigurableCachableObjectFactory;
import org.kalypso.commons.factory.FactoryException;
import org.kalypso.contribs.java.lang.reflect.ClassUtilities;
import org.kalypso.core.i18n.Messages;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.ZmlURLConstants;
import org.kalypso.zml.filters.AbstractFilterType;
import org.kalypso.zml.filters.ObjectFactory;
import org.xml.sax.InputSource;

/**
 * Handles filter information found within an URL and creates the corresponding filter.
 * <p>
 * This class uses the Singleton Pattern.
 * 
 * @author schlienger
 */
public class FilterFactory
{
  private final ConfigurableCachableObjectFactory m_fact;

  private static FilterFactory m_instance = null;

  /** jaxb context for filter stuff */
  public static final JAXBContext JC_FILTER = JaxbUtilities.createQuiet( //

      ObjectFactory.class, org.kalypso.wechmann.ObjectFactory.class, org.kalypso.zml.filters.valuecomp.ObjectFactory.class, org.kalypso.zml.ObjectFactory.class, org.w3._1999.xlinkext.ObjectFactory.class

  );

  /**
   * Constructor. Reads the properties and creates the factory.
   */
  protected FilterFactory( )
  {
    final InputStream ins = getClass().getResourceAsStream( "resource/filters.props" ); //$NON-NLS-1$
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
    m_fact = new ConfigurableCachableObjectFactory( props, false, getClass().getClassLoader() );
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
  public static IFilterCreator getCreatorInstance( final AbstractFilterType aft ) throws FactoryException
  {
    final String className = ClassUtilities.getOnlyClassName( aft.getClass() );
    final IFilterCreator creator = (IFilterCreator) getInstance().m_fact.getObjectInstance( className, IFilterCreator.class );
    return creator;
  }

  /**
   * Creates a filtered observation if the specified url contains the specification for creating a filter. Otherwise
   * directly returns the given observation.
   * 
   * @return IObservation
   */
  public static IObservation createFilterFrom( final String href, final IObservation obs, final URL context ) throws SensorException
  {
    final String strFilterXml = getFilterPart( href );
    if( strFilterXml == null )
      return obs;
    final StringReader sr = new StringReader( strFilterXml );
    final IObservation obsFilter;
    try
    {
      final Unmarshaller unmarshaller = JC_FILTER.createUnmarshaller();
      final JAXBElement<AbstractFilterType> value = (JAXBElement<AbstractFilterType>) unmarshaller.unmarshal( new InputSource( sr ) );
      // final IntervallFilterType ift = (IntervallFilterType) value.getValue();
      final AbstractFilterType af = value.getValue();
      sr.close();
      final IFilterCreator creator = getCreatorInstance( af );
      obsFilter = creator.createFilter( af, obs, context );
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
   * Returns the filter part of the given url
   */
  public static String getFilterPart( final URL url ) throws SensorException
  {
    return getFilterPart( url.toExternalForm() );
  }

  /**
   * Returns the filter part of the given url-string
   */
  public static String getFilterPart( final String strUrl ) throws SensorException
  {
    if( strUrl == null || strUrl.length() == 0 )
      return null;
    final int i1 = strUrl.indexOf( ZmlURLConstants.TAG_FILTER1 );
    if( i1 == -1 )
      return null;
    final int i2 = strUrl.indexOf( ZmlURLConstants.TAG_FILTER2, i1 );
    if( i2 == -1 )
      throw new SensorException( Messages.getString("org.kalypso.ogc.sensor.filter.FilterFactory.1") + strUrl ); //$NON-NLS-1$
    final String strFilterXml = strUrl.substring( i1 + ZmlURLConstants.TAG_FILTER1.length(), i2 );
    return strFilterXml;
  }

  /**
   * Creates a filter hierarchy using the InputSource.
   */
  public static IObservation createFilter( final InputSource ins, final URL context ) throws SensorException
  {
    try
    {
      final AbstractFilterType af = (AbstractFilterType) JC_FILTER.createUnmarshaller().unmarshal( ins );
      final IFilterCreator creator = getCreatorInstance( af );
      return creator.createFilter( af, null, context );
    }
    catch( Exception e )
    {
      throw new SensorException( e );
    }
  }

  /**
   * Creates a filter hierarchy using the InputStream.
   */
  public static IObservation createFilter( final InputStream ins, final URL context ) throws SensorException
  {
    try
    {
      final JAXBElement<AbstractFilterType> element = (JAXBElement<AbstractFilterType>) JC_FILTER.createUnmarshaller().unmarshal( ins );
      final AbstractFilterType af = element.getValue();
      final IFilterCreator creator = getCreatorInstance( af );
      return creator.createFilter( af, null, context );
    }
    catch( Exception e )
    {
      throw new SensorException( e );
    }
  }
}