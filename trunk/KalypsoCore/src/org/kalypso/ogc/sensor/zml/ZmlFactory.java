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
package org.kalypso.ogc.sensor.zml;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.TimeZone;
import java.util.Map.Entry;
import java.util.logging.Logger;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.apache.commons.io.IOUtils;
import org.kalypso.java.util.PropertiesHelper;
import org.kalypso.java.xml.XMLUtilities;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationConstants;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.FilterFactory;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.proxy.ArgsObservationProxy;
import org.kalypso.ogc.sensor.proxy.AutoProxyFactory;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.zml.values.IZmlValues;
import org.kalypso.ogc.sensor.zml.values.ZmlArrayValues;
import org.kalypso.ogc.sensor.zml.values.ZmlLinkValues;
import org.kalypso.ogc.sensor.zml.values.ZmlTuppleModel;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.util.parser.IParser;
import org.kalypso.util.parser.ParserException;
import org.kalypso.util.parser.ParserFactory;
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.util.xml.XmlTypes;
import org.kalypso.util.xml.xlink.IXlink;
import org.kalypso.util.xml.xlink.JAXBXLink;
import org.kalypso.zml.AxisType;
import org.kalypso.zml.MetadataListType;
import org.kalypso.zml.MetadataType;
import org.kalypso.zml.ObjectFactory;
import org.kalypso.zml.Observation;
import org.kalypso.zml.ObservationType;
import org.kalypso.zml.AxisType.ValueArrayType;
import org.kalypso.zml.AxisType.ValueLinkType;
import org.xml.sax.InputSource;

/**
 * Factory for ZML-Files. ZML is a flexible format that covers following
 * possibilities:
 * <ul>
 * <li>inlined: values are stored as array of items in each axis definition
 * <li>linked: values are stored in an external CSV-like file
 * <li>block-inlined: values are stored CSV-like, but in the zml file itself
 * </ul>
 * The block-inlined Format is used with valueLink elements and if the
 * Href-Attribute is not specified, is empty, or contains "#data".
 * 
 * @author schlienger
 */
public class ZmlFactory
{
  private final static ObjectFactory OF = new ObjectFactory();

  private static ParserFactory m_parserFactory = null;

  private static Properties m_parserProps = null;

  private static Logger LOG = Logger.getLogger( ZmlFactory.class.getName() );

  private ZmlFactory()
  {
    // not to be instanciated
  }

  private static Properties getProperties()
  {
    if( m_parserProps == null )
    {
      InputStream ins = null;

      try
      {
        m_parserProps = new Properties();

        ins = ZmlFactory.class
            .getResourceAsStream( "resource/types2parser.properties" );

        m_parserProps.load( ins );

        return m_parserProps;
      }
      catch( IOException e )
      {
        throw new RuntimeException( e );
      }
      finally
      {
        IOUtils.closeQuietly( ins );
      }
    }

    return m_parserProps;
  }

  /**
   * Helper, man sollte es benutzen um auf die ParserFactory zugreifen zu können
   * 
   * @return parser factory
   */
  public static synchronized ParserFactory getParserFactory()
  {
    if( m_parserFactory == null )
      m_parserFactory = new ParserFactory( getProperties(), ZmlFactory.class
          .getClassLoader() );

    return m_parserFactory;
  }

  /**
   * Supported types are listed in the types2parser.properties file.
   * 
   * TODO: noch das default format (_format) hinzufügen und eventuell die xs:
   * Zeugs wegmachen Siehe properties datei
   * 
   * @param className
   * @return the XSD-Type for the given Java-Class
   */
  public static String getXSDTypeFor( final String className )
  {
    return getProperties().getProperty( className );
  }

  /**
   * Parses the XML and creates an IObservation object.
   * 
   * @see ZmlFactory#parseXML(InputSource, String, URL)
   * 
   * @param url the url specification of the zml
   * @param identifier [optional] ID für Repository
   * 
   * @return IObservation object
   * 
   * @throws SensorException in case of parsing or creation problem
   */
  public static IObservation parseXML( final URL url, final String identifier )
      throws SensorException
  {
    InputStream inputStream = null;

    try
    {
      final String zmlId = ZmlURL.getIdentifierPart( url );

      if( ZmlURL.isUseAsContext( url ) )
      {
        /*
         * if there is a fragment called "useascontext" then we are dealing with
         * a special kind of zml-url: the scheme denotes solely a context, the
         * observation is strictly built using the query part and the context.
         */

        // create the real context
        final URL context = new URL( zmlId );

        // directly return the observation
        return decorateObservation( null, url.toExternalForm(), context );
      }

      final String scheme = ZmlURL.getSchemePart( url );
      if( scheme.startsWith( "file" ) || scheme.startsWith( "platform" ) )
      {
        /*
         * if this is a local url, we remove the query part because Eclipse
         * Platform's URLStreamHandler cannot deal with it.
         */

        // only take the simple part of the url
        final URL tmpUrl = new URL( zmlId );

        // stream is closed in finally
        inputStream = tmpUrl.openStream();
      }
      else
      {
        // default behaviour (might use a specific stream handler like
        // the OCSUrlStreamHandler )
        inputStream = url.openStream();
      }

      inputStream = new BufferedInputStream( inputStream );

      // url is given as an argument here (and not tmpUrl) in order not to
      // loose the query part we might have removed because of Eclipse's
      // url handling.
      return parseXML( new InputSource( inputStream ), identifier, url );
    }
    catch( final IOException e )
    {
      throw new SensorException( "Error while unmarshalling: "
          + url.toExternalForm(), e );
    }
    finally
    {
      IOUtils.closeQuietly( inputStream );
    }
  }

  /**
   * Parse the XML and create an IObservation instance.
   * 
   * @param source contains the zml
   * @param identifier [optional] the identifier of the resulting observation
   * @param context [optional] the context of the source in order to resolve
   *          relative url
   */
  public static IObservation parseXML( final InputSource source,
      final String identifier, final URL context ) throws SensorException
  {
    final Observation obs;

    try
    {
      final Unmarshaller u = getUnmarshaller();

      obs = (Observation)u.unmarshal( source );
    }
    catch( final JAXBException e )
    {
      throw new SensorException( e );
    }

    // metadata
    final MetadataList metadata = new MetadataList();
    metadata.put( ObservationConstants.MD_NAME, obs.getName() );

    if( obs.getMetadataList() != null )
    {
      final List mdList = obs.getMetadataList().getMetadata();

      for( final Iterator it = mdList.iterator(); it.hasNext(); )
      {
        final MetadataType md = (MetadataType)it.next();

        final String value;
        if( md.getValue() != null )
          value = md.getValue();
        else if( md.getData() != null )
          value = md.getData().replaceAll( XMLUtilities.CDATA_BEGIN_REGEX, "" )
              .replaceAll( XMLUtilities.CDATA_END_REGEX, "" );
        else
          value = "";

        metadata.put( md.getName(), value );
      }
    }

    // axes and values
    final List tmpList = obs.getAxis();
    final Map valuesMap = new HashMap( tmpList.size() );

    final String data = obs.getData(); // data is optional and can be null

    for( int i = 0; i < tmpList.size(); i++ )
    {
      final AxisType tmpAxis = (AxisType)tmpList.get( i );

      final Properties props = PropertiesHelper.parseFromString( tmpAxis
          .getDatatype(), '#' );
      final String type = props.getProperty( "TYPE" );
      String format = props.getProperty( "FORMAT" );

      final IParser parser;
      final IZmlValues values;
      try
      {
        // if format not specified, then we use the default specification
        // found in the properties file. Every type can have a default format
        // declared in this file using the convention that the property
        // must be build using the type name followed by the '_format' string.
        if( format == null || format == "" )
          format = getProperties().getProperty( type + "_format" );

        parser = getParserFactory().createParser( type, format );

        values = createValues( context, tmpAxis, parser, data );
      }
      catch( final Exception e ) // generic exception caught for simplicity
      {
        throw new SensorException( e );
      }

      final IAxis axis = new DefaultAxis( tmpAxis.getName(), tmpAxis.getType(),
          tmpAxis.getUnit(), parser.getObjectClass(), tmpAxis.isKey() );

      valuesMap.put( axis, values );
    }

    final ZmlTuppleModel model = new ZmlTuppleModel( valuesMap );

    IXlink target = null;
    if( obs.getTarget() != null )
      target = new JAXBXLink( obs.getTarget() );

    final String href = context != null ? context.toExternalForm() : "";

    final SimpleObservation zmlObs = new SimpleObservation( href, identifier,
        obs.getName(), obs.isEditable(), target, metadata, model.getAxisList(),
        model );

    return decorateObservation( zmlObs, href, context );
  }

  /**
   * Central method for decorating the observation according to its context and
   * identifier. It internally checks for:
   * <ol>
   * <li>a filter specification (for example: interpolation filter)
   * <li>a proxy specification (for example: from-to)
   * <li>an auto-proxy possibility (for example: WQ-Metadata)
   * </ol>
   */
  private static IObservation decorateObservation( final IObservation zmlObs,
      final String href, final URL context ) throws SensorException
  {
    // tricky: maybe make a filtered observation out of this one
    final IObservation filteredObs = FilterFactory.createFilterFrom( href,
        zmlObs, context );

    // tricky: check if a proxy has been specified in the url
    final IObservation proxyObs = createProxyFrom( href, filteredObs );

    // tricky: check if the observation is auto-proxyable using its own metadata
    // (for instance WQ)
    final IObservation autoProxyObs = AutoProxyFactory.getInstance()
        .proxyObservation( proxyObs );

    return autoProxyObs;
  }

  /**
   * Helper: may create a proxy observation depending on the information coded
   * in the url.
   * 
   * @return proxy or original observation
   */
  private static IObservation createProxyFrom( final String href,
      final IObservation baseObs )
  {
    if( href == null || href.length() == 0 )
      return baseObs;

    IVariableArguments args = null;

    // check if a DateRange proxy can be created
    args = ZmlURL.checkDateRange( href );
    if( args != null )
      return new ArgsObservationProxy( args, baseObs );

    return baseObs;
  }

  /**
   * Parses the values and create the corresponding objects.
   * 
   * @param context context into which the original file exists
   * @param axisType binding object for axis
   * @param parser configured parser enabled for parsing the values according to
   *          axis spec
   * @param data [optional] contains the data-block if observation is
   *          block-inline
   * @return corresponding values depending on value axis type
   * 
   * @throws ParserException
   * @throws MalformedURLException
   * @throws IOException
   */
  private static IZmlValues createValues( final URL context,
      final AxisType axisType, final IParser parser, final String data )
      throws ParserException, MalformedURLException, IOException
  {
    final ValueArrayType va = axisType.getValueArray();
    if( va != null )
      return new ZmlArrayValues( va, parser );

    // loader for linked values, here we specify where base location is
    final ValueLinkType vl = axisType.getValueLink();
    if( vl != null )
      return new ZmlLinkValues( vl, parser, context, data );

    throw new IllegalArgumentException( "AxisType is not supported: "
        + axisType.toString() );
  }

  /**
   * Cover method of createXML( IObservation, IVariableArguments, TimeZone )
   */
  public static ObservationType createXML( final IObservation obs,
      final IVariableArguments args ) throws FactoryException
  {
    return createXML( obs, args, null );
  }

  /**
   * Creates an XML-Observation ready for marshalling.
   * 
   * TODO: complete for target property, etc..
   * 
   * @param obs
   * @param args
   * @param timezone the timezone into which dates should be converted before
   *          serialized
   * @return an ObservationType object, ready for marshalling.
   * 
   * @throws FactoryException
   */
  public static ObservationType createXML( final IObservation obs,
      final IVariableArguments args, final TimeZone timezone )
      throws FactoryException
  {
    try
    {
      // first of all fetch values
      final ITuppleModel values = obs.getValues( args );

      final ObservationType obsType = OF.createObservation();
      obsType.setName( obs.getName() );
      obsType.setEditable( obs.isEditable() );

      final MetadataListType metadataListType = OF.createMetadataListType();
      obsType.setMetadataList( metadataListType );
      final List metadataList = metadataListType.getMetadata();
      for( final Iterator it = obs.getMetadataList().entrySet().iterator(); it
          .hasNext(); )
      {
        final Map.Entry entry = (Entry)it.next();

        final String mdKey = (String)entry.getKey();
        final String mdValue = (String)entry.getValue();

        final MetadataType mdType = OF.createMetadataType();
        mdType.setName( mdKey );

        // TRICKY: if this looks like an xml-string then pack it
        // into a CDATA section and use the 'data'-Element instead
        if( mdValue.startsWith( XMLUtilities.XML_HEADER_BEGIN ) )
          mdType.setData( XMLUtilities.encapsulateInCDATA( mdValue ) );
        else
          mdType.setValue( mdValue );

        metadataList.add( mdType );
      }

      // insert timezone in the metadata is specified
      if( timezone != null )
      {
        final MetadataType mdType = OF.createMetadataType();
        mdType.setName( TimeserieConstants.MD_TIMEZONE );
        mdType.setValue( timezone.getID() );

        metadataList.add( mdType );
      }

      final List axisList = obsType.getAxis();
      final IAxis[] axes = obs.getAxisList();
      for( int i = 0; i < axes.length; i++ )
      {
        if( axes[i].isPersistable() )
        {
          final AxisType axisType = OF.createAxisType();

          final String xsdType = getXSDTypeFor( axes[i].getDataClass()
              .getName() );

          axisType.setDatatype( xsdType );
          axisType.setName( axes[i].getName() );
          axisType.setUnit( axes[i].getUnit() );
          axisType.setType( axes[i].getType() );
          axisType.setKey( axes[i].isKey() );

          final ValueArrayType valueArrayType = OF
              .createAxisTypeValueArrayType();

          valueArrayType.setSeparator( ";" );
          valueArrayType
              .setValue( buildValueString( values, axes[i], timezone ) );

          axisType.setValueArray( valueArrayType );

          axisList.add( axisType );
        }
      }

      return obsType;
    }
    catch( Exception e )
    {
      throw new FactoryException( e );
    }
  }

  /**
   * TODO: verbessern: kein If-statement mehr und anderen datentypen?
   * 
   * @return string that contains the serialized values
   */
  private static String buildValueString( final ITuppleModel model,
      final IAxis axis, final TimeZone timezone ) throws SensorException
  {
    final StringBuffer sb = new StringBuffer();

    if( java.util.Date.class.isAssignableFrom( axis.getDataClass() ) )
      buildStringDateAxis( model, axis, sb, timezone );
    else if( Number.class.isAssignableFrom( axis.getDataClass() ) )
      buildStringNumberAxis( model, axis, sb );
    else if( String.class.isAssignableFrom( axis.getDataClass() ) )
      buildStringAxis( model, axis, sb );
    else
      throw new IllegalArgumentException( "Data type currently not supported" );

    return sb.toString();
  }

  private static void buildStringAxis( ITuppleModel model, IAxis axis,
      StringBuffer sb ) throws SensorException
  {
    final int amount = model.getCount() - 1;
    for( int i = 0; i < amount; i++ )
      sb.append( model.getElement( i, axis ) ).append( ";" );

    if( amount > 0 )
      sb.append( model.getElement( amount, axis ) );
  }

  private static void buildStringDateAxis( final ITuppleModel model,
      final IAxis axis, final StringBuffer sb, final TimeZone timezone )
      throws SensorException
  {
    XmlTypes.PDATE.setTimezone( timezone );

    final int amount = model.getCount() - 1;
    for( int i = 0; i < amount; i++ )
      sb.append( XmlTypes.PDATE.toString( model.getElement( i, axis ) ) )
          .append( ";" );

    if( amount > 0 )
      sb.append( XmlTypes.PDATE.toString( model.getElement( amount, axis ) ) );
  }

  /**
   * Uses the default toString() method of the elements. TODO: check if this
   * always works fine for XML-Schema types
   */
  private static void buildStringNumberAxis( final ITuppleModel model,
      final IAxis axis, final StringBuffer sb ) throws SensorException
  {
    final int amount = model.getCount() - 1;
    for( int i = 0; i < amount; i++ )
    {
      final Object elt = model.getElement( i, axis );

      if( elt == null )
        LOG.warning( "Element " + i + " is null for Axis: " + axis );

      sb.append( elt ).append( ";" );
    }

    if( amount > 0 )
    {
      final Object elt = model.getElement( amount, axis );

      if( elt == null )
        LOG.warning( "Element " + amount + " is null for Axis: " + axis );

      sb.append( elt );
    }
  }

  public static Marshaller getMarshaller() throws JAXBException
  {
    final Marshaller marshaller = OF.createMarshaller();
    marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );

    return marshaller;
  }

  private static Unmarshaller getUnmarshaller() throws JAXBException
  {
    final Unmarshaller unmarshaller = OF.createUnmarshaller();

    return unmarshaller;
  }

  /**
   * @return valid parser for the given axis
   */
  public static IParser createParser( final IAxis axis )
      throws FactoryException
  {
    final ParserFactory pf = getParserFactory();

    return pf.createParser( "JAVA_" + axis.getDataClass().getName(), null );
  }

  /**
   * Helper method for simply writing the observation to a file
   * 
   * @throws SensorException if an IOException or a FactoryException is thrown
   *           internally
   */
  public static void writeToFile( final IObservation obs, final File file )
      throws SensorException
  {
    OutputStream outs = null;
    try
    {
      final ObservationType xml = createXML( obs, null );

      outs = new FileOutputStream( file );

      getMarshaller().marshal( xml, outs );

      outs.close();
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new SensorException( e );
    }
    finally
    {
      IOUtils.closeQuietly( outs );
    }
  }
}