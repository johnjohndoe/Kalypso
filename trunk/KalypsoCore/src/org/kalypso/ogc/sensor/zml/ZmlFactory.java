package org.kalypso.ogc.sensor.zml;

import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.apache.commons.io.IOUtils;
import org.kalypso.java.util.PropertiesHelper;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationConstants;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.FilterFactory;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.proxy.ArgsObservationProxy;
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
 * Factory for ZML-Files.
 * 
 * @author schlienger
 */
public class ZmlFactory
{
  private final static ObjectFactory OF = new ObjectFactory();

  private static ParserFactory m_parserFactory = null;

  private static Properties m_parserProps = null;

  private ZmlFactory( )
  {
    // not to be instanciated
  }

  private static Properties getProperties( )
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
  public static synchronized ParserFactory getParserFactory( )
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
   * Parses the XML and creates a IObservation object.
   * 
   * @see ZmlFactory#parseXML(InputSource, String, URL)
   * 
   * @param url
   * @param identifier
   * @return IObservation object
   * 
   * @throws SensorException
   */
  public static IObservation parseXML( final URL url, final String identifier )
      throws SensorException
  {
    InputStream inputStream = null;

    try
    {
      final String zmlId = ZmlURL.getIdentifierPart( url );

      // if no id specified, only use the filter part to retrieve observation
      if( zmlId == null || zmlId.length() == 0 )
        return FilterFactory.createFilter( url.getQuery() );

      // check if this is a local url. In the positive, we remove the
      // query part because Eclipse Platform's URLStreamHandler cannot deal with
      // it.
      final String scheme = ZmlURL.getSchemePart( url );
      if( scheme.startsWith( "file" ) || scheme.startsWith( "platform" ) )
      {
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

      // url is given as an argument here (and not tmpUrl) in order not to
      // loose the query part we might have removed because of Eclipse's 
      // url handling.
      return parseXML( new InputSource( inputStream ), identifier, url );
    }
    catch( IOException e )
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
   * Parses the XML and creates an IObservation object.
   * 
   * @param source
   * @param identifier
   * @param context
   * @return IObservation
   * 
   * @throws SensorException
   */
  public static IObservation parseXML( final InputSource source,
      final String identifier, final URL context ) throws SensorException
  {
    final Observation obs;

    try
    {
      final Unmarshaller u = getUnmarshaller();

      obs = (Observation) u.unmarshal( source );
    }
    catch( JAXBException e )
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
        final MetadataType md = (MetadataType) it.next();

        metadata.put( md.getName(), md.getValue() );
      }
    }

    // axes and values
    final List tmpList = obs.getAxis();
    final Map valuesMap = new HashMap( tmpList.size() );

    for( int i = 0; i < tmpList.size(); i++ )
    {
      final AxisType tmpAxis = (AxisType) tmpList.get( i );

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

        values = createValues( context, tmpAxis, parser );
      }
      catch( Exception e ) // generic exception caught for simplicity
      {
        throw new SensorException( e );
      }

      final IAxis axis = new DefaultAxis( tmpAxis.getName(), tmpAxis.getType(),
          tmpAxis.getUnit(), parser.getObjectClass(), i, tmpAxis.isKey() );

      valuesMap.put( axis, values );
    }

    final ZmlTuppleModel model = new ZmlTuppleModel( valuesMap );

    IXlink target = null;
    if( obs.getTarget() != null )
      target = new JAXBXLink( obs.getTarget() );

    final ZmlObservation zmlObs = new ZmlObservation( context.toExternalForm(), identifier, obs
        .getName(), obs.isEditable(), target, metadata, model.getAxisList(),
        model );

    // tricky: first check if a proxy has been specified in the url
    final IObservation proxyObs = createProxyFrom( context, zmlObs );

    // tricky: maybe make a filtered observation out of this one
    final IObservation filteredObs = FilterFactory.createFilterFrom( context,
        proxyObs );

    return filteredObs;
  }

  /**
   * Helper: mey create a proxy observation depending on the information coded
   * in the url.
   * 
   * @param context
   * @param baseObs
   * @return proxy or original observation
   */
  private static IObservation createProxyFrom( final URL context,
      final IObservation baseObs )
  {
    final String str = context.toExternalForm();

    IVariableArguments args = null;

    // check if a DateRange proxy can be created
    args = ZmlURL.checkDateRange( str );
    if( args != null )
      return new ArgsObservationProxy( args, baseObs );

    return baseObs;
  }

  /**
   * Parses the values and create the corresponding objects.
   * 
   * @param context
   * @param axisType
   * @param parser
   * @return corresponding values depending on value axis type
   * 
   * @throws ParserException
   * @throws MalformedURLException
   * @throws IOException
   */
  private static IZmlValues createValues( final URL context,
      final AxisType axisType, final IParser parser ) throws ParserException,
      MalformedURLException, IOException
  {
    final ValueArrayType va = axisType.getValueArray();
    if( va != null )
      return new ZmlArrayValues( va, parser );

    // loader for linked values, here we specify where base location is
    final ValueLinkType vl = axisType.getValueLink();
    if( vl != null )
      return new ZmlLinkValues( vl, parser, context );

    throw new IllegalArgumentException( "AxisType is not supported: "
        + axisType.toString() );
  }

  /**
   * Creates an XML-Observation ready for marshalling.
   * 
   * TODO: complete for target property, etc..
   * 
   * @param obs
   * @param args
   * @return an ObservationType object, ready for marshalling.
   * 
   * @throws FactoryException
   */
  public static ObservationType createXML( final IObservation obs,
      final IVariableArguments args ) throws FactoryException
  {
    try
    {
      final ObservationType obsType = OF.createObservation();
      obsType.setName( obs.getName() );
      obsType.setEditable( obs.isEditable() );

      final MetadataListType metadataListType = OF.createMetadataListType();
      obsType.setMetadataList( metadataListType );
      final List metadataList = metadataListType.getMetadata();
      for( final Iterator it = obs.getMetadataList().entrySet().iterator(); it
          .hasNext(); )
      {
        final Map.Entry entry = (Entry) it.next();

        final String mdKey = (String) entry.getKey();
        final String mdValue = (String) entry.getValue();

        final MetadataType mdType = OF.createMetadataType();
        mdType.setName( mdKey );
        mdType.setValue( mdValue );
        metadataList.add( mdType );
      }

      final ITuppleModel values = obs.getValues( args );

      final List axisList = obsType.getAxis();
      final IAxis[] axes = obs.getAxisList();
      for( int i = 0; i < axes.length; i++ )
      {
        final AxisType axisType = OF.createAxisType();

        final String xsdType = getXSDTypeFor( axes[i].getDataClass().getName() );

        axisType.setDatatype( xsdType );
        axisType.setName( axes[i].getName() );
        axisType.setUnit( axes[i].getUnit() );
        axisType.setType( axes[i].getType() );
        axisType.setKey( axes[i].isKey() );

        final ValueArrayType valueArrayType = OF.createAxisTypeValueArrayType();

        valueArrayType.setSeparator( ";" );
        valueArrayType.setValue( buildValueString( values, axes[i] ) );

        axisType.setValueArray( valueArrayType );

        axisList.add( axisType );
      }

      return obsType;
    }
    catch( Exception e )
    {
      throw new FactoryException( e );
    }
  }

  /**
   * TODO: verbessern
   * 
   * @param model
   * @param axis
   * @return string that contains the serialized values
   * @throws SensorException
   */
  private static String buildValueString( final ITuppleModel model,
      final IAxis axis ) throws SensorException
  {
    StringBuffer sb = new StringBuffer();

    if( java.util.Date.class.isAssignableFrom( axis.getDataClass() ) )
      buildStringDateAxis( model, axis, sb );
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

  /**
   * TODO: marc, check if the date format can be fetched from the properties
   * here
   * 
   * @param model
   * @param axis
   * @param sb
   * @throws SensorException
   */
  private static void buildStringDateAxis( final ITuppleModel model,
      final IAxis axis, final StringBuffer sb ) throws SensorException
  {
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
   * 
   * @param model
   * @param axis
   * @param sb
   * @throws SensorException
   */
  private static void buildStringNumberAxis( final ITuppleModel model,
      final IAxis axis, final StringBuffer sb ) throws SensorException
  {
    final int amount = model.getCount() - 1;
    for( int i = 0; i < amount; i++ )
      sb.append( model.getElement( i, axis ) ).append( ";" );

    if( amount > 0 )
      sb.append( model.getElement( amount, axis ) );
  }

  public static Marshaller getMarshaller( ) throws JAXBException
  {
    final Marshaller marshaller = OF.createMarshaller();
    marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
    return marshaller;
  }

  private static Unmarshaller getUnmarshaller( ) throws JAXBException
  {
    return OF.createUnmarshaller();
  }
}