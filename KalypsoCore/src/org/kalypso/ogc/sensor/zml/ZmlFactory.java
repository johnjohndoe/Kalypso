package org.kalypso.ogc.sensor.zml;

import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.kalypso.java.util.PropertiesHelper;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.zml.values.IZmlValues;
import org.kalypso.ogc.sensor.zml.values.ZmlArrayValues;
import org.kalypso.ogc.sensor.zml.values.ZmlLinkValues;
import org.kalypso.ogc.sensor.zml.values.ZmlTuppleModel;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.util.parser.IParser;
import org.kalypso.util.parser.ParserException;
import org.kalypso.util.parser.ParserFactory;
import org.kalypso.util.runtime.IVariableArguments;
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
 * @author schlienger
 */
public class ZmlFactory
{
  private final static ObjectFactory m_objectFactory = new ObjectFactory();

  private final static SimpleDateFormat m_df = new SimpleDateFormat( "yyyy.MM.dd HH:mm:ss" );

  private final static NumberFormat m_nf = NumberFormat.getInstance();

  private static ParserFactory m_parserFactory = null;

  private static Properties m_props = null;

  private static Marshaller m_marsh = null;

  private static Unmarshaller m_unmarsh = null;

  private ZmlFactory()
  {
  // not to be instanciated
  }

  private static Properties getProperties()
  {
    if( m_props == null )
    {
      m_props = new Properties();

      try
      {
        m_props.load( ZmlFactory.class.getResourceAsStream( "resource/types2parser.properties" ) );

        return m_props;
      }
      catch( IOException e )
      {
        // TODO: logging oder etwas?
        throw new RuntimeException( e );
      }
    }

    return m_props;
  }

  /**
   * Helper, man sollte es benutzen um auf die ParserFactory zugreifen zu können
   */
  private static synchronized ParserFactory getParserFactory()
  {
    if( m_parserFactory == null )
      m_parserFactory = new ParserFactory( getProperties(), ZmlFactory.class.getClassLoader() );

    return m_parserFactory;
  }

  /**
   * Returns the XSD-Type for the given Java-Class. Supported types are listed
   * in the types2parser.properties file.
   */
  public static String getXSDTypeFor( final String className )
  {
    return getProperties().getProperty( className );
  }

  /**
   * Parses the XML and creates a IObservation object.
   * 
   * @throws SensorException
   */
  public static IObservation parseXML( final URL url, final String identifier )
      throws SensorException
  {
    InputStream inputStream = null;
    Observation obs = null;

    try
    {
      // stream is closed in finally
      inputStream = url.openStream();

      final Unmarshaller u = getUnmarshaller();

      obs = (Observation)u.unmarshal( new InputSource( inputStream ) );
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      throw new SensorException( "Error while unmarshalling: " + url.toExternalForm(), e );
    }
    finally
    {
      if( inputStream != null )
        try
        {
          inputStream.close();
        }
        catch( IOException e1 )
        {
          e1.printStackTrace();
        }
    }

    // metadata
    final MetadataList metadata = new MetadataList();
    metadata.put( MetadataList.MD_NAME, obs.getName() );

    if( obs.getMetadataList() != null )
    {
      final List mdList = obs.getMetadataList().getMetadata();

      for( final Iterator it = mdList.iterator(); it.hasNext(); )
      {
        final MetadataType md = (MetadataType)it.next();

        metadata.put( md.getName(), md.getValue() );
      }
    }

    // axes and values
    final List tmpList = obs.getAxis();
    final Map valuesMap = new HashMap( tmpList.size() );

    for( int i = 0; i < tmpList.size(); i++ )
    {
      final AxisType tmpAxis = (AxisType)tmpList.get( i );

      final Properties props = PropertiesHelper.parseFromString( tmpAxis.getDatatype(), '#' );
      final String type = props.getProperty( "TYPE" );
      final String format = props.getProperty( "FORMAT" );

      final IParser parser;
      final IZmlValues values;
      try
      {
        parser = getParserFactory().createParser( type, format );

        values = createValues( url, tmpAxis, parser );
      }
      catch( Exception e ) // generic exception caught for simplicity
      {
        throw new SensorException( e );
      }

      final IAxis axis = new DefaultAxis( tmpAxis.getName(), tmpAxis.getType(), tmpAxis.getUnit(),
          parser.getObjectClass(), i, tmpAxis.isKey() );

      valuesMap.put( axis, values );
    }

    final IAxis[] axes = (IAxis[])valuesMap.keySet().toArray( new IAxis[valuesMap.size()] );

    final ZmlTuppleModel model = new ZmlTuppleModel( axes, valuesMap );
    
    final IObservation zmlObs = new SimpleObservation( identifier, obs.getName(), obs.isEditable(),
        new JAXBXLink( obs.getTarget() ), metadata, axes, model );
    
    return zmlObs;
  }

  /**
   * Parses the values and create the corresponding objects.
   * 
   * @throws ParserException
   * @throws MalformedURLException
   * @throws IOException
   */
  private static IZmlValues createValues( final URL context, final AxisType axisType,
      final IParser parser ) throws ParserException, MalformedURLException, IOException
  {
    final ValueArrayType va = axisType.getValueArray();
    if( va != null )
      return new ZmlArrayValues( va, parser );

    // loader for linked values, here we specify where base location is
    final ValueLinkType vl = axisType.getValueLink();
    if( vl != null )
      return new ZmlLinkValues( vl, parser, context );

    throw new IllegalArgumentException( "AxisType is not supported: " + axisType.toString() );
  }

  /**
   * Creates an XML-Observation ready for marshalling.
   * 
   * TODO: complete for target property, etc..
   * 
   * @throws FactoryException
   */
  public static ObservationType createXML( final IObservation obs, final IVariableArguments args )
      throws FactoryException
  {
    try
    {
      final ObservationType obsType = m_objectFactory.createObservation();
      obsType.setName( obs.getName() );
      obsType.setEditable( obs.isEditable() );

      final MetadataListType metadataListType = m_objectFactory.createMetadataListType();
      obsType.setMetadataList( metadataListType );
      final List metadataList = metadataListType.getMetadata();
      for( final Iterator it = obs.getMetadataList().entrySet().iterator(); it.hasNext(); )
      {
        final Map.Entry entry = (Entry)it.next();

        final String mdKey = (String)entry.getKey(); //(String)it.next();
        final String mdValue = (String)entry.getValue(); //obs.getMetadata().getProperty(
        // mdKey );

        final MetadataType mdType = m_objectFactory.createMetadataType();
        mdType.setName( mdKey );
        mdType.setValue( mdValue );
        metadataList.add( mdType );
      }

      final ITuppleModel values = obs.getValues( args );

      final List axisList = obsType.getAxis();
      final IAxis[] axes = obs.getAxisList();
      for( int i = 0; i < axes.length; i++ )
      {
        final AxisType axisType = m_objectFactory.createAxisType();

        final String xsdType = getXSDTypeFor( axes[i].getDataClass().getName() );

        axisType.setDatatype( xsdType );
        axisType.setName( axes[i].getLabel() );
        axisType.setUnit( axes[i].getUnit() );
        axisType.setType( "value" );

        final ValueArrayType valueArrayType = m_objectFactory.createAxisTypeValueArrayType();

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
   */
  private static String buildValueString( final ITuppleModel model, final IAxis axis )
      throws SensorException
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

  private static void buildStringAxis( ITuppleModel model, IAxis axis, StringBuffer sb )
      throws SensorException
  {
    final int amount = model.getCount() - 1;
    for( int i = 0; i < amount; i++ )
      sb.append( model.getElement( i, axis ) ).append( ";" );

    if( amount > 0 )
      sb.append( model.getElement( amount, axis ) );
  }

  private static void buildStringDateAxis( final ITuppleModel model, final IAxis axis,
      final StringBuffer sb ) throws SensorException
  {
    final int amount = model.getCount() - 1;
    for( int i = 0; i < amount; i++ )
      sb.append( m_df.format( model.getElement( i, axis ) ) ).append( ";" );

    if( amount > 0 )
      sb.append( m_df.format( model.getElement( amount, axis ) ) );
  }

  private static void buildStringNumberAxis( final ITuppleModel model, final IAxis axis,
      final StringBuffer sb ) throws SensorException
  {
    final int amount = model.getCount() - 1;
    for( int i = 0; i < amount; i++ )
      sb.append( m_nf.format( model.getElement( i, axis ) ) ).append( ";" );

    if( amount > 0 )
      sb.append( m_nf.format( model.getElement( amount, axis ) ) );
  }

  public static Marshaller getMarshaller() throws JAXBException
  {
    if( m_marsh == null )
      m_marsh = m_objectFactory.createMarshaller();

    return m_marsh;
  }

  private static Unmarshaller getUnmarshaller() throws JAXBException
  {
    if( m_unmarsh == null )
      m_unmarsh = m_objectFactory.createUnmarshaller();

    return m_unmarsh;
  }
}