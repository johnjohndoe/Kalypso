package org.kalypso.ogc.sensor.zml;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.zml.values.IZmlValuesLoader;
import org.kalypso.ogc.sensor.zml.values.ValueArray;
import org.kalypso.ogc.sensor.zml.values.ValueLink;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.util.parser.ParserFactory;
import org.kalypso.zml.AxisType;
import org.kalypso.zml.MetadataListType;
import org.kalypso.zml.MetadataType;
import org.kalypso.zml.ObjectFactory;
import org.kalypso.zml.ObservationType;
import org.kalypso.zml.AxisType.ValueArrayType;

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
        m_props.load( ZmlAxis.class.getResourceAsStream( "resource/types2parser.properties" ) );

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
  public static ParserFactory getParserFactory()
  {
    if( m_parserFactory == null )
      m_parserFactory = new ParserFactory( getProperties(), ZmlAxis.class.getClassLoader() );

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
   * ValueFactory um die entsprechende ValuesLoader zu erzeugen.
   */
  public static IZmlValuesLoader createLoader( final URL baseUrl, final AxisType axisType,
      final ZmlAxis axis ) throws MalformedURLException
  {
    // loader for inline values, no need to specify where base location is
    Object va = axisType.getValueArray();
    if( va != null )
      return new ValueArray( (AxisType.ValueArrayType)va, axis );

    // loader for linked values, here we specify where base location is
    Object vl = axisType.getValueLink();
    if( vl != null )
      return new ValueLink( baseUrl, (AxisType.ValueLinkType)vl, axis );

    throw new IllegalArgumentException( "AxisType is not supported: " + axisType.toString() );
  }

  /**
   * Creates an XML-Observation ready for marshalling.
   * 
   * TODO: complete for target property, etc..
   * 
   * @throws FactoryException
   */
  public static ObservationType createXML( final IObservation obs ) throws FactoryException
  {
    try
    {
      final ObservationType obsType = m_objectFactory.createObservation();
      obsType.setName( obs.getName() );
      obsType.setEditable( obs.isEditable() );

      final MetadataListType metadataListType = m_objectFactory.createMetadataListType();
      obsType.setMetadataList(metadataListType);
      final List metadataList = metadataListType.getMetadata();
      for( final Iterator it = obs.getMetadata().entrySet().iterator(); it.hasNext(); )
      {
        final Map.Entry entry = (Entry)it.next();
        
        final String mdKey = (String)entry.getKey();  //(String)it.next();
        final String mdValue = (String)entry.getValue(); //obs.getMetadata().getProperty( mdKey );

        final MetadataType mdType = m_objectFactory.createMetadataType();
        mdType.setName( mdKey );
        mdType.setValue( mdValue );
        metadataList.add( mdType );
      }

      final ITuppleModel values = obs.getValues( null );

      final List axisList = obsType.getAxis();
      final IAxis[] axes = obs.getAxisList();
      for( int i = 0; i < axes.length; i++ )
      {
        final AxisType axisType = m_objectFactory.createAxisType();

        final String xsdType = getXSDTypeFor( axes[i].getDataClass().getName() );

        axisType.setDatatype( xsdType );
        axisType.setName( axes[i].getLabel() );
        axisType.setUnit( axes[i].getUnit() );

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
  {
    StringBuffer sb = new StringBuffer();

    if( java.util.Date.class.isAssignableFrom( axis.getDataClass() ) )
      buildStringDateAxis( model, axis, sb );
    else if( Number.class.isAssignableFrom( axis.getDataClass() ) )
      buildStringNumberAxis( model, axis, sb );
    else
      throw new IllegalArgumentException( "data type currently not supported" );

    return sb.toString();
  }

  private static void buildStringDateAxis( final ITuppleModel model, final IAxis axis,
      final StringBuffer sb )
  {
    final int amount = model.getCount() - 1;
    for( int i = 0; i < amount; i++ )
      sb.append( m_df.format( model.getElement( i, axis.getPosition() ) ) ).append( ";" );

    sb.append( m_df.format( model.getElement( amount, axis.getPosition() ) ) );
  }

  private static void buildStringNumberAxis( final ITuppleModel model, final IAxis axis,
      final StringBuffer sb )
  {
    final int amount = model.getCount() - 1;
    for( int i = 0; i < amount; i++ )
      sb.append( m_nf.format( model.getElement( i, axis.getPosition() ) ) ).append( ";" );

    sb.append( m_nf.format( model.getElement( amount, axis.getPosition() ) ) );
  }
  
  public static Marshaller getMarshaller() throws JAXBException
  {
    return m_objectFactory.createMarshaller();
  }
}