package org.kalypso.ogc.sensor.zml;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Properties;

import org.kalypso.java.properties.PropertiesHelper;
import org.kalypso.ogc.sensor.DefaultAxis;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.values.IZmlValuesLoader;
import org.kalypso.ogc.sensor.zml.values.IZmlValues;
import org.kalypso.ogc.sensor.zml.values.ZmlTuppleModel;
import org.kalypso.ogc.sensor.zml.values.ZmlValueFactory;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.util.parser.IParser;
import org.kalypso.util.parser.ParserFactory;
import org.kalypso.zml.AxisType;

/**
 * Wrapper über der Zml-AxisType
 * 
 * @author schlienger
 */
public class ZmlAxis extends DefaultAxis
{
  private static ParserFactory m_parserFactory = null;

  private final AxisType m_axisType;

  private IZmlValues m_values = null;

  private IParser m_parser;

  private String m_type;

  private String m_format;

  /**
   * Constructor
   * 
   * @param axisType
   *          AxisType Instanz aus der JAXB Serialisierung
   * @param position
   *          die Position in der Tupple
   */
  public ZmlAxis( final AxisType axisType, final int position ) throws SensorException
  {
    super( axisType.getName(), axisType.getUnit(), null, false, position );

    m_axisType = axisType;

    // datatype beinhaltet TYPE=...#FORMAT=...
    Properties props = PropertiesHelper.parseFromString( m_axisType.getDatatype(), '#' );
    m_type = props.getProperty( "TYPE" );
    m_format = props.getProperty( "FORMAT" );

    try
    {
      m_parser = getParserFactory().createParser( m_type, m_format );
    }
    catch( FactoryException e )
    {
      throw new SensorException( e );
    }

    m_dataClass = m_parser.getObjectClass();
  }

  public AxisType getAxisType()
  {
    return m_axisType;
  }

  /**
   * Helper that fetches the values. Uses a <code>IZmlValuesLoader</code> to
   * load the values either from the zml (inline) or from some external
   * resource.
   * 
   * @param context url of the ZmlObservation document, used when paths are relative
   * @param model the model
   */
  public void fetchValues( final URL context, final ZmlTuppleModel model ) throws SensorException
  {
    IZmlValuesLoader loader;
    try
    {
      loader = ZmlValueFactory.createLoader( context, m_axisType, this );
    }
    catch( MalformedURLException e )
    {
      throw new SensorException( e );
    }
    
    loader.setModel( model );
    m_values = loader.load();
  }

  public IZmlValues getValues()
  {
    return m_values;
  }

  /**
   * Liefert den passenden IParser um die Werte der Achse zu parsen
   * (read/write).
   * <p>
   * Die Achsen müssen im XML mit der 'datatype' Tag beschrieben werden. Dieser
   * Tag beinhaltet die Typ und Format Spezifikation.
   * <p>
   * Beispiel:
   * 
   * <pre>
   *  <Axis name="Pegel" unit="m" datatype="TYPE=xs:double#FORMAT=">
   *  <Axis name="Datum" unit="" datatype="TYPE=xs:date#FORMAT=yyyy MM dd hh:mm">
   * </pre>
   */
  public IParser getParser()
  {
    return m_parser;
  }

  /**
   * Helper, man sollte es benutzen um auf die ParserFactory zugreifen zu können
   */
  private static ParserFactory getParserFactory()
  {
    if( m_parserFactory == null )
    {
      Properties props = new Properties();
      try
      {
        props.load( ZmlAxis.class.getResourceAsStream( "resource/types2parser.properties" ) );
      }
      catch( IOException e )
      {
        // TODO: logging oder etwas?
        throw new RuntimeException( e );
      }

      m_parserFactory = new ParserFactory( props, ZmlAxis.class.getClassLoader() );
    }

    return m_parserFactory;
  }
}