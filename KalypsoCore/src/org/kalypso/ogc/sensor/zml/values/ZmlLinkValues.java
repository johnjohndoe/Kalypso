package org.kalypso.ogc.sensor.zml.values;

import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Hashtable;
import java.util.Map;
import java.util.Properties;

import org.kalypso.java.util.PropertiesHelper;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.util.io.CSV;
import org.kalypso.util.parser.IParser;
import org.kalypso.util.parser.ParserException;
import org.kalypso.zml.AxisType.ValueLinkType;

/**
 * @author schlienger
 */
public class ZmlLinkValues implements IZmlValues
{
  private final CSV m_csv;

  private final IParser m_parser;

  private Map m_helper = new Hashtable();

  private final int m_column;

  public ZmlLinkValues( final ValueLinkType vl, final IParser parser, final URL context )
      throws MalformedURLException, IOException
  {
    m_parser = parser;

    final Properties hrefProps = PropertiesHelper.parseFromString( vl.getHref(), '#' );

    final String type = hrefProps.getProperty( "TYPE" );
    m_column = Integer.valueOf( hrefProps.getProperty( "COLUMN" ) ).intValue() - 1;

    final String sline = hrefProps.getProperty( "LINE" );
    final int line = sline == null ? 1 : Integer.valueOf( sline ).intValue();

    URL url = null;

    // depending on path type, complement with currentPath
    if( type.equals( "relative" ) )
      url = new URL( context, hrefProps.getProperty( "LOCATION" ) );
    else
      url = new URL( hrefProps.getProperty( "LOCATION" ) );

    // stream is closed in CSV()
    m_csv = new CSV( new InputStreamReader( url.openStream() ), vl.getSeparator(), line );
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValues#getElement(int)
   */
  public Object getElement( int index ) throws SensorException
  {
    try
    {
      // get item from csv file
      final String item = m_csv.getItem( index, m_column );

      // parse item using axis parser
      final Object obj = m_parser.parse( item );

      // tricky: store relation between element and index for future needs
      m_helper.put( obj, new Integer( index ) );

      return obj;
    }
    catch( ParserException e )
    {
      throw new SensorException( e );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValues#setElement(int,
   *      java.lang.Object)
   */
  public void setElement( int index, Object element ) throws SensorException
  {
    // tricky: set it in our map-helper (Siehe this.indexOf() )
    m_helper.put( element, new Integer( index ) );

    try
    {
      // set it in CSV
      m_csv.setItem( index, m_column, m_parser.toString( element ) );
    }
    catch( ParserException e )
    {
      throw new SensorException( e );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValues#getCount()
   */
  public int getCount()
  {
    return m_csv.getLines();
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValues#indexOf(java.lang.Object)
   */
  public int indexOf( final Object obj )
  {
    Integer iobj = (Integer)m_helper.get( obj );
    if( iobj == null )
      return -1;

    return iobj.intValue();
  }
}