package org.kalypso.ogc.sensor.zml.values;

import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Hashtable;
import java.util.Map;
import java.util.Properties;

import org.kalypso.java.util.PropertiesHelper;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.ZmlAxis;
import org.kalypso.util.io.CSV;
import org.kalypso.util.parser.ParserException;
import org.kalypso.zml.AxisType;
import org.kalypso.zml.AxisType.ValueLinkType;

/**
 * ValueLink where values are outside of the Zml-File. The syntax of the href
 * should be as follows:
 * <p>
 * <pre>
 * href="TYPE=...#LOCATION=...#COLUMN=...#LINE=..."
 * </pre>
 * TYPE: type of the path: relative (to the zml file) or absolute
 * LOCATIOM: the path
 * COLUMN: the column number into which the values for the axis can be found
 * LINE: [optional, default 1] the line number to begin at
 * 
 * @author schlienger
 */
public class ValueLink implements IZmlValuesLoader, IZmlValues
{
  private final ValueLinkType m_valueLink;
  private final ZmlAxis m_axis;

  private ZmlTuppleModel m_model;

  private CSV m_csv = null;
  private Map m_helper = new Hashtable();
  
  private final String m_type;
  private final int m_column;
  private final URL m_url;
  private final int m_line;

  /**
   * Constructor, parses the href from the valueLink.
   * 
   *@param context needed when link type is relative, it is then used as context for building the full url of the document storing the values
   * @throws MalformedURLException
   */
  public ValueLink( final URL context, final AxisType.ValueLinkType valueLink, final ZmlAxis axis ) throws MalformedURLException
  {
    m_valueLink = valueLink;
    m_axis = axis;
    
    Properties hrefProps = PropertiesHelper.parseFromString( valueLink.getHref(), '#' );
    
    m_type = hrefProps.getProperty( "TYPE" );
    m_column = Integer.valueOf( hrefProps.getProperty("COLUMN") ).intValue() - 1;
    
    final String sline = hrefProps.getProperty( "LINE" );
    m_line = sline == null ? 1 : Integer.valueOf(sline).intValue();
    
    // depending on path type, complement with currentPath
    if( m_type.equals( "relative" ) )
      m_url = new URL( context, hrefProps.getProperty( "LOCATION" ) );
    else
      m_url = new URL( hrefProps.getProperty( "LOCATION" ) );
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValuesLoader#load()
   */
  public IZmlValues load() throws SensorException
  {
    try
    {
      m_csv = (CSV)m_model.getPoolObject( m_url );
      if( m_csv == null )
      {
        m_csv = new CSV( new InputStreamReader( m_url.openStream() ), m_valueLink.getSeparator(), m_line );
        m_model.putPoolObject( m_url, m_csv );
      }

      return this;
    }
    catch( Exception e )
    {
      throw new SensorException( e );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValuesLoader#setModel(org.kalypso.ogc.sensor.zml.values.ZmlTuppleModel)
   */
  public void setModel( ZmlTuppleModel model )
  {
    m_model = model;
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValues#getCount()
   */
  public int getCount()
  {
    return m_csv.getLines();
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValues#getElement(int)
   */
  public Object getElement( int index )
  {
    try
    {
      // get item from csv file
      String item = m_csv.getItem( index, m_column );
      
      // parse item using axis parser
      Object obj = m_axis.getParser().parse( item );
      
      // tricky: store relation between element and index for future needs
      m_helper.put( obj, new Integer( index ) );
      
      return obj;
    }
    catch( ParserException e )
    {
      // TODO handling
      throw new RuntimeException( e );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValues#setElement(int,
   *      java.lang.Object)
   */
  public void setElement( int index, Object element )
  {
    try
    {
      // tricky: set it in our map-helper (Siehe this.indexOf() )
      m_helper.put( element, new Integer( index ) );
      
      // set it in CSV
      m_csv.setItem( index, m_column, m_axis.getParser().toString( element ) );
    }
    catch( ParserException e )
    {
      throw new RuntimeException( e );
    }
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
