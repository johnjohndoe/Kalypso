package org.kalypso.ogc.sensor.zml.values;

import java.io.File;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.util.Hashtable;
import java.util.Map;
import java.util.Properties;

import org.kalypso.java.properties.PropertiesHelper;
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
 * href="TYPE=...#LOCATION=...#COLUMN=..."
 * </pre>
 * TYPE: type of the path: relative (to the eclipse-project) or absolute
 * LOCATIOM: the path
 * COLUMN: the column number into which the values for the axis can be found
 * 
 * @author schlienger
 */
public class ValueLink implements IZmlValuesLoader, IZmlValuesProvider
{
  private final ValueLinkType m_valueLink;
  private final ZmlAxis m_axis;

  private ZmlTuppleModel m_model;

  private CSV m_csv = null;
  private Map m_helper = new Hashtable();
  
  private final String m_type;
  private final String m_location;
  private final int m_column;

  /**
   * Constructor, parses the href from the valueLink.
   */
  public ValueLink( final String currentPath, final AxisType.ValueLinkType valueLink, final ZmlAxis axis )
  {
    m_valueLink = valueLink;
    m_axis = axis;
    
    Properties hrefProps = PropertiesHelper.parseFromString( valueLink.getHref(), '#' );
    
    m_type = hrefProps.getProperty( "TYPE" );
    m_column = Integer.valueOf( hrefProps.getProperty("COLUMN") ).intValue() - 1;
    
    String loc = hrefProps.getProperty( "LOCATION" );
    
    // depending on path type, complement with currentPath
    if( m_type.equals( "relative" ) )
      m_location = currentPath + File.separatorChar + loc;
    else
      m_location = loc;
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValuesLoader#load()
   */
  public IZmlValuesProvider load() throws SensorException
  {
    try
    {
      m_csv = (CSV)m_model.getPoolObject( m_location );
      if( m_csv == null )
      {
        Reader r = null;
        
        if( m_type.equals( "relative" ) )
          r = new FileReader( m_location );
        else
          r = new InputStreamReader( new URL( m_location ).openStream() );
        
        m_csv = new CSV( r, m_valueLink.getSeparator() );
        m_model.putPoolObject( m_location, m_csv );
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
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValuesProvider#getCount()
   */
  public int getCount()
  {
    return m_csv.getLines();
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValuesProvider#getElement(int)
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
      throw new RuntimeException( e );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValuesProvider#setElement(int,
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
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValuesProvider#indexOf(java.lang.Object)
   */
  public int indexOf( final Object obj )
  {
    return ((Integer)m_helper.get( obj )).intValue();
  }
}
