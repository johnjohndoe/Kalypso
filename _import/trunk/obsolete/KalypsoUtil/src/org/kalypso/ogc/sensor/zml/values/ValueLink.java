package org.kalypso.ogc.sensor.zml.values;

import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.ParseException;
import java.util.Properties;

import org.kalypso.java.properties.PropertiesHelper;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.ZmlAxis;
import org.kalypso.util.factory.ValueObjectFactory;
import org.kalypso.util.io.CSV;
import org.kalypso.util.xml.xlink.IXlink;
import org.kalypso.util.xml.xlink.JaxbXlink;
import org.kalypso.util.xml.xlink.resolver.IResolver;
import org.kalypso.zml.AxisType;
import org.kalypso.zml.AxisType.ValueLinkType;

/**
 * ValueLink where values are outside of the Zml-File.
 * 
 * @author schlienger
 */
public class ValueLink extends JaxbXlink implements IZmlValuesLoader, IZmlValuesProvider, IResolver
{
  private final ValueLinkType m_valueLink;

  private final ZmlAxis m_axis;

  private ZmlTuppleModel m_model;

  private CSV m_csv = null;

  private String m_path = null;
  private int m_column = 0;

  public ValueLink( final AxisType.ValueLinkType valueLink, final ZmlAxis axis )
  {
    super( valueLink );
    
    m_valueLink = valueLink;
    m_axis = axis;
    
    resolve( this );
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValuesLoader#load()
   */
  public IZmlValuesProvider load() throws SensorException
  {
    try
    {
      m_csv = (CSV)m_model.getPoolObject( m_path );
      if( m_csv == null )
      {
        m_csv = new CSV( new InputStreamReader( toUrl().openStream() ), m_valueLink
            .getSeparator() );
        m_model.putPoolObject( m_path, m_csv );
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
      return m_csv.getItem( index, m_column, m_axis.getDataClass() );
    }
    catch( ParseException e )
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
    m_csv.setItem( index, m_column, ValueObjectFactory.toStringRepresentation( element ) );
  }
  
  /**
   * @see org.kalypso.util.xml.xlink.JaxbXlink#toUrl()
   */
  public URL toUrl() throws MalformedURLException
  {
    return new URL( m_path );
  }

  /**
   * @see org.kalypso.util.xml.xlink.resolver.IResolver#resolve(org.kalypso.util.xml.xlink.IXlink)
   */
  public Object resolve( IXlink link )
  {
    Properties hrefProps = PropertiesHelper.parseFromString( link.getHRef(), '#' );
    
    m_path = hrefProps.getProperty( "PATH" );
    m_column = Integer.valueOf( hrefProps.getProperty("COLUMN") ).intValue();
    
    return this;
  }
}
