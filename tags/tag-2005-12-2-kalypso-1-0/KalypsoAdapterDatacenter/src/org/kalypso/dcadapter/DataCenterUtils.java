package org.kalypso.dcadapter;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.apache.commons.io.IOUtils;

/**
 * DataCenterUtils
 * 
 * @author marc
 */
public class DataCenterUtils
{
  private static Properties m_props;

  private DataCenterUtils( )
  {
    // empty
  }
  
  private static Properties getProperties()
  {
    if( m_props == null )
    {
      m_props = new Properties();
      
      final InputStream stream = DataCenterUtils.class.getResourceAsStream( "resource/datacenter.ini" );
      
      try
      {
        m_props.load( stream );
      }
      catch( IOException e )
      {
        e.printStackTrace();
      }
      finally
      {
        IOUtils.closeQuietly( stream );
      }
    }
    
    return m_props;
  }

  public static String toKalypsoUnit( final String datacenterUnit )
  {
    return datacenterUnit;
  }
  
  public static String toKalypsoType( final String datacenterType )
  {
    final String key = "TYPE_" + datacenterType;
    
    return getProperties().getProperty( key, "W" );
  }
}
