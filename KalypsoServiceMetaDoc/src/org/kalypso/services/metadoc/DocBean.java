package org.kalypso.services.metadoc;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

/**
 * DocBean
 * 
 * @author schlienger
 */
public class DocBean implements Serializable
{
  private Map m_metadata;
  private String m_location;

  public DocBean( )
  {
    this( "" );
  }
  
  public DocBean( final String location )
  {
    this( new HashMap(), location );
  }
  
  public DocBean( final Map metadata, final String location )
  {
    m_metadata = metadata;
    m_location = location;
  }
  
  /**
   * @return Returns the metadata.
   */
  public Map getMetadata( )
  {
    return m_metadata;
  }
  
  /**
   * @param metadata The metadata to set.
   */
  public void setMetadata( final Map metadata )
  {
    m_metadata = metadata;
  }
  
  /**
   * @return Returns the location.
   */
  public String getLocation( )
  {
    return m_location;
  }
  
  /**
   * @param location The location to set.
   */
  public void setLocation( String location )
  {
    m_location = location;
  }
}
