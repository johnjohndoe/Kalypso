package org.kalypso.repository.beans;

import java.io.Serializable;

/**
 * A Repository Item Bean: an element of a repository.
 * 
 * @author schlienger
 */
public class ItemBean implements Serializable
{
  private String m_id;

  private String m_name;

  private String m_repId;

  public ItemBean()
  {
    this( "", "", "" );
  }

  /**
   * @param id
   *          identifier of this item
   * @param name
   *          name of this item
   * @param repId
   *          identifier of the repository this item belongs to
   */
  public ItemBean( final String id, final String name, final String repId )
  {
    m_id = id;
    m_name = name;
    m_repId = repId;
  }

  public String getId()
  {
    return m_id;
  }

  public void setId( final String id )
  {
    m_id = id;
  }

  public String getName()
  {
    return m_name;
  }

  public void setName( final String name )
  {
    m_name = name;
  }

  public String getRepId()
  {
    return m_repId;
  }

  public void setRepId( String repId )
  {
    m_repId = repId;
  }
  
  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  public boolean equals( Object obj )
  {
    if( obj == null || !(obj instanceof ItemBean) )
      return false;
    
    final ItemBean other = (ItemBean)obj;
    
    return m_id.equals( other.getId() );
  }
}