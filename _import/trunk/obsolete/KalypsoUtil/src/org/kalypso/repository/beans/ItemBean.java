package org.kalypso.repository.beans;

import java.io.Serializable;

/**
 * A Repository Item Bean: an element of a repository.
 * 
 * @author schlienger
 */
public class ItemBean implements Serializable
{
  private int m_id;

  private String m_name;

  public ItemBean()
  {
    this( 0, "" );
  }

  public ItemBean( final int id, final String name )
  {
    m_id = id;
    m_name = name;
  }

  public int getId()
  {
    return m_id;
  }

  public void setId( int id )
  {
    m_id = id;
  }

  public String getName()
  {
    return m_name;
  }

  public void setName( String name )
  {
    m_name = name;
  }
}