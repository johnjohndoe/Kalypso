package org.kalypso.repository.conf;

import java.util.List;

/**
 * The holder for a set of configuration items
 * 
 * @author schlienger
 */
public class RepositoryConfig
{
  private List m_items;

  public RepositoryConfig( final List items )
  {
    m_items = items;
  }

  public List getItems()
  {
    return m_items;
  }
}