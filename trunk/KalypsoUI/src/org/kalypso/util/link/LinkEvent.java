package org.kalypso.util.link;

import java.util.EventObject;

/**
 * @author schlienger
 */
public class LinkEvent extends EventObject
{
  private final ObjectLink m_link;

  public LinkEvent( final Object src, final ObjectLink link )
  {
    super( src );

    m_link = link;
  }

  public ObjectLink getLink()
  {
    return m_link;
  }
}