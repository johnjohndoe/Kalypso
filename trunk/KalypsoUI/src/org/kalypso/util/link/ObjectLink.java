package org.kalypso.util.link;

import org.kalypso.util.xml.xlink.IXlink;

/**
 * @author schlienger
 */
public class ObjectLink
{
  public final static Object UNRESOLVED = new Object();
  
  private final String m_linkType;

  private final IXlink m_xlink;

  private Object m_linkedObject = UNRESOLVED;

  public ObjectLink( final String linkType, final IXlink xlink )
  {
    m_linkType = linkType;
    m_xlink = xlink;
  }

  public String getLinkType()
  {
    return m_linkType;
  }

  public IXlink getXlink()
  {
    return m_xlink;
  }
  
  public void linkResolved( final Object object )
  {
    m_linkedObject = object;
  }
  
  public boolean isResolved()
  {
    return m_linkedObject != UNRESOLVED;
  }
  
  public Object getLinkedObject()
  {
    if( !isResolved() )
      throw new IllegalStateException("Link not resolved yet");
    
    return m_linkedObject;
  }
}