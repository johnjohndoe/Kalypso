package org.kalypso.ogc.sensor.template;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * @author schlienger
 */
public abstract class AbstractTemplateEventProvider implements
    ITemplateEventProvider
{
  private final List m_listeners = new ArrayList();

  /**
   * @see org.kalypso.ogc.sensor.template.ITemplateEventProvider#fireTemplateChanged(org.kalypso.ogc.sensor.template.TemplateEvent)
   */
  public void fireTemplateChanged( TemplateEvent evt )
  {
    synchronized( m_listeners )
    {
      for( Iterator it = m_listeners.iterator(); it.hasNext(); )
      {
        ITemplateEventListener l = (ITemplateEventListener) it.next();

        l.onTemplateChanged( evt );
      }
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.template.ITemplateEventProvider#addTemplateEventListener(org.kalypso.ogc.sensor.template.ITemplateEventListener)
   */
  public void addTemplateEventListener( ITemplateEventListener l )
  {
    synchronized( m_listeners )
    {
      m_listeners.add( l );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.template.ITemplateEventProvider#removeTemplateEventListener(org.kalypso.ogc.sensor.template.ITemplateEventListener)
   */
  public void removeTemplateEventListener( ITemplateEventListener l )
  {
    synchronized( m_listeners )
    {
      m_listeners.remove( l );
    }
  }
}