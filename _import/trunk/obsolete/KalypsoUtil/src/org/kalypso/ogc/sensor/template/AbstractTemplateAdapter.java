package org.kalypso.ogc.sensor.template;

import java.util.Iterator;
import java.util.List;
import java.util.Vector;

/**
 * An abstract implementation of the ITemplateAdapter that takes care of the
 * listener handling. Clients can extend this class.
 * 
 * @author schlienger
 */
public abstract class AbstractTemplateAdapter implements ITemplateAdapter
{
  private final List m_listeners = new Vector();

  /**
   * @see org.kalypso.ogc.sensor.template.ITemplateAdapter#addListener(org.kalypso.ogc.sensor.template.ITemplateListener)
   */
  public void addListener( ITemplateListener l )
  {
    m_listeners.add( l );
  }

  /**
   * @see org.kalypso.ogc.sensor.template.ITemplateAdapter#removeListener(org.kalypso.ogc.sensor.template.ITemplateListener)
   */
  public void removeListener( ITemplateListener l )
  {
    m_listeners.remove( l );
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTemplate#fireTemplateLoaded()
   */
  public void fireTemplateLoaded()
  {
    for( Iterator it = m_listeners.iterator(); it.hasNext(); )
    {
      ITemplateListener l = (ITemplateListener)it.next();

      l.onTemplateLoaded();
    }
  }
}