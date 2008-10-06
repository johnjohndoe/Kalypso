package org.kalypsodeegree.model.feature.event;

import java.util.ArrayList;
import java.util.Collection;

/**
 * @author Belger
 */
public class ModellEventProviderAdapter implements ModellEventProvider
{
  private final Collection<ModellEventListener> m_listeners = new ArrayList<ModellEventListener>();

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#dispose()
   */
  public void dispose( )
  {
    m_listeners.clear();
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#addModellListener(org.kalypsodeegree.model.feature.event.ModellEventListener)
   */
  public void addModellListener( final ModellEventListener listener )
  {
    m_listeners.add( listener );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#removeModellListener(org.kalypsodeegree.model.feature.event.ModellEventListener)
   */
  public void removeModellListener( ModellEventListener listener )
  {
    m_listeners.remove( listener );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#fireModellEvent(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void fireModellEvent( ModellEvent event )
  {
    final ModellEventListener[] listeners = m_listeners.toArray( new ModellEventListener[m_listeners.size()] );
    for( int i = 0; i < listeners.length; i++ )
      listeners[i].onModellChange( event );
  }
}