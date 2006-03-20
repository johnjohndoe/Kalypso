package org.kalypsodeegree.model.feature.event;

import java.util.ArrayList;
import java.util.Collection;

/**
 * @author Belger
 */
public class ModellEventProviderAdapter implements ModellEventProvider
{
  private final Collection myListeners = new ArrayList();

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#addModellListener(org.kalypsodeegree.model.feature.event.ModellEventListener)
   */
  public void addModellListener( ModellEventListener listener )
  {
    myListeners.add( listener );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#removeModellListener(org.kalypsodeegree.model.feature.event.ModellEventListener)
   */
  public void removeModellListener( ModellEventListener listener )
  {
    myListeners.remove( listener );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#fireModellEvent(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void fireModellEvent( ModellEvent event )
  {
    final ModellEventListener[] listeners = (ModellEventListener[])myListeners
        .toArray( new ModellEventListener[myListeners.size()] );
    for( int i = 0; i < listeners.length; i++ )
      listeners[i].onModellChange( event );
  }
}