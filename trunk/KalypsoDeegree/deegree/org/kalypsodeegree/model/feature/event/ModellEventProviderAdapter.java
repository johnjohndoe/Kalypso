package org.kalypsodeegree.model.feature.event;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

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
    synchronized( myListeners )
    {
      myListeners.add( listener );
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#removeModellListener(org.kalypsodeegree.model.feature.event.ModellEventListener)
   */
  public void removeModellListener( ModellEventListener listener )
  {
    synchronized( myListeners )
    {
      myListeners.remove( listener );
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#fireModellEvent(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void fireModellEvent( ModellEvent event )
  {
    synchronized( myListeners )
    {
      for( final Iterator iter = myListeners.iterator(); iter.hasNext(); )
        ( (ModellEventListener)iter.next() ).onModellChange( event );
    }
  }
}