package org.kalypso.ogc.gml.event;

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
   * @see org.kalypso.ogc.gml.event.ModellEventProvider#addModellListener(org.kalypso.ogc.gml.event.ModellEventListener)
   */
  public void addModellListener( ModellEventListener listener )
  {
    myListeners.add( listener );
  }

  /**
   * @see org.kalypso.ogc.gml.event.ModellEventProvider#removeModellListener(org.kalypso.ogc.gml.event.ModellEventListener)
   */
  public void removeModellListener( ModellEventListener listener )
  {
    myListeners.remove( listener );
  }

  /**
   * @see org.kalypso.ogc.gml.event.ModellEventProvider#fireModellEvent(org.kalypso.ogc.gml.event.ModellEvent)
   */
  public void fireModellEvent( ModellEvent event )
  {
    for( final Iterator iter = myListeners.iterator(); iter.hasNext(); )
      ((ModellEventListener)iter.next()).onModellChange(event);
  }
}
