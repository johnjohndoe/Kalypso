package org.kalypso.util.observer;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;

/**
 * <p>Implementation des Publisher/Observer Patterns, um Objekte zu 
 * überwachen, die selbst kein Publisher sind.</p> 
 * 
 * <p>Das zu überwachende Objekt und die Listener werden bei
 * der Registry angemeldet. Ändert jemand den Zustand des zu 
 * überwachenden Objektes, muss die entsprechende fire-Methode
 * aufgerufen werden. Die OberserverRegistry informiert dann
 * alle angemeldeten listener.</p>
 * 
 * @author belger
 */
public final class ObserverRegistry
{
  private HashMap m_observerHash = new LinkedHashMap();

  private List getObserverList( final Object publisher )
  {
    final List obsList = (List)m_observerHash.get( publisher );
    if( obsList != null )
      return obsList;
    
     final List newList = new ArrayList();
      m_observerHash.put( publisher, newList );
      return newList;
  }

  public void addListener( final Object publisher, final IObserver l )
  {
    getObserverList( publisher ).add( l );
  }
  
  public void removeListener( final Object publisher, final IObserver l )
  {
    getObserverList( publisher ).remove( l );
  }
  
  public void firePublisherChanged( final Object publisher, final Object hint )
  {
    final List obsList = getObserverList(publisher);
    
    for( final Iterator oIt = obsList.iterator(); oIt.hasNext(); )
      ((IObserver)oIt.next()).onPublisherChanged( publisher, hint );
  }
}
