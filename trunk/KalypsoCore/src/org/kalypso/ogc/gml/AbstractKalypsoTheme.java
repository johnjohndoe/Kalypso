package org.kalypso.ogc.gml;

import org.kalypso.ogc.gml.event.ModellEvent;
import org.kalypso.ogc.gml.event.ModellEventListener;
import org.kalypso.ogc.gml.event.ModellEventProviderAdapter;

/**
 * <p>Abstract implementation of IKalypsoTheme</p>
 * <p>Implements common features to all KalypsoTheme's</p>
 * 
 * @author Belger
 */
public abstract class AbstractKalypsoTheme implements IKalypsoTheme
{
  private final ModellEventProviderAdapter m_eventProvider = new ModellEventProviderAdapter();

  private String m_name;
  
  public AbstractKalypsoTheme( final String name )
  {
    m_name = name;
  }

  public String getName()
  {
    return m_name;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#setName(java.lang.String)
   */
  public final void setName( final String name )
  {
    m_name = name;
    
    fireModellEvent( null );
  }

  /**
   * 
   * @see org.kalypso.ogc.gml.event.ModellEventListener#onModellChange(org.kalypso.ogc.event.ModellEvent)
   */
  public void onModellChange( ModellEvent modellEvent )
  {
    fireModellEvent( modellEvent );
  }

  public final void addModellListener( ModellEventListener listener )
  {
    m_eventProvider.addModellListener( listener );
  }

  public final void fireModellEvent( ModellEvent event )
  {
    m_eventProvider.fireModellEvent( event );
  }

  public final void removeModellListener( ModellEventListener listener )
  {
    m_eventProvider.removeModellListener( listener );
  }

  public String toString()
  {
    return m_name;
  }
}
