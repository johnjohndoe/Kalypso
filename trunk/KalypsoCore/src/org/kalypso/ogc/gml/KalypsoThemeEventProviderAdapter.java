/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ogc.gml;

import java.util.ArrayList;
import java.util.Collection;

/**
 * Convenience class for adding theme listener support
 * 
 * @author Stefan Kurzbach
 */
public class KalypsoThemeEventProviderAdapter implements IKalypsoThemeEventProvider
{

  private final Collection<IKalypsoThemeListener> m_listeners = new ArrayList<IKalypsoThemeListener>();

  /**
   * @see org.kalypso.ogc.gml.IKalypsoThemeEventProvider#addKalypsoThemeListener(org.kalypso.ogc.gml.IKalypsoThemeListener)
   */
  public void addKalypsoThemeListener( final IKalypsoThemeListener listener )
  {
    m_listeners.add( listener );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoThemeEventProvider#removeKalypsoThemeListener(org.kalypso.ogc.gml.IKalypsoThemeListener)
   */
  public void removeKalypsoThemeListener( final IKalypsoThemeListener listener )
  {
    m_listeners.remove( listener );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoThemeEventProvider#fireKalypsoThemeEvent(org.kalypso.ogc.gml.KalypsoThemeEvent)
   */
  public void fireKalypsoThemeEvent( final KalypsoThemeEvent event )
  {
    final IKalypsoThemeListener[] listeners = m_listeners.toArray( new IKalypsoThemeListener[m_listeners.size()] );
    for( int i = 0; i < listeners.length; i++ )
      listeners[i].kalypsoThemeChanged( event );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoThemeEventProvider#dispose()
   */
  public void dispose( )
  {
    m_listeners.clear();
  }
}
