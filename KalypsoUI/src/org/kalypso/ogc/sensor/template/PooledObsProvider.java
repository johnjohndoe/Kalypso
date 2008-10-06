/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.template;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.ResourcePool;

/**
 * A Theme for an IObservation
 * 
 * @author schlienger
 */
public final class PooledObsProvider implements IObsProvider, IPoolListener
{
  private boolean m_isDisposed = false;

  private final List<IObsProviderListener> m_listeners = new ArrayList<IObsProviderListener>();

  private final ResourcePool m_pool = KalypsoGisPlugin.getDefault().getPool();

  private IObservation m_observation;

  private final IPoolableObjectType m_key;

  private final IRequest m_args;

  public PooledObsProvider( final IPoolableObjectType key, final IRequest args )
  {
    m_args = args;
    m_key = key;

    m_pool.addPoolListener( this, key );
  }

  public void dispose( )
  {
    m_isDisposed = true;

    m_pool.removePoolListener( this );
  }

  public IObservation getObservation( )
  {
    return m_observation;
  }

  /**
   * Remove the observation and inform listeners that theme changed
   * 
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object)
   */
  public void objectInvalid( final IPoolableObjectType key, final Object oldValue )
  {
    if( key == m_key )
      setObservation( null );
  }

  /**
   * Set the loaded observation and inform listeners that this theme has changed
   * 
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object,
   *      org.eclipse.core.runtime.IStatus)
   */
  public final void objectLoaded( final IPoolableObjectType key, final Object newValue, final IStatus status )
  {
    if( !m_isDisposed )
      setObservation( (IObservation) newValue );
  }

  private void setObservation( final IObservation obs )
  {
    if( m_observation != obs )
    {
      m_observation = obs;
      fireChanged();
    }
  }

  public void fireChanged( )
  {
    synchronized( m_listeners )
    {
      final Object[] listeners = m_listeners.toArray();
      for( int i = 0; i < listeners.length; i++ )
        ((IObsProviderListener) listeners[i]).obsProviderChanged();
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.template.IObsProvider#addListener(org.kalypso.ogc.sensor.template.IObsProviderListener)
   */
  public void addListener( final IObsProviderListener l )
  {
    m_listeners.add( l );
  }

  /**
   * @see org.kalypso.ogc.sensor.template.IObsProvider#removeListener(org.kalypso.ogc.sensor.template.IObsProviderListener)
   */
  public void removeListener( final IObsProviderListener l )
  {
    m_listeners.remove( l );
  }

  /**
   * @see org.kalypso.ogc.sensor.template.IObsProvider#getArguments()
   */
  public IRequest getArguments( )
  {
    return m_args;
  }

  /**
   * @see org.kalypso.ogc.sensor.template.IObsProvider#copy()
   */
  public IObsProvider copy( )
  {
    return new PooledObsProvider( m_key, m_args );
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#isDisposed()
   */
  public boolean isDisposed( )
  {
    return m_isDisposed;
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#dirtyChanged(org.kalypso.util.pool.IPoolableObjectType, boolean)
   */
  public void dirtyChanged( IPoolableObjectType key, boolean isDirty )
  {
    // TODO Auto-generated method stub
  }
}