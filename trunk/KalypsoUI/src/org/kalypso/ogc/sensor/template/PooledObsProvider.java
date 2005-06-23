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
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.runtime.IVariableArguments;
import org.kalypso.ogc.sensor.IObservation;
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
  private static final int STATUS_DISPOSED = 0;

  private static final int STATUS_LOADED = 1;

  private static final int STATUS_ERROR = 2;

  private static final int STATUS_LOADING = 3;

  private final List m_listeners = new ArrayList();

  private final ResourcePool m_pool = KalypsoGisPlugin.getDefault().getPool();

  private IObservation m_observation;

  private int m_loadedStatus;

  private final IPoolableObjectType m_key;

  private final IVariableArguments m_args;

  public PooledObsProvider( final IPoolableObjectType key, final IVariableArguments args )
  {
    m_args = args;
    m_loadedStatus = STATUS_LOADING;
    m_key = key;

    m_pool.addPoolListener( this, key );
  }

  public void dispose()
  {
    m_pool.removePoolListener( this );

    m_loadedStatus = STATUS_DISPOSED;
  }

  public IObservation getObservation()
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
    m_loadedStatus = STATUS_LOADING;

    if( key == m_key )
      setObservation( null );
  }

  /**
   * Set the loaded observation and inform listeners that this theme has changed
   * 
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object, org.eclipse.core.runtime.IStatus)
   */
  public final void objectLoaded( final IPoolableObjectType key, final Object newValue, final IStatus status )
  {
    m_loadedStatus = newValue == null ? STATUS_ERROR : STATUS_LOADED;

    setObservation( (IObservation)newValue );
  }

  private void setObservation( final IObservation obs )
  {
    if( m_observation != obs )
    {
      m_observation = obs;
      fireChanged();
    }
  }

  public void fireChanged()
  {
    for( final Iterator it = m_listeners.iterator(); it.hasNext(); )
      ( (IObsProviderListener)it.next() ).obsProviderChanged();
  }

  public boolean isLoading()
  {
    return m_loadedStatus == STATUS_LOADING;
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
  public IVariableArguments getArguments()
  {
    return m_args;
  }

  /**
   * @see org.kalypso.ogc.sensor.template.IObsProvider#copy()
   */
  public IObsProvider copy()
  {
    return new PooledObsProvider( m_key, m_args );
  }
}