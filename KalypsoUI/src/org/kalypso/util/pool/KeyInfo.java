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
package org.kalypso.util.pool;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.eclipse.core.runtime.jobs.MutexSchedulingRule;
import org.kalypso.loader.ILoader;
import org.kalypso.loader.ILoaderListener;
import org.kalypso.loader.LoaderException;
import org.kalypso.ui.KalypsoGisPlugin;

final class KeyInfo extends Job implements ILoaderListener
{
  protected final static Logger LOGGER = Logger.getLogger( KeyInfo.class.getName() );

  /**
   * für den BorrowObjectJob, damit alle Objekte des Pools nacheinander geladen
   * werden
   */
  private static final ISchedulingRule MUTEX = new MutexSchedulingRule();

  private final List m_listeners = Collections.synchronizedList( new LinkedList() );

  private Object m_object = null;

  private final ILoader m_loader;

  private final IPoolableObjectType m_key;

  public KeyInfo( final IPoolableObjectType key, final ILoader loader )
  {
    super( "Lade Resource: " + key.toString() );

    m_key = key;
    m_loader = loader;

    m_loader.addLoaderListener( this );

    setPriority( Job.LONG );

    // Jobs auf dem gleichen Pool müssen nacheinander laufen!
    setRule( MUTEX );
  }

  public void dispose()
  {
    m_listeners.clear();

    // TODO: this doesn't really kills the job
    // are there means to kill it?
    cancel();

    m_loader.removeLoaderListener( this );

    if( m_object != null )
      m_loader.release( m_object );
  }

  public void addListener( final IPoolListener l )
  {
    m_listeners.add( l );

    if( m_object != null )
      l.objectLoaded( m_key, m_object, Status.OK_STATUS );
    else if( getState() == Job.NONE )
      schedule();
  }

  public boolean removeListener( final IPoolListener l )
  {
    return m_listeners.remove( l );
  }

  public Object getObject()
  {
    return m_object;
  }

  /**
   * @see org.kalypso.loader.ILoaderListener#onLoaderObjectInvalid(java.lang.Object,
   *      boolean)
   */
  public void onLoaderObjectInvalid( final Object object, final boolean bCannotReload )
      throws Exception
  {
    if( m_object == object )
    {
      LOGGER.info( "Object invalid for key: " + m_key );
      
      m_object = null;

      final IPoolListener[] ls = (IPoolListener[])m_listeners
          .toArray( new IPoolListener[m_listeners.size()] );
      // TRICKY: objectInvalid may add/remove PoolListener for this key,
      // so we cannot iterate over m_listeners
      for( int i = 0; i < ls.length; i++ )
        ls[i].objectInvalid( m_key, object );

      if( !bCannotReload && getState() == Job.NONE )
        schedule();
    }
  }

  /**
   * @see org.eclipse.core.internal.jobs.InternalJob#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  protected IStatus run( final IProgressMonitor monitor )
  {
    final IStatus status = loadObject( monitor );

      final IPoolListener[] ls = (IPoolListener[])m_listeners
          .toArray( new IPoolListener[m_listeners.size()] );
      // TRICKY: objectLoaded may add a new PoolListener for this key,
      // so we cannot iterate over m_listeners
      for( int i = 0; i < ls.length; i++ )
        ls[i].objectLoaded( m_key, m_object, status );

    return status;
  }

  /**
   * @see org.eclipse.core.internal.jobs.InternalJob#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  protected IStatus loadObject( final IProgressMonitor monitor )
  {
    try
    {
      LOGGER.info( "Loading object for key: " + m_key );
      m_object = m_loader.load( m_key.getLocation(), m_key.getContext(), monitor );
    }
    catch( final Throwable e )
    {
      if( m_key.isIgnoreExceptions() )
        return Status.CANCEL_STATUS;

      return new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0,
          "Fehler beim Laden einer Resource", e );
    }

    return Status.OK_STATUS;
  }

  public boolean isEmpty()
  {
      return m_listeners.isEmpty();
  }

  public void saveObject( final IProgressMonitor monitor ) throws LoaderException
  {
    m_loader.save( m_key.getLocation(), m_key.getContext(), monitor, m_object );
  }

  public String toString()
  {
    StringBuffer b=new StringBuffer();
    b.append("KeyInfo:\n");
    if(m_object!=null)
    b.append("  pooled object (type): "+m_object.getClass().getName()+"\n");
    else
      b.append(" !!!!!!!!!!!!!!!!!!!! WARNING !!!!!!!!!!!!!!!!!!!!\n pooled object (type): NULL \n");
    b.append("  loader (type): "+m_loader.getClass().getName()+"\n");
    b.append("  key: "+m_key+"\n");
    b.append("  number of listeners: "+m_listeners.size()+"\n");
    return b.toString();
  }
}