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

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.loader.ILoader;
import org.kalypso.loader.ILoaderListener;
import org.kalypso.loader.LoaderException;

public final class KeyInfo extends Job implements ILoaderListener
{
  protected final static Logger LOGGER = Logger.getLogger( KeyInfo.class.getName() );

  private final Collection<IPoolListener> m_listeners = Collections.synchronizedSet( new HashSet<IPoolListener>() );

  private Object m_object = null;

  private final ILoader m_loader;

  private final IPoolableObjectType m_key;

  /** Flag, indicating if the associated object needs saving. */
  private boolean m_isDirty = false;

  public KeyInfo( final IPoolableObjectType key, final ILoader loader, final ISchedulingRule rule )
  {
    super( "Lade Resource: " + key.toString() );

    m_key = key;
    m_loader = loader;

    m_loader.addLoaderListener( this );

    setPriority( Job.LONG );
    setRule( rule );
  }

  public void dispose( )
  {
    m_listeners.clear();

    // TODO: this doesn't really kills the job
    // are there means to kill it?
    cancel();

    m_loader.removeLoaderListener( this );

    synchronized( this )
    {
      if( m_object != null )
        m_loader.release( m_object );
    }
  }

  public void addListener( final IPoolListener l )
  {
    m_listeners.add( l );

    final Object o = m_object;
    if( o != null )
      l.objectLoaded( m_key, o, Status.OK_STATUS );
    else if( getState() == Job.NONE )
      schedule();
  }

  public boolean removeListener( final IPoolListener l )
  {
    return m_listeners.remove( l );
  }

  /**
   * @see org.kalypso.loader.ILoaderListener#onLoaderObjectInvalid(java.lang.Object, boolean)
   */
  public void onLoaderObjectInvalid( final Object object, final boolean bCannotReload ) throws Exception
  {
    Object oldObject = null;
    synchronized( this )
    {
      if( m_object == object )
      {
        LOGGER.info( "Object " + object + " invalid for key: " + m_key );

        m_loader.release( m_object );
        m_object = null;

        // nur Objekt invalidieren, wenn nicht neu geladen werden kann
        // ansonsten einfach neu laden, dann werden die listener ja auch
        // informiert
        if( bCannotReload )
          oldObject = object;
        else
        {
          if( getState() == Job.NONE )
            schedule();
        }
      }
    }

    if( oldObject != null )
    {
      // TRICKY: objectInvalid may add/remove PoolListener for this key,
      // so we cannot iterate over m_listeners
      final IPoolListener[] ls = m_listeners.toArray( new IPoolListener[m_listeners.size()] );
      for( int i = 0; i < ls.length; i++ )
        ls[i].objectInvalid( m_key, oldObject );
    }
  }

  /**
   * @see org.eclipse.core.internal.jobs.InternalJob#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    Object o = null;
    IStatus status = null;
    synchronized( this )
    {
      status = loadObject( monitor );
      o = m_object;
    }

    // TRICKY: objectLoaded may add a new PoolListener for this key,
    // so we cannot iterate over m_listeners
    final IPoolListener[] ls = m_listeners.toArray( new IPoolListener[m_listeners.size()] );
    for( int i = 0; i < ls.length; i++ )
      ls[i].objectLoaded( m_key, o, status );

    return status;
  }

  /**
   * @see org.eclipse.core.internal.jobs.InternalJob#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  protected IStatus loadObject( final IProgressMonitor monitor )
  {
    synchronized( this )
    {
      final String location = m_key.getLocation();
      try
      {
        LOGGER.info( "Loading object for key: " + m_key );
        m_object = m_loader.load( location, m_key.getContext(), monitor );
      }
      catch( final Throwable e )
      {
        m_object = null;

        e.printStackTrace();

        if( m_key.isIgnoreExceptions() )
          return Status.CANCEL_STATUS;

        return StatusUtilities.statusFromThrowable( e, "Fehler beim Laden von " + location );
      }
    }

    return Status.OK_STATUS;
  }

  public boolean isEmpty( )
  {
    return m_listeners.isEmpty();
  }

  public void saveObject( final IProgressMonitor monitor ) throws LoaderException
  {
    synchronized( this )
    {
      m_loader.save( m_key.getLocation(), m_key.getContext(), monitor, m_object );
      setDirty( false );
    }
  }

  @Override
  public String toString( )
  {
    final StringBuffer b = new StringBuffer();
    b.append( "KeyInfo:\n" );
    if( m_object != null )
      b.append( "  pooled object (type): " + m_object.getClass().getName() + "\n" );
    else
      b.append( " !!!!!!!!!!!!!!!!!!!! WARNING !!!!!!!!!!!!!!!!!!!!\n pooled object (type): NULL \n" );
    b.append( "  loader (type): " + m_loader.getClass().getName() + "\n" );
    b.append( "  key: " + m_key + "\n" );
    b.append( "  number of listeners: " + m_listeners.size() + "\n" );
    return b.toString();
  }

  public IPoolableObjectType getKey( )
  {
    return m_key;
  }

  public IPoolListener[] getListeners( )
  {
    return m_listeners.toArray( new IPoolListener[m_listeners.size()] );
  }

  public Object getObject( )
  {
    return m_object;
  }

  public boolean isDirty( )
  {
    return m_isDirty;
  }

  public void setDirty( final boolean isDirty )
  {
    if( m_isDirty == isDirty )
      return;

    m_isDirty = isDirty;

    final IPoolListener[] ls = m_listeners.toArray( new IPoolListener[m_listeners.size()] );

    // TRICKY: objectInvalid may add/remove PoolListener for this key,
    // so we cannot iterate over m_listeners
    for( int i = 0; i < ls.length; i++ )
      ls[i].dirtyChanged( m_key, isDirty );
  }
}