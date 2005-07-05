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
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.logging.Logger;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.kalypso.commons.factory.FactoryException;
import org.kalypso.contribs.eclipse.core.runtime.jobs.MutexSchedulingRule;
import org.kalypso.loader.ILoader;
import org.kalypso.loader.ILoaderFactory;
import org.kalypso.loader.LoaderException;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * @author d�mming,belger
 */
public class ResourcePool
{
  private final static Logger m_logger = Logger.getLogger( ResourcePool.class.getName() );

  private final ILoaderFactory m_factory;

  /** type -> loader */
  private final Map m_loaderCache = new HashMap();

  /** key -> KeyInfo */
  private final Map m_keyInfos = new TreeMap( KeyComparator.getInstance() );

  /**
   * Rule f�r die KeyInfos. Das Laden der eigentlichen Objekte soll nacheinander stattfinden.
   */
  private final ISchedulingRule m_mutex = new MutexSchedulingRule();

  public ResourcePool( final ILoaderFactory factory )
  {
    m_factory = factory;
  }

  public void dispose()
  {
    synchronized( m_keyInfos )
    {
      for( final Iterator iter = m_keyInfos.entrySet().iterator(); iter.hasNext(); )
      {
        final Map.Entry entry = (Entry)iter.next();
        ( (KeyInfo)entry.getValue() ).dispose();
      }

      m_keyInfos.clear();
    }

    m_loaderCache.clear();
  }

  /**
   * F�gt einen neuen Listener zum Pool f�r eine bestimmten Key hinzu Ist das Objekt f�r den key vorhanden, wird der
   * Listener sofort informiert
   * 
   * @param l
   * @param key
   */
  public void addPoolListener( final IPoolListener l, final IPoolableObjectType key )
  {
    synchronized( m_keyInfos )
    {
      KeyInfo info = (KeyInfo)m_keyInfos.get( key );
      if( info == null )
      {
        try
        {
          final ILoader loader = getLoader( key.getType() );
          info = new KeyInfo( key, loader, m_mutex );
          m_keyInfos.put( key, info );
        }
        catch( final Exception e )
        {
          final RuntimeException iae = new IllegalArgumentException( "No Loader for type: " + key.getType() );
          m_logger.throwing( getClass().getName(), "addPoolListener", iae );
          throw iae;
        }
      }

      info.addListener( l );
    }
  }

  public void removePoolListener( final IPoolListener l )
  {
    synchronized( m_keyInfos )
    {
      for( final Iterator iter = m_keyInfos.entrySet().iterator(); iter.hasNext(); )
      {
        final Map.Entry entry = (Entry)iter.next();

        final IPoolableObjectType key = (IPoolableObjectType)entry.getKey();
        final KeyInfo info = (KeyInfo)entry.getValue();
        if( info.removeListener( l ) && info.isEmpty() )
        {
          m_logger.info( "Releasing key (no more listeners): " + key );
          info.dispose();
          iter.remove();
        }
      }
    }
  }

  private ILoader getLoader( final String type ) throws FactoryException
  {
    ILoader loader = (ILoader)m_loaderCache.get( type );
    if( loader == null )
    {
      loader = m_factory.getLoaderInstance( type );
      m_loaderCache.put( type, loader );
    }

    return loader;
  }

  public void saveObject( final Object object, final IProgressMonitor monitor ) throws LoaderException
  {
    synchronized( m_keyInfos )
    {
      if( object == null )
        return;

      final Collection values = m_keyInfos.values();
      for( final Iterator iter = values.iterator(); iter.hasNext(); )
      {
        final KeyInfo info = (KeyInfo)iter.next();
        if( info.getObject() == object )
          info.saveObject( monitor );
      }
    }
  }

  public KeyInfo[] getInfos()
  {
    return (KeyInfo[])m_keyInfos.values().toArray( new KeyInfo[0] );
  }

  public Object getObject( final PoolableObjectType key ) throws CoreException
  {
    final KeyInfo info = (KeyInfo)m_keyInfos.get( key );
    if( info != null )
    {
      try
      {
        // wait for info
        info.join();

        final IStatus result = info.getResult();
        if( result.isOK() )
          return info.getObject();

        throw new CoreException( result );
      }
      catch( final InterruptedException e )
      {
        e.printStackTrace();

        throw new CoreException( KalypsoGisPlugin.createErrorStatus( "Ladevorgang unterbrochen", e ) );
      }
    }

    // falls object nicht bereits da,
    // einfach einen key nur f�rs laden erzeugen, und gleich wieder disposen
    KeyInfo info2 = null;
    try
    {
      final ILoader loader = getLoader( key.getType() );
      info2 = new KeyInfo( key, loader, m_mutex );
      final IStatus result = info2.loadObject( new NullProgressMonitor() );
      if( result.isOK() )
        return info2.getObject();

      throw new CoreException( result );
    }
    catch( final Exception e )
    {
      throw new CoreException( KalypsoGisPlugin.createErrorStatus( "Fehler beim Laden", e ) );
    }
    finally
    {
      if( info2 != null )
        info2.dispose();
    }
  }
}