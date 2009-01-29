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
package org.kalypso.core.util.pool;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.logging.Logger;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.commons.factory.FactoryException;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.i18n.Messages;
import org.kalypso.loader.ILoader;
import org.kalypso.loader.ILoaderFactory;
import org.kalypso.loader.LoaderException;

/**
 * @author Andreas von Dömming
 * @author Gernot Belger
 */
public class ResourcePool
{
  /**
   * Constant for system property intended to be defined in config.ini.
   * <p>
   * if true, the pool asks to save released objects if they are dirty.
   * </p>
   */
  private static final String CONFIG_INI_DO_ASK_FOR_POOL_SAVE = "kalypso.ask_for_pool_save"; //$NON-NLS-1$

  private final static boolean DO_LOG = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.core/debug/resourcepool/keys" ) ); //$NON-NLS-1$

  private final static Logger LOGGER = Logger.getLogger( ResourcePool.class.getName() );

  private final ILoaderFactory m_factory;

  /** type -> loader */
  private final Map<String, ILoader> m_loaderCache = new HashMap<String, ILoader>();

  /** key -> KeyInfo */
  private final Map<IPoolableObjectType, KeyInfo> m_keyInfos = new TreeMap<IPoolableObjectType, KeyInfo>( KeyComparator.getInstance() );

  public ResourcePool( final ILoaderFactory factory )
  {
    m_factory = factory;
  }

  public void dispose( )
  {
    synchronized( m_keyInfos )
    {
      for( final Entry<IPoolableObjectType, KeyInfo> entry : m_keyInfos.entrySet() )
        entry.getValue().dispose();
      m_keyInfos.clear();
    }
    m_loaderCache.clear();
  }

  /**
   * Fügt einen neuen Listener zum Pool für eine bestimmten Key hinzu Ist das Objekt für den key vorhanden, wird der
   * Listener sofort informiert
   */
  public KeyInfo addPoolListener( final IPoolListener l, final IPoolableObjectType key )
  {
    // never register a disposed listener to the pool !
    if( l.isDisposed() )
      return null;

    synchronized( m_keyInfos )
    {
      KeyInfo info = m_keyInfos.get( key );
      if( info == null )
        try
      {
          final ILoader loader = getLoader( key.getType() );
          info = new KeyInfo( key, loader );
          m_keyInfos.put( key, info );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        e.getMessage();
        final RuntimeException iae = new IllegalArgumentException( Messages.getString( "org.kalypso.util.pool.ResourcePool.1" ) + key.getType() ); //$NON-NLS-1$
        ResourcePool.LOGGER.throwing( getClass().getName(), "addPoolListener", iae ); //$NON-NLS-1$
        throw iae;
      }

      info.addListener( l );

      return info;
    }
  }

  public void removePoolListener( final IPoolListener l )
  {
    final List<KeyInfo> infosToDispose = new ArrayList<KeyInfo>();

    synchronized( m_keyInfos )
    {
      for( final Iterator<Entry<IPoolableObjectType, KeyInfo>> iter = m_keyInfos.entrySet().iterator(); iter.hasNext(); )
      {
        final Entry<IPoolableObjectType, KeyInfo> entry = iter.next();

        final IPoolableObjectType key = entry.getKey();
        final KeyInfo info = entry.getValue();
        if( info.removeListener( l ) && info.isEmpty() )
        {
          if( ResourcePool.DO_LOG )
            ResourcePool.LOGGER.info( Messages.getString( "org.kalypso.util.pool.ResourcePool.3" ) + key ); //$NON-NLS-1$

          iter.remove();

          infosToDispose.add( info );
        }
      }
    }

    final ISchedulingRule mutex = ResourcesPlugin.getWorkspace().getRoot();

    for( final KeyInfo info : infosToDispose )
    {
      final String askForSaveProperty = System.getProperty( CONFIG_INI_DO_ASK_FOR_POOL_SAVE, "false" ); //$NON-NLS-1$
      final boolean askForSave = Boolean.parseBoolean( askForSaveProperty );

      if( !info.isDirty() )
        info.dispose();
      else if( askForSave )
      {
        final UIJob job = new SaveAndDisposeInfoJob( Messages.getString( "org.kalypso.util.pool.ResourcePool.5" ), info ); //$NON-NLS-1$
        job.setUser( true );
        job.setRule( mutex );
        job.schedule();
      }
      else
      {
        System.out.println( Messages.getString( "org.kalypso.util.pool.ResourcePool.6" ) + info.getObject() ); //$NON-NLS-1$
        info.dispose();
      }
    }
  }

  private ILoader getLoader( final String type ) throws FactoryException
  {
    ILoader loader = m_loaderCache.get( type );
    if( loader == null )
    {
      loader = m_factory.getLoaderInstance( type );
      m_loaderCache.put( type, loader );
    }

    return loader;
  }

  public void saveObject( final Object object, final IProgressMonitor monitor ) throws LoaderException
  {
    final List<KeyInfo> infosToSave = new ArrayList<KeyInfo>();

    synchronized( m_keyInfos )
    {
      if( object == null )
        return;

      final Collection<KeyInfo> values = m_keyInfos.values();
      for( final KeyInfo info : values )
        if( info.getObject() == object )
          infosToSave.add( info );
    }

    // REMARK: we do not save inside the sync-block, because saving may cause acces to
    // the pool (Example: saving a GML might cause access to Xlinked properties)
    for( final KeyInfo keyInfo : infosToSave )
      keyInfo.saveObject( monitor );
  }

  /** Get the key info which is responsible for a given object. */
  public KeyInfo getInfo( final Object object )
  {
    synchronized( m_keyInfos )
    {
      if( object == null )
        return null;

      final Collection<KeyInfo> values = m_keyInfos.values();
      for( final KeyInfo info : values )
      {
        if( info.getObject() == object )
          return info;
      }

      return null;
    }
  }

  public KeyInfo[] getInfos( )
  {
    return m_keyInfos.values().toArray( new KeyInfo[0] );
  }

  /**
   * Specific method for synchron-loading. If the given key is already present, the associated object is returned. Else
   * a new key is temporary created for the purpose of loading. Once done, the key is disposed.
   * <p>
   * Use this method if you want direct-loading (synchronuous).
   * <p>
   * Bear in mind that the pool-listener mechanism is bypassed here.
   */
  public Object getObject( final IPoolableObjectType key ) throws CoreException// TODO: synchronize
  {
    final KeyInfo info = m_keyInfos.get( key );
    if( info != null )
      try
    {
        // wait for info
        info.join();

        final IStatus result = info.getResult();
        if( !result.isOK() )
          throw new CoreException( result );

        return info.getObject();
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();

      throw new CoreException( StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.util.pool.ResourcePool.7" ) ) ); //$NON-NLS-1$
    }

    // falls object nicht bereits da,
    // einfach einen key nur fürs laden erzeugen, und gleich wieder disposen
    KeyInfo info2 = null;
    try
    {
      final ILoader loader = getLoader( key.getType() );
      info2 = new KeyInfo( key, loader );
      final IStatus result = info2.loadObject( new NullProgressMonitor() );
      if( (result.getSeverity() & IStatus.ERROR) == 1 )
        throw new CoreException( result );

      return info2.getObject();
    }
    catch( final Exception e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.util.pool.ResourcePool.8" ) ) ); //$NON-NLS-1$
    }
    finally
    {
      if( info2 != null )
        info2.dispose();
    }
  }

  public KeyInfo getInfoForKey( final IPoolableObjectType poolKey )// TODO: synchronize
  {
    return m_keyInfos.get( poolKey );
  }
}