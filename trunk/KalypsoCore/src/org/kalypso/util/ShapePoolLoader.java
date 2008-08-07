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
package org.kalypso.util;

import java.net.URL;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.KeyComparator;
import org.kalypso.util.pool.KeyInfo;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;

/**
 * This class loads a workspace via the pool.
 * 
 * @author Holger Albert
 */
public class ShapePoolLoader
{
  /**
   * The key for the workspace of the profiles.
   */
  protected IPoolableObjectType m_poolKey = null;

  /**
   * The workspace of the profiles.
   */
  protected CommandableWorkspace m_workspace = null;

  /**
   * Listener on the pool for getting the workspace.
   */
  private IPoolListener m_poolListener = new IPoolListener()
  {
    /**
     * @see org.kalypso.util.pool.IPoolListener#dirtyChanged(org.kalypso.util.pool.IPoolableObjectType, boolean)
     */
    public void dirtyChanged( IPoolableObjectType key, boolean isDirty )
    {
    }

    /**
     * @see org.kalypso.util.pool.IPoolListener#isDisposed()
     */
    public boolean isDisposed( )
    {
      return false;
    }

    /**
     * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType,
     *      java.lang.Object)
     */
    public void objectInvalid( IPoolableObjectType key, Object oldValue )
    {
      m_workspace = null;
    }

    /**
     * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType,
     *      java.lang.Object, org.eclipse.core.runtime.IStatus)
     */
    public void objectLoaded( IPoolableObjectType key, Object newValue, IStatus status )
    {
      if( KeyComparator.getInstance().compare( key, m_poolKey ) == 0 )
      {
        if( status.isOK() )
        {
          m_workspace = (CommandableWorkspace) newValue;
          return;
        }

        m_workspace = null;
      }
    }
  };

  /**
   * The constructor.
   */
  public ShapePoolLoader( )
  {
    /* Initialize the members. */
    m_poolKey = null;
    m_workspace = null;
  }

  /**
   * This function inits and loads the workspace via the pool, if it is not already loaded.
   * 
   * @param path
   *          The path.
   * @param filename
   *          The filename.
   * @param coordinateSystem
   *          The coordinate system.
   */
  public void load( IFolder path, String filename, String coordinateSystem ) throws Exception
  {
    /* The url context. */
    URL context = ResourceUtilities.createURL( path );

    /* Create the pool key. */
    m_poolKey = new PoolableObjectType( "shape", filename + "#" + coordinateSystem, context );

    /* Get the pool. */
    ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();

    /* Add a listener to the pool. */
    pool.addPoolListener( m_poolListener, m_poolKey );
  }

  /**
   * This function returns the workspace.
   * 
   * @return The workspace out of the pool. Or null, if {@link #load(String, IFolder)} was not called or if an error has
   *         occured.
   */
  public CommandableWorkspace getWorkspace( )
  {
    if( m_poolKey == null )
      return null;

    /* If still loading, wait for pool to load. */
    ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();
    KeyInfo info = pool.getInfoForKey( m_poolKey );

    /* Check if job is running. */
    if( info.getResult() == null )
    {
      /* The job is running and there fore we have to wait. */
      try
      {
        info.join();
      }
      catch( InterruptedException e )
      {
        /* Get the Status. */
        IStatus status = StatusUtilities.statusFromThrowable( e );

        /* Log the error. */
        KalypsoCorePlugin.getDefault().getLog().log( status );
      }
    }

    return m_workspace;
  }

  /**
   * This function saves the workspace.
   */
  public void saveWorkspace( ) throws Exception
  {
    if( m_workspace == null )
      return;

    /* Get the pool. */
    ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();

    /* Save the workspace. */
    pool.saveObject( m_workspace, new NullProgressMonitor() );
  }

  /**
   * This function finishes the pool loader and removes the listener.
   */
  public void finish( )
  {
    /* Get the pool. */
    ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();

    /* Remove the listener. */
    pool.removePoolListener( m_poolListener );
  }

  /**
   * This function will only load the workspace, but does not add a listener to the pool.
   * 
   * @param path
   *          The path.
   * @param filename
   *          The filename.
   * @param coordinateSystem
   *          The coordinate system.
   * @return The workspace.
   */
  public static CommandableWorkspace getWorkspace( IFolder path, String filename, String coordinateSystem ) throws Exception
  {
    /* The url context. */
    URL context = ResourceUtilities.createURL( path );

    /* Create the pool key. */
    PoolableObjectType poolKey = new PoolableObjectType( "shape", filename + "#" + coordinateSystem, context );

    /* Get the pool. */
    ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();

    return (CommandableWorkspace) pool.getObject( poolKey );
  }
}