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
package org.kalypso.util.pool;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.jface.operation.IRunnableContext;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * @author kuch
 */
public abstract class AbstractPool implements IPoolListener, ICommandTarget
{

  protected IPoolableObjectType m_poolKey = null;

  protected CommandableWorkspace m_workspace = null;

  protected boolean m_dirty = false;

  private String m_fileName;

  private URL m_context;

  public void close( )
  {
    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
    pool.removePoolListener( this );
    m_poolKey = null;
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#dirtyChanged(org.kalypso.util.pool.IPoolableObjectType, boolean)
   */
  public void dirtyChanged( final IPoolableObjectType key, final boolean isDirty )
  {
    saveWorkspace( null );
  }

  public void dispose( )
  {
    if( m_poolKey != null )
    {
      final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
      pool.removePoolListener( this );

      m_poolKey = null;
    }
  }

  public CommandableWorkspace getWorkspace( )
  {
    return m_workspace;
  }

  public boolean isDirty( )
  {
    return m_dirty;
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#isDisposed()
   */
  public boolean isDisposed( )
  {
    // TODO Auto-generated method stub
    return false;
  }

  protected void loadGmlWorkspace( final URL context, final String fileName, final IRunnableContext runnableContext ) throws MalformedURLException, CoreException
  {
    m_context = context;
    m_fileName = fileName;
    final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
    if( m_poolKey != null )
    {
      pool.removePoolListener( this );
      m_poolKey = null;
    }

    m_poolKey = new PoolableObjectType( "gml", fileName, context ); //$NON-NLS-1$

    pool.addPoolListener( this, m_poolKey );
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object)
   */
  public void objectInvalid( final IPoolableObjectType key, final Object oldValue )
  {
    m_workspace = null;
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object,
   *      org.eclipse.core.runtime.IStatus)
   */
  public void objectLoaded( final IPoolableObjectType key, final Object newValue, final IStatus status )
  {
    if( KeyComparator.getInstance().compare( key, m_poolKey ) == 0 )
      if( status.isOK() )
        m_workspace = (CommandableWorkspace) newValue;
      else
        m_workspace = null;
  }

  /**
   * @see org.kalypso.commons.command.ICommandTarget#postCommand(org.kalypso.commons.command.ICommand,
   *      java.lang.Runnable)
   */
  public void postCommand( final ICommand command, final Runnable runnable )
  {
    try
    {
      m_workspace.postCommand( command );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  public void saveWorkspace( final IJobChangeListener listener )
  {
    final WorkspaceJob job = new WorkspaceJob( String.format( "Saving workspace: %s", m_fileName ) )
    {
      @Override
      public IStatus runInWorkspace( IProgressMonitor monitor )
      {
        monitor.beginTask( getName(), 1 );

        try
        {
          KalypsoGisPlugin.getDefault().getPool().saveObject( m_workspace, new NullProgressMonitor() );
        }
        catch( LoaderException e )
        {
          e.printStackTrace();
        }

        monitor.done();

        m_dirty = false;

        return Status.OK_STATUS;
      }
    };

    if( listener != null )
      job.addJobChangeListener( listener );

    job.schedule();
  }

  public void setDirty( )
  {
    m_dirty = true;
  }
}
