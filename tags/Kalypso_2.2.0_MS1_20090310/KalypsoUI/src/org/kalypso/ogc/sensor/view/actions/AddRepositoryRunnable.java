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
package org.kalypso.ogc.sensor.view.actions;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ListDialog;
import org.eclipse.ui.progress.IProgressService;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.sensor.view.actions.AddRepositoryHandler.ChooseRepositoryLabelProvider;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.RepositoriesExtensions;
import org.kalypso.repository.RepositoryException;
import org.kalypso.repository.conf.RepositoryFactoryConfig;
import org.kalypso.repository.container.IRepositoryContainer;
import org.kalypso.repository.factory.IRepositoryFactory;

/**
 * @author kuch
 */
public class AddRepositoryRunnable implements Runnable
{
  private final Shell m_shell;

  private final IRepositoryContainer m_repositoryContainer;

  public AddRepositoryRunnable( final IRepositoryContainer repositoryContainer, final Shell shell )
  {
    m_repositoryContainer = repositoryContainer;
    m_shell = shell;
  }

  /**
   * @see java.lang.Runnable#run()
   */
  public void run( )
  {
    final ListDialog dlg = new ListDialog( m_shell );
    dlg.setLabelProvider( new ChooseRepositoryLabelProvider() );
    dlg.setContentProvider( new ArrayContentProvider() );
    dlg.setTitle( Messages.getString("org.kalypso.ogc.sensor.view.actions.AddRepositoryRunnable.0") ); //$NON-NLS-1$

    try
    {
      dlg.setInput( RepositoriesExtensions.retrieveExtensions() );

      if( dlg.open() != Window.OK )
        return;

      final RepositoryFactoryConfig cfg = (RepositoryFactoryConfig) dlg.getResult()[0];
      final IRepositoryFactory f = cfg.getFactory( );

      if( f.configureRepository() )
      {
        final IProgressService progressService = PlatformUI.getWorkbench().getProgressService();
        progressService.busyCursorWhile( new IRunnableWithProgress()
        {
          public void run( final IProgressMonitor monitor ) throws InvocationTargetException
          {
            monitor.beginTask( Messages.getString("org.kalypso.ogc.sensor.view.actions.AddRepositoryRunnable.1"), 2 ); //$NON-NLS-1$

            final IRepository rep;
            try
            {
              rep = f.createRepository();

              monitor.worked( 1 );

              m_repositoryContainer.addRepository( rep );

              monitor.worked( 1 );
            }
            catch( final RepositoryException e )
            {
              throw new InvocationTargetException( e );
            }
            finally
            {
              monitor.done();
            }
          }
        } );
      }
    }
    catch( final InvocationTargetException e )
    {
      e.getCause().printStackTrace();

      MessageDialog.openError( m_shell, Messages.getString("org.kalypso.ogc.sensor.view.actions.AddRepositoryRunnable.2"), e.getTargetException().getLocalizedMessage() ); //$NON-NLS-1$
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      MessageDialog.openError( m_shell, Messages.getString("org.kalypso.ogc.sensor.view.actions.AddRepositoryRunnable.3"), e.getLocalizedMessage() ); //$NON-NLS-1$
    }
  }

}
