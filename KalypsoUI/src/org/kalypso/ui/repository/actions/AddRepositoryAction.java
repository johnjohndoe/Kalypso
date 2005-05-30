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
package org.kalypso.ui.repository.actions;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ListDialog;
import org.eclipse.ui.progress.IProgressService;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.RepositoriesExtensions;
import org.kalypso.repository.RepositoryException;
import org.kalypso.repository.conf.RepositoryFactoryConfig;
import org.kalypso.repository.factory.IRepositoryFactory;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.repository.view.ObservationChooser;

/**
 * Ein Repository hinzufügen.
 * 
 * @author schlienger
 */
public class AddRepositoryAction extends AbstractRepositoryExplorerAction
{
  public AddRepositoryAction( final ObservationChooser explorer )
  {
    super( explorer, "Repository hinzufügen", ImageProvider.IMAGE_ZML_REPOSITORY_ADD, "Fügt ein Repository hinzu..." );
  }

  /**
   * @see org.eclipse.jface.action.IAction#run()
   */
  public void run()
  {
    final ListDialog dlg = new ListDialog( getShell() );
    dlg.setLabelProvider( new LabelProvider() );
    dlg.setContentProvider( new ArrayContentProvider() );
    dlg.setTitle( "Repository Typ auswählen" );

    try
    {
      dlg.setInput( RepositoriesExtensions.retrieveExtensions() );

      if( dlg.open() != Window.OK )
        return;

      final RepositoryFactoryConfig cfg = (RepositoryFactoryConfig)dlg.getResult()[0];

      final IRepositoryFactory f = cfg.createFactory( getClass().getClassLoader() );

      if( f.configureRepository() )
      {
        final IProgressService progressService = PlatformUI.getWorkbench().getProgressService();
        progressService.busyCursorWhile( new IRunnableWithProgress()
        {
          public void run( IProgressMonitor monitor ) throws InvocationTargetException
          {
            monitor.beginTask( "Repository hinzufügen", 2 );

            final IRepository rep;
            try
            {
              rep = f.createRepository();

              monitor.worked( 1 );

              getRepositoryContainer().addRepository( rep );

              monitor.worked( 1 );
            }
            catch( RepositoryException e )
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
      
      MessageDialog.openError( getShell(), "Repository hinzufügen", e.getTargetException()
          .getLocalizedMessage() );     
    }
    catch( Exception e )
    {
      e.printStackTrace();
      MessageDialog.openError( getShell(), "Repository hinzufügen", e.getLocalizedMessage() );
    }
  }
}