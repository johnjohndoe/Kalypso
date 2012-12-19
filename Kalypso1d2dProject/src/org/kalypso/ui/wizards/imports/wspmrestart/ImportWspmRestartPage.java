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
package org.kalypso.ui.wizards.imports.wspmrestart;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.model.WorkbenchContentProvider;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.simulation.ui.calccase.ModelNature;

/**
 * @author Gernot Belger
 */
public class ImportWspmRestartPage extends WizardPage
{
  protected static final IPath PATH_LS_FILE = Path.fromPortableString( "_aktuell/Daten/L‰ngsschnitt.gml" ); //$NON-NLS-1$

  private IFile m_lengthSection;

  public ImportWspmRestartPage( final String pageName )
  {
    super( pageName );

    setTitle( Messages.getString("org.kalypso.ui.wizards.imports.wspmrestart.ImportWspmRestartPage.1") ); //$NON-NLS-1$
    setDescription( Messages.getString("org.kalypso.ui.wizards.imports.wspmrestart.ImportWspmRestartPage.2") ); //$NON-NLS-1$
  }

  @Override
  public void createControl( final Composite parent )
  {
    /* Filters every none-WSPM project */
    final ViewerFilter wspmProjectFilter = new ViewerFilter()
    {

      @Override
      public boolean select( final Viewer viewer, final Object parentElement, final Object element )
      {
        try
        {
          if( element instanceof IProject )
          {
            final IProject project = (IProject) element;
            if( !project.isOpen() )
              return false;

            if( project.getNature( ModelNature.ID ) == null )
              return false;

            final IFolder folder = project.getFolder( "Ergebnisse" ); //$NON-NLS-1$
            return folder.exists();
          }

          if( element instanceof IFolder )
          {
            final IFolder folder = (IFolder) element;
            if( folder.getName().equals( "Ergebnisse" ) && parentElement instanceof IProject ) //$NON-NLS-1$
              return true;

            if( parentElement instanceof IFolder && ((IFolder) parentElement).getName().equals( "Ergebnisse" ) ) //$NON-NLS-1$
            {
              final IFile file = folder.getFile( PATH_LS_FILE );
              return file.exists();
            }

            return false;
          }
        }
        catch( final CoreException e )
        {
          Kalypso1d2dProjectPlugin.getDefault().getLog().log( e.getStatus() );
        }

        return false;
      }
    };

    final TreeViewer treeViewer = new TreeViewer( parent );

    treeViewer.setContentProvider( new WorkbenchContentProvider() );
    treeViewer.setLabelProvider( new WorkbenchLabelProvider() );
    treeViewer.setFilters( new ViewerFilter[] { wspmProjectFilter } );
    treeViewer.setInput( ResourcesPlugin.getWorkspace().getRoot() );

    treeViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleSelectionChanged( (IStructuredSelection) event.getSelection() );
      }
    } );

    setControl( treeViewer.getControl() );
  }

  protected void handleSelectionChanged( final IStructuredSelection selection )
  {
    final Object firstElement = selection.getFirstElement();
    if( firstElement instanceof IFolder )
    {
      final IFolder folder = (IFolder) firstElement;
      final IFile file = folder.getFile( PATH_LS_FILE );
      if( file.exists() )
        m_lengthSection = file;
      else
        m_lengthSection = null;
    }
    else
      m_lengthSection = null;

    setPageComplete( m_lengthSection != null );
  }

  public IFile getLengthSection( )
  {
    return m_lengthSection;
  }

}
