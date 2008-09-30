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
package org.kalypso.ui.repository.view;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.i18n.Messages;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.IRepositoryListener;
import org.kalypso.repository.RepositoryException;
import org.kalypso.repository.container.IRepositoryContainer;
import org.kalypso.repository.container.IRepositoryContainerListener;

/**
 * Tree Content provider for contents of the RepositoryExplorer.
 * 
 * @author schlienger
 */
public class RepositoryTreeContentProvider implements ITreeContentProvider
{
  private IRepositoryContainerListener m_containerListener;

  private IRepositoryListener m_repositoryListener;

  /**
   * Helper
   * 
   * @return item or throws IllegalArgumentException if type is not an IRepositoryItem
   */
  private IRepositoryItem testArg( final Object arg )
  {
    if( !(arg instanceof IRepositoryItem) )
      throw new IllegalArgumentException();

    return (IRepositoryItem) arg;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  public Object[] getChildren( final Object parentElement )
  {
    final IRepositoryItem item = testArg( parentElement );

    try
    {
      return item.getChildren();
    }
    catch( final RepositoryException e )
    {
      e.printStackTrace();
      MessageDialog.openError( PlatformUI.getWorkbench().getDisplay().getActiveShell(), Messages.getString( "org.kalypso.ui.repository.view.RepositoryTreeContentProvider.0" ), e.getLocalizedMessage() ); //$NON-NLS-1$

      return new Object[0];
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  public Object getParent( final Object element )
  {
    final IRepositoryItem item = testArg( element );

    if( item == null )
      return null;

    try
    {
      return item.getParent();
    }
    catch( final RepositoryException e )
    {
      e.printStackTrace();
      MessageDialog.openError( PlatformUI.getWorkbench().getDisplay().getActiveShell(), Messages.getString( "org.kalypso.ui.repository.view.RepositoryTreeContentProvider.1" ), e.getLocalizedMessage() ); //$NON-NLS-1$

      return null;
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  public boolean hasChildren( final Object element )
  {
    final IRepositoryItem item = testArg( element );

    try
    {
      return item.hasChildren();
    }
    catch( final RepositoryException e )
    {
      e.printStackTrace();
      MessageDialog.openError( PlatformUI.getWorkbench().getDisplay().getActiveShell(), Messages.getString( "org.kalypso.ui.repository.view.RepositoryTreeContentProvider.2" ), e.getLocalizedMessage() ); //$NON-NLS-1$

      return false;
    }
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( final Object inputElement )
  {
    final IRepositoryContainer container = (IRepositoryContainer) inputElement;

    return container.getRepositories();
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose( )
  {
    // empty
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
    /* oldInput has an IRepositoryContainerListener attached? remove old listener */
    if( oldInput instanceof IRepositoryContainer )
    {
      final IRepositoryContainer con = (IRepositoryContainer) oldInput;

      con.removeRepositoryContainerListener( m_containerListener );

      final IRepository[] repositories = con.getRepositories();
      for( final IRepository ar : repositories )
      {
        ar.removeRepositoryListener( m_repositoryListener );
      }
    }

    /* attach new IRepositoryContainerLister to new repository input! */
    if( newInput instanceof IRepositoryContainer )
    {
      final IRepositoryContainer con = (IRepositoryContainer) newInput;

      /* Re-create both listeners in order to have a fresh reference to the new viewer */
      m_repositoryListener = new IRepositoryListener()
      {
        public void onRepositoryStructureChanged( )
        {
          ViewerUtilities.refresh( viewer, true );
        }
      };

      m_containerListener = new IRepositoryContainerListener()
      {
        public void onRepositoryContainerChanged( )
        {
          ViewerUtilities.refresh( viewer, true );

          /* Re-Register all listeners for the repositories */

          final IRepository[] repositories = con.getRepositories();
          for( final IRepository repository : repositories )
          {
            repository.removeRepositoryListener( m_repositoryListener );
            repository.addRepositoryListener( m_repositoryListener );
          }
        }
      };

      con.addRepositoryContainerListener( m_containerListener );

      m_containerListener.onRepositoryContainerChanged();
    }

  }
}