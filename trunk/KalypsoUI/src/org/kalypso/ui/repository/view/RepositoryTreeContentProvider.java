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
import org.eclipse.ui.internal.Workbench;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;
import org.kalypso.repository.container.IRepositoryContainer;

/**
 * Tree Content provider for contents of the RepositoryExplorer.
 * 
 * @author schlienger
 */
public class RepositoryTreeContentProvider implements ITreeContentProvider
{
  /**
   * Helper
   * 
   * @return item or throws IllegalArgumentException if type is not an IRepositoryItem
   */
  private IRepositoryItem testArg( Object arg )
  {
    if( !( arg instanceof IRepositoryItem ) )
      throw new IllegalArgumentException();

    return (IRepositoryItem)arg;
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
    catch( RepositoryException e )
    {
      e.printStackTrace();
      MessageDialog.openError( Workbench.getInstance().getDisplay().getActiveShell(),
          "Operation konnte nicht durchgeführt werden", e.getLocalizedMessage() );

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
    catch( RepositoryException e )
    {
      e.printStackTrace();
      MessageDialog.openError( Workbench.getInstance().getDisplay().getActiveShell(),
          "Operation konnte nicht durchgeführt werden", e.getLocalizedMessage() );

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
    catch( RepositoryException e )
    {
      e.printStackTrace();
      MessageDialog.openError( Workbench.getInstance().getDisplay().getActiveShell(),
          "Operation konnte nicht durchgeführt werden", e.getLocalizedMessage() );

      return false;
    }
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( final Object inputElement )
  {
    final IRepositoryContainer container = (IRepositoryContainer)inputElement;

    return container.getRepositories().toArray();
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose()
  {
  // empty
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  public void inputChanged( Viewer viewer, Object oldInput, Object newInput )
  {
    // empty
  }
}