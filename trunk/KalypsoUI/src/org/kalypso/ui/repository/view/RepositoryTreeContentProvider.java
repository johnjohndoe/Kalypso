package org.kalypso.ui.repository.view;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.internal.Workbench;
import org.kalypso.repository.IRepositoryContainer;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;

/**
 * Tree Content provider for contents of the RepositoryExplorer.
 * 
 * @author schlienger
 */
public class RepositoryTreeContentProvider implements ITreeContentProvider
{
  /**
   * helper
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
      MessageDialog.openError( Workbench.getInstance().getDisplay().getActiveShell(), "Operation konnte nicht durchgeführt werden", e.getLocalizedMessage() );
      
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
      MessageDialog.openError( Workbench.getInstance().getDisplay().getActiveShell(), "Operation konnte nicht durchgeführt werden", e.getLocalizedMessage() );
      
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
      MessageDialog.openError( Workbench.getInstance().getDisplay().getActiveShell(), "Operation konnte nicht durchgeführt werden", e.getLocalizedMessage() );
      
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
  // ?
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer,
   *      java.lang.Object, java.lang.Object)
   */
  public void inputChanged( Viewer viewer, Object oldInput, Object newInput )
  {
  // TODO
  }
}