package org.kalypso.ui.repository.view;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.repository.IRepositoryContainer;
import org.kalypso.repository.IRepositoryItem;

/**
 * @author schlienger
 *  
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

    return item.getChildren();
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  public Object getParent( final Object element )
  {
    final IRepositoryItem item = testArg( element );

    if( item == null )
      return null;

    return item.getParent();
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  public boolean hasChildren( final Object element )
  {
    final IRepositoryItem item = testArg( element );

    return item.hasChildren();
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