package org.kalypso.util.repository.view;

import java.util.Arrays;
import java.util.List;
import java.util.Vector;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.util.repository.IRepository;
import org.kalypso.util.repository.IRepositoryItem;

/**
 * @author schlienger
 *  
 */
public class RepositoriesContentProvider implements ITreeContentProvider
{
  private final List m_reps = new Vector();

  public RepositoriesContentProvider( )
  {
    this( new IRepository[0] );
  }
  
  public RepositoriesContentProvider( IRepository[] repositories )
  {
    m_reps.addAll( Arrays.asList( repositories ) );
  }

  public void addRepository( IRepository rep )
  {
    m_reps.add( rep );
  }
  
  public void removeRepository( IRepository rep )
  {
    m_reps.remove( rep );
  }
  
  public int getRepositoriesCount()
  {
    return m_reps.size();
  }
  
  /**
   * helper
   */
  private IRepositoryItem testArg( Object arg )
  {
    if( ! (arg instanceof IRepositoryItem) )
      throw new IllegalArgumentException();

    return (IRepositoryItem)arg;
  }
  
  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  public Object[] getChildren( Object parentElement )
  {
    IRepositoryItem item = testArg( parentElement );
    
    if( item == null )
        return m_reps.toArray( new IRepository[m_reps.size()] );
    
    return item.getChildren();
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  public Object getParent( Object element )
  {
    IRepositoryItem item = testArg( element );
    
    if( item == null )
      return null;
    
    return item.getParent();
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  public boolean hasChildren( Object element )
  {
    IRepositoryItem item = testArg( element );
    
    if( item == null )
      return m_reps.size() != 0;
    
    return item.hasChildren();
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( Object inputElement )
  {
    return getChildren( inputElement );
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