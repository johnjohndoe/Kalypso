package org.kalypso.ui.wizard.ocs.idtable;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;


public class IdTableContentProvider implements IStructuredContentProvider
{
  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( Object inputElement )
  {
    final ArrayList ids = new ArrayList( );
    
    if( inputElement instanceof List )
    {
      final List list = (List) inputElement;
      
      for( Iterator it = list.iterator(); it.hasNext(); )
      {
        Object object = it.next();
        
        if( object instanceof IFile )
        {
          final IFile file = (IFile) object;
          
          final IdStruct struct = IdStruct.createUsing( file );
          
          if( struct != null )
            ids.add( struct );
        }
      }
      
      return ids.toArray();
    }
    
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose( )
  {
    // empty
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object, java.lang.Object)
   */
  public void inputChanged( Viewer viewer, Object oldInput, Object newInput )
  {
    // empty
  }
}