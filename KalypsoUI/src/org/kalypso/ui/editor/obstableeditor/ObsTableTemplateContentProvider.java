package org.kalypso.ui.editor.obstableeditor;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.ITableViewTemplate;
import org.kalypso.ogc.sensor.tableview.ITableViewTheme;

/**
 * ObsDiagTemplateContentProvider
 * 
 * @author schlienger
 */
public class ObsTableTemplateContentProvider implements ITreeContentProvider
{
  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  public Object[] getChildren( Object parentElement )
  {
    if( parentElement instanceof ITableViewTemplate )
    {
      ITableViewTemplate tpl = (ITableViewTemplate) parentElement;
      
      return tpl.getThemes().toArray();
    }
    
    if( parentElement instanceof ITableViewTheme )
    {
      ITableViewTheme theme = (ITableViewTheme) parentElement;
      
      return theme.getColumns().toArray();
    }
      
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  public Object getParent( Object element )
  {
    if( element instanceof ITableViewColumn )
    {
      ITableViewColumn col = (ITableViewColumn) element;
      
      return col.getTheme();
    }
    
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  public boolean hasChildren( Object element )
  {
    if( element instanceof ITableViewTemplate )
    {
      ITableViewTemplate tpl = (ITableViewTemplate) element;
      
      return tpl.getThemes().size() > 0;
    }
    
    if( element instanceof ITableViewTheme )
    {
      ITableViewTheme theme = (ITableViewTheme) element;
      
      return theme.getColumns().size() > 0;
    }
    
    return false;
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( Object inputElement )
  {
    ITableViewTemplate template = (ITableViewTemplate) inputElement;
    
    return template.getThemes().toArray();
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object, java.lang.Object)
   */
  public void inputChanged( Viewer viewer, Object oldInput, Object newInput )
  {
    // empty
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose( )
  {
    // empty
  }
}
