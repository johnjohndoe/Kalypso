package org.kalypso.ui.editor.abstractobseditor;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.ogc.sensor.diagview.DiagViewCurve;
import org.kalypso.ogc.sensor.diagview.DiagViewTheme;
import org.kalypso.ogc.sensor.tableview.TableViewColumn;
import org.kalypso.ogc.sensor.tableview.TableViewTheme;
import org.kalypso.ogc.sensor.template.AbstractViewTemplate;

/**
 * ObsTemplateContentProvider
 * 
 * @author schlienger
 */
public class ObsTemplateContentProvider implements ITreeContentProvider
{
  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  public Object[] getChildren( Object parentElement )
  {
    if( parentElement instanceof AbstractViewTemplate )
    {
      final AbstractViewTemplate tpl = (AbstractViewTemplate) parentElement;
      
      return tpl.getThemes().toArray();
    }
    
    if( parentElement instanceof TableViewTheme )
    {
      final TableViewTheme theme = (TableViewTheme) parentElement;
      
      return theme.getColumns().toArray();
    }

    if( parentElement instanceof DiagViewTheme )
    {
      final DiagViewTheme theme = (DiagViewTheme) parentElement;
      
      return theme.getCurves().toArray();
    }
    
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  public Object getParent( Object element )
  {
    if( element instanceof TableViewColumn )
    {
      final TableViewColumn col = (TableViewColumn) element;
      
      return col.getTheme();
    }

    if( element instanceof DiagViewCurve )
    {
      final DiagViewCurve col = (DiagViewCurve) element;
      
      return col.getTheme();
    }
    
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  public boolean hasChildren( Object element )
  {
    if( element instanceof AbstractViewTemplate )
    {
      final AbstractViewTemplate tpl = (AbstractViewTemplate) element;
      
      return tpl.getThemes().size() > 0;
    }
    
    if( element instanceof TableViewTheme )
    {
      final TableViewTheme theme = (TableViewTheme) element;
      
      return theme.getColumns().size() > 0;
    }
    
    if( element instanceof DiagViewTheme )
    {
      final DiagViewTheme theme = (DiagViewTheme) element;
      
      return theme.getCurves().size() > 0;
    }
    
    return false;
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( Object inputElement )
  {
    final AbstractViewTemplate template = (AbstractViewTemplate) inputElement;
    
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
