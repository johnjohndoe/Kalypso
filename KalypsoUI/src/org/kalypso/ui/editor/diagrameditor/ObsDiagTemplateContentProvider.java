package org.kalypso.ui.editor.diagrameditor;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.ogc.sensor.diagview.IDiagramCurve;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplateTheme;

/**
 * ObsDiagTemplateContentProvider
 * 
 * @author schlienger
 */
public class ObsDiagTemplateContentProvider implements ITreeContentProvider
{
  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  public Object[] getChildren( Object parentElement )
  {
    if( parentElement instanceof IDiagramTemplate )
    {
      IDiagramTemplate tpl = (IDiagramTemplate) parentElement;
      
      return tpl.getThemes().toArray();
    }
    
    if( parentElement instanceof IDiagramTemplateTheme )
    {
      IDiagramTemplateTheme theme = (IDiagramTemplateTheme) parentElement;
      
      return theme.getCurves().toArray();
    }
      
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  public Object getParent( Object element )
  {
    if( element instanceof IDiagramCurve )
    {
      IDiagramCurve curve = (IDiagramCurve) element;
      
      return curve.getTheme();
    }
    
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  public boolean hasChildren( Object element )
  {
    if( element instanceof IDiagramTemplate )
    {
      IDiagramTemplate tpl = (IDiagramTemplate) element;
      
      return tpl.getThemes().size() > 0;
    }
    
    if( element instanceof IDiagramTemplateTheme )
    {
      IDiagramTemplateTheme theme = (IDiagramTemplateTheme) element;
      
      return theme.getCurves().size() > 0;
    }
    
    return false;
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( Object inputElement )
  {
    IDiagramTemplate template = (IDiagramTemplate) inputElement;
    
    return template.getThemes().toArray();
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object, java.lang.Object)
   */
  public void inputChanged( Viewer viewer, Object oldInput, Object newInput )
  {
    //System.out.println( "INPUT CHANGED (ContentProvider)" );
//    m_template = (IDiagramTemplate) newInput;
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose( )
  {
    // empty
  }
}
