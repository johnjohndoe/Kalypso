package org.kalypso.ui.editor.gmleditor.ui;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.ui.editor.gmleditor.util.model.GMLDocumentEvent;
import org.kalypso.ui.editor.gmleditor.util.model.FeatureElement;
import org.kalypso.ui.editor.gmleditor.util.model.IGMLDocumentListener;
import org.kalypso.ui.editor.gmleditor.util.model.Model;
import org.kalypso.ui.editor.gmleditor.util.model.PropertyElement;

public class GMLEditorContentProvider implements ITreeContentProvider, IGMLDocumentListener
{
  private static Object[] EMPTY_ARRAY = new Object[0];

  protected TreeViewer viewer;

  /*
   * @see IContentProvider#dispose()
   */
  public void dispose()
  {/**/}

  public void inputChanged( Viewer m_viewer, Object oldInput, Object newInput )
  {
    this.viewer = (TreeViewer)m_viewer;
  }
  protected void removeListenerFrom( FeatureElement fe )
  {
    /**/
  }

  protected void addListenerTo( FeatureElement fe )
  {/**/
  }

  /*
   * @see ITreeContentProvider#getChildren(Object)
   */
  public Object[] getChildren( Object parentElement )
  {
    if( parentElement instanceof FeatureElement )
    {
      FeatureElement fe = (FeatureElement)parentElement;
      return fe.getElements();
    }
    else if( parentElement instanceof PropertyElement )
    {
      PropertyElement pe = (PropertyElement)parentElement;
      return pe.getElements();
    }
    return EMPTY_ARRAY;
  }

  /*
   * @see ITreeContentProvider#getParent(Object)
   */
  public Object getParent( Object element )
  {
    if( element instanceof Model )
    {
      return ( (Model)element ).getParent();
    }
    return null;
  }

  /*
   * @see ITreeContentProvider#hasChildren(Object)
   */
  public boolean hasChildren( Object element )
  {
    return getChildren( element ).length > 0;
  }

  /*
   * @see IStructuredContentProvider#getElements(Object)
   */
  public Object[] getElements( Object inputElement )
  {
    return getChildren( inputElement );
  }


  public void add( GMLDocumentEvent event )
  {

  }

  public void remove( GMLDocumentEvent event )
  {
    add( event );
  }

  /**
   * @see org.kalypso.ui.editor.gmleditor.util.model.IGMLDocumentListener#onChange(org.kalypso.ui.editor.gmleditor.util.model.GMLDocumentEvent)
   */
  public void onChange( GMLDocumentEvent event )
  {
    System.out.println("document has changed, contentprovided!!!");
  }

}