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

  protected TreeViewer m_viewer;

  /*
   * @see IContentProvider#dispose()
   */
  public void dispose()
  {/**/}

  /*
   * @see IContentProvider#inputChanged(Viewer, Object, Object)
   */
  /**
   * Notifies this content provider that the given viewer's input has been
   * switched to a different element.
   * <p>
   * A typical use for this method is registering the content provider as a
   * listener to changes on the new input (using model-specific means), and
   * deregistering the viewer from the old input. In response to these change
   * notifications, the content provider propagates the changes to the viewer.
   * </p>
   * 
   * @param viewer
   *          the viewer
   * @param oldInput
   *          the old input element, or <code>null</code> if the viewer did
   *          not previously have an input
   * @param newInput
   *          the new input element, or <code>null</code> if the viewer does
   *          not have an input
   */
  public void inputChanged( Viewer viewer, Object oldInput, Object newInput )
  {
    m_viewer = (TreeViewer)viewer;
    //		if(oldInput != null) {
    //			removeListenerFrom((FeaturePropertyElement)oldInput);
    //		}
    //		if(newInput != null) {
    //			addListenerTo((MovingBox)newInput);
    //		}
  }

  /**
   * Because the domain model does not have a richer listener model, recursively
   * remove this listener from each child box of the given box
   * 
   * @param fe
   */
  protected void removeListenerFrom( FeatureElement fe )
  {
  //		box.removeListener(this);
  //		for (Iterator iterator = box.getBoxes().iterator(); iterator.hasNext();) {
  //			MovingBox aBox = (MovingBox) iterator.next();
  //			removeListenerFrom(aBox);
  //		}
  }

  /**
   * Because the domain model does not have a richer listener model, recursively
   * add this listener to each child box of the given box
   * 
   * @param fe
   */
  protected void addListenerTo( FeatureElement fe )
  {
  //		box.addListener(this);
  //		for (Iterator iterator = box.getBoxes().iterator(); iterator.hasNext();) {
  //			MovingBox aBox = (MovingBox) iterator.next();
  //			addListenerTo(aBox);
  //		}
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

  /*
   * @see IDeltaListener#add(DeltaEvent)
   */
  public void add( GMLDocumentEvent event )
  {
  //    Object movingBox = ( (Model)event.receiver() ).getParent();
  //    viewer.refresh( movingBox, false );
  }

  /*
   * @see IDeltaListener#remove(DeltaEvent)
   */
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