package org.kalypso.editor.mapeditor.actions;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredViewer;
import org.kalypso.eclipse.jface.action.FullAction;
import org.kalypso.util.list.IListManipulator;

/**
 * @author belger
 */
public class MoveThemeUpAction extends FullAction implements ISelectionChangedListener
{
  private final StructuredViewer m_viewer;
  private IListManipulator m_listManipulator;

  public MoveThemeUpAction( final String text, final ImageDescriptor image, final String tooltipText, final StructuredViewer structuredViewer, final IListManipulator listManip )
  {
    super( text, image, tooltipText );
    
    m_viewer = structuredViewer;
    m_listManipulator = listManip;
    
    m_viewer.addSelectionChangedListener( this );
    
    refresh();
  }
  
  public void dispose()
  {
    m_viewer.removeSelectionChangedListener( this );
  }
  
  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {
    m_listManipulator.moveElementUp( ((IStructuredSelection)m_viewer.getSelection()).getFirstElement() );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( final SelectionChangedEvent event )
  {
    refresh();
  }

  private void refresh()
  {
    boolean bEnable = false;
    
    final IStructuredSelection s = (IStructuredSelection)m_viewer.getSelection();
    if( !s.isEmpty() )
    {
      final Object[] elements = ((IStructuredContentProvider)m_viewer.getContentProvider()).getElements(m_viewer.getInput());
          
      bEnable = ( elements[0] != s.getFirstElement() ); 
    }

    setEnabled( bEnable );
  }
}
