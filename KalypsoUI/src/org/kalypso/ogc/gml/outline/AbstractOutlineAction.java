package org.kalypso.ogc.gml.outline;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.kalypso.eclipse.jface.action.FullAction;
import org.kalypso.util.list.IListManipulator;

/**
 * @author belger
 */
public abstract class AbstractOutlineAction extends FullAction implements ISelectionChangedListener
{
  private final GisMapOutlineViewer m_outlineViewer;
  private IListManipulator m_listManipulator;

  public AbstractOutlineAction( final String text, final ImageDescriptor image, final String tooltipText, final GisMapOutlineViewer selectionProvider, final IListManipulator listManip )
  {
    super( text, image, tooltipText );
    
    m_outlineViewer = selectionProvider;
    m_listManipulator = listManip;
    
    m_outlineViewer.addSelectionChangedListener( this );
    
    refresh();
  }
  
  public void dispose()
  {
    m_outlineViewer.removeSelectionChangedListener( this );
  }
  
  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( final SelectionChangedEvent event )
  {
    refresh();
  }
  
  protected IListManipulator getListManipulator()
  {
    return m_listManipulator;
  }
  
  protected GisMapOutlineViewer getOutlineviewer()
  {
    return m_outlineViewer;
  }

  protected abstract void refresh();
}
