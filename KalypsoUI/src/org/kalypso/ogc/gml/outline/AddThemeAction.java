package org.kalypso.ogc.gml.outline;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredViewer;
import org.kalypso.eclipse.jface.action.FullAction;
import org.kalypso.util.list.IListManipulator;

/**
 * @author belger
 */
public class AddThemeAction extends FullAction
{
  private final StructuredViewer m_viewer;
  private IListManipulator m_listManipulator;

  public AddThemeAction( final String text, final ImageDescriptor image, final String tooltipText, final StructuredViewer structuredViewer, final IListManipulator listManip )
  {
    super( text, image, tooltipText );
    
    m_viewer = structuredViewer;
    m_listManipulator = listManip;
  }
  
  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {
    m_listManipulator.addElement( ((IStructuredSelection)m_viewer.getSelection()).getFirstElement() );
  }
}
