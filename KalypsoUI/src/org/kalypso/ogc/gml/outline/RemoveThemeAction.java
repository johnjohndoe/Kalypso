package org.kalypso.ogc.gml.outline;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypso.util.list.IListManipulator;

/**
 * @author belger
 */
public class RemoveThemeAction extends AbstractOutlineAction
{
  public RemoveThemeAction( final String text, final ImageDescriptor image, final String tooltipText, final GisMapOutlineViewer outlineViewer, final IListManipulator listManip )
  {
    super( text, image, tooltipText, outlineViewer, listManip );
  }
  
  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {
    getListManipulator().removeElement( ((IStructuredSelection)getOutlineviewer().getSelection()).getFirstElement() );
  }

  protected void refresh()
  {
    final IStructuredSelection s = (IStructuredSelection)getOutlineviewer().getSelection();

    setEnabled( !s.isEmpty() );
  }
}
