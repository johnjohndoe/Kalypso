package org.kalypso.ogc.gml.outline;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.util.list.IListManipulator;

/**
 * @author belger
 */
public class MoveThemeDownAction extends AbstractOutlineAction
{
  public MoveThemeDownAction( final String text, final ImageDescriptor image, final String tooltipText, final GisMapOutlineViewer outlineViewer, final IListManipulator listManip )
  {
    super( text, image, tooltipText, outlineViewer, listManip );
  }
  
  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {
    getListManipulator().moveElementDown( ((IStructuredSelection)getOutlineviewer().getSelection()).getFirstElement() );
  }

  protected final void refresh()
  {
    boolean bEnable = false;
    
    final IStructuredSelection s = (IStructuredSelection)getOutlineviewer().getSelection();
      
//    if( !s.isEmpty() && (s.getFirstElement() instanceof PoolableKalypsoFeatureTheme))
    if( !s.isEmpty() && (s.getFirstElement() instanceof IKalypsoTheme))
    {    	  
      final Object[] elements = getOutlineviewer().getContentProvider().getElements( getOutlineviewer().getMapModell() );
          
      bEnable = ( elements[elements.length - 1] != s.getFirstElement() ); 
    }

    setEnabled( bEnable );
  }
}
