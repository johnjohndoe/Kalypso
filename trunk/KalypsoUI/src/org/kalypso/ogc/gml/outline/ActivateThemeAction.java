package org.kalypso.ogc.gml.outline;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.ActivateThemeCommand;
import org.kalypso.ogc.gml.mapmodel.IMapModell;

/**
 * @author belger
 */
public class ActivateThemeAction extends AbstractOutlineAction
{
  public ActivateThemeAction( final String text, final ImageDescriptor image,
      final String tooltipText, final GisMapOutlineViewer outlineViewer )
  {
    super( text, image, tooltipText, outlineViewer, null );

    refresh();
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {
    final Object o = ( (IStructuredSelection)getOutlineviewer().getSelection() ).getFirstElement();
    
    if( o instanceof IKalypsoTheme )
    {
      final IMapModell mapModell = getOutlineviewer().getMapModell();
      getOutlineviewer().postCommand( new ActivateThemeCommand( mapModell, (IKalypsoTheme)o ), null );
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( final SelectionChangedEvent event )
  {
    refresh();
  }

  protected void refresh()
  {
    boolean bEnable = false;

    final IStructuredSelection s = (IStructuredSelection)getOutlineviewer().getSelection();

    if( s.getFirstElement() instanceof IKalypsoTheme )
      bEnable = true;    
    setEnabled( bEnable );
  }
}