package org.kalypso.ogc.gml.outline;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.internal.Workbench;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ui.editor.mapeditor.views.StyleEditorViewPart;

/**
 * @author belger
 */
public class OpenStyleDialogAction extends AbstractOutlineAction
{
  public OpenStyleDialogAction( final String text, final ImageDescriptor image,
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
    StyleEditorViewPart part = null;
    IWorkbenchWindow window = Workbench.getInstance().getActiveWorkbenchWindow();    
    Object o = ( (IStructuredSelection)getOutlineviewer().getSelection() ).getFirstElement();

    try
    {
      part = (StyleEditorViewPart)window.getActivePage().showView(
          "org.kalypso.editor.mapeditor.views.styleeditor" );

      if( part != null )
        part.setSelectionChangedProvider( getOutlineviewer() );

      // if UserStyle selected path that on to styleeditor
      if( o instanceof ThemeStyleTreeObject )
      {
        final IKalypsoTheme theme = ( (ThemeStyleTreeObject)o ).getTheme();                   

        if( part != null && theme instanceof IKalypsoFeatureTheme )
        {                 
          KalypsoUserStyle kalypsoStyle = ( (ThemeStyleTreeObject)o ).getStyle();
          part.initStyleEditor( kalypsoStyle, (IKalypsoFeatureTheme)theme );
        }
        else
          part.initStyleEditor( null, null );
      }
      else if( o instanceof IKalypsoTheme )
        part.initStyleEditor( null, null );
    }
    catch( Exception e )
    {
      e.printStackTrace();
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

    if( s.getFirstElement() instanceof ThemeStyleTreeObject )
      bEnable = true;    
    setEnabled( bEnable );
  }
}