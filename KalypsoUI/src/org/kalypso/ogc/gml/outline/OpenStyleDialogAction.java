package org.kalypso.ogc.gml.outline;

import org.deegree.model.feature.FeatureType;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.internal.Workbench;
import org.kalypso.editor.mapeditor.views.StyleEditorViewPart;
import org.kalypso.ogc.gml.IKalypsoLayer;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.KalypsoUserStyle;

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
        IKalypsoLayer layer = ( (ThemeStyleTreeObject)o ).getTheme().getLayer();

        if( part != null && layer instanceof KalypsoFeatureLayer )
        {
          FeatureType ft = ( (KalypsoFeatureLayer)layer ).getFeatureType();         
          KalypsoUserStyle kalypsoStyle = ( (ThemeStyleTreeObject)o ).getStyle();
          part.initStyleEditor( kalypsoStyle, ft );
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