package org.kalypso.editor.mapeditor.actions;

import org.deegree.model.feature.FeatureType;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.internal.Workbench;
import org.kalypso.eclipse.jface.action.FullAction;
import org.kalypso.editor.mapeditor.GisMapOutlinePage;
import org.kalypso.editor.mapeditor.ThemeStyleTreeObject;
import org.kalypso.editor.mapeditor.views.StyleEditorViewPart;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.KalypsoUserStyle;

/**
 * @author belger
 */
public class OpenStyleDialogAction extends FullAction implements ISelectionChangedListener
{

  private final StructuredViewer m_viewer;

  private final GisMapOutlinePage m_page;

  public OpenStyleDialogAction( final String text, final ImageDescriptor image,
      final String tooltipText, final StructuredViewer structuredViewer, GisMapOutlinePage page )
  {
    super( text, image, tooltipText );

    m_viewer = structuredViewer;

    m_viewer.addSelectionChangedListener( this );

    m_page = page;

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

    StyleEditorViewPart part = null;
    IWorkbenchWindow window = Workbench.getInstance().getActiveWorkbenchWindow();
    Object o = ( (IStructuredSelection)m_viewer.getSelection() ).getFirstElement();

    try
    {
      part = (StyleEditorViewPart)window.getActivePage().showView(
          "org.kalypso.editor.mapeditor.views.styleeditor" );

      if( part != null )
      {
        part.setSelectionChangedProvider( m_page );
      }
      // if UserStyle selected path that on to styleeditor
      if( o instanceof ThemeStyleTreeObject )
      {
        KalypsoFeatureLayer layer = ( (ThemeStyleTreeObject)o ).getTheme().getLayer();

        FeatureType ft = layer.getFeatureType();
        KalypsoUserStyle kalypsoStyle = ( (ThemeStyleTreeObject)o ).getStyle();
        if( part != null )
          part.initStyleEditor( kalypsoStyle, ft );
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

  private void refresh()
  {
    boolean bEnable = false;

    final IStructuredSelection s = (IStructuredSelection)m_viewer.getSelection();

    if( s.getFirstElement() instanceof ThemeStyleTreeObject )
      bEnable = true;

    setEnabled( bEnable );
  }
}