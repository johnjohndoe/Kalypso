package org.kalypso.editor.mapeditor.views;

import org.deegree.model.feature.FeatureType;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.editor.styleeditor.SLDEditorGuiBuilder;
import org.kalypso.ogc.gml.IKalypsoLayer;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ogc.gml.outline.ThemeStyleTreeObject;

/**
 * 
 *  
 */

public class StyleEditorViewPart extends ViewPart implements ISelectionChangedListener
{
  private ISelectionProvider gmop = null;

  private SLDEditorGuiBuilder guiBuilder = null;

  public void setSelectionChangedProvider( final ISelectionProvider selectionProvider )
  {
    if( this.gmop != selectionProvider )
    {
      this.gmop = selectionProvider;
      gmop.addSelectionChangedListener( this );
    }
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#dispose()
   */
  public void dispose()
  {
    super.dispose();
    if( gmop != null )
      gmop.removeSelectionChangedListener( this );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( Composite parent )
  {
    guiBuilder = new SLDEditorGuiBuilder( parent, this );
  }

  public void initStyleEditor( KalypsoUserStyle userStyle, FeatureType featureType )
  {
    guiBuilder.buildSWTGui( userStyle, featureType );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  public void setFocus()
  {
  // 
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( SelectionChangedEvent event )
  {
    Object o = ( (IStructuredSelection)event.getSelection() ).getFirstElement();
    if( o instanceof ThemeStyleTreeObject )
    {
      IKalypsoLayer layer = ( (ThemeStyleTreeObject)o ).getTheme().getLayer();
      if( !( layer instanceof KalypsoFeatureLayer ) )
        initStyleEditor( null, null );
      else
      {
        FeatureType ft = ( (KalypsoFeatureLayer)layer ).getFeatureType();
        KalypsoUserStyle kalypsoStyle = ( (ThemeStyleTreeObject)o ).getStyle();
        initStyleEditor( kalypsoStyle, ft );
      }
    }
    else if( o instanceof IKalypsoTheme )
      initStyleEditor( null, null );
  }
}