package org.kalypso.ogc.gml.map;

import org.eclipse.swt.widgets.Shell;
import org.kalypso.ogc.gml.map.widgets.EditFeatureWidget;
import org.kalypso.ogc.gml.widgets.CreateGeometryFeatureWidget;
import org.kalypso.ogc.gml.widgets.PanToWidget;
import org.kalypso.ogc.gml.widgets.SelectWidget;
import org.kalypso.ogc.gml.widgets.SingleElementSelectWidget;
import org.kalypso.ogc.gml.widgets.ToggleSelectWidget;
import org.kalypso.ogc.gml.widgets.UnSelectWidget;
import org.kalypso.ogc.gml.widgets.ZoomInByRectWidget;
import org.kalypso.ogc.gml.widgets.ZoomInWidget;

/**
 * @author belger
 */
public class MapPanelHelper
{
  private MapPanelHelper()
  {
    // wird nicht instantitiert
  }
  
  public static final void createWidgetsForMapPanel( final Shell shell, final MapPanel panel )
  {
    panel.setWidget( MapPanel.WIDGET_ZOOM_IN, new ZoomInWidget() );
    panel.setWidget( MapPanel.WIDGET_ZOOM_IN_RECT, new ZoomInByRectWidget() );
    panel.setWidget( MapPanel.WIDGET_PAN, new PanToWidget() );
    panel.setWidget( MapPanel.WIDGET_EDIT_FEATURE, new EditFeatureWidget( shell ) );
    panel.setWidget( MapPanel.WIDGET_CREATE_FEATURE, new CreateGeometryFeatureWidget() );
    panel.setWidget( MapPanel.WIDGET_SELECT, new SelectWidget() );
    panel.setWidget( MapPanel.WIDGET_UNSELECT, new UnSelectWidget() );
    panel.setWidget( MapPanel.WIDGET_TOGGLE_SELECT, new ToggleSelectWidget() );
    panel.setWidget( MapPanel.WIDGET_SINGLE_SELECT, new SingleElementSelectWidget() );
  }

}
