package org.kalypso.ogc.gml.mapactions;

import org.kalypso.ogc.gml.mapmodel.MapPanel;
import org.kalypso.ogc.gml.widgets.ZoomInWidget;
import org.kalypso.ui.ImageProvider;

/**
 * @author belger
 */
public class ZoomInWidgetAction extends AbstractSelectWidgetAction
{
  public ZoomInWidgetAction( final MapPanel mapPanel )
  {
    super( mapPanel, new ZoomInWidget(), ImageProvider.IMAGE_MAPVIEW_ZOOMIN, "Ausschnitt vergrössern" );
  }

}
