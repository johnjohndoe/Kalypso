package org.kalypso.ogc.gml.mapactions;

import org.kalypso.ogc.MapPanel;
import org.kalypso.ogc.widgets.ZoomInWidget;
import org.kalypso.plugin.ImageProvider;

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
