package org.kalypso.ogc.gml.mapactions;

import org.kalypso.ogc.MapPanel;
import org.kalypso.ogc.widgets.ZoomInByRectWidget;
import org.kalypso.plugin.ImageProvider;

/**
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp</a>
 */
public class ZoomInByRectWidgetAction extends AbstractSelectWidgetAction
{
  public ZoomInByRectWidgetAction( final MapPanel mapPanel )
  {
    super( mapPanel, new ZoomInByRectWidget(), ImageProvider.IMAGE_MAPVIEW_ZOOMIN, "Ausschnitt vergrössern" );
  }

}
