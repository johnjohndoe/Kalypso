package org.kalypso.ogc.gml.map.actions;

import org.kalypso.ogc.gml.mapmodel.MapPanel;
import org.kalypso.ogc.gml.widgets.ZoomInByRectWidget;
import org.kalypso.ui.ImageProvider;

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
