package org.kalypso.ogc.gml.mapactions;

import org.kalypso.ogc.MapPanel;
import org.kalypso.ogc.widgets.SelectWidget;
import org.kalypso.plugin.ImageProvider;

/**
 * @author belger
 */
public class SelectWidgetAction extends AbstractSelectWidgetAction
{
  public SelectWidgetAction( final MapPanel mapPanel )
  {
    super( mapPanel, new SelectWidget(), ImageProvider.IMAGE_MAPVIEW_SELECT, "Selektieren" );
  }

}
