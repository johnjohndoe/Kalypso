package org.kalypso.ogc.gml.mapactions;

import org.kalypso.ogc.gml.mapmodel.MapPanel;
import org.kalypso.ogc.gml.widgets.SelectWidget;
import org.kalypso.ui.ImageProvider;

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
