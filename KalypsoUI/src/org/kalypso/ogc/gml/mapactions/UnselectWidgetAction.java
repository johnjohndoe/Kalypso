package org.kalypso.ogc.gml.mapactions;

import org.kalypso.ogc.MapPanel;
import org.kalypso.ogc.widgets.UnSelectWidget;
import org.kalypso.plugin.ImageProvider;

/**
 * @author belger
 */
public class UnselectWidgetAction extends AbstractSelectWidgetAction
{
  public UnselectWidgetAction( final MapPanel mapPanel )
  {
    super( mapPanel, new UnSelectWidget(), ImageProvider.IMAGE_MAPVIEW_UNSELECT, "Deselektieren" );
  }

}
