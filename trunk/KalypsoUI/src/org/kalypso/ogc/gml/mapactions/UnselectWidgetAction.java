package org.kalypso.ogc.gml.mapactions;

import org.kalypso.ogc.gml.mapmodel.MapPanel;
import org.kalypso.ogc.gml.widgets.UnSelectWidget;
import org.kalypso.ui.ImageProvider;

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
