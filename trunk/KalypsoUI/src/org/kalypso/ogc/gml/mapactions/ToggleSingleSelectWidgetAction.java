package org.kalypso.ogc.gml.mapactions;

import org.kalypso.ogc.MapPanel;
import org.kalypso.ogc.widgets.SingleElementSelectWidget;
import org.kalypso.plugin.ImageProvider;

/**
 * @author belger
 */
public class ToggleSingleSelectWidgetAction extends AbstractSelectWidgetAction
{
  public ToggleSingleSelectWidgetAction( final MapPanel mapPanel )
  {
    super( mapPanel, new SingleElementSelectWidget(), ImageProvider.IMAGE_MAPVIEW_TOGGLESELECT, "Einzel-Selektion" );
   }

}
