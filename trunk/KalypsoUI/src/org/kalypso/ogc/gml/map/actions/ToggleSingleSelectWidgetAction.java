package org.kalypso.ogc.gml.map.actions;

import org.kalypso.ogc.gml.mapmodel.MapPanel;
import org.kalypso.ogc.gml.widgets.SingleElementSelectWidget;
import org.kalypso.ui.ImageProvider;

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
