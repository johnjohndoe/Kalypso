package org.kalypso.ogc.gml.mapactions;

import org.kalypso.ogc.MapPanel;
import org.kalypso.ogc.widgets.ToggleSelectWidget;
import org.kalypso.plugin.ImageProvider;

/**
 * @author belger
 */
public class ToggleSelectWidgetAction extends AbstractSelectWidgetAction
{
  public ToggleSelectWidgetAction( final MapPanel mapPanel )
  {
    super( mapPanel, new ToggleSelectWidget(), ImageProvider.IMAGE_MAPVIEW_TOGGLESELECT,
        "Selektion umkehren" );

  }

}