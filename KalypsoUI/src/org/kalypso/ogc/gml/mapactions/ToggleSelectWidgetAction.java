package org.kalypso.ogc.gml.mapactions;

import org.kalypso.ogc.gml.mapmodel.MapPanel;
import org.kalypso.ogc.gml.widgets.ToggleSelectWidget;
import org.kalypso.ui.ImageProvider;

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