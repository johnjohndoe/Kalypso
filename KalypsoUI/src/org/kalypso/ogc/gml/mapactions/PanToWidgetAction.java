package org.kalypso.ogc.gml.mapactions;

import org.kalypso.ogc.MapPanel;
import org.kalypso.ogc.widgets.PanToWidget;
import org.kalypso.plugin.ImageProvider;

/**
 * @author belger
 */
public class PanToWidgetAction extends AbstractSelectWidgetAction
{
  public PanToWidgetAction( final MapPanel mapPanel )
  {
    super( mapPanel, new PanToWidget(), ImageProvider.IMAGE_MAPVIEW_PAN, "Ausschnitt verschieben" );
  }
}
