package org.kalypso.ogc.gml.map.actions;

import org.kalypso.ogc.gml.mapmodel.MapPanel;
import org.kalypso.ogc.gml.widgets.PanToWidget;
import org.kalypso.ui.ImageProvider;

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
