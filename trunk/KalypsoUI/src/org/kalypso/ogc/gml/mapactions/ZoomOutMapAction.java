package org.kalypso.ogc.gml.mapactions;

import org.deegree.model.geometry.GM_Envelope;
import org.kalypso.ogc.MapPanel;
import org.kalypso.ogc.widgets.ChangeExtentCommand;
import org.kalypso.plugin.ImageProvider;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandTarget;

/**
 * @author belger
 */
public class ZoomOutMapAction extends AbstractCommandAction
{
  public ZoomOutMapAction( final ICommandTarget commandTarget, final MapPanel mapPanel )
  {
    super( commandTarget, mapPanel, null, ImageProvider.IMAGE_MAPVIEW_ZOOMOUT, "Auschnitt verkleinern" );
  }

  /**
   * @see org.kalypso.ogc.gml.mapactions.AbstractCommandAction#runInternal()
   */
  public ICommand runInternal()
  {
    final MapPanel mapPanel = getMapPanel();

    final GM_Envelope fullExtent = mapPanel.getZoomOutBoundingBox();
    return new ChangeExtentCommand( mapPanel, fullExtent );
  }
}
