package org.kalypso.ogc.gml.map.actions;

import org.deegree.model.geometry.GM_Envelope;
import org.kalypso.ogc.gml.command.ChangeExtentCommand;
import org.kalypso.ogc.gml.mapmodel.MapPanel;
import org.kalypso.ui.ImageProvider;
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
   * @see org.kalypso.ogc.gml.map.actions.AbstractCommandAction#runInternal()
   */
  public ICommand runInternal()
  {
    final MapPanel mapPanel = getMapPanel();

    final GM_Envelope fullExtent = mapPanel.getZoomOutBoundingBox();
    return new ChangeExtentCommand( mapPanel, fullExtent );
  }
}
