package org.kalypso.editor.tableeditor.layerTable;

import org.kalypso.ogc.gml.KalypsoFeature;

/**
 * @author bce
 */
public interface ILayerTableModelListener
{
  public void onRowsChanged( final KalypsoFeature fe);

  public void onColumnsChanged();
}
