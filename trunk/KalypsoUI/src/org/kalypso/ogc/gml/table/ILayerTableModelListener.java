package org.kalypso.ogc.gml.table;

import org.kalypso.ogc.gml.KalypsoFeature;

/**
 * @author bce
 */
public interface ILayerTableModelListener
{
  public void onRowsChanged( final KalypsoFeature fe);

  public void onColumnsChanged();
}
