package org.kalypso.ogc.gml.table;

import org.deegree.model.feature.Feature;

/**
 * @author bce
 */
public interface ILayerTableModelListener
{
  public void onRowsChanged( final Feature fe);

  public void onColumnsChanged();
}
