package org.kalypso.editor.tableeditor.layerTable;

import org.deegree.model.feature.Feature;

/**
 * @author bce
 */
public interface ILayerTableModelListener
{
  public void onRowsChanged( final Feature row );

  public void onColumnsChanged();
}
