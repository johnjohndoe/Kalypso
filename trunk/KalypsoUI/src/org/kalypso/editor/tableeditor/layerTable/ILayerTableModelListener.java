package org.kalypso.editor.tableeditor.layerTable;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureTypeProperty;

/**
 * @author bce
 */
public interface ILayerTableModelListener
{
  public void onColumnChanged( final FeatureTypeProperty ftp );

  public void onRowsChanged( final Feature row );
}
