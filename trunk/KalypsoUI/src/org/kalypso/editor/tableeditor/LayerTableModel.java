package org.kalypso.editor.tableeditor;

import org.kalypso.ogc.gml.KalypsoFeatureLayer;


/**
 * @author bce
 */
public class LayerTableModel
{
  private final KalypsoFeatureLayer myLayer;
  private final String[] myColumns;
  
  public LayerTableModel( final KalypsoFeatureLayer layer, final String[] columns )
  {
    myLayer = layer;
    myColumns = columns;
  }

  public KalypsoFeatureLayer getLayer()
  {
    return myLayer;
  }

  public String[] getColumns()
  {
    return myColumns;
  }
}
