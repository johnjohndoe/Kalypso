package org.kalypso.editor.tableeditor.layerTable;

import org.kalypso.ogc.sort.DisplayContext;

/**
 * @author bce
 */
public interface ILayerTableModelListener
{
  public void onRowsChanged( final DisplayContext dc );

  public void onColumnsChanged();
}
