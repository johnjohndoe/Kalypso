package org.kalypso.ogc.gml.table.wizard;

import java.io.PrintWriter;
import java.io.Writer;

import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.ui.metadoc.IExportableTableDocument;
import org.kalypso.util.io.CSV;

/**
 * ExportableTableDocument
 * 
 * @author schlienger
 */
public class ExportableLayerTable implements IExportableTableDocument
{
  private final LayerTableViewer m_layerTable;
  private boolean m_onlyRows = false;

  public ExportableLayerTable( final LayerTableViewer layerTable )
  {
    m_layerTable = layerTable;
  }

  /**
   * @see org.kalypso.ui.metadoc.IExportableDocument#exportDocument(java.io.Writer)
   */
  public void exportDocument( final Writer writer )
  {
    final String[][] csv = m_layerTable.exportTable( m_onlyRows );
    final PrintWriter pw = new PrintWriter( writer );
    CSV.writeCSV( csv, pw );
    pw.close();
  }

  /**
   * @see org.kalypso.ui.metadoc.IExportableTableDocument#setOnlySelectedRows(boolean)
   */
  public void setOnlySelectedRows( boolean flag )
  {
    m_onlyRows = flag;
  }
}
