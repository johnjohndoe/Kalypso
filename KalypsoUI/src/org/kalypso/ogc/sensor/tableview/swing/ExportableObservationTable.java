package org.kalypso.ogc.sensor.tableview.swing;

import java.io.OutputStream;
import java.io.OutputStreamWriter;

import org.apache.commons.io.IOUtils;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ui.metadoc.IExportableTableDocument;

/**
 * ExportableObservationTable
 * 
 * @author schlienger
 */
public class ExportableObservationTable implements IExportableTableDocument
{
  private final ObservationTable m_table;

  private boolean m_onlySelected;

  public ExportableObservationTable( final ObservationTable table )
  {
    m_table = table;
  }

  /**
   * @see org.kalypso.ui.metadoc.IExportableTableDocument#setOnlySelectedRows(boolean)
   */
  public void setOnlySelectedRows( final boolean flag )
  {
    m_onlySelected = flag;
  }

  /**
   * @see org.kalypso.ui.metadoc.IExportableDocument#exportDocument(java.io.OutputStream)
   */
  public void exportDocument( final OutputStream outs ) throws Exception
  {
    final ITuppleModel values = m_table.m_model
        .getValues( m_onlySelected ? m_table.getSelectedRows() : null );

    final OutputStreamWriter writer = new OutputStreamWriter( outs );
    try
    {
      ObservationUtilities.dump( values, ";", writer );
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }

  /**
   * @see org.kalypso.ui.metadoc.IExportableDocument#getDocumentExtension()
   */
  public String getDocumentExtension( )
  {
    return ".csv";
  }
}