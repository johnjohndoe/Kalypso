package org.kalypso.ogc.sensor.diagview.jfreechart;

import java.io.OutputStream;

import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.kalypso.ui.metadoc.IExportableDocument;

/**
 * ExportableChart
 * 
 * @author schlienger
 */
public class ExportableChart implements IExportableDocument
{
  public final static String EXT_JPEG = ".jpg";
  public final static String EXT_PNG = ".png";
  
  private final JFreeChart m_chart;
  private final String m_fileExt;
  private final int m_width;
  private final int m_height;

  public ExportableChart( final JFreeChart chart, final String fileExt, final int width, final int height )
  {
    m_chart = chart;
    m_fileExt = fileExt;
    m_width = width;
    m_height = height;
  }

  /**
   * @see org.kalypso.ui.metadoc.IExportableDocument#exportDocument(java.io.OutputStream)
   */
  public void exportDocument( final OutputStream outs ) throws Exception
  {
    if( m_fileExt.equalsIgnoreCase( EXT_JPEG ) )
      ChartUtilities.writeChartAsJPEG( outs, m_chart, m_width, m_height );
    else if( m_fileExt.equalsIgnoreCase( EXT_PNG ) )
      ChartUtilities.writeChartAsPNG( outs, m_chart, m_width, m_height );
    else
      throw new IllegalStateException( "File extension not supported: " + m_fileExt );
  }

  /**
   * @see org.kalypso.ui.metadoc.IExportableDocument#getDocumentExtension()
   */
  public String getDocumentExtension( )
  {
    return m_fileExt;
  }
}
