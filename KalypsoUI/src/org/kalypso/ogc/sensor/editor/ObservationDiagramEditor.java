package org.kalypso.ogc.sensor.editor;

import java.awt.Frame;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IFileEditorInput;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.data.time.TimeSeriesCollection;
import org.kalypso.editor.AbstractEditorPart;
import org.kalypso.ogc.sensor.view.ShowObservationInDiagramJob;
import org.kalypso.ogc.sensor.zml.ZmlObservation;
import org.xml.sax.InputSource;

/**
 * @author schlienger
 *  
 */
public class ObservationDiagramEditor extends AbstractEditorPart
{
  private JFreeChart m_chart = null;

  protected final TimeSeriesCollection m_tsCol = new TimeSeriesCollection();

  /**
   * @see org.kalypso.editor.AbstractEditorPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( Composite parent )
  {
    super.createPartControl( parent );

    m_chart = ChartFactory
        .createTimeSeriesChart( "", "Datum", "Wert", m_tsCol, false, false, false );

    ChartPanel chartPanel = new ChartPanel( m_chart );
    chartPanel.setMouseZoomable( true, false );

    // SWT-AWT Brücke für die Darstellung von JFreeChart
    Frame vFrame = SWT_AWT.new_Frame( new Composite( parent, SWT.RIGHT | SWT.EMBEDDED ) );

    vFrame.setVisible( true );
    chartPanel.setVisible( true );
    vFrame.add( chartPanel );
  }

  /**
   * @see org.kalypso.editor.AbstractEditorPart#doSaveInternal(org.eclipse.core.runtime.IProgressMonitor,
   *      org.eclipse.ui.IFileEditorInput)
   */
  protected void doSaveInternal( IProgressMonitor monitor, IFileEditorInput input )
  {
    // todo
  }

  /**
   * @see org.kalypso.editor.AbstractEditorPart#load()
   */
  protected void loadInternal()
  {
    final IFileEditorInput input = (IFileEditorInput)getEditorInput();

    try
    {
      IFile f = input.getFile();
      InputSource ins = new InputSource( f.getContents() );
      ins.setEncoding( f.getCharset() );
      
      ZmlObservation obs = new ZmlObservation( f.getProject().getLocation().toOSString(), input.getName(), ins );

      setContentDescription( input.getFile().getName() );
      setPartName( input.getFile().getName() );
      
      getEditorSite().getShell().getDisplay().asyncExec( new ShowObservationInDiagramJob( m_tsCol, obs ) );
    }
    catch( Exception e1 )
    {
      e1.printStackTrace();
    }
  }
}
