package org.kalypso.ui.editor.diagrameditor;

import java.awt.Frame;

import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IFileEditorInput;
import org.jfree.chart.ChartPanel;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationChart;
import org.kalypso.ogc.sensor.template.ObservationTemplateHelper;
import org.kalypso.ui.editor.AbstractEditorPart;

/**
 * Observation Diagram Editor.
 * 
 * @author schlienger
 */
public class ObservationDiagramEditor extends AbstractEditorPart
{
  private IDiagramTemplate m_template = null;

  private Frame m_diagFrame = null;

  private ObservationChart m_obsChart = null;

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( Composite parent )
  {
    super.createPartControl( parent );

    // SWT-AWT Brücke für die Darstellung von JFreeChart
    m_diagFrame = SWT_AWT.new_Frame( new Composite( parent, SWT.RIGHT
        | SWT.EMBEDDED ) );

    JTextField txt = new JTextField( "Daten werden geladen..." );
    txt.setEditable( false );

    m_diagFrame.add( txt );
    m_diagFrame.setVisible( true );
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#dispose()
   */
  public void dispose( )
  {
    super.dispose();

    if( m_template != null )
      m_template.removeTemplateEventListener( m_obsChart );
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#doSaveInternal(org.eclipse.core.runtime.IProgressMonitor,
   *      org.eclipse.ui.IFileEditorInput)
   */
  protected void doSaveInternal( IProgressMonitor monitor,
      IFileEditorInput input )
  {
    // todo
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#loadInternal(org.eclipse.core.runtime.IProgressMonitor,
   *      org.eclipse.ui.IFileEditorInput)
   */
  protected void loadInternal( final IProgressMonitor monitor,
      final IFileEditorInput input )
  {
    monitor.beginTask( "Vorlage Laden", IProgressMonitor.UNKNOWN );

    final Runnable runnable = new Runnable()
    {
      public void run( )
      {
        try
        {
          m_template = ObservationTemplateHelper.loadDiagramTemplate( input
              .getFile() );

          m_obsChart = new ObservationChart( m_template );
          m_template.addTemplateEventListener( m_obsChart );

          m_diagFrame.removeAll();

          final ChartPanel chartPanel = new ChartPanel( m_obsChart );
          chartPanel.setMouseZoomable( true, false );
          m_diagFrame.add( chartPanel );

          m_diagFrame.setVisible( true );
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
      }
    };

    try
    {
      SwingUtilities.invokeAndWait( runnable );
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      e.printStackTrace();
    }
    finally
    {
      monitor.done();
    }
  }
}