package org.kalypso.ogc.sensor.editor;

import java.awt.Frame;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IFileEditorInput;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.kalypso.editor.AbstractEditorPart;
import org.kalypso.ogc.sensor.diagview.jfreechart.ChartFactory;
import org.kalypso.ogc.sensor.template.DiagramViewTemplate;
import org.kalypso.ogc.sensor.template.ITemplateListener;
import org.kalypso.util.factory.FactoryException;

/**
 * Observation Diagram Editor.
 * 
 * @author schlienger
 */
public class ObservationDiagramEditor extends AbstractEditorPart implements ITemplateListener
{
  private DiagramViewTemplate m_template = null;

  private Frame m_vFrame = null;

  /**
   * @see org.kalypso.editor.AbstractEditorPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( Composite parent )
  {
    super.createPartControl( parent );

    // SWT-AWT Brücke für die Darstellung von JFreeChart
    m_vFrame = SWT_AWT.new_Frame( new Composite( parent, SWT.RIGHT | SWT.EMBEDDED ) );
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
   * @see org.kalypso.editor.AbstractEditorPart#loadInternal(org.eclipse.core.runtime.IProgressMonitor,
   *      org.eclipse.ui.IFileEditorInput)
   */
  protected void loadInternal( final IProgressMonitor monitor, final IFileEditorInput input )
  {
    m_template = new DiagramViewTemplate( input.getFile(), monitor );
    m_template.addListener( this );
  }

  /**
   * @see org.kalypso.ogc.sensor.template.ITemplateListener#onTemplateLoaded()
   */
  public void onTemplateLoaded()
  {
    try
    {
      JFreeChart chart = ChartFactory.createObservationChart( m_template );

      ChartPanel chartPanel = new ChartPanel( chart );
      chartPanel.setMouseZoomable( true, false );
      chartPanel.setVisible( true );
      
      m_vFrame.add( chartPanel );
      m_vFrame.setVisible( true );
    }
    catch( FactoryException e )
    {
      // TODO handling
      throw new RuntimeException( e );
    }
  }
}