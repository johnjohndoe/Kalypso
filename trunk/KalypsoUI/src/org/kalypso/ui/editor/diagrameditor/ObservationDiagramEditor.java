package org.kalypso.ui.editor.diagrameditor;

import java.awt.Frame;
import java.io.Writer;

import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.util.ListenerList;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.jfree.chart.ChartPanel;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.eclipse.util.SetContentThread;
import org.kalypso.ogc.sensor.diagview.ObservationTemplateHelper;
import org.kalypso.ogc.sensor.diagview.impl.LinkedDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.impl.ObservationDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationChart;
import org.kalypso.ogc.sensor.template.ITemplateEventListener;
import org.kalypso.ogc.sensor.template.TemplateEvent;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.AbstractEditorPart;

/**
 * Observation Diagram Editor.
 * 
 * @author schlienger
 */
public class ObservationDiagramEditor extends AbstractEditorPart implements ITemplateEventListener
{
  protected ObservationDiagramTemplate m_template = null;

  protected Frame m_diagFrame = null;

  protected ObservationChart m_obsChart = null;

  protected ObsDiagOutlinePage m_outline;

  private ListenerList m_listener;

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

    m_listener = new ListenerList();
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#getAdapter(java.lang.Class)
   */
  public Object getAdapter( Class adapter )
  {
    if( adapter == IContentOutlinePage.class )
    {
      // lazy loading
      if( m_outline == null || m_outline.getControl().isDisposed() )
      {
        // TODO check if ok to dispose when not null
        if( m_outline != null )
          m_outline.dispose();

        m_outline = new ObsDiagOutlinePage();
      }

      return m_outline;
    }

    return super.getAdapter( adapter );
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#dispose()
   */
  public void dispose( )
  {
    if( m_template != null )
    {
      m_template.removeTemplateEventListener( m_obsChart );
      m_template.dispose();
    }

    if( m_outline != null )
      m_outline.dispose();

    super.dispose();
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#doSaveInternal(org.eclipse.core.runtime.IProgressMonitor,
   *      org.eclipse.ui.IFileEditorInput)
   */
  protected void doSaveInternal( IProgressMonitor monitor,
      IFileEditorInput input ) throws CoreException
  {
    if( m_template == null )
      return;

    final SetContentThread thread = new SetContentThread( input.getFile(),
        !input.getFile().exists(), false, true, monitor )
    {
      protected void write( Writer writer ) throws Throwable
      {
        final ObsdiagviewType type = ObservationTemplateHelper.buildDiagramTemplateXML( m_template );
        
        ObservationTemplateHelper.saveDiagramTemplateXML( type, writer );
      }
    };

    thread.start();
    try
    {
      thread.join();
      
      if( thread.getFileException() != null )
        throw thread.getFileException();
      
      if( thread.getThrown() != null )
        throw new CoreException( KalypsoGisPlugin.createErrorStatus( "Diagrammvorlage speichern", thread.getThrown() ) );
    }
    catch( InterruptedException e )
    {
      e.printStackTrace();
      throw new CoreException( KalypsoGisPlugin.createErrorStatus( "Diagrammvorlage speichern", e ) );
    }
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
          final ObsdiagviewType baseTemplate = ObservationTemplateHelper
              .loadDiagramTemplateXML( input.getFile().getContents() );
          m_template = new LinkedDiagramTemplate( baseTemplate,
              ResourceUtilities.createURL( input.getFile() ) );

          m_obsChart = new ObservationChart( m_template );
          m_template.addTemplateEventListener( m_obsChart );

          if( m_outline != null )
            m_outline.setTemplate( m_template );

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

  /**
   * @see org.kalypso.ogc.sensor.template.ITemplateEventListener#onTemplateChanged(org.kalypso.ogc.sensor.template.TemplateEvent)
   */
  public void onTemplateChanged( TemplateEvent evt )
  {
    // TODO set the dirty flag
  }
}