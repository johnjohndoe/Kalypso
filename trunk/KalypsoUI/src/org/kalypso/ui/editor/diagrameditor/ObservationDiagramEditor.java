package org.kalypso.ui.editor.diagrameditor;

import java.awt.Frame;
import java.io.Writer;
import java.net.URL;

import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.jfree.chart.ChartPanel;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.eclipse.util.SetContentThread;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagramTemplateUtils;
import org.kalypso.ogc.sensor.diagview.impl.LinkedDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationChart;
import org.kalypso.ogc.sensor.template.ITemplateEventListener;
import org.kalypso.ogc.sensor.template.TemplateEvent;
import org.kalypso.ogc.sensor.template.TemplateStorage;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.AbstractEditorPart;

/**
 * Observation Diagram Editor.
 * 
 * @author schlienger
 */
public class ObservationDiagramEditor extends AbstractEditorPart implements
    ITemplateEventListener
{
  protected final LinkedDiagramTemplate m_template = new LinkedDiagramTemplate();

  protected Frame m_diagFrame = null;

  protected ObservationChart m_obsChart = null;

  protected ObsDiagOutlinePage m_outline = null;

  private boolean m_dirty = false;

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( Composite parent )
  {
    super.createPartControl( parent );

    // SWT-AWT Brücke für die Darstellung von JFreeChart
    m_diagFrame = SWT_AWT.new_Frame( new Composite( parent, SWT.RIGHT
        | SWT.EMBEDDED ) );

    // listener on template in order to set dirty flag when template changes
    m_template.addTemplateEventListener( this );

    try
    {
      m_obsChart = new ObservationChart( m_template );
      m_template.addTemplateEventListener( m_obsChart );

      final ChartPanel chartPanel = new ChartPanel( m_obsChart );
      chartPanel.setMouseZoomable( true, false );
      m_diagFrame.add( chartPanel );

      m_diagFrame.setVisible( true );
    }
    catch( SensorException e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#getAdapter(java.lang.Class)
   */
  public Object getAdapter( Class adapter )
  {
    if( adapter == IContentOutlinePage.class )
    {
      // lazy loading
      if( m_outline == null || m_outline.getControl() != null && m_outline.getControl().isDisposed() )
      {
        // TODO check if ok to dispose when not null
        if( m_outline != null )
          m_outline.dispose();

        m_outline = new ObsDiagOutlinePage();
        m_outline.setTemplate( m_template );
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
      m_template.removeTemplateEventListener( this );
      m_template.removeTemplateEventListener( m_obsChart );
      m_template.dispose();
    }

    if( m_outline != null )
      m_outline.dispose();
    
    if( m_obsChart != null )
      m_obsChart.dispose();

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
        final ObsdiagviewType type = DiagramTemplateUtils
            .buildDiagramTemplateXML( m_template );

        DiagramTemplateUtils.saveDiagramTemplateXML( type, writer );

        resetDirty();
      }
    };

    thread.start();
    try
    {
      thread.join();

      if( thread.getFileException() != null )
        throw thread.getFileException();

      if( thread.getThrown() != null )
        throw new CoreException( KalypsoGisPlugin.createErrorStatus(
            "Diagrammvorlage speichern", thread.getThrown() ) );
      
      fireDirty();
    }
    catch( InterruptedException e )
    {
      e.printStackTrace();
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Diagrammvorlage speichern", e ) );
    }
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#loadInternal(org.eclipse.core.runtime.IProgressMonitor,
   *      org.eclipse.ui.IFileEditorInput)
   */
  protected void loadInternal( final IProgressMonitor monitor,
      final IStorageEditorInput input )
  {
    monitor.beginTask( "Vorlage Laden", IProgressMonitor.UNKNOWN );

    try
    {
      final IStorage storage = input.getStorage();

      if( storage instanceof TemplateStorage )
      {
        final TemplateStorage ts = (TemplateStorage) storage;
        m_template.setTitle( ts.getName() );
        m_template.addObservation( ts.getName(), ts.getContext(), ts.getHref(),
            "zml", false, null );
      }
      else
      {
        final ObsdiagviewType baseTemplate = DiagramTemplateUtils
            .loadDiagramTemplateXML( storage.getContents() );

        final String strUrl = ResourceUtilities.createURLSpec( input
            .getStorage().getFullPath() );
        m_template.setBaseTemplate( baseTemplate, new URL( strUrl ) );
      }
    }
    catch( Exception e )
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
    if( evt.isType( TemplateEvent.TYPE_ADD | TemplateEvent.TYPE_REMOVE
        | TemplateEvent.TYPE_REMOVE_ALL ) )
    {
      m_dirty = true;

      getSite().getShell().getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          fireDirty();
        }
      } );
    }
  }

  protected void resetDirty( )
  {
    m_dirty = false;
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#isDirty()
   */
  public boolean isDirty( )
  {
    return m_dirty;
  }

  /**
   * @return chart
   */
  public ObservationChart getChart( )
  {
    return m_obsChart;
  }

  /**
   * @return template
   */
  public LinkedDiagramTemplate getTemplate( )
  {
    return m_template;
  }
}