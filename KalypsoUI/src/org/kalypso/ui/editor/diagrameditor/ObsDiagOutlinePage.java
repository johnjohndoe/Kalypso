package org.kalypso.ui.editor.diagrameditor;

import java.io.File;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerDropAdapter;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.FileTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.diagview.IDiagramCurve;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplateTheme;
import org.kalypso.ogc.sensor.diagview.impl.ObservationDiagramTemplate;
import org.kalypso.ogc.sensor.template.ITemplateEventListener;
import org.kalypso.ogc.sensor.template.TemplateEvent;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.diagrameditor.actions.RemoveThemeAction;

/**
 * ObsDiagOutlinePage
 * 
 * @author schlienger
 */
public class ObsDiagOutlinePage extends ContentOutlinePage implements
    ITemplateEventListener
{
  protected ObservationDiagramTemplate m_template;
  private RemoveThemeAction m_removeThemeAction;

  /**
   * @see org.eclipse.ui.views.contentoutline.ContentOutlinePage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    super.createControl( parent );

    // drop support for files
    Transfer[] transfers = new Transfer[] { FileTransfer.getInstance() };
    getTreeViewer().addDropSupport( DND.DROP_COPY | DND.DROP_MOVE, transfers,
        new DropAdapter( getTreeViewer() ) );

    getTreeViewer().setContentProvider( new ObsDiagTemplateContentProvider() );
    getTreeViewer().setInput( m_template );
    
    m_removeThemeAction = new RemoveThemeAction( this );
  }
  
  /**
   * @return the selected theme or null
   */
  public IDiagramTemplateTheme getSelectedTheme()
  {
    final ISelection sel = getSelection();
    
    if( sel instanceof IStructuredSelection )
    {
      final Object element = ((IStructuredSelection)sel).getFirstElement();

      if( element instanceof IDiagramTemplateTheme )
        return (IDiagramTemplateTheme) element;
      
      if( element instanceof IDiagramCurve )
        return ((IDiagramCurve)element).getTheme();
    }
    
    return null;
  }
  
  /**
   * @return template
   */
  public ObservationDiagramTemplate getTemplate( )
  {
    return m_template;
  }
  
  /**
   * @see org.eclipse.ui.part.IPage#setActionBars(org.eclipse.ui.IActionBars)
   */
  public void setActionBars( IActionBars actionBars )
  {
    final IToolBarManager toolBarManager = actionBars.getToolBarManager();
    toolBarManager.add( m_removeThemeAction );
    actionBars.updateActionBars();
  }

  /**
   * This method must be called from the ui thread
   */
  protected void refreshViewer()
  {
    if( getTreeViewer() != null )
      getTreeViewer().refresh();
  }

  /**
   * This method must be called from the ui thread
   */
  protected void setTemplateAsInput()
  {
    getTreeViewer().setInput( m_template );
  }
  
  /**
   * @see org.kalypso.ogc.sensor.template.ITemplateEventListener#onTemplateChanged(org.kalypso.ogc.sensor.template.TemplateEvent)
   */
  public void onTemplateChanged( TemplateEvent evt )
  {
    getSite().getShell().getDisplay().asyncExec( new Runnable()
    {
      public void run( )
      {
        refreshViewer();
      }
    } );
  }

  /**
   * @param template
   */
  public void setTemplate( ObservationDiagramTemplate template )
  {
    if( m_template != null )
      m_template.removeTemplateEventListener( this );

    m_template = template;

    getSite().getShell().getDisplay().asyncExec( new Runnable()
    {
      public void run( )
      {
        setTemplateAsInput();
        refreshViewer();
      }
    } );

    if( m_template != null )
      m_template.addTemplateEventListener( this );
  }

  /**
   * @see org.eclipse.ui.part.IPage#dispose()
   */
  public void dispose( )
  {
    if( m_template != null )
      m_template.removeTemplateEventListener( this );

    m_template = null;
  }

  /**
   * DropAdapter
   * 
   * @author schlienger
   */
  private class DropAdapter extends ViewerDropAdapter
  {
    /**
     * @param viewer
     */
    protected DropAdapter( Viewer viewer )
    {
      super( viewer );

      setScrollExpandEnabled( false );
      setFeedbackEnabled( false );
    }

    /**
     * @see org.eclipse.jface.viewers.ViewerDropAdapter#performDrop(java.lang.Object)
     */
    public boolean performDrop( Object data )
    {
      if( m_template == null )
        return false;
      
      final String[] files = (String[]) data;

      final Job updateTemplateJob = new Job( "Diagram aktualisieren" )
      {
        protected IStatus run( IProgressMonitor monitor )
        {
          monitor.beginTask( getName(), IProgressMonitor.UNKNOWN );

          try
          {
            for( int i = 0; i < files.length; i++ )
            {
              final IObservation obs = ZmlFactory.parseXML( new File( files[i] ).toURL(), files[i] );

              m_template.addObservation( obs, null );
            }
            
            return Status.OK_STATUS;
          }
          catch( Exception e )
          {
            return new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0, "", e );
          }
          finally
          {
            monitor.done();
          }
        }
      };

      updateTemplateJob.schedule();
      
      return true;
    }

    /**
     * @see org.eclipse.jface.viewers.ViewerDropAdapter#validateDrop(java.lang.Object,
     *      int, org.eclipse.swt.dnd.TransferData)
     */
    public boolean validateDrop( Object target, int operation,
        TransferData transferType )
    {
      if( !FileTransfer.getInstance().isSupportedType( transferType ) )
        return false;

      // TODO maybe check that it is a ZML-File...

      return true;
    }
  }
}