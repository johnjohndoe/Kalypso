package org.kalypso.ui.editor.diagrameditor;

import java.net.URL;
import java.util.Iterator;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerDropAdapter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.FileTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.internal.dialogs.ContainerCheckedTreeViewer;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.eclipse.ui.internal.dialogs.ContainerCheckedTreeViewer2;
import org.kalypso.eclipse.ui.views.contentouline.ContentOutlinePage2;
import org.kalypso.ogc.sensor.diagview.IDiagramCurve;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplateTheme;
import org.kalypso.ogc.sensor.diagview.impl.LinkedDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.impl.ObservationDiagramTemplate;
import org.kalypso.ogc.sensor.template.ITemplateEventListener;
import org.kalypso.ogc.sensor.template.TemplateEvent;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.diagrameditor.actions.RemoveThemeAction;

/**
 * ObsDiagOutlinePage
 * 
 * @author schlienger
 */
public class ObsDiagOutlinePage extends ContentOutlinePage2 implements
    ITemplateEventListener, ICheckStateListener
{
  protected LinkedDiagramTemplate m_template;

  private RemoveThemeAction m_removeThemeAction;

  //  private FilterAxesAction m_filterAxesAction;

  /**
   * @see org.kalypso.eclipse.ui.views.contentouline.ContentOutlinePage2#createTreeViewer(org.eclipse.swt.widgets.Composite)
   */
  protected TreeViewer createTreeViewer( final Composite parent )
  {
    return new ContainerCheckedTreeViewer2( parent, SWT.MULTI | SWT.H_SCROLL
        | SWT.V_SCROLL );
  }

  /**
   * @see org.eclipse.ui.part.IPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    super.createControl( parent );

    // drop support for files
    Transfer[] transfers = new Transfer[] { FileTransfer.getInstance() };

    final ContainerCheckedTreeViewer tv = (ContainerCheckedTreeViewer) getTreeViewer();

    tv.addDropSupport( DND.DROP_COPY | DND.DROP_MOVE, transfers,
        new DropAdapter( tv ) );

    tv.setLabelProvider( new ObsDiagTemplateLabelProvider() );
    tv.setContentProvider( new ObsDiagTemplateContentProvider() );
    tv.setInput( m_template );

    tv.addCheckStateListener( this );

    m_removeThemeAction = new RemoveThemeAction( this );
    //    m_filterAxesAction = new FilterAxesAction(this);
  }

  /**
   * @return the selected theme or null
   */
  public IDiagramTemplateTheme getSelectedTheme( )
  {
    final ISelection sel = getSelection();

    if( sel instanceof IStructuredSelection )
    {
      final Object element = ((IStructuredSelection) sel).getFirstElement();

      if( element instanceof IDiagramTemplateTheme )
        return (IDiagramTemplateTheme) element;

      if( element instanceof IDiagramCurve )
        return ((IDiagramCurve) element).getTheme();
    }

    return null;
  }

  /**
   * @return true if a theme is selected
   */
  public boolean isThemeSelected( )
  {
    final ISelection sel = getSelection();

    if( sel instanceof IStructuredSelection )
    {
      final Object element = ((IStructuredSelection) sel).getFirstElement();

      return element instanceof IDiagramTemplateTheme;
    }
    
    return false;
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

    //    toolBarManager.add( m_filterAxesAction );
    toolBarManager.add( m_removeThemeAction );

    actionBars.updateActionBars();
  }

  /**
   * This method must be called from the ui thread
   */
  protected void refreshViewer( )
  {
    getTreeViewer().refresh();
  }

  /**
   * This method must be called from the ui thread
   */
  protected void setTemplateAsInput( )
  {
    getTreeViewer().setInput( m_template );
  }

  /**
   * @see org.kalypso.ogc.sensor.template.ITemplateEventListener#onTemplateChanged(org.kalypso.ogc.sensor.template.TemplateEvent)
   */
  public void onTemplateChanged( TemplateEvent evt )
  {
    if( getTreeViewer() != null )
    {
      getSite().getShell().getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          refreshViewer();
        }
      } );
    }
  }

  /**
   * @param template
   */
  public void setTemplate( LinkedDiagramTemplate template )
  {
    if( m_template != null )
      m_template.removeTemplateEventListener( this );

    m_template = template;

    if( getTreeViewer() != null )
    {
      getSite().getShell().getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          setTemplateAsInput();
          refreshViewer();
        }
      } );
    }

    if( m_template != null )
      m_template.addTemplateEventListener( this );
  }

  /**
   * @see org.eclipse.ui.part.IPage#dispose()
   */
  public void dispose( )
  {
    if( m_removeThemeAction != null )
      m_removeThemeAction.dispose();
    
    if( m_template != null )
      m_template.removeTemplateEventListener( this );

    final ContainerCheckedTreeViewer tv = (ContainerCheckedTreeViewer) getTreeViewer();
    if( tv != null )
      tv.removeCheckStateListener( this );

    m_template = null;
  }

  /**
   * @see org.eclipse.jface.viewers.ICheckStateListener#checkStateChanged(org.eclipse.jface.viewers.CheckStateChangedEvent)
   */
  public void checkStateChanged( final CheckStateChangedEvent event )
  {
    final Object element = event.getElement();

    if( element instanceof IDiagramCurve )
    {
      final IDiagramCurve curve = (IDiagramCurve) element;
      curve.setShown( event.getChecked() );
    }
    else if( element instanceof IDiagramTemplateTheme )
    {
      final IDiagramTemplateTheme theme = (IDiagramTemplateTheme) element;

      for( final Iterator it = theme.getCurves().iterator(); it.hasNext(); )
      {
        final IDiagramCurve curve = (IDiagramCurve) it.next();
        curve.setShown( event.getChecked() );
      }
    }
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
            final IWorkspaceRoot wksp = ResourcesPlugin.getWorkspace()
                .getRoot();

            for( int i = 0; i < files.length; i++ )
            {
              IFile file = wksp.getFileForLocation( new Path( files[i] ) );
              file = (IFile) wksp.findMember( file.getFullPath() );
              final URL url = ResourceUtilities.createURL( file );

              m_template.addObservation( file.getName(), url, url
                  .toExternalForm(), "zml", false, null );
            }

            return Status.OK_STATUS;
          }
          catch( Exception e )
          {
            return new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0, "",
                e );
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