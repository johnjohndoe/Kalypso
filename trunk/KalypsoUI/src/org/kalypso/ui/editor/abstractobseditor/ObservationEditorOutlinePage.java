package org.kalypso.ui.editor.abstractobseditor;

import java.net.URL;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
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
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.eclipse.ui.views.contentouline.ContentOutlinePage2;
import org.kalypso.ogc.sensor.template.IObsViewEventListener;
import org.kalypso.ogc.sensor.template.ObsView;
import org.kalypso.ogc.sensor.template.ObsViewEvent;
import org.kalypso.ogc.sensor.template.ObsViewItem;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.abstractobseditor.actions.RemoveThemeAction;

/**
 * AbstractObsOutlinePage
 * 
 * @author schlienger
 */
public class ObservationEditorOutlinePage extends ContentOutlinePage2 implements IObsViewEventListener,
    ICheckStateListener
{
  protected ObsView m_view;

  private final AbstractObservationEditor m_editor;

  private IAction m_removeThemeAction;

  public ObservationEditorOutlinePage( final AbstractObservationEditor editor )
  {
    m_editor = editor;
  }

  /**
   * @see org.eclipse.ui.part.IPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    super.createControl( parent );

    final ContainerCheckedTreeViewer tv = (ContainerCheckedTreeViewer)getTreeViewer();

    // drop support for files
    Transfer[] transfers = new Transfer[]
    { FileTransfer.getInstance() };
    tv.addDropSupport( DND.DROP_COPY | DND.DROP_MOVE, transfers, new DropAdapter( tv, m_editor ) );

    tv.setLabelProvider( new ObsTemplateLabelProvider() );
    tv.setContentProvider( new ObsTemplateContentProvider() );
    setView( m_view );

    tv.addCheckStateListener( this );

    m_removeThemeAction = new RemoveThemeAction( this );
  }

  /**
   * @see org.kalypso.eclipse.ui.views.contentouline.ContentOutlinePage2#createTreeViewer(org.eclipse.swt.widgets.Composite)
   */
  protected TreeViewer createTreeViewer( final Composite parent )
  {
    return new ContainerCheckedTreeViewer( parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL );
  }

  /**
   * @return the selected theme or null
   */
  public ObsViewItem getSelectedItem()
  {
    final ISelection sel = getSelection();

    if( sel instanceof IStructuredSelection )
    {
      final Object element = ( (IStructuredSelection)sel ).getFirstElement();
      if( element instanceof ObsViewItem )
        return (ObsViewItem)element;
    }

    return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.template.IObsViewEventListener#onObsViewChanged(org.kalypso.ogc.sensor.template.ObsViewEvent)
   */
  public void onObsViewChanged( final ObsViewEvent evt )
  {
    final TreeViewer tv = getTreeViewer();
    if( tv != null && !tv.getControl().isDisposed() )
    {
      tv.getControl().getDisplay().asyncExec( new Runnable()
      {
        public void run()
        {
          tv.refresh();
          refreshCheckState( (ContainerCheckedTreeViewer)tv );
        }
      } );
    }
  }

  public void setView( final ObsView view )
  {
    if( m_view != null )
      m_view.removeObsViewListener( this );

    m_view = view;

    final ContainerCheckedTreeViewer tv = (ContainerCheckedTreeViewer)getTreeViewer();
    if( tv != null )
    {
      getSite().getShell().getDisplay().syncExec( new Runnable()
      {
        public void run()
        {
          tv.setInput( m_view );

          refreshCheckState( tv );
        }
      } );
    }

    if( m_view != null )
      m_view.addObsViewEventListener( this );
  }

  protected void refreshCheckState( final ContainerCheckedTreeViewer tv )
  {
    if( m_view != null )
    {
      final ObsViewItem[] items = m_view.getItems();
      for( int i = 0; i < items.length; i++ )
      {
        final ObsViewItem item = items[i];
        tv.setChecked( item, item.isShown() );
      }
    }
  }

  /**
   * @see org.eclipse.ui.part.IPage#dispose()
   */
  public void dispose()
  {
    if( m_view != null )
      m_view.removeObsViewListener( this );

    final ContainerCheckedTreeViewer tv = (ContainerCheckedTreeViewer)getTreeViewer();
    if( tv != null )
      tv.removeCheckStateListener( this );

    m_view = null;
  }

  /**
   * @see org.eclipse.jface.viewers.ICheckStateListener#checkStateChanged(org.eclipse.jface.viewers.CheckStateChangedEvent)
   */
  public void checkStateChanged( final CheckStateChangedEvent event )
  {
    final Object element = event.getElement();

    if( element instanceof ObsViewItem )
    {
      final ObsViewItem curve = (ObsViewItem)element;
      curve.setShown( event.getChecked() );
    }
  }

  public AbstractObservationEditor getEditor()
  {
    return m_editor;
  }

  public ObsView getView()
  {
    return m_view;
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
   * DropAdapter
   * 
   * @author schlienger
   */
  private class DropAdapter extends ViewerDropAdapter
  {
    protected final AbstractObservationEditor m_editor2;

    protected DropAdapter( Viewer viewer, AbstractObservationEditor editor )
    {
      super( viewer );
      m_editor2 = editor;

      setScrollExpandEnabled( false );
      setFeedbackEnabled( false );
    }

    /**
     * @see org.eclipse.jface.viewers.ViewerDropAdapter#performDrop(java.lang.Object)
     */
    public boolean performDrop( Object data )
    {
      if( m_view == null )
        return false;

      final String[] files = (String[])data;

      final AbstractObservationEditor editor = m_editor2;

      final Job updateTemplateJob = new Job( "Vorlage aktualisieren" )
      {
        protected IStatus run( IProgressMonitor monitor )
        {
          monitor.beginTask( getName(), IProgressMonitor.UNKNOWN );

          try
          {
            final IWorkspaceRoot wksp = ResourcesPlugin.getWorkspace().getRoot();

            for( int i = 0; i < files.length; i++ )
            {
              IFile file = wksp.getFileForLocation( new Path( files[i] ) );
              file = (IFile)wksp.findMember( file.getFullPath() );
              final URL url = ResourceUtilities.createURL( file );

              editor.loadObservation( url, url.toExternalForm() );
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
     * @see org.eclipse.jface.viewers.ViewerDropAdapter#validateDrop(java.lang.Object, int,
     *      org.eclipse.swt.dnd.TransferData)
     */
    public boolean validateDrop( Object target, int operation, TransferData transferType )
    {
      if( !FileTransfer.getInstance().isSupportedType( transferType ) )
        return false;

      // TODO maybe check that it is a ZML-File...

      return true;
    }
  }

}