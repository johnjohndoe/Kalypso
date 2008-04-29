package org.kalypso.ui.editor.abstractobseditor;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;

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
import org.kalypso.contribs.eclipse.ui.views.contentoutline.ContentOutlinePage2;
import org.kalypso.contribs.java.util.Arrays;
import org.kalypso.ogc.sensor.template.IObsViewEventListener;
import org.kalypso.ogc.sensor.template.ObsView;
import org.kalypso.ogc.sensor.template.ObsViewEvent;
import org.kalypso.ogc.sensor.template.ObsViewItem;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.abstractobseditor.actions.RemoveThemeAction;
import org.kalypso.ui.editor.abstractobseditor.actions.SetIgnoreTypesAction;
import org.kalypso.ui.editor.diagrameditor.ObservationDiagramEditor;
import org.kalypso.ui.editor.diagrameditor.actions.EditDiagCurveAction;

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

  private IAction m_editThemeAction;

  private IAction m_removeThemeAction;

  private IAction m_setIgnoreTypesAction;

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

    m_editThemeAction = new EditDiagCurveAction( this );
    m_removeThemeAction = new RemoveThemeAction( this );
    m_setIgnoreTypesAction = new SetIgnoreTypesAction( this );
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.views.contentoutline.ContentOutlinePage2#createTreeViewer(org.eclipse.swt.widgets.Composite)
   */
  protected TreeViewer createTreeViewer( final Composite parent )
  {
    return new ContainerCheckedTreeViewer( parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL );
  }

  /**
   * @return the selected items or empty array
   */
  public ObsViewItem[] getSelectedItems()
  {
    final ISelection sel = getSelection();
    final List items = new ArrayList();

    if( sel instanceof IStructuredSelection )
    {
      final IStructuredSelection structSel = (IStructuredSelection)sel;

      Arrays.addAllOfClass( structSel.toList(), items, ObsViewItem.class );
    }

    return (ObsViewItem[])items.toArray( new ObsViewItem[items.size()] );
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

  /**
   * @see org.kalypso.ogc.sensor.template.IObsViewEventListener#onPrintObsView(org.kalypso.ogc.sensor.template.ObsViewEvent)
   */
  public void onPrintObsView( final ObsViewEvent evt )
  {
  // nothing to do
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
      final ObsViewItem item = (ObsViewItem)element;
      item.setShown( event.getChecked() );
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
  public void setActionBars( final IActionBars actionBars )
  {
    final IToolBarManager toolBarManager = actionBars.getToolBarManager();
    toolBarManager.add( m_removeThemeAction );
    toolBarManager.add( m_setIgnoreTypesAction );

    if( m_editor instanceof ObservationDiagramEditor )
      toolBarManager.add( m_editThemeAction );

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