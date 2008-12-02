package org.kalypso.ui.editor.abstractobseditor;

import java.util.ArrayList;
import java.util.List;

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
import org.eclipse.ui.dialogs.ContainerCheckedTreeViewer;
import org.kalypso.contribs.eclipse.ui.views.contentoutline.ContentOutlinePage2;
import org.kalypso.contribs.java.util.Arrays;
import org.kalypso.ogc.sensor.template.IObsViewEventListener;
import org.kalypso.ogc.sensor.template.ObsView;
import org.kalypso.ogc.sensor.template.ObsViewEvent;
import org.kalypso.ogc.sensor.template.ObsViewItem;
import org.kalypso.ui.editor.abstractobseditor.actions.RemoveThemeAction;
import org.kalypso.ui.editor.abstractobseditor.actions.SetIgnoreTypesAction;
import org.kalypso.ui.editor.abstractobseditor.commands.DropZmlCommand;
import org.kalypso.ui.editor.abstractobseditor.commands.SetShownCommand;

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

  private IAction m_setIgnoreTypesAction;

  public ObservationEditorOutlinePage( final AbstractObservationEditor editor )
  {
    m_editor = editor;
  }

  /**
   * @see org.eclipse.ui.part.IPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
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
    m_setIgnoreTypesAction = new SetIgnoreTypesAction( this );
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.views.contentoutline.ContentOutlinePage2#createTreeViewer(org.eclipse.swt.widgets.Composite)
   */
  @Override
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
    final List<ObsViewItem> items = new ArrayList<ObsViewItem>();

    if( sel instanceof IStructuredSelection )
    {
      final IStructuredSelection structSel = (IStructuredSelection)sel;

      Arrays.addAllOfClass( structSel.toList(), items, ObsViewItem.class );
    }

    return items.toArray( new ObsViewItem[items.size()] );
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
  @Override
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
      m_editor.postCommand( new SetShownCommand( item, event.getChecked() ), null );
//      item.setShown( event.getChecked() );
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
  @Override
  public void setActionBars( IActionBars actionBars )
  {
    final IToolBarManager toolBarManager = actionBars.getToolBarManager();
    toolBarManager.add( m_removeThemeAction );
    toolBarManager.add( m_setIgnoreTypesAction );
    
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
    @Override
    public boolean performDrop( Object data )
    {
      if( m_view == null )
        return false;

      final String[] files = (String[])data;

      m_editor2.postCommand( new DropZmlCommand( m_editor2, m_view, files ), null );
      
      return true;
    }

    /**
     * @see org.eclipse.jface.viewers.ViewerDropAdapter#validateDrop(java.lang.Object, int,
     *      org.eclipse.swt.dnd.TransferData)
     */
    @Override
    public boolean validateDrop( Object target, int operation, TransferData transferType )
    {
      if( !FileTransfer.getInstance().isSupportedType( transferType ) )
        return false;

      // TODO maybe check that it is a ZML-File...

      return true;
    }
  }
}