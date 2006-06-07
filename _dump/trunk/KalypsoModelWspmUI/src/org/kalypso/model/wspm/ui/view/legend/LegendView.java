package org.kalypso.model.wspm.ui.view.legend;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.PartInitException;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.editor.ProfilchartEditor;
import org.kalypso.model.wspm.ui.view.AbstractProfilViewPart;

import de.belger.swtchart.layer.IChartLayer;
import de.belger.swtchart.legend.ChartLegend;

/**
 * @author belger
 */
public class LegendView extends AbstractProfilViewPart implements ISelectionProvider
{
  private final List<ISelectionChangedListener> m_listeners = new ArrayList<ISelectionChangedListener>(
      10 );

  private ChartLegend m_chartlegend;

  @Override
  public void init( final IViewSite site ) throws PartInitException
  {
    super.init( site );

    site.setSelectionProvider( this );
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#dispose()
   */
  @Override
  public void dispose( )
  {
    super.dispose();

    if( m_chartlegend != null )
    {
      m_chartlegend.dispose();
      m_chartlegend = null;
    }
  }

  @Override
  protected Control createContent( final Composite parent )
  {
    final ProfilchartEditor editor = getProfilchartEditor();
    if( editor != null )
      editor.saveLegend( m_chartlegend );
    if( m_chartlegend != null )
    {
      m_chartlegend.dispose();
      m_chartlegend = null;
    }

    final Object layoutdata = new GridData( GridData.FILL_BOTH );
    if( editor == null )
      return createLabel( parent, "Kein Profileditor vorhanden.", layoutdata );
    else if( editor.getProfil() == null )
      return createLabel( parent, "Kein Profil geladen.", layoutdata );
    else
    {
      m_chartlegend = editor.createChartLegend( parent, SWT.BORDER );
      if( m_chartlegend == null )
        return createLabel( parent, "UI nicht initialisiert.", layoutdata );

      m_chartlegend.getSelectionProvider().addSelectionChangedListener(
          new ISelectionChangedListener()
          {
            public void selectionChanged( final SelectionChangedEvent event )
            {
              // auch den activen layer in der ProfilViewData setzen
              final IStructuredSelection selection = (IStructuredSelection)event.getSelection();
              getViewData().setActiveLayer( (IChartLayer)selection.getFirstElement() );

              fireSelectionChanged( selection );
            }
          } );

      final Control control = m_chartlegend.getControl();

      control.addMouseListener( new MouseAdapter()
      {
        @Override
        public void mouseDoubleClick( final MouseEvent e )
        {
          showLayerProperties();
        }
      } );

      control.setLayoutData( layoutdata );

      getSite().setSelectionProvider( m_chartlegend.getSelectionProvider() );

      final MenuManager menuMgr = new MenuManager( "#PopupMenu" ); //$NON-NLS-1$
      // menuMgr.setRemoveAllWhenShown( true );
      menuMgr.addMenuListener( new IMenuListener()
      {
        public void menuAboutToShow( final IMenuManager manager )
        {
          LegendView.this.fillContextMenu( manager );
        }
      } );
      final Menu menu = menuMgr.createContextMenu( control );
      control.setMenu( menu );
      // Be sure to register it so that other plug-ins can add actions.
      getSite().registerContextMenu( menuMgr, this );

      return control;
    }
  }

  public void showLayerProperties( )
  {
    // just open layer view
    try
    {
      getSite().getPage().showView( "com.bce.profil.eclipse.view.LayerView" );
    }
    catch( final PartInitException ex )
    {
      ErrorDialog.openError( getSite().getShell(), "Profil-Legende",
          "Konnte Themeneigenschaften nicht öffnen", ex.getStatus() );
      KalypsoModelWspmUIPlugin.getDefault().getLog().log( ex.getStatus() );
    }
  }

  private Label createLabel( final Composite parent, final String text, final Object layoutdata )
  {
    final Label label = new Label( parent, SWT.BORDER );
    label.setLayoutData( layoutdata );
    label.setText( text );
    return label;
  }

  public void onProfilViewDataChanged( )
  {
    final Shell shell = getSite().getShell();
    if( shell == null || shell.isDisposed() )
      return;

    final ISelectionProvider selectionProvider = m_chartlegend.getSelectionProvider();
    shell.getDisplay().asyncExec( new Runnable()
    {
      public void run( )
      {
        final IStructuredSelection selection = (IStructuredSelection)selectionProvider
            .getSelection();
        final Object layer = selection.getFirstElement();
        final ProfilchartEditor editor = getProfilchartEditor();
        final Object activeLayer = editor == null ? null : editor.getViewData().getActiveLayer();
        if( layer != activeLayer && activeLayer != null )
          selectionProvider.setSelection( new StructuredSelection( activeLayer ) );
      }
    } );

  }

  @Override
  protected void saveState( )
  {
    getProfilchartEditor().saveLegend( m_chartlegend );
  }

  public void addSelectionChangedListener( final ISelectionChangedListener listener )
  {
    m_listeners.add( listener );
  }

  public ISelection getSelection( )
  {
    if( m_chartlegend != null )
      return m_chartlegend.getSelectionProvider().getSelection();

    return new StructuredSelection();
  }

  public void removeSelectionChangedListener( final ISelectionChangedListener listener )
  {
    m_listeners.remove( listener );
  }

  public void setSelection( final ISelection selection )
  {
    if( m_chartlegend != null )
      m_chartlegend.getSelectionProvider().setSelection( selection );
  }

  protected void fireSelectionChanged( final ISelection selection )
  {
    final SelectionChangedEvent event = new SelectionChangedEvent( this, selection );
    final ISelectionChangedListener[] listeners = m_listeners
        .toArray( new ISelectionChangedListener[m_listeners.size()] );
    for( final ISelectionChangedListener listener : listeners )
      listener.selectionChanged( event );
  }

  /**
   * Contributes actions to the pop-up menu.
   */
  void fillContextMenu( final IMenuManager menu )
  {
    // update enabled state for actions that aren't updated in selectionChanged
    // final IStructuredSelection selection = (IStructuredSelection)getSelection();
    // resolveMarkerAction.setEnabled( resolveMarkerAction.shouldEnable( selection ) );

    // add the actions to the menu
    // menu.add( removeTaskAction );
    // menu.add( new Separator() );
    menu.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
    menu.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS + "-end" ) ); //$NON-NLS-1$
    // menu.add( propertiesAction );
  }

}
