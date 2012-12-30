/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ui.rrm.internal.gml.feature.view.dialogs;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.jface.dialog.EnhancedTrayDialog;
import org.kalypso.contribs.eclipse.jface.viewers.tree.CollapseAllTreeItemsAction;
import org.kalypso.contribs.eclipse.jface.viewers.tree.ExpandAllTreeItemsAction;
import org.kalypso.contribs.eclipse.swt.widgets.SectionUtils;
import org.kalypso.contribs.eclipse.ui.forms.ToolkitUtils;
import org.kalypso.model.hydrology.binding.timeseries.IStationCollection;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.view.StationsByStationsStrategy;
import org.kalypso.ui.rrm.internal.timeseries.view.actions.CleanSearchPanelAction;
import org.kalypso.ui.rrm.internal.timeseries.view.edit.TimeseriesChartComposite;
import org.kalypso.ui.rrm.internal.timeseries.view.edit.TimeseriesDialogSource;
import org.kalypso.ui.rrm.internal.timeseries.view.filter.TimeseriesBrowserSearchViewer;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeContentProvider;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeLabelComparator;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeLabelProvider;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeModel;

/**
 * @author Dirk Kuch
 */
public class ChooseTimeseriesDialog extends EnhancedTrayDialog
{
  private static final String DIALOG_SCREEN_SIZE = "choose.time.series.dialog.screen.size"; //$NON-NLS-1$

  private static final String DIALOG_SASH_FORM_WEIGHTS = "choose.time.series.dialog.weights"; //$NON-NLS-1$

  private final IStationCollection m_collection;

  private final String m_parameterType;

  private TreeViewer m_treeViewer;

  private final CommandableWorkspace m_workspace;

  private ITimeseries m_selection;

  private TimeseriesChartComposite m_chart;

  private TreeNodeModel m_model;

  private static final int BUTTON_RESET_ID = 5000;

  public ChooseTimeseriesDialog( final Shell shell, final CommandableWorkspace workspace, final IStationCollection collection, final String parameterType )
  {
    super( shell );

    m_workspace = workspace;
    m_collection = collection;
    m_parameterType = parameterType;

    setShellStyle( SWT.CLOSE | SWT.MAX | SWT.TITLE | SWT.BORDER | SWT.APPLICATION_MODAL | SWT.RESIZE );
    setHelpAvailable( false );
    setDialogHelpAvailable( false );
  }

  @Override
  protected void createButtonsForButtonBar( final Composite parent )
  {
    createButton( parent, BUTTON_RESET_ID, Messages.getString( "ChooseTimeseriesDialog.0" ), false ); //$NON-NLS-1$

    createButton( parent, 5010, "", false ).setVisible( false ); //$NON-NLS-1$

    createButton( parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL, true );
    createButton( parent, IDialogConstants.CANCEL_ID, IDialogConstants.CANCEL_LABEL, false );
  }

  @Override
  protected void buttonPressed( final int buttonId )
  {
    if( buttonId == BUTTON_RESET_ID )
      resetPressed();

    super.buttonPressed( buttonId );
  }

  private void resetPressed( )
  {
    m_selection = null;

    super.okPressed();
  }

  @Override
  protected final Control createDialogArea( final Composite parent )
  {
    final FormToolkit toolkit = ToolkitUtils.createToolkit( parent );
    getShell().setText( Messages.getString( "ChooseTimeseriesDialog_0" ) ); //$NON-NLS-1$

    final Composite base = toolkit.createComposite( parent, SWT.NULL );
    base.setLayout( GridLayoutFactory.fillDefaults().create() );

    final Point screen = getScreenSize( DIALOG_SCREEN_SIZE );

    final GridData data = new GridData( GridData.FILL, GridData.FILL, true, true );
    data.widthHint = screen.x;
    data.heightHint = screen.y;
    base.setLayoutData( data );

    base.addControlListener( new ControlAdapter()
    {
      @Override
      public void controlResized( final ControlEvent e )
      {
        setScreenSize( DIALOG_SCREEN_SIZE, base.getSize() );
      }
    } );

    final SashForm form = new SashForm( base, SWT.HORIZONTAL );
    form.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true, 2, 0 ) );

    final Composite leftPane = toolkit.createComposite( form );
    final GridLayout layout = GridLayoutFactory.fillDefaults().create();
    layout.verticalSpacing = 0;
    leftPane.setLayout( layout );

    final Composite rightPane = toolkit.createComposite( form );
    rightPane.setLayout( GridLayoutFactory.fillDefaults().create() );

    createTreeViewer( leftPane, toolkit ).setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    createSearchPanel( leftPane, toolkit ).setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    createDiagramView( rightPane, toolkit ).setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

    form.setWeights( getWeights() );

    leftPane.addControlListener( new ControlAdapter()
    {
      @Override
      public void controlResized( final ControlEvent e )
      {
        setWeights( form.getWeights() );
      }
    } );

    return base;
  }

  private Control createDiagramView( final Composite body, final FormToolkit toolkit )
  {
    final IWorkbench context = PlatformUI.getWorkbench();
    m_chart = new TimeseriesChartComposite( body, toolkit, context, TimeseriesChartComposite.class.getResource( "/etc/timeseries/diagram.kod" ) ); //$NON-NLS-1$

    if( Objects.isNotNull( m_selection ) )
    {
      try
      {
        m_chart.setSelection( new TimeseriesDialogSource( m_selection ) );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }

    return m_chart;
  }

  private Composite createTreeViewer( final Composite body, final FormToolkit toolkit )
  {
    final ToolBar toolbar = new ToolBar( body, SWT.RIGHT_TO_LEFT );
    toolbar.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    final ToolBarManager manager = new ToolBarManager( toolbar );

    m_treeViewer = new TreeViewer( body, SWT.FLAT | SWT.SINGLE | SWT.BORDER );
    m_treeViewer.setContentProvider( new TreeNodeContentProvider() );
    m_treeViewer.setLabelProvider( new TreeNodeLabelProvider() );
    m_treeViewer.setComparator( new TreeNodeLabelComparator() );

    manager.add( new CollapseAllTreeItemsAction( m_treeViewer ) );
    manager.add( new ExpandAllTreeItemsAction( m_treeViewer ) );
    manager.update( true );

    toolkit.adapt( toolbar );

    final StationsByStationsStrategy strategy = new StationsByStationsStrategy( m_collection );

    m_model = new TreeNodeModel( strategy, m_treeViewer, m_workspace );
    m_treeViewer.setInput( m_model );
    m_treeViewer.expandToLevel( 2 );

    if( Objects.isNotNull( m_selection ) )
      m_model.refreshTree( m_selection );

    m_treeViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        doSelectionChangedEvent( resolve( event.getSelection() ) );
      }

      private ITimeseries resolve( final ISelection selection )
      {
        if( !(selection instanceof IStructuredSelection) )
          return null;

        final IStructuredSelection structured = (IStructuredSelection)selection;
        final Object element = structured.getFirstElement();
        if( !(element instanceof TreeNode) )
          return null;

        final TreeNode node = (TreeNode)element;
        final Object adapter = node.getAdapter( ITimeseries.class );
        if( adapter instanceof ITimeseries )
          return (ITimeseries)adapter;

        return null;
      }
    } );

    return m_treeViewer.getTree();
  }

  protected void doSelectionChangedEvent( final ITimeseries timeseries )
  {

    final Button button = getButton( OK );
    if( button != null )
      button.setEnabled( Objects.isNotNull( timeseries ) );

    setSelection( timeseries );
  }

  private Control createSearchPanel( final Composite body, final FormToolkit toolkit )
  {
    final Section section = toolkit.createSection( body, ExpandableComposite.TITLE_BAR | ExpandableComposite.TWISTIE | ExpandableComposite.EXPANDED );
    section.setText( Messages.getString( "ChooseTimeseriesDialog_1" ) ); //$NON-NLS-1$
    section.setLayout( new FillLayout() );

    final ToolBarManager toolbar = SectionUtils.createSectionToolbar( section );

    final TimeseriesBrowserSearchViewer searchPanel = new TimeseriesBrowserSearchViewer( section, toolkit, m_treeViewer );
    searchPanel.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    searchPanel.setParameterType( m_parameterType );

    toolbar.add( new CleanSearchPanelAction( searchPanel ) );
    toolbar.update( true );

    section.setClient( searchPanel );
    toolkit.adapt( searchPanel );

    return section;
  }

  public ITimeseries getSelection( )
  {
    return m_selection;
  }

  public void setSelection( final ITimeseries timeseries )
  {
    try
    {
      m_selection = timeseries;
      if( m_selection != null && m_chart != null )
        m_chart.setSelection( new TimeseriesDialogSource( timeseries ) );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  private int[] getWeights( )
  {
    final IDialogSettings settings = KalypsoUIRRMPlugin.getDefault().getDialogSettings();

    final String weights = settings.get( DIALOG_SASH_FORM_WEIGHTS );
    if( weights == null || weights.trim().isEmpty() )
      return new int[] { 35, 65 };

    final String[] parts = weights.split( "," ); //$NON-NLS-1$
    final int[] w = new int[parts.length];

    for( int i = 0; i < parts.length; i++ )
    {
      w[i] = Integer.valueOf( parts[i] );
    }

    return w;
  }

  protected void setWeights( final int[] weights )
  {
    final StringBuffer buffer = new StringBuffer();
    for( final int weight : weights )
    {
      buffer.append( String.format( Messages.getString( "EditTimeseriesDialog_3" ), weight ) ); //$NON-NLS-1$
    }

    final IDialogSettings settings = KalypsoUIRRMPlugin.getDefault().getDialogSettings();
    settings.put( DIALOG_SASH_FORM_WEIGHTS, StringUtils.chop( buffer.toString() ) );
  }
}
