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

import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
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
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.jface.dialog.EnhancedTrayDialog;
import org.kalypso.contribs.eclipse.swt.widgets.SectionUtils;
import org.kalypso.contribs.eclipse.ui.forms.ToolkitUtils;
import org.kalypso.model.hydrology.timeseries.binding.IStationCollection;
import org.kalypso.model.hydrology.timeseries.binding.ITimeseries;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.view.StationsByStationsStrategy;
import org.kalypso.ui.rrm.internal.timeseries.view.actions.CleanSearchPanelAction;
import org.kalypso.ui.rrm.internal.timeseries.view.filter.TimeseriesBrowserSearchViewer;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeContentProvider;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeLabelProvider;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeModel;

/**
 * @author Dirk Kuch
 */
public class ChooseTimeseriesDialog extends EnhancedTrayDialog
{
  private static final String DIALOG_SCREEN_SIZE = "choose.time.series.dialog.screen.size"; //$NON-NLS-1$

  private final IStationCollection m_collection;

  private final String m_parameterType;

  private TreeViewer m_treeViewer;

  private final CommandableWorkspace m_workspace;

  private ITimeseries m_selection;

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
  protected final Control createDialogArea( final Composite parent )
  {
    final FormToolkit toolkit = ToolkitUtils.createToolkit( parent );
    getShell().setText( Messages.getString("ChooseTimeseriesDialog_0") ); //$NON-NLS-1$

    final Composite base = toolkit.createComposite( parent, SWT.NULL );
    base.setLayout( new GridLayout() );

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

    createTreeViewer( base ).setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    createSearchPanel( base, toolkit ).setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    return base;
  }

  private Composite createTreeViewer( final Composite base )
  {
    m_treeViewer = new TreeViewer( base, SWT.FLAT | SWT.SINGLE );
    m_treeViewer.setContentProvider( new TreeNodeContentProvider() );
    m_treeViewer.setLabelProvider( new TreeNodeLabelProvider() );

    final StationsByStationsStrategy strategy = new StationsByStationsStrategy( m_collection );

    final TreeNodeModel input = new TreeNodeModel( strategy, m_workspace, m_treeViewer );
    m_treeViewer.setInput( input );

    if( Objects.isNotNull( m_selection ) )
      input.refreshTree( m_selection );

    m_treeViewer.expandToLevel( 2 );

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

        final IStructuredSelection structured = (IStructuredSelection) selection;
        final Object element = structured.getFirstElement();
        if( !(element instanceof TreeNode) )
          return null;

        final TreeNode node = (TreeNode) element;
        final Object adapter = node.getAdapter( ITimeseries.class );
        if( adapter instanceof ITimeseries )
          return (ITimeseries) adapter;

        return null;
      }
    } );

    return m_treeViewer.getTree();
  }

  protected void doSelectionChangedEvent( final ITimeseries timeseries )
  {
    final Button button = getButton( OK );
    button.setEnabled( Objects.isNotNull( timeseries ) );

    setSelection( timeseries );
  }

  private Control createSearchPanel( final Composite parent, final FormToolkit toolkit )
  {
    final Section section = toolkit.createSection( parent, ExpandableComposite.TITLE_BAR | ExpandableComposite.TWISTIE | ExpandableComposite.EXPANDED );
    section.setText( Messages.getString("ChooseTimeseriesDialog_1") ); //$NON-NLS-1$
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
    m_selection = timeseries;
  }
}
