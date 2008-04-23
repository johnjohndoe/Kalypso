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
package org.kalypso.model.wspm.sobek.core.wizard.pages.renderer;

import java.awt.Frame;
import java.util.GregorianCalendar;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.model.wspm.sobek.core.Messages;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode.BOUNDARY_TYPE;
import org.kalypso.model.wspm.sobek.core.ui.boundarycondition.RepositoryLabelProvider;
import org.kalypso.model.wspm.sobek.core.ui.boundarycondition.RepositoryTreeContentProvider;
import org.kalypso.model.wspm.sobek.core.ui.boundarycondition.RepositoryViewerFilter;
import org.kalypso.model.wspm.sobek.core.wizard.pages.PageEditBoundaryConditionTimeSeries;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagView;
import org.kalypso.ogc.sensor.diagview.jfreechart.ChartFactory;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationChart;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.tableview.TableView;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTable;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTablePanel;
import org.kalypso.ogc.sensor.template.ObsView;
import org.kalypso.ogc.sensor.template.ObsViewUtils;
import org.kalypso.ogc.sensor.template.PlainObsProvider;
import org.kalypso.ogc.sensor.zml.repository.ZmlObservationItem;

/**
 * @author kuch
 */
public class TimeSeriesComposite extends Composite
{
  protected ZmlObservationItem m_selectedTreeItem = null;

  private final PageEditBoundaryConditionTimeSeries m_page;

  private TreeViewer m_reposTree;

  public TimeSeriesComposite( final PageEditBoundaryConditionTimeSeries page, final Composite container, final int flags )
  {
    super( container, flags );
    m_page = page;

    final GridLayout gridLayout = new GridLayout();
    gridLayout.marginWidth = 0;
    setLayout( gridLayout );
    setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
  }

  public void render( )
  {
    final Group group = new Group( this, SWT.NULL );
    group.setLayout( new GridLayout( 2, false ) );
    group.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    group.setText( Messages.PageEditBoundaryConditionTimeSeries_18 );

    final Composite cBrowser = new Composite( group, SWT.NULL );
    final GridData ld = new GridData( GridData.FILL, GridData.FILL, false, true );
    ld.widthHint = ld.minimumWidth = 200;
    cBrowser.setLayoutData( ld );
    final GridLayout bLayout = new GridLayout();
    bLayout.marginWidth = bLayout.horizontalSpacing = 0;
    cBrowser.setLayout( bLayout );

    m_reposTree = getTSBrowser( cBrowser );

    final Composite cClient = new Composite( group, SWT.NULL );
    cClient.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    final GridLayout cLayout = new GridLayout( 2, true );
    cLayout.marginWidth = bLayout.horizontalSpacing = 0;
    cClient.setLayout( cLayout );

    renderTimeSeriesDiagrams( m_reposTree, cClient );

    checkPageCompleted();
  }

  private TreeViewer getTSBrowser( final Composite body )
  {
    final TreeViewer viewer = new TreeViewer( body );
    viewer.getTree().setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    viewer.setContentProvider( new RepositoryTreeContentProvider( m_page.getSettings() ) );
    viewer.setLabelProvider( new RepositoryLabelProvider() );

    viewer.setInput( m_page.getModel().getRepositoryContainer() );
    viewer.addFilter( new RepositoryViewerFilter() );

    viewer.expandAll();

    viewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      /**
       * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
       */
      public void selectionChanged( final SelectionChangedEvent event )
      {
        checkPageCompleted();
      }
    } );

    return viewer;
  }

  private void renderTimeSeriesDiagrams( final TreeViewer reposTree, final Composite body )
  {
    try
    {
      /* diagramm */
      final Composite cDiagram = new Composite( body, SWT.NULL );
      cDiagram.setLayout( new FillLayout() );
      cDiagram.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

      final BOUNDARY_TYPE boundaryNodeType = m_page.getSettings().getBoundaryNodeType();

      final DiagView diagView = new DiagView( true );
      final ObservationChart chart = new ObservationChart( diagView );

      final Frame dFrame = SWT_AWT.new_Frame( new Composite( cDiagram, SWT.RIGHT | SWT.EMBEDDED | SWT.Paint ) );
      dFrame.add( ChartFactory.createChartPanel( chart ) );

      /* table view */
      final Composite cTable = new Composite( body, SWT.NULL );
      cTable.setLayout( new FillLayout() );
      cTable.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

      final TableView tableView = new TableView();
      final ObservationTable table = new ObservationTable( tableView );

      final Frame tFrame = SWT_AWT.new_Frame( new Composite( cTable, SWT.RIGHT | SWT.EMBEDDED | SWT.Paint ) );
      tFrame.add( new ObservationTablePanel( table ) );

      reposTree.addSelectionChangedListener( new ISelectionChangedListener()
      {

        public void selectionChanged( final SelectionChangedEvent event )
        {
          // always remove items first (we don't know which selection we get)
          diagView.removeAllItems();
          tableView.removeAllItems();

          final TreeSelection selection = (TreeSelection) reposTree.getSelection();
          final Object element = selection.getFirstElement();
          if( !(element instanceof ZmlObservationItem) )
            return;

          final ZmlObservationItem item = (ZmlObservationItem) element;

          final IObservation observation = (IObservation) item.getAdapter( IObservation.class );

          final GregorianCalendar startDate = m_page.getSettings().getStartDate();
          final GregorianCalendar endDate = m_page.getSettings().getEndDate();
          final DateRange dateRange = new DateRange( startDate.getTime(), endDate.getTime() );

          final PlainObsProvider provider = new PlainObsProvider( observation, new ObservationRequest( dateRange ) );
          diagView.addObservation( provider, ObsViewUtils.DEFAULT_ITEM_NAME, new ObsView.ItemData( false, null, null ) );
          tableView.addObservation( provider, ObsViewUtils.DEFAULT_ITEM_NAME, new ObsView.ItemData( false, null, null ) );

          m_selectedTreeItem = item;

          checkPageCompleted();
        }
      } );
    }
    catch( final SensorException e )
    {
      e.printStackTrace();
    }
  }

  protected void checkPageCompleted( )
  {
    if( m_selectedTreeItem == null )
    {
      m_page.setMessage( null );
      m_page.setErrorMessage( Messages.PageEditBoundaryConditionTimeSeries_3 );
      m_page.setPageComplete( false );

      return;
    }

    final Object adapter = m_selectedTreeItem.getAdapter( IObservation.class );
    if( !(adapter instanceof IObservation) )
    {
      m_page.setMessage( null );
      m_page.setErrorMessage( Messages.PageEditBoundaryConditionTimeSeries_4 );
      m_page.setPageComplete( false );

      return;
    }

    try
    {
      final IObservation observation = (IObservation) adapter;

      final GregorianCalendar startDate = m_page.getSettings().getStartDate();
      final GregorianCalendar endDate = m_page.getSettings().getEndDate();
      final DateRange dateRange = new DateRange( startDate.getTime(), endDate.getTime() );

      final PlainObsProvider provider = new PlainObsProvider( observation, new ObservationRequest( dateRange ) );
      final IRequest request = provider.getArguments();

      final ITuppleModel values = observation.getValues( request );
      if( values.getCount() <= 0 )
      {
        m_page.setMessage( null );
        m_page.setErrorMessage( Messages.PageEditBoundaryConditionTimeSeries_5 );
        m_page.setPageComplete( false );

        return;
      }
    }
    catch( final SensorException e )
    {
      m_page.setMessage( null );
      m_page.setErrorMessage( Messages.PageEditBoundaryConditionTimeSeries_6 + e.getMessage() );
      m_page.setPageComplete( false );

      return;
    }

    m_page.setMessage( null );
    m_page.setErrorMessage( null );
    m_page.setPageComplete( true );
  }

  public ZmlObservationItem getZmlObservationItem( )
  {
    return m_selectedTreeItem;
  }

}
