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
package org.kalypso.model.wspm.sobek.core.wizard.pages;

import java.awt.Frame;
import java.util.Date;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekModelMember;
import org.kalypso.model.wspm.sobek.core.ui.boundarycondition.RepositoryLabelProvider;
import org.kalypso.model.wspm.sobek.core.ui.boundarycondition.RepositoryTreeContentProvider;
import org.kalypso.model.wspm.sobek.core.ui.boundarycondition.RepositoryViewerFilter;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagView;
import org.kalypso.ogc.sensor.diagview.jfreechart.ChartFactory;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationChart;
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
public class PageEditBoundaryConditionTimeSeries extends WizardPage
{
  private final IBoundaryConditionGeneral m_settings;

  private final ISobekModelMember m_model;

  public PageEditBoundaryConditionTimeSeries( final ISobekModelMember model, final IBoundaryConditionGeneral settings )
  {
    super( "editBoundaryConditionTimeSeries" );
    m_model = model;
    m_settings = settings;

    setTitle( "Edit boundary condition" );
    setDescription( "Enter boundary condition parameters, please." );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    setPageComplete( false );

    final Object layoutData = parent.getLayoutData();
    if( layoutData instanceof GridData )
    {
      final GridData pLayout = (GridData) layoutData;
      pLayout.widthHint = 800;
      pLayout.heightHint = 400;
      parent.layout();
    }

    final Composite container = new Composite( parent, SWT.NULL );
    container.setLayout( new GridLayout( 2, false ) );
    setControl( container );

    final Composite cBrowser = new Composite( container, SWT.NULL );
    final GridData ld = new GridData( GridData.FILL, GridData.FILL, false, true );
    ld.widthHint = ld.minimumWidth = 200;
    cBrowser.setLayoutData( ld );
    final GridLayout bLayout = new GridLayout();
    bLayout.marginWidth = bLayout.horizontalSpacing = 0;
    cBrowser.setLayout( bLayout );

    final TreeViewer reposTree = getTSBrowser( cBrowser );

    final Composite cClient = new Composite( container, SWT.NULL );
    cClient.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    final GridLayout cLayout = new GridLayout( 2, true );
    cLayout.marginWidth = bLayout.horizontalSpacing = 0;
    cClient.setLayout( cLayout );

    renderDetails( reposTree, cClient );

    checkPageCompleted();
  }

  private void renderDetails( final TreeViewer reposTree, final Composite body )
  {
    try
    {

      /* diagramm */
      final Composite cDiagram = new Composite( body, SWT.NULL );
      cDiagram.setLayout( new FillLayout() );
      cDiagram.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

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
          try
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

            // TODO
            DateRange dateRange = null;

            final ITuppleModel values = observation.getValues( null );
            final IAxis[] axisList = values.getAxisList();
            final IAxis axis = ObservationUtilities.findAxisByType( axisList, "date" );
            final Date dFrom = (Date) values.getElement( 0, axis );
            final Date dTo = (Date) values.getElement( values.getCount() - 1, axis );
            dateRange = new DateRange( dFrom, dTo );

            final PlainObsProvider provider = new PlainObsProvider( observation, new ObservationRequest( dateRange ) );
            diagView.addObservation( provider, ObsViewUtils.DEFAULT_ITEM_NAME, new ObsView.ItemData( false, null, null ) );
            tableView.addObservation( provider, ObsViewUtils.DEFAULT_ITEM_NAME, new ObsView.ItemData( false, null, null ) );
          }
          catch( final SensorException e )
          {
            // TODO Auto-generated catch block
            e.printStackTrace();
          }
        }
      } );

    }
    catch( final SensorException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  private TreeViewer getTSBrowser( final Composite body )
  {
    final TreeViewer viewer = new TreeViewer( body );
    viewer.getTree().setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

    viewer.setContentProvider( new RepositoryTreeContentProvider( m_settings ) );
    viewer.setLabelProvider( new RepositoryLabelProvider() );

    viewer.setInput( m_model.getRepositoryContainer() );

    viewer.addFilter( new RepositoryViewerFilter() );

    viewer.expandAll();

    return viewer;
  }

  protected void checkPageCompleted( )
  {
    setMessage( null );
    setErrorMessage( null );
    setPageComplete( false );
  }

}
