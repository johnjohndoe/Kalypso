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
import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.List;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekModelMember;
import org.kalypso.model.wspm.sobek.core.ui.boundarycondition.RepositoryLabelProvider;
import org.kalypso.model.wspm.sobek.core.ui.boundarycondition.RepositoryTreeContentProvider;
import org.kalypso.model.wspm.sobek.core.ui.boundarycondition.RepositoryViewerFilter;
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
public class PageEditBoundaryConditionTimeSeries extends WizardPage
{
  protected final IBoundaryConditionGeneral m_settings;

  private final ISobekModelMember m_model;

  protected Button m_bTimeSeries;

  protected Button m_bConstant;

  private TreeViewer m_reposTree;

  private Text m_tConstant;

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
      pLayout.widthHint = 900;
      pLayout.heightHint = 600;
      parent.layout();
    }

    final Composite container = new Composite( parent, SWT.NULL );
    container.setLayout( new GridLayout() );
    setControl( container );

    final Group gMapping = new Group( container, SWT.NONE );
    gMapping.setLayout( new GridLayout() );
    gMapping.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    gMapping.setText( "Type of boundary condition data input" );

    m_bTimeSeries = new Button( gMapping, SWT.RADIO );
    m_bTimeSeries.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false, 2, 0 ) );
    m_bTimeSeries.setText( "Select a time series from the time series repository." );

    m_bConstant = new Button( gMapping, SWT.RADIO );
    m_bConstant.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false, 2, 0 ) );
    m_bConstant.setText( "Set a constant value for corrosponding discharge, waterlevel or Q-H relation" );

    /* time series repository */
    final Group gRepos = new Group( container, SWT.NULL );
    gRepos.setLayout( new GridLayout( 2, false ) );
    gRepos.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    gRepos.setText( "Time Series Repository" );

    final List<Control> tsControls = renderRepositorySection( gRepos );

    /* constant value */
    final Group gConstant = new Group( container, SWT.NULL );
    gConstant.setLayout( new GridLayout( 2, false ) );
    gConstant.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    gConstant.setText( "Constant value for coorosponding discharge, waterlevel or Q-H relation" );

    final List<Control> vControls = renderConstantValue( gConstant );

    m_bTimeSeries.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        if( m_bTimeSeries.getSelection() )
        {
          for( final Control control : tsControls )
            control.setEnabled( true );

          for( final Control control : vControls )
            control.setEnabled( false );

        }

      }
    } );

    m_bConstant.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        if( m_bConstant.getSelection() )
        {
          for( final Control control : tsControls )
            control.setEnabled( false );

          for( final Control control : vControls )
            control.setEnabled( true );
        }
      }
    } );

    /* set selection */
    m_bTimeSeries.setSelection( true );
    for( final Control control : tsControls )
      control.setEnabled( true );
    for( final Control control : vControls )
      control.setEnabled( false );

    checkPageCompleted();
  }

  private List<Control> renderConstantValue( final Composite cBody )
  {
    final List<Control> controls = new ArrayList<Control>();

    new Label( cBody, SWT.NONE ).setText( "Value" );

    m_tConstant = new Text( cBody, SWT.BORDER );
    m_tConstant.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    controls.add( m_tConstant );

    return controls;
  }

  private List<Control> renderRepositorySection( final Composite repos )
  {
    final List<Control> controls = new ArrayList<Control>();

    final Composite cBrowser = new Composite( repos, SWT.NULL );
    final GridData ld = new GridData( GridData.FILL, GridData.FILL, false, true );
    ld.widthHint = ld.minimumWidth = 200;
    cBrowser.setLayoutData( ld );
    final GridLayout bLayout = new GridLayout();
    bLayout.marginWidth = bLayout.horizontalSpacing = 0;
    cBrowser.setLayout( bLayout );

    m_reposTree = getTSBrowser( cBrowser );
    controls.add( m_reposTree.getTree() );

    final Composite cClient = new Composite( repos, SWT.NULL );
    cClient.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    final GridLayout cLayout = new GridLayout( 2, true );
    cLayout.marginWidth = bLayout.horizontalSpacing = 0;
    cClient.setLayout( cLayout );

    renderDetails( m_reposTree, cClient );

    return controls;
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
          // always remove items first (we don't know which selection we get)
          diagView.removeAllItems();
          tableView.removeAllItems();

          final TreeSelection selection = (TreeSelection) reposTree.getSelection();
          final Object element = selection.getFirstElement();
          if( !(element instanceof ZmlObservationItem) )
            return;

          final ZmlObservationItem item = (ZmlObservationItem) element;

          final IObservation observation = (IObservation) item.getAdapter( IObservation.class );

          final GregorianCalendar startDate = m_settings.getStartDate();
          final GregorianCalendar endDate = m_settings.getEndDate();
          final DateRange dateRange = new DateRange( startDate.getTime(), endDate.getTime() );

          final PlainObsProvider provider = new PlainObsProvider( observation, new ObservationRequest( dateRange ) );
          diagView.addObservation( provider, ObsViewUtils.DEFAULT_ITEM_NAME, new ObsView.ItemData( false, null, null ) );
          tableView.addObservation( provider, ObsViewUtils.DEFAULT_ITEM_NAME, new ObsView.ItemData( false, null, null ) );
        }
      } );
    }
    catch( final SensorException e )
    {
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

  protected void checkPageCompleted( )
  {
    /* time series repository selected? check selection and values! */
    if( m_bTimeSeries.getSelection() )
    {
      final TreeSelection selection = (TreeSelection) m_reposTree.getSelection();
      final Object element = selection.getFirstElement();

      if( !(element instanceof ZmlObservationItem) )
      {
        setMessage( null );
        setErrorMessage( "No time series repository item selected." );
        setPageComplete( false );

        return;
      }

      final ZmlObservationItem item = (ZmlObservationItem) element;
      final Object adapter = item.getAdapter( IObservation.class );
      if( !(adapter instanceof IObservation) )
      {
        setMessage( null );
        setErrorMessage( "No time series data found." );
        setPageComplete( false );

        return;
      }

      try
      {
        final IObservation observation = (IObservation) adapter;

        final GregorianCalendar startDate = m_settings.getStartDate();
        final GregorianCalendar endDate = m_settings.getEndDate();
        final DateRange dateRange = new DateRange( startDate.getTime(), endDate.getTime() );

        final PlainObsProvider provider = new PlainObsProvider( observation, new ObservationRequest( dateRange ) );
        final IRequest request = provider.getArguments();

        final ITuppleModel values = observation.getValues( request );
        if( values.getCount() <= 0 )
        {
          setMessage( null );
          setErrorMessage( "No time series values in given date range found." );
          setPageComplete( false );

          return;
        }
      }
      catch( final SensorException e )
      {
        setMessage( null );
        setErrorMessage( "Error reading time series repository:" + e.getMessage() );
        setPageComplete( false );

        return;
      }
    }
    else if( m_bConstant.getSelection() )
    {
      if( m_tConstant.getText() == null || m_tConstant.getText().trim().equals( "" ) )
      {
        setMessage( null );
        setErrorMessage( "No constant value defined." );
        setPageComplete( false );

        return;
      }
      try
      {
        Double.valueOf( m_tConstant.getText() );
      }
      catch( final NumberFormatException e )
      {
        setMessage( null );
        setErrorMessage( "Constant value is not a number." );
        setPageComplete( false );

        return;
      }
    }

    setMessage( null );
    setErrorMessage( null );
    setPageComplete( true );
  }

}
