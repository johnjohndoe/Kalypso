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
import java.util.GregorianCalendar;

import org.apache.commons.lang.NotImplementedException;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
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
import org.kalypso.util.swt.WizardFeatureLabel;
import org.kalypso.util.swt.WizardFeatureTextBox;

/**
 * @author kuch
 */
public class PageEditBoundaryConditionTimeSeries extends WizardPage
{
  public enum TS_TYPE
  {
    eConstant,
    eTimeSeries;

    /**
     * @see java.lang.Enum#toString()
     */
    @Override
    public String toString( )
    {
      final TS_TYPE type = TS_TYPE.valueOf( name() );
      switch( type )
      {
        case eConstant:
          return "Set a constant value for corrosponding discharge, waterlevel or Q-H relation";

        case eTimeSeries:
          return "Select a time series from the time series repository.";

        default:
          throw new NotImplementedException();
      }
    }
  }

  protected TS_TYPE m_type = TS_TYPE.eTimeSeries;

  protected ZmlObservationItem m_selectedTreeItem = null;

  protected final IBoundaryConditionGeneral m_settings;

  private final ISobekModelMember m_model;

  private TreeViewer m_reposTree;

  private WizardFeatureTextBox m_tConstant;

  protected Group m_subGroup;

  private final IBoundaryNodeLastfallCondition m_condition;

  private WizardFeatureTextBox m_tConstantIntervall;

  public PageEditBoundaryConditionTimeSeries( final ISobekModelMember model, final IBoundaryNodeLastfallCondition condition, final IBoundaryConditionGeneral settings )
  {
    super( "editBoundaryConditionTimeSeries" );
    m_model = model;
    m_condition = condition;
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

    final TS_TYPE[] input = new TS_TYPE[] { TS_TYPE.eTimeSeries, TS_TYPE.eConstant };

    final ComboViewer viewer = new ComboViewer( gMapping, SWT.READ_ONLY | SWT.BORDER );
    viewer.getCombo().setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    viewer.setLabelProvider( new LabelProvider() );
    viewer.setContentProvider( new ArrayContentProvider() );

    viewer.setInput( input );

    viewer.addSelectionChangedListener( new ISelectionChangedListener()
    {

      public void selectionChanged( final SelectionChangedEvent event )
      {
        final StructuredSelection selection = (StructuredSelection) event.getSelection();
        m_type = (TS_TYPE) selection.getFirstElement();

        if( m_subGroup != null )
        {
          if( !m_subGroup.isDisposed() )
            m_subGroup.dispose();

          m_subGroup = null;
        }

        switch( m_type )
        {
          case eConstant:
            renderConstantValue( container );
            break;

          case eTimeSeries:
            renderTimeSeriesRepository( container );
            break;

          default:
            throw new IllegalStateException( "Type not supported: " + m_type.name() );
        }

        container.layout();
      }
    } );

    viewer.setSelection( new StructuredSelection( TS_TYPE.eTimeSeries ) );

    checkPageCompleted();
  }

  protected void renderConstantValue( final Composite parent )
  {
    m_subGroup = new Group( parent, SWT.NULL );
    m_subGroup.setLayout( new GridLayout( 2, false ) );
    m_subGroup.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    m_subGroup.setText( "Constant value for coorosponding discharge, waterlevel or Q-H relation" );

    /* const value */
    new WizardFeatureLabel( m_condition.getFeature(), ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_CONST_VALUE, "Constant value", m_subGroup );

    m_tConstant = new WizardFeatureTextBox( m_condition.getFeature(), ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_CONST_VALUE );
    m_tConstant.draw( m_subGroup, new GridData( GridData.FILL, GridData.FILL, true, false ), SWT.BORDER );

    /* const intervall */
    new WizardFeatureLabel( m_condition.getFeature(), ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_CONST_VALUE_INTERVALL, "Intervall of value", m_subGroup );

    m_tConstantIntervall = new WizardFeatureTextBox( m_condition.getFeature(), ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_CONST_VALUE_INTERVALL );
    m_tConstantIntervall.draw( m_subGroup, new GridData( GridData.FILL, GridData.FILL, true, false ), SWT.BORDER );
  }

  protected void renderTimeSeriesRepository( final Composite parent )
  {
    m_subGroup = new Group( parent, SWT.NULL );
    m_subGroup.setLayout( new GridLayout( 2, false ) );
    m_subGroup.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    m_subGroup.setText( "Time Series Repository" );

    final Composite cBrowser = new Composite( m_subGroup, SWT.NULL );
    final GridData ld = new GridData( GridData.FILL, GridData.FILL, false, true );
    ld.widthHint = ld.minimumWidth = 200;
    cBrowser.setLayoutData( ld );
    final GridLayout bLayout = new GridLayout();
    bLayout.marginWidth = bLayout.horizontalSpacing = 0;
    cBrowser.setLayout( bLayout );

    m_reposTree = getTSBrowser( cBrowser );

    final Composite cClient = new Composite( m_subGroup, SWT.NULL );
    cClient.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    final GridLayout cLayout = new GridLayout( 2, true );
    cLayout.marginWidth = bLayout.horizontalSpacing = 0;
    cClient.setLayout( cLayout );

    renderDetails( m_reposTree, cClient );

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

// final Object lnkTimeSeries = m_condition.getFeature().getProperty(
// ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_LNK_TIME_SERIES );
// if( lnkTimeSeries instanceof TimeseriesLinkType )
// {
// final IRepositoryContainer container = m_model.getRepositoryContainer();
//
// final TimeseriesLinkType lnk = (TimeseriesLinkType) lnkTimeSeries;
// final IRepository[] repositories = container.getRepositories();
//
// IRepositoryItem item = null;
//
// for( final IRepository repository : repositories )
// try
// {
//
// item = repository.findItem( lnk.getHref() );
// break;
// }
// catch( final RepositoryException e )
// {
// continue;
// }
//
// if( !(item instanceof FileItem) )
// return viewer;
// }

    return viewer;
  }

  protected void checkPageCompleted( )
  {
    /* time series repository selected? check selection and values! */
    if( TS_TYPE.eTimeSeries.equals( m_type ) )
    {

      if( m_selectedTreeItem == null )
      {
        setMessage( null );
        setErrorMessage( "No time series repository item selected." );
        setPageComplete( false );

        return;
      }

      final Object adapter = m_selectedTreeItem.getAdapter( IObservation.class );
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
    else if( TS_TYPE.eConstant.equals( m_type ) )
    {
      if( m_tConstant.getText() == null || "".equals( m_tConstant.getText().trim() ) )
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

  public TS_TYPE getTypeOfTimeSeries( )
  {
    return m_type;
  }

  public ZmlObservationItem getZmlObservationItem( )
  {
    return m_selectedTreeItem;
  }

}
