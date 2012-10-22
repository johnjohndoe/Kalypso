/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.ViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition.BOUNDARY_TYPE;
import org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitMetaTable;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;

/**
 * @author Madanagopal
 * @author Dejan Antanaskovic
 */
public class SelectedCalculationComponent
{
  private TableViewer m_subCalcUnitsTableViewer;

  private Text m_txtCalcUnitName;

  private Text m_txtCalcUnitType;

  private Text m_txtNumberOfElements2D;

  private Text m_txtNumberOfElements1D;

  private Text m_numberOfBoundaryConditions;

  private Text m_numberOfBoundaryConditionsWaves;

  private Text m_numberOfContinuityLines;

  private final CalculationUnitDataModel m_dataModel;

  public SelectedCalculationComponent( final CalculationUnitDataModel dataModel )
  {
    m_dataModel = dataModel;
  }

  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    final CalculationUnitDataModel dataModel = m_dataModel;
    final Display display = parent.getDisplay();
    dataModel.addKeyBasedDataChangeListener( new KeyBasedDataModelChangeListener()
    {
      @Override
      public void dataChanged( final String key, final Object newValue )
      {
        final Runnable runnable = new Runnable()
        {
          @Override
          public void run( )
          {
            if( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER.equals( key ) )
            {
              updateThisSection( newValue );
            }
          }
        };
        display.syncExec( runnable );
      }
    } );

    final IFEDiscretisationModel1d2d discModel = dataModel.getData( IFEDiscretisationModel1d2d.class, ICommonKeys.KEY_DISCRETISATION_MODEL );
    final GMLWorkspace workspace = discModel.getWorkspace();
    final ModellEventListener modelListener = new ModellEventListener()
    {
      @Override
      public void onModellChange( final ModellEvent modellEvent )
      {
        final Runnable runnable = new Runnable()
        {
          @Override
          public void run( )
          {
            final ICalculationUnit selectedCalculationUnit = dataModel.getSelectedCalculationUnit();
            updateThisSection( selectedCalculationUnit );
          }
        };
        display.syncExec( runnable );
      }
    };
    workspace.addModellListener( modelListener );

    final Composite rootComposite = toolkit.createComposite( parent, SWT.NONE );
    rootComposite.setLayout( new GridLayout( 2, false ) );

    createGUI( rootComposite, toolkit );

    rootComposite.addDisposeListener( new DisposeListener()
    {
      @Override
      public void widgetDisposed( final DisposeEvent e )
      {
        workspace.removeModellListener( modelListener );
      }
    } );

    return rootComposite;
  }

  protected void updateThisSection( final Object newValue )
  {
    m_txtCalcUnitName.setText( "" ); //$NON-NLS-1$
    m_txtCalcUnitType.setText( "" ); //$NON-NLS-1$
    m_txtNumberOfElements1D.setText( "0" ); //$NON-NLS-1$
    m_txtNumberOfElements2D.setText( "0" ); //$NON-NLS-1$
    m_numberOfContinuityLines.setText( "0" ); //$NON-NLS-1$
    m_numberOfBoundaryConditions.setText( "0" ); //$NON-NLS-1$
    m_numberOfBoundaryConditionsWaves.setText( "0" ); //$NON-NLS-1$
    m_subCalcUnitsTableViewer.setInput( new ICalculationUnit1D2D[] {} );

    final ICalculationUnit calcUnit = (ICalculationUnit) newValue;
    if( calcUnit != null )
    {
      m_txtCalcUnitName.setText( calcUnit.getName() );
      m_numberOfContinuityLines.setText( String.valueOf( calcUnit.getContinuityLines().size() ) );
      m_numberOfBoundaryConditions.setText( String.valueOf( CalcUnitOps.countAssignedBoundaryConditions( getBoundaryConditions(), calcUnit, BOUNDARY_TYPE.HydroBoundary ) ) );
      m_numberOfBoundaryConditionsWaves.setText( String.valueOf( CalcUnitOps.countAssignedBoundaryConditions( getBoundaryConditions(), calcUnit, BOUNDARY_TYPE.WavesBoundary ) ) );
    }

    if( newValue instanceof ICalculationUnit1D )
    {
      m_txtCalcUnitType.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.SelectedCalculationComponent.6" ) ); //$NON-NLS-1$
      m_subCalcUnitsTableViewer.setInput( new Object[] {} );
      m_txtNumberOfElements1D.setText( String.valueOf( ((ICalculationUnit)newValue).getElements1D().size() ) );
    }
    else if( newValue instanceof ICalculationUnit2D )
    {
      m_txtCalcUnitType.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.SelectedCalculationComponent.7" ) ); //$NON-NLS-1$
      m_subCalcUnitsTableViewer.setInput( new Object[] {} );
      m_txtNumberOfElements2D.setText( String.valueOf( ((ICalculationUnit)newValue).getElements2D().size() ) );
    }
    else if( newValue instanceof ICalculationUnit1D2D )
    {
      m_txtCalcUnitType.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.SelectedCalculationComponent.8" ) ); //$NON-NLS-1$
      m_subCalcUnitsTableViewer.setInput( ((ICalculationUnit1D2D)newValue).getSubCalculationUnits().toArray() );
      m_txtNumberOfElements1D.setText( String.valueOf( ((ICalculationUnit)newValue).getElements1D().size() ) );
      m_txtNumberOfElements2D.setText( String.valueOf( ((ICalculationUnit)newValue).getElements2D().size() ) );
    }
  }

  private List<IBoundaryCondition> getBoundaryConditions( )
  {
    final CommandableWorkspace workspace = KeyBasedDataModelUtil.getBCWorkSpace( m_dataModel );
    final Feature bcHolderFeature = workspace.getRootFeature();
    final IFlowRelationshipModel flowRelationshipsModel = (IFlowRelationshipModel) bcHolderFeature.getAdapter( IFlowRelationshipModel.class );
    final IFeatureBindingCollection<IFlowRelationship> allFlowRelationshipsList = flowRelationshipsModel.getFlowRelationsShips();
    final List<IBoundaryCondition> conditions = new ArrayList<>();
    for( final Feature relationship : allFlowRelationshipsList )
    {
      if( relationship instanceof IBoundaryCondition )
        conditions.add( (IBoundaryCondition) relationship );
    }
    return conditions;
  }

  private void createGUI( final Composite parent, final FormToolkit toolkit )
  {
    toolkit.createLabel( parent, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.SelectedCalculationComponent.9" ) ); //$NON-NLS-1$
    m_txtCalcUnitName = toolkit.createText( parent, "", SWT.SINGLE | SWT.BORDER ); //$NON-NLS-1$
    m_txtCalcUnitName.setEnabled( false );
    m_txtCalcUnitName.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );

    toolkit.createLabel( parent, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.SelectedCalculationComponent.11" ) ); //$NON-NLS-1$
    m_txtCalcUnitType = toolkit.createText( parent, "", SWT.SINGLE | SWT.BORDER ); //$NON-NLS-1$
    m_txtCalcUnitType.setEnabled( false );
    m_txtCalcUnitType.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );

    toolkit.createLabel( parent, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.SelectedCalculationComponent.13" ) ); //$NON-NLS-1$
    m_txtNumberOfElements1D = toolkit.createText( parent, "", SWT.SINGLE | SWT.BORDER ); //$NON-NLS-1$
    m_txtNumberOfElements1D.setEnabled( false );
    m_txtNumberOfElements1D.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );

    toolkit.createLabel( parent, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.SelectedCalculationComponent.15" ) ); //$NON-NLS-1$
    m_txtNumberOfElements2D = toolkit.createText( parent, "", SWT.SINGLE | SWT.BORDER ); //$NON-NLS-1$
    m_txtNumberOfElements2D.setEnabled( false );
    m_txtNumberOfElements2D.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );

    toolkit.createLabel( parent, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.SelectedCalculationComponent.17" ) ); //$NON-NLS-1$
    m_numberOfContinuityLines = toolkit.createText( parent, "", SWT.SINGLE | SWT.BORDER ); //$NON-NLS-1$
    m_numberOfContinuityLines.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );
    m_numberOfContinuityLines.setEnabled( false );

    toolkit.createLabel( parent, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.SelectedCalculationComponent.19" ) ); //$NON-NLS-1$
    m_numberOfBoundaryConditions = toolkit.createText( parent, "", SWT.SINGLE | SWT.BORDER ); //$NON-NLS-1$
    m_numberOfBoundaryConditions.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );
    m_numberOfBoundaryConditions.setEnabled( false );

    toolkit.createLabel( parent, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.SelectedCalculationComponent.20" ) ); //$NON-NLS-1$
    m_numberOfBoundaryConditionsWaves = toolkit.createText( parent, "", SWT.SINGLE | SWT.BORDER ); //$NON-NLS-1$
    m_numberOfBoundaryConditionsWaves.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );
    m_numberOfBoundaryConditionsWaves.setEnabled( false );

    final Label subUnitLabel = toolkit.createLabel( parent, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.SelectedCalculationComponent.21" ), SWT.BEGINNING ); //$NON-NLS-1$
    subUnitLabel.setLayoutData( new GridData( SWT.LEFT, SWT.BEGINNING, false, false ) );
    m_subCalcUnitsTableViewer = new TableViewer( parent, SWT.FILL | SWT.BORDER );

    final Table subCalcUnitsTable = m_subCalcUnitsTableViewer.getTable();
    subCalcUnitsTable.addControlListener( new ColumnsResizeControlListener() );

    subCalcUnitsTable.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 1, 1 ) );
    subCalcUnitsTable.setLinesVisible( false );
    m_subCalcUnitsTableViewer.setContentProvider( new ArrayContentProvider() );

    final ViewerColumn nameColumn = CalculationUnitMetaTable.createUnitNameColumn( m_subCalcUnitsTableViewer );
    ColumnViewerSorter.setSortState( nameColumn, Boolean.FALSE );
  }
}