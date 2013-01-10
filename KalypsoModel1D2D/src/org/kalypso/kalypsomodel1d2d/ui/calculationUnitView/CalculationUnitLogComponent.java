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
package org.kalypso.kalypsomodel1d2d.ui.calculationUnitView;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.core.status.StatusTableViewer;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.IGeoStatus;
import org.kalypsodeegree_impl.gml.binding.commons.IStatusCollection;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * This component shows the geo-log of the selected calculation unit in a table view.
 * 
 * @author Gernot Belger
 */
public class CalculationUnitLogComponent
{
  private final CalculationUnitDataModel m_dataModel;

  public CalculationUnitLogComponent( final CalculationUnitDataModel dataModel )
  {
    m_dataModel = dataModel;
  }

  public Control createControl( final FormToolkit toolkit, final Composite parent )
  {
    final Composite rootComposite = toolkit.createComposite( parent, SWT.NONE );
    GridLayoutFactory.fillDefaults().applyTo( rootComposite );

    guiProblemViewer( rootComposite, toolkit );

    return rootComposite;
  }

  private void guiProblemViewer( final Composite parent, final FormToolkit toolkit )
  {
    final Label noLogLabel = toolkit.createLabel( parent, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitLogComponent.0" ) ); //$NON-NLS-1$
    final GridData noLogGridData = new GridData( SWT.CENTER, SWT.CENTER, true, true );
    noLogLabel.setLayoutData( noLogGridData );
    noLogLabel.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitLogComponent.1" ) ); //$NON-NLS-1$

    final StatusTableViewer logViewer = new StatusTableViewer( parent, SWT.FULL_SELECTION | SWT.BORDER );
    logViewer.addTimeColumn();
    logViewer.setInput( null );

    final Control logControl = logViewer.getControl();

    toolkit.adapt( (Composite)logControl );

    final GridData tableGridData = new GridData( SWT.FILL, SWT.FILL, true, true );
    logControl.setLayoutData( tableGridData );

    logViewer.getViewer().addPostSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleSelectionChanged( event );
      }
    } );

    noLogLabel.setVisible( true );
    noLogGridData.exclude = false;
    logControl.setVisible( false );
    tableGridData.exclude = true;

    /* Data change events */
    final CalculationUnitDataModel dataModel = m_dataModel;
    final KeyBasedDataModelChangeListener dataListener = new KeyBasedDataModelChangeListener()
    {
      @Override
      public void dataChanged( final String key, final Object newValue )
      {
        if( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER.equals( key ) && newValue instanceof ICalculationUnit )
        {
          final IStatusCollection collection = findGeoStatusCollection( (ICalculationUnit)newValue );
          if( collection == null || collection.isEmpty() )
          {
            // FIXME: makes no sense: noLogLabel should show empty log, but actually its visible if table is not selected...
            logControl.setVisible( false );
            noLogLabel.setVisible( true );
            noLogGridData.exclude = false;
            tableGridData.exclude = true;
            logViewer.setInput( new IStatus[] {} );
          }
          else
          {
            logControl.setVisible( true );
            noLogLabel.setVisible( false );
            noLogGridData.exclude = true;
            tableGridData.exclude = false;
            final IFeatureBindingCollection<IGeoStatus> statusList = collection.getStatusList();
            logViewer.setInput( statusList.toArray( new IStatus[statusList.size()] ) );
          }

          parent.layout();
        }
      }
    };
    dataModel.addKeyBasedDataChangeListener( dataListener );
  }

  protected void handleSelectionChanged( final SelectionChangedEvent event )
  {
    final IStructuredSelection selection = (IStructuredSelection)event.getSelection();
    if( selection.isEmpty() )
      return;

    final Object firstElement = selection.getFirstElement();
    if( firstElement instanceof IGeoStatus )
    {
      final IGeoStatus status = (IGeoStatus)firstElement;
      final GM_Object location = status.getLocation();

      final IMapPanel mapPanel = m_dataModel.getData( IMapPanel.class, ICommonKeys.KEY_MAP_PANEL );

      if( location instanceof GM_Point )
      {
        final GM_Envelope panedBBox = mapPanel.getBoundingBox().getPaned( (GM_Point)location );
        final GM_Envelope scaledEnvelope = GeometryUtilities.scaleEnvelope( panedBBox, 0.0 );
        mapPanel.setBoundingBox( scaledEnvelope );
      }
      else if( location != null )
      {
        final GM_Envelope env = location.getEnvelope();
        final GM_Envelope envelope = env.getBuffer( env.getWidth() / 10 );
        mapPanel.setBoundingBox( envelope );
      }
    }
  }

  protected IStatusCollection findGeoStatusCollection( final ICalculationUnit calcUnit )
  {
    try
    {
      final IScenarioDataProvider dataProvider = m_dataModel.getData( IScenarioDataProvider.class, ICommonKeys.KEY_DATA_PROVIDER );
      final IContainer scenarioFolder = dataProvider.getScenarioFolder();
      final IScenarioResultMeta scenarioMeta = dataProvider.getModel( IScenarioResultMeta.class.getName() );
      final ICalcUnitResultMeta calcUnitMeta = scenarioMeta.findCalcUnitMetaResult( calcUnit.getId() );
      if( calcUnitMeta == null )
        return null;

      final IFolder calcUnitFolder = scenarioFolder.getFolder( calcUnitMeta.getFullPath() );
      final IResource logResource = calcUnitFolder.findMember( "simulation_log.gml" ); //$NON-NLS-1$
      if( logResource == null )
        return null;

      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( ResourceUtilities.createURL( logResource ), null );
      return (IStatusCollection)workspace.getRootFeature().getAdapter( IStatusCollection.class );
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
    }

    return null;
  }
}