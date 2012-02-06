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

import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.jface.viewers.DefaultTableViewer;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.core.status.StatusLabelProvider;
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
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.IGeoStatus;
import org.kalypsodeegree_impl.gml.binding.commons.IStatusCollection;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

import de.renew.workflow.connector.cases.ICaseDataProvider;

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
    final Composite rootComposite = toolkit.createComposite( parent, SWT.NONE | SWT.END );
    rootComposite.setLayout( new GridLayout() );

    guiProblemViewer( rootComposite, toolkit );

    return rootComposite;
  }

  private void guiProblemViewer( final Composite parent, final FormToolkit toolkit )
  {
    final Label noLogLabel = toolkit.createLabel( parent, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitLogComponent.0" ) ); //$NON-NLS-1$
    final GridData noLogGridData = new GridData( SWT.FILL, SWT.END, true, true );
    noLogLabel.setLayoutData( noLogGridData );
    noLogLabel.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitLogComponent.1" ) ); //$NON-NLS-1$

    final DefaultTableViewer logTableViewer = new DefaultTableViewer( parent, SWT.FULL_SELECTION | SWT.BORDER | SWT.V_SCROLL );

    final Table table = logTableViewer.getTable();
    toolkit.adapt( table );

    table.setHeaderVisible( true );
    table.setLinesVisible( true );
    final GridData tableGridData = new GridData( SWT.FILL, SWT.END, true, true );
    table.setLayoutData( tableGridData );

    StatusLabelProvider.addSeverityColumn( logTableViewer );
    StatusLabelProvider.addMessageColumn( logTableViewer );
    StatusLabelProvider.addTimeColumn( logTableViewer );

    logTableViewer.setContentProvider( new ArrayContentProvider() );
    logTableViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleSelectionChanged( event );
      }
    } );

    logTableViewer.addDoubleClickListener( new IDoubleClickListener()
    {
      @Override
      public void doubleClick( final DoubleClickEvent event )
      {
        final IStructuredSelection sel = (IStructuredSelection) event.getSelection();
        final IStatus status = (IStatus) sel.getFirstElement();
        if( status != null )
        {
          final StatusDialog dialog = new StatusDialog( parent.getShell(), status, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitLogComponent.2" ) ); //$NON-NLS-1$
          dialog.open();
        }
      }
    } );

    noLogLabel.setVisible( false );
    noLogGridData.exclude = true;
    table.setVisible( false );
    tableGridData.exclude = true;

    /* Data change events */
    final CalculationUnitDataModel dataModel = m_dataModel;
    dataModel.addKeyBasedDataChangeListener( new KeyBasedDataModelChangeListener()
    {
      @Override
      public void dataChanged( final String key, final Object newValue )
      {
        if( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER.equals( key ) && newValue instanceof ICalculationUnit )
        {
          final List<IGeoStatus> list = findGeoStatusCollection( (ICalculationUnit) newValue );
          if( list == null || list.isEmpty() )
          {
            table.setVisible( false );
            noLogLabel.setVisible( true );
            noLogGridData.exclude = false;
            tableGridData.exclude = true;
            ViewerUtilities.setInput( logTableViewer, new Object[] {}, false );
          }
          else
          {
            table.setVisible( true );
            noLogLabel.setVisible( false );
            noLogGridData.exclude = true;
            tableGridData.exclude = false;
            ViewerUtilities.setInput( logTableViewer, list, false );
          }

          parent.layout();
        }
      }
    } );

  }

  protected void handleSelectionChanged( final SelectionChangedEvent event )
  {
    final IStructuredSelection selection = (IStructuredSelection) event.getSelection();

    if( selection.isEmpty() )
      return;

    final Object firstElement = selection.getFirstElement();
    if( firstElement instanceof IGeoStatus )
    {
      final IGeoStatus status = (IGeoStatus) firstElement;
      final GM_Object location = status.getLocation();

      final IMapPanel mapPanel = m_dataModel.getData( IMapPanel.class, ICommonKeys.KEY_MAP_PANEL );

      if( location instanceof GM_Point )
      {
        final GM_Envelope panedBBox = mapPanel.getBoundingBox().getPaned( (GM_Point) location );
        final GM_Envelope scaledEnvelope = GeometryUtilities.scaleEnvelope( panedBBox, 0.7 );
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

  protected List<IGeoStatus> findGeoStatusCollection( final ICalculationUnit calcUnit )
  {
    try
    {
      final SzenarioDataProvider dataProvider = (SzenarioDataProvider) m_dataModel.getData( ICaseDataProvider.class, ICommonKeys.KEY_DATA_PROVIDER );
      final IContainer scenarioFolder = dataProvider.getScenarioFolder();
      final IScenarioResultMeta scenarioMeta = dataProvider.getModel( IScenarioResultMeta.class );
      final ICalcUnitResultMeta calcUnitMeta = scenarioMeta.findCalcUnitMetaResult( calcUnit.getGmlID() );
      if( calcUnitMeta == null )
        return null;

      final IFolder calcUnitFolder = scenarioFolder.getFolder( calcUnitMeta.getFullPath() );
      final IResource logResource = calcUnitFolder.findMember( "simulation_log.gml" ); //$NON-NLS-1$
      if( logResource == null )
        return null;

      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( ResourceUtilities.createURL( logResource ), null );
      return (List<IGeoStatus>) workspace.getRootFeature().getAdapter( IStatusCollection.class );
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
    }

    return null;
  }

}
