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

import java.net.MalformedURLException;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.gml.binding.commons.IGeoStatus;
import org.kalypsodeegree_impl.gml.binding.commons.IStatusCollection;

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

  public void createControl( final FormToolkit toolkit, final Composite parent )
  {
    final TableViewer guiProblemViewer = guiProblemViewer( parent, toolkit );

    final CalculationUnitDataModel dataModel = m_dataModel;
    dataModel.addKeyBasedDataChangeListener( new KeyBasedDataModelChangeListener()
    {
      public void dataChanged( final String key, final Object newValue )
      {
        final Display display = parent.getDisplay();
        final Runnable runnable = new Runnable()
        {
          public void run( )
          {
            if( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER.equals( key ) && newValue instanceof ICalculationUnit )
            {
              final List<IGeoStatus> list = findGeoStatusCollection( (ICalculationUnit) newValue );
              if( list != null )
                guiProblemViewer.setInput( list );
              else
                guiProblemViewer.setInput( new String[] { "Keine Log-Datei vorhanden" } );
            }
          }
        };
        display.syncExec( runnable );
      }
    } );
  }

  private TableViewer guiProblemViewer( final Composite parent, final FormToolkit toolkit )
  {
    final Composite rootComposite = toolkit.createComposite( parent );
    rootComposite.setLayout( new GridLayout() );

    final TableViewer logTableViewer = new TableViewer( rootComposite, SWT.BORDER );

    final Table table = logTableViewer.getTable();
    table.setLinesVisible( true );
    table.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    // TODO: change label provider!
    logTableViewer.setLabelProvider( new LabelProvider() );

    logTableViewer.setContentProvider( new ArrayContentProvider() );
    logTableViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleSelectionChanged( event );
      }
    } );

    final TableColumn lineColumn = new TableColumn( table, SWT.LEFT );
    lineColumn.setWidth( 200 );

    return logTableViewer;
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

      if( location != null )
      {
        final MapPanel mapPanel = m_dataModel.getData( MapPanel.class, ICommonKeys.KEY_MAP_PANEL );
        mapPanel.setBoundingBox( location.getEnvelope() );
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

      final IFeatureWrapperCollection<IResultMeta> children = calcUnitMeta.getChildren();
      for( final IResultMeta resultMeta : children )
      {
        if( resultMeta instanceof IDocumentResultMeta )
        {
          final IDocumentResultMeta doc = (IDocumentResultMeta) resultMeta;
          if( doc.getDocumentType() == IDocumentResultMeta.DOCUMENTTYPE.log )
          {
            if( doc.getPath().toString().endsWith( "log.gml" ) )
            {
              final IPath fullPath = doc.getFullPath();
              final IFile logFile = scenarioFolder.getFile( fullPath );
              final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( ResourceUtilities.createURL( logFile ), null );
              return (List<IGeoStatus>) workspace.getRootFeature().getAdapter( IStatusCollection.class );
            }
          }
        }
      }
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return null;
  }
}
