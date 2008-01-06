/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.internal.ide.IDEInternalWorkbenchImages;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.jface.viewers.DefaultTableViewer;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.gml.ui.jface.FeatureWrapperLabelProvider;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.IGeoStatus;
import org.kalypsodeegree_impl.gml.binding.commons.IStatusCollection;
import org.kalypsodeegree_impl.gml.binding.commons.NamedFeatureHelper;
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
    final Composite rootComposite = toolkit.createComposite( parent, SWT.NONE );
    rootComposite.setLayout( new GridLayout() );

    guiProblemViewer( rootComposite, toolkit );

    return rootComposite;
  }

  private void guiProblemViewer( final Composite parent, final FormToolkit toolkit )
  {
    final Label noLogLabel = toolkit.createLabel( parent, "Kein Log vorhanden." );
    final GridData noLogGridData = new GridData( SWT.FILL, SWT.FILL, true, true );
    noLogLabel.setLayoutData( noLogGridData );
    noLogLabel.setToolTipText( "Vermutlich wurde noch keine Berechnung durchgeführt." );

    final DefaultTableViewer logTableViewer = new DefaultTableViewer( parent, SWT.FULL_SELECTION | SWT.BORDER );

    final Table table = logTableViewer.getTable();
    toolkit.adapt( table );

    table.setHeaderVisible( true );
    table.setLinesVisible( true );
    final GridData tableGridData = new GridData( SWT.FILL, SWT.FILL, true, true );
    table.setLayoutData( tableGridData );

    final FeatureWrapperLabelProvider labelProvider = new FeatureWrapperLabelProvider( logTableViewer )
    {
      /**
       * Get the IDE image at path.
       * 
       * @param path
       * @return Image
       */
      @SuppressWarnings("restriction")
      private Image getIDEImage( String constantName )
      {
        return JFaceResources.getResources().createImageWithDefault( IDEInternalWorkbenchImages.getImageDescriptor( constantName ) );
      }

      /**
       * @see org.kalypso.gml.ui.jface.FeatureWrapperLabelProvider#getColumnImage(java.lang.Object, int)
       */
      @SuppressWarnings("restriction")
      @Override
      public Image getColumnImage( Object element, int columnIndex )
      {
        if( columnIndex == 0 )
        {
          final IGeoStatus status = (IGeoStatus) element;
          switch( status.getSeverity() )
          {
            case IGeoStatus.OK:
              return KalypsoModel1D2DPlugin.getImageProvider().getImage( KalypsoModel1D2DUIImages.IMGKEY.OK );

              // case IGeoStatus.CANCEL:
              // return getIDEImage( IDEInternalWorkbenchImages.IMG_OBJS_INCOMPLETE_TSK );

            case IGeoStatus.ERROR:
              return getIDEImage( IDEInternalWorkbenchImages.IMG_OBJS_ERROR_PATH );

            case IGeoStatus.WARNING:
              return getIDEImage( IDEInternalWorkbenchImages.IMG_OBJS_WARNING_PATH );

            case IGeoStatus.INFO:
              return getIDEImage( IDEInternalWorkbenchImages.IMG_OBJS_INFO_PATH );

            default:
              return null;
          }
        }

        return super.getColumnImage( element, columnIndex );
      }

      /**
       * @see org.kalypso.gml.ui.jface.FeatureWrapperLabelProvider#getColumnText(java.lang.Object, int)
       */
      @Override
      public String getColumnText( Object element, int columnIndex )
      {
        if( columnIndex == 0 )
          return "";

        return super.getColumnText( element, columnIndex );
      }
    };

    logTableViewer.setLabelProvider( labelProvider );
    logTableViewer.setContentProvider( new ArrayContentProvider() );
    logTableViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleSelectionChanged( event );
      }
    } );

    logTableViewer.addColumn( IGeoStatus.QNAME_PROP_STATUS_SEVERITY.toString(), "Art", null, 30, 0, false, SWT.CENTER, false );
    logTableViewer.addColumn( NamedFeatureHelper.GML_DESCRIPTION.toString(), "Beschreibung", null, 500, 0, false, SWT.LEFT, true );
    logTableViewer.addColumn( IGeoStatus.QNAME_PROP_STATUS_TIME.toString(), "Zeit", null, 150, 0, false, SWT.LEFT, false );

    noLogLabel.setVisible( false );
    noLogGridData.exclude = true;
    table.setVisible( false );
    tableGridData.exclude = true;

    /* Data change events */
    final CalculationUnitDataModel dataModel = m_dataModel;
    dataModel.addKeyBasedDataChangeListener( new KeyBasedDataModelChangeListener()
    {
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

      final MapPanel mapPanel = m_dataModel.getData( MapPanel.class, ICommonKeys.KEY_MAP_PANEL );

      if( location instanceof GM_Point )
      {
        final GM_Envelope panedBBox = mapPanel.getBoundingBox().getPaned( (GM_Point) location );
        final GM_Envelope scaledEnvelope = GeometryUtilities.scaleEnvelope( panedBBox, 0.7 );
        mapPanel.setBoundingBox( scaledEnvelope );
      }
      else if( location != null )
      {
        final GM_Envelope env = GeometryUtilities.getEnvelope( location );
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
      final IResource logResource = calcUnitFolder.findMember( "simulation_log.gml" );
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( ResourceUtilities.createURL( logResource ), null );
      return (List<IGeoStatus>) workspace.getRootFeature().getAdapter( IStatusCollection.class );

      // final IFeatureWrapperCollection<IResultMeta> children = calcUnitMeta.getChildren();
      // for( final IResultMeta resultMeta : children )
      // {
      // if( resultMeta instanceof IDocumentResultMeta )
      // {
      // final IDocumentResultMeta doc = (IDocumentResultMeta) resultMeta;
      // if( doc.getDocumentType() == IDocumentResultMeta.DOCUMENTTYPE.log )
      // {
      // // HACK at the moment, the log has no unique identifier, so we use the end of the file name
      // if( doc.getPath().toString().endsWith( "log.gml" ) )
      // {
      // final IPath fullPath = doc.getFullPath();
      // final IFile logFile = scenarioFolder.getFile( fullPath );
      // final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( ResourceUtilities.createURL( logFile ), null
      // );
      // return (List<IGeoStatus>) workspace.getRootFeature().getAdapter( IStatusCollection.class );
      // }
      // }
      // }
      // }
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
    }

    return null;
  }
}
