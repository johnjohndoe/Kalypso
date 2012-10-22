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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.model.ICommandPoster;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ogc.gml.selection.IFeatureSelectionListener;
import org.kalypsodeegree.model.elevation.IElevationModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_PolygonPatch;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Madanagopal
 * @author Patrice Congo
 */
public class ApplyElevationWidgetDataModel extends KeyBasedDataModel implements IFeatureSelectionListener
{
  public static final String SELECTED_NODE_KEY = "_SELECTED_NODE_KEY"; //$NON-NLS-1$

  public static final String ELEVATION_THEME = "_ELEVATION_THEME_"; //$NON-NLS-1$

  public static final String NODE_THEME = "_NODE_THEME_"; //$NON-NLS-1$

  private static final String[] KEYS = { ITerrainModel.class.toString(), ITerrainElevationModelSystem.class.toString(), ITerrainElevationModel.class.toString(), IMapModell.class.toString(),
      SELECTED_NODE_KEY, GM_PolygonPatch.class.toString(), IFEDiscretisationModel1d2d.class.toString(), IMapPanel.class.toString(), ELEVATION_THEME, NODE_THEME };

  private final boolean m_ignoreMapSelection = false;

  private final IScenarioDataProvider m_dataProvider;

  private List<IFE1D2DNode> m_selectedNodeList;

  public ApplyElevationWidgetDataModel( )
  {
    super( KEYS, null );

    m_dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
  }

  public void setMapModell( final IMapModell mapModell )
  {
    // this.mapModell = mapModell;
    setData( IMapModell.class.toString(), mapModell );
  }

  public IMapModell getMapModell( )
  {
    // return mapModell;
    return (IMapModell)getData( IMapModell.class.toString() );
  }

  public IFEDiscretisationModel1d2d getDiscretisationModel( )
  {
    try
    {
      return m_dataProvider.getModel( IFEDiscretisationModel1d2d.class.getName() );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }
    return null;
  }

  public CommandableWorkspace getDiscretisationModelWorkspace( )
  {
    try
    {
      return ((ICommandPoster)m_dataProvider).getCommandableWorkSpace( IFEDiscretisationModel1d2d.class.getName() );
    }
    catch( final IllegalArgumentException e )
    {
      e.printStackTrace();
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }
    return null;
  }

  private ITerrainElevationModel[] getElevationModels( )
  {
    return (ITerrainElevationModel[])getData( ITerrainElevationModel.class.toString() );
  }

  public void setElevationModels( final ITerrainElevationModel[] elevationModels )
  {
    setData( ITerrainElevationModel.class.toString(), elevationModels, true );
  }

  public ITerrainElevationModelSystem getElevationModelSystem( )
  {
    return (ITerrainElevationModelSystem)getData( ITerrainElevationModelSystem.class.toString() );
  }

  public void setElevationModelSystem( final ITerrainElevationModelSystem elevationModelSystem )
  {
    setData( ITerrainElevationModelSystem.class.toString(), elevationModelSystem );
  }

  public List<IFE1D2DNode> getSelectedNode( )
  {
    return (List<IFE1D2DNode>)getData( SELECTED_NODE_KEY );
  }

  public void setSelectedNode( final List<IFE1D2DNode> selectedNode )
  {
    setData( SELECTED_NODE_KEY, selectedNode, false );
  }

  public GM_PolygonPatch getSelectionArea( )
  {
    // return selectionArea;
    return (GM_PolygonPatch)getData( GM_PolygonPatch.class.toString() );
  }

  public void setSelectionArea( final GM_PolygonPatch selectionArea )
  {
    // this.selectionArea = selectionArea;
    setData( GM_PolygonPatch.class.toString(), selectionArea );
  }

  public IKalypsoFeatureTheme getElevationTheme( )
  {
    // return elevationTheme;
    return (IKalypsoFeatureTheme)getData( ELEVATION_THEME );
  }

  public void setElevationTheme( final IKalypsoFeatureTheme elevationTheme )
  {
    // this.elevationTheme = elevationTheme;
    setData( ELEVATION_THEME, elevationTheme );
  }

  public IMapPanel getMapPanel( )
  {
    return (IMapPanel)getData( IMapPanel.class.toString() );
  }

  public void setMapPanel( final IMapPanel mapPanel )
  {
    mapPanel.getSelectionManager().addSelectionListener( this );
    setData( IMapPanel.class.toString(), mapPanel );
  }

  @Override
  public void selectionChanged( final Object source, final IFeatureSelection selection )
  {
    // TODO pat maybe get th list from dataModel
    if( m_ignoreMapSelection )
    {
      return;
    }
    final List<IFE1D2DNode> nodes = new ArrayList<>();
    for( final EasyFeatureWrapper wrapper : selection.getAllFeatures() )
    {
      final Feature feature = wrapper.getFeature();
      if( feature instanceof IFE1D2DNode )
      {
        nodes.add( (IFE1D2DNode)feature.getAdapter( IFE1D2DNode.class ) );
      }
    }

    setSelectedNode( nodes );
  }

  public void setSelectedNodeList( final List<IFE1D2DNode> selectionNodeList )
  {
    m_selectedNodeList = selectionNodeList;
  }

  public List<IFE1D2DNode> getSelectedNodeList( )
  {
    return m_selectedNodeList;
  }

  public void saveModels( ) throws CoreException
  {
    m_dataProvider.saveModel( new NullProgressMonitor() );
  }

  /**
   * Returns an elevation model based on the curretn selection of models. The node heights are assigned according to
   * this model.<br/>
   * <ul>
   * <li>If no model is selected, all models are used.</li>
   * <li>If one ore more models are selected, only these models are used to determine the height.</li>
   * </ul>
   */
  public final IElevationModel getElevationProvider( )
  {
    final ITerrainElevationModel[] selectedModels = getElevationModels();
    if( selectedModels == null )
      return getElevationModelSystem();

    return new ElevationArrayModel( selectedModels );
  }
}