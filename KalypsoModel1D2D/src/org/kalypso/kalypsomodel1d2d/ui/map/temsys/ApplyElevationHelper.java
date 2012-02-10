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

import org.kalypso.kalypsomodel1d2d.ops.NodeOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeNodePositionCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele.ChangeTerrainElevationSystemCommand;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * @author Thomas Jung
 * 
 */
public class ApplyElevationHelper
{
  public static void assignElevationToSelectedNodes( final ApplyElevationWidgetDataModel dataModel, final List<IFE1D2DNode> nodeList ) throws Exception
  {
    final IFeatureWrapperCollection<ITerrainElevationModel> elevationModels = dataModel.getTerrainElevationModels();
    if( elevationModels == null )
    {
      return;
    }

    final IMapPanel mapPanel = dataModel.getMapPanel();
    if( mapPanel == null )
      return;

    final IFEDiscretisationModel1d2d model1d2d = dataModel.getDiscretisationModel();

    if( model1d2d == null )
      return;

    IElevationProvider elevationProvider = null;
    // dataModel.getElevationModel();
    // if( elevationProvider == null )
    // {
    // elevationProvider = dataModel.getElevationModelSystem();
    // if( elevationProvider == null )
    // return;
    // }

    final CommandableWorkspace workspace = dataModel.getDiscretisationModelWorkspace();
    if( workspace == null )
      return;

    List<IFE1D2DNode> lListNodesToAssign = new ArrayList<IFE1D2DNode>();
    lListNodesToAssign.addAll( nodeList );
    // to provide real assign of elevations according to selected order in elevations model view
    for( int i = 0; i < elevationModels.size() && lListNodesToAssign.size() > 0; ++i )
    {
      elevationProvider = elevationModels.get( i );
      if( elevationProvider == null )
        continue;
      final ChangeTerrainElevationSystemCommand compositeCommand = new ChangeTerrainElevationSystemCommand( workspace, model1d2d, dataModel.getElevationModelSystem() );
      ChangeNodePositionCommand changePosCmd;

      for( int j = lListNodesToAssign.size() - 1; j >= 0; --j )
      // for( final IFE1D2DNode node : nodeList )
      {
        IFE1D2DNode node = lListNodesToAssign.get( j );
        if( node != null )
        {
          try
          {
            final double elevation = elevationProvider.getElevation( node.getPoint() );
            changePosCmd = new ChangeNodePositionCommand( model1d2d, node, elevation, false );
            changePosCmd.process();
            compositeCommand.addCommand( changePosCmd, null );
            if( !Double.isNaN( elevation ) )
            {
              lListNodesToAssign.remove( node );
            }
          }
          catch( Exception e )
          {
          }
        }
      }
      try
      {
        workspace.postCommand( compositeCommand );
      }
      catch( final Throwable th )
      {
        th.printStackTrace();
      }
    }
  }

  public static IFE1D2DNode[] getAllNonElevationNodes( final ApplyElevationWidgetDataModel dataModel )
  {
    final List<IFE1D2DNode> allNodes = dataModel.getDiscretisationModel().getNodes();
    final List<IFE1D2DNode> noElevationNodes = new ArrayList<IFE1D2DNode>();

    for( int i = 0; i < allNodes.size(); i++ )
    {

      try
      {
        if( !NodeOps.hasElevation( allNodes.get( i ) ) )
        {
          noElevationNodes.add( allNodes.get( i ) );
        }
      }
      catch( final RuntimeException e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }

    return noElevationNodes.toArray( new IFE1D2DNode[] {} );

  }

}
