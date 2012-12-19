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

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeNodeElevationCommand;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.elevation.IElevationModel;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Thomas Jung
 */
public class ApplyElevationHelper
{
  public static void assignElevationToSelectedNodes( final ApplyElevationWidgetDataModel dataModel, final List<IFE1D2DNode> nodeList ) throws Exception
  {
    final IMapPanel mapPanel = dataModel.getMapPanel();
    if( mapPanel == null )
      return;

    final IFEDiscretisationModel1d2d model1d2d = dataModel.getDiscretisationModel();
    if( model1d2d == null )
      return;

    final CommandableWorkspace workspace = dataModel.getDiscretisationModelWorkspace();
    if( workspace == null )
      return;

    final IElevationModel elevationModel = dataModel.getElevationProvider();
    if( elevationModel == null )
      return;

    final ChangeNodeElevationCommand command = new ChangeNodeElevationCommand( model1d2d );

    for( final IFE1D2DNode node : nodeList )
    {
      final GM_Point point = node.getPoint();
      final double elevation = elevationModel.getElevation( point );
      if( !Double.isNaN( elevation ) )
        command.addNodeElevation( node, elevation );
    }

    workspace.postCommand( command );
  }

  public static IFE1D2DNode[] getAllNonElevationNodes( final ApplyElevationWidgetDataModel dataModel )
  {
    final IFE1D2DNode[] allNodes = dataModel.getDiscretisationModel().getNodes();
    final List<IFE1D2DNode> noElevationNodes = new ArrayList<>();

    for( final IFE1D2DNode node : allNodes )
    {
      try
      {
        final GM_Point point = node.getPoint();
        if( Double.isNaN( point.getZ() ) )
        {
          noElevationNodes.add( node );
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