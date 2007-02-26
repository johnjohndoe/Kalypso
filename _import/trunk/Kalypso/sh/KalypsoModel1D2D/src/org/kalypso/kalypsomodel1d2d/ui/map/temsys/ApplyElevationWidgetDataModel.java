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

import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypsodeegree.model.geometry.GM_Polygon;

/**
 * @author Madanagopal
 * @author Patrice Congo
 *
 */
public class ApplyElevationWidgetDataModel
{
  private ITerrainModel terrainModel;
  private ITerrainElevationModelSystem elevationModelSystem;
  private ITerrainElevationModel elevationModel;
  private IFEDiscretisationModel1d2d discretisationModel;
  private List<IFE1D2DNode> selectedNode;
  private GM_Polygon selectionArea;
  
  public ApplyElevationWidgetDataModel( )
  {
    //empty
  }

  public IFEDiscretisationModel1d2d getDiscretisationModel( )
  {
    return discretisationModel;
  }

  public void setDiscretisationModel( IFEDiscretisationModel1d2d discretisationModel )
  {
    this.discretisationModel = discretisationModel;
  }

  public ITerrainElevationModel getElevationModel( )
  {
    return elevationModel;
  }

  public void setElevationModel( ITerrainElevationModel elevationModel )
  {
    this.elevationModel = elevationModel;
  }

  public ITerrainElevationModelSystem getElevationModelSystem( )
  {
    return elevationModelSystem;
  }
  
  public void setElevationModelSystem( ITerrainElevationModelSystem elevationModelSystem )
  {
    this.elevationModelSystem = elevationModelSystem;
  }

  public List<IFE1D2DNode> getSelectedNode( )
  {
    return selectedNode;
  }

  public void setSelectedNode( List<IFE1D2DNode> selectedNode )
  {
    this.selectedNode = selectedNode;
  }

  public GM_Polygon getSelectionArea( )
  {
    return selectionArea;
  }

  public void setSelectionArea( GM_Polygon selectionArea )
  {
    this.selectionArea = selectionArea;
  }

  public ITerrainModel getTerrainModel( )
  {
    return terrainModel;
  }

  public void setTerrainModel( ITerrainModel terrainModel )
  {
    this.terrainModel = terrainModel;
    this.elevationModelSystem=terrainModel.getTerrainElevationModelSystem();
  }
  
  
  
  
}
