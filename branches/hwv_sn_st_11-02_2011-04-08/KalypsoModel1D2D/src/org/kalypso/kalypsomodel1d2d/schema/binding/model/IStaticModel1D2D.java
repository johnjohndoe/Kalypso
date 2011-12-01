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
package org.kalypso.kalypsomodel1d2d.schema.binding.model;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.modeling.IStaticModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindModel;

/**
 * Interface for classes representing a wb1d2d:StaticModel1D2D
 * 
 * @author Patrice Congo
 * 
 */
public interface IStaticModel1D2D extends IStaticModel
{
  /**
   * To get the discretisation sub-model of this static model
   * 
   * @return the discretisation of this static model
   */
  public IFEDiscretisationModel1d2d getDiscretisationModel( );

  /**
   * To get the flow relationship sub-model of this static model
   * 
   * @return the flow relationship sub-model of this static model
   */
  public IFlowRelationshipModel getFlowRelationshipModel( );

  /**
   * To get the terrain sub-model of this static model
   * 
   * @return the terrain model of this static model
   */
  public ITerrainModel getTerrainModel( );

  /**
   * To get the wind sub-model of this static model
   * 
   * @return the wind model of this static model
   */
  public IWindModel getWindModel( );

}
