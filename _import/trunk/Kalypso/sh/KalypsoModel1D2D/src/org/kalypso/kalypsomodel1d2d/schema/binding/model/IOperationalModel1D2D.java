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

import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsosimulationmodel.core.modeling.IOperationalModel;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Primitive;

/**
 * Interface for classes representing an
 * op1d2d:OperationalModel
 * 
 * @author Patrice Congo
 *
 */
public interface IOperationalModel1D2D extends IOperationalModel
{
  /**
   * To get the initial condition this operational model holds
   * @return the list of initial condition this operational model holds
   * 
   */
  public IFeatureWrapperCollection<IFeatureWrapper2> getInitialConditions();
  
  /**
   * To get the boundary conditions this operational model holds
   * @return the list of boundary condition this operational model holds
   * 
   */
  public IFeatureWrapperCollection<IBoundaryCondition> getBoundaryConditions();
  
  /**
   * To get the boundary conditions that are aplicable inside the given zone
   * @param zone a {@link org.kalypsodeegree.model.geometry.GM_Point} or
   *        {@link org.deegree.model.geometry.GM_Surface} defining the zone
   *        inside which boundary condition are to be searched 
   * @return the list of {@link IBoundaryCondition}s that are applicable 
   *                inside the zone  
   */
  public List<IBoundaryCondition> getApplicableBoundaryConditions(GM_Primitive zone);
  
}
