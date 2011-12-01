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
package org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * @author Madanagopal
 *
 */
public class CalculationUnitCheckDataModel
{
  
  IFeatureWrapperCollection<IBoundaryCondition> boundaryConditions;
  IFeatureWrapperCollection<IContinuityLine2D> continuityLine2Ds;
  IControlModel1D2D controlModel;
  
  public CalculationUnitCheckDataModel( )
  {
  }

  public IFeatureWrapperCollection<IBoundaryCondition> getBoundaryConditions( )
  {
    return boundaryConditions;
  }

  public void setBoundaryConditions( IFeatureWrapperCollection<IBoundaryCondition> boundaryConditions )
  {
    this.boundaryConditions = boundaryConditions;
  }

  public IFeatureWrapperCollection<IContinuityLine2D> getBoundaryLines( )
  {
    return continuityLine2Ds;
  }

  public void setBoundaryLines( IFeatureWrapperCollection<IContinuityLine2D> continuityLine2Ds )
  {
    this.continuityLine2Ds = continuityLine2Ds;
  }

  public IControlModel1D2D getControlModel( )
  {
    return controlModel;
  }

  public void setControlModel( IControlModel1D2D controlModel )
  {
    this.controlModel = controlModel;
  }  
}
