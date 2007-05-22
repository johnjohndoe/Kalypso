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
package org.kalypso.kalypsomodel1d2d.ops;

import java.util.ArrayList;
import java.util.Collection;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;

/**
 * Provide utility methods around calculation units
 *  
 * @author Patrice Congo
 */
@SuppressWarnings("unchecked")
public class CalUnitOps
{
  private CalUnitOps( )
  {
    //i hate being instantiated
  }
  
  /**
   * To get the parent units of the given unit inside the specified 
   * model
   * 
   * @param calculationUnit the calculation unit which parents are 
   *            to be search
   * @param model1d2d the model where to look for the parent units
   * @return a collection containing the parent unit of the given 
   *            model1d2d 
   */
  public static final Collection<ICalculationUnit1D2D> getParentUnit( 
                              final ICalculationUnit calculationUnit, 
                              final IFEDiscretisationModel1d2d model1d2d )
  {
    Assert.throwIAEOnNullParam(  calculationUnit, "calculationUnit" );
    Assert.throwIAEOnNullParam( model1d2d, "model1d2d" );
    IFeatureWrapperCollection<IFE1D2DComplexElement> complexElements = 
                                            model1d2d.getComplexElements();
    final Collection<ICalculationUnit1D2D> parents = 
                          new ArrayList<ICalculationUnit1D2D>();
    for( IFE1D2DComplexElement ce:complexElements )
    {
      if( ce instanceof ICalculationUnit1D2D )
      {
        ICalculationUnit1D2D parent = (ICalculationUnit1D2D) ce;
        if( parent.getSubUnits().contains( calculationUnit ) )
        {
          parents.add( parent );
        }
      }
    }
    return parents;
  }
  
}
