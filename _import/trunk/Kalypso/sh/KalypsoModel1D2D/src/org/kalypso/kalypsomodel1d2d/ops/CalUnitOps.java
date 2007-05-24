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
import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;


/**
 * Provide utility methods around calculation units
 *  
 * @author Patrice Congo
 */

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
   * @throws IllegalArgumentException if any argument is null
   */
  public static final Collection<ICalculationUnit1D2D> getParentUnit( 
                              final ICalculationUnit calculationUnit, 
                              final IFEDiscretisationModel1d2d model1d2d )
                              throws IllegalArgumentException
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
  
  /**
   * To get the all calculation units of the given discretisation model
   * @param model1d2d the discretisation model
   * @return a collection containing the calculation unit of the dicretisation 
   *            model
   * @throws IllegalArgumentException if the argument model1d2d is null
   */
  public static final List<ICalculationUnit> getModelCalculationUnits(
                              IFEDiscretisationModel1d2d model1d2d )
                              throws IllegalArgumentException
  {
    Assert.throwIAEOnNullParam( model1d2d, "model1d2d" );
    final List<ICalculationUnit> calUnits = new ArrayList<ICalculationUnit>();
    IFeatureWrapperCollection<IFE1D2DComplexElement> complexElements = 
                                                model1d2d.getComplexElements();
    for(IFE1D2DComplexElement ce : complexElements )
    {
      if( ce instanceof ICalculationUnit )
      {
        calUnits.add( (ICalculationUnit) ce );
      }
    }
    return calUnits;
  }
  
  /**
   * Answer the number of the 2d elements the given
   * calculation unit has
   * @param calUnit the calUnit to inspect
   * @return a 0 or positive integer representing the calculation
   *        unit 2d element
   * @throws IllegalArgumentException if calUnit is null
   */
  public static int getNum2DElement( ICalculationUnit calUnit )
  {
    Assert.throwIAEOnNullParam( calUnit, "calUnit" );
    if( calUnit instanceof ICalculationUnit1D )
    {
      return 0;
    }
    else if( calUnit instanceof ICalculationUnit2D )
    {
      return calUnit.getElements().size();      
    }
    else if( calUnit instanceof ICalculationUnit1D2D )
    {
      int num=0; 
      for( Object ele : calUnit.getElements() )
      {
        if( ele instanceof IElement2D )
        {
          num++;
        }
      }
      return num;
    }
    else
    {
      throw new IllegalArgumentException(
          "Cannot handle this calculation unit:"+calUnit );
    }
  }
  
  /**
   * Answer the number of the 1d elements the given
   * calculation unit has
   * @param calUnit the calUnit to inspect
   * @return a 0 or positive integer representing the calculation
   *        unit 1d element
   * @throws IllegalArgumentException if calUnit is null
   */
  public static int getNum1DElement( 
                    ICalculationUnit calUnit )
                    throws IllegalArgumentException
  {
    Assert.throwIAEOnNullParam( calUnit, "calUnit" );
    if( calUnit instanceof ICalculationUnit2D )
    {
      return 0;
    }
    else if( calUnit instanceof ICalculationUnit1D )
    {
      return calUnit.getElements().size();      
    }
    else if( calUnit instanceof ICalculationUnit1D2D )
    {
      int num=0; 
      for( Object ele : calUnit.getElements() )
      {
        if( ele instanceof IElement1D )
        {
          num++;
        }
      }
      return num;
    }
    else
    {
      throw new IllegalArgumentException(
          "Cannot handle this calculation unit:"+calUnit );
    }
  }
  
  /**
   * Answer whether the given calculation unit has an up-stream
   * boundary line
   * 
   * @param calUnit the calculation unit to assert
   * @return true if the calculation unit has an upstream boundary 
   *            line otherwise false
   * @throws IllegalArgumentException if the argument calUnit is null
   */
  public static boolean hasUpBoundary( 
                      ICalculationUnit calUnit )
                      throws IllegalArgumentException
  {
    Assert.throwIAEOnNullParam( calUnit, "calUnit" );
    return calUnit.getUpStreamBoundaryLine() !=null;
  }
  
  /**
   * Answer whether the given calculation unit has a down-stream
   * boundary line
   * 
   * @param calUnit the calculation unit to assert
   * @return true if the calculation unit has a boundary line
   *            otherwise false
   * @throws IllegalArgumentException if the argument calUnit is null
   */
  public static boolean hasDownBoundary( 
                        ICalculationUnit calUnit )
                        throws IllegalArgumentException
  {
    Assert.throwIAEOnNullParam( calUnit, "calUnit" );
    return calUnit.getDownStreamBoundaryLine() !=null;
  }
  
  
}
