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
package org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.invariants;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.IProblem;
import org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.ProblemDescriptor;
import org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * Provides checking conditions to see if any elements overlap in the 1D2D Calculation Unit
 * 
 * @author Madanagopal
 * 
 */
@SuppressWarnings("unchecked")
public class InvariantOverlappingElements implements ICalculationValidateInterface
{
  private final ICalculationUnit1D2D mainCalculation1D2D;

  private List<ICalculationUnit> subUnits;

  private final List<IFeatureWrapper2> bufferList = new ArrayList<IFeatureWrapper2>();

  private List<IFENetItem> bufSubUnits;

  private ICalculationUnit2D thisCalcUnit;

  private ICalculationUnit2D toCompareCalcUnit;

  private final List<IProblem> invariantErrorMessages = new ArrayList<IProblem>();

  public InvariantOverlappingElements( final ICalculationUnit1D2D mainCalculation1D2D )
  {
    this.mainCalculation1D2D = mainCalculation1D2D;
  }

  /**
   * Provides Validating Conditions for this Class.
   * 
   * @see org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.invariants.ICalculationValidateInterface#checkAllInvariants()
   */
  public void checkAllInvariants( )
  {
    subUnits = mainCalculation1D2D.getSubUnits();
    System.out.println( "subUnits " + subUnits.size() );

    for( final IFeatureWrapper2 subUnit : subUnits )
    {

      if( subUnit instanceof ICalculationUnit2D )
      {

        thisCalcUnit = (ICalculationUnit2D) subUnit;
        System.out.println( thisCalcUnit.getName() );

      }
      for( final IFeatureWrapper2 subUnit_ : subUnits )

      {
        System.out.println( "subUnits " + subUnits.size() );
        if( subUnit_ instanceof ICalculationUnit2D )
        {
          toCompareCalcUnit = (ICalculationUnit2D) subUnit_;
          System.out.println( toCompareCalcUnit.getName() );

          if( thisCalcUnit.equals( toCompareCalcUnit ) )
            continue;
          if( bufferList.contains( thisCalcUnit ) )
            continue;
          bufSubUnits = thisCalcUnit.getElements();
          System.out.println( bufSubUnits.size() );
          for( final IFeatureWrapper2 element : bufSubUnits )
          {
            if( toCompareCalcUnit.getElements().contains( element ) )
            {
              // @TODO Can Change the Focus to particular Calculation Unit
              invariantErrorMessages.add( new ProblemDescriptor( null, "Overlapping Elements in " + toCompareCalcUnit.getName(), mainCalculation1D2D, mainCalculation1D2D ) );
              System.out.println( "Overlapping Elements in " + toCompareCalcUnit.getName() );
            }
          }
          bufferList.add( thisCalcUnit );
        }
      }
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.validate.test.calculation_unit.ICalculationValidateInterface#getBrokenInvariantMessages()
   */
  public List<IProblem> getBrokenInvariantMessages( )
  {
    return invariantErrorMessages;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.validate.test.calculation_unit.ICalculationValidateInterface#getCalculationUnit()
   */
  public ICalculationUnit getCalculationUnit( )
  {
    return mainCalculation1D2D;
  }

}
