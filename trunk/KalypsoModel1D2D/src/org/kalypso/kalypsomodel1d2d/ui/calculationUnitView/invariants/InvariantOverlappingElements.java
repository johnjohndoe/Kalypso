/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Provides checking conditions to see if any elements overlap in the 1D2D Calculation Unit
 * 
 * @author Madanagopal
 * 
 */
public class InvariantOverlappingElements implements ICalculationValidateInterface
{
  /**
   * Provides Validating Conditions for this Class.
   * 
   * @see org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.invariants.ICalculationValidateInterface#checkAllInvariants(org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit)
   */
  @Override
  public List<IProblem> checkAllInvariants( final ICalculationUnit calcUnit )
  {
    final List<IProblem> invariantErrorMessages = new ArrayList<IProblem>();

    final List<Feature> bufferList = new ArrayList<Feature>();

    if( !(calcUnit instanceof ICalculationUnit1D2D) )
      return invariantErrorMessages;

    final ICalculationUnit1D2D calcUnit1d2d = (ICalculationUnit1D2D) calcUnit;

    final List<ICalculationUnit> subUnits = calcUnit1d2d.getChangedSubUnits();
    for( final ICalculationUnit subUnit : subUnits )
    {
      ICalculationUnit2D thisCalcUnit;

      if( subUnit instanceof ICalculationUnit2D )
        thisCalcUnit = (ICalculationUnit2D) subUnit;
      else
        thisCalcUnit = null;

      for( final Feature subUnit_ : subUnits )
      {
        if( subUnit_ instanceof ICalculationUnit2D )
        {
          final ICalculationUnit2D toCompareCalcUnit = (ICalculationUnit2D) subUnit_;

          if( thisCalcUnit.equals( toCompareCalcUnit ) )
            continue;
          if( bufferList.contains( thisCalcUnit ) )
            continue;

          final List<IFENetItem> bufSubUnits = thisCalcUnit.getElements();
          for( final Feature element : bufSubUnits )
          {
            if( toCompareCalcUnit.getElements().contains( element ) )
              invariantErrorMessages.add( new ProblemDescriptor( null, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.invariants.InvariantOverlappingElements.0", toCompareCalcUnit.getName() ), calcUnit, calcUnit1d2d ) ); //$NON-NLS-1$
          }
          bufferList.add( thisCalcUnit );
        }
      }
    }
    return invariantErrorMessages;
  }
}
