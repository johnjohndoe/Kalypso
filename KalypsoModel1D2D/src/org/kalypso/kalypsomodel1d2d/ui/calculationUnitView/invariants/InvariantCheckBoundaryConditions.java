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
import org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.IProblem;
import org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.ProblemDescriptor;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;

/**
 * Provides Validating conditions to check if Every Calculation Unit has atleast 2 boundary conditions.
 * 
 * @author Madanagopal
 * 
 */
public class InvariantCheckBoundaryConditions implements ICalculationValidateInterface
{
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.invariants.ICalculationValidateInterface#checkAllInvariants(org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit)
   */
  @Override
  public List<IProblem> checkAllInvariants( final ICalculationUnit calculationUnit )
  {
    final List<IProblem> invResults = new ArrayList<IProblem>();
    final int size = calculationUnit.getContinuityLines().size();
    if( ((size < 2) && (size > 0)) || (size > 2) )
    {
      final String message = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.invariants.InvariantCheckBoundaryConditions.0", calculationUnit.getName() ); //$NON-NLS-1$
      invResults.add( new ProblemDescriptor( null, message, calculationUnit, calculationUnit ) );
    }
    else if( size == 0 )
    {
      final String message = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.invariants.InvariantCheckBoundaryConditions.1", calculationUnit.getName() ); //$NON-NLS-1$
      invResults.add( new ProblemDescriptor( null, message, calculationUnit, calculationUnit ) );
    }

    return invResults;
  }
}
