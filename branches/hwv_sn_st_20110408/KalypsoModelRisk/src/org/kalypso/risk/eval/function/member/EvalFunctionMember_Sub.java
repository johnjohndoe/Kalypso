/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  DenickestraÃŸe 22
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
package org.kalypso.risk.eval.function.member;

import java.util.Stack;

import org.kalypso.risk.eval.function.EOperatorPriority;

/**
 * @author Dejan Antanaskovic
 * 
 */
public class EvalFunctionMember_Sub extends AbstractEvalFunctionMember
{

  /**
   * @see org.kalypso.risk.eval.function.IEvalFunctionMember#getPattern()
   */
  @Override
  public String getOperator( )
  {
    return "-"; //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.risk.eval.function.IEvalFunctionMember#getPriority()
   */
  @Override
  public EOperatorPriority getPriority( )
  {
    return EOperatorPriority.ADD_SUB;
  }

  /**
   * @see org.kalypso.risk.eval.function.IEvalFunctionMember#calculate(java.util.Stack)
   */
  @Override
  public double calculate( final Stack<Double> stack )
  {
    final double d2 = stack.pop().doubleValue();
    if( stack.size() > 0 )
    {
      final double d1 = stack.pop().doubleValue();
      return d1 - d2;
    }
    else
      return -d2;
  }

}
