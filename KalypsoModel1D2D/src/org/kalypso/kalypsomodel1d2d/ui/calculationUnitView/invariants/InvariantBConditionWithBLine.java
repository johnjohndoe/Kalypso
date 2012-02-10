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

import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.ops.EdgeOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.IProblem;
import org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.ProblemDescriptor;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;

/**
 * Provides Validating Conditions for Checking if every boundary line has atleast one boundary condition
 * 
 * @author Madanagopal
 * 
 */
public class InvariantBConditionWithBLine implements ICalculationValidateInterface
{
  private final IFlowRelationshipModel m_flowRelationship;

  public InvariantBConditionWithBLine( final IFlowRelationshipModel flowRelationship )
  {
    m_flowRelationship = flowRelationship;
  }

  /**
   * Runs the validating Checks
   * 
   * @see org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.invariants.ICalculationValidateInterface#checkAllInvariants(org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit)
   */
  @Override
  public List<IProblem> checkAllInvariants( final ICalculationUnit calcUnit )
  {
    final List<IProblem> invariantErrorMessages = new ArrayList<IProblem>();

    invariant_CheckEachBLhasBC( invariantErrorMessages, calcUnit );
    if( calcUnit instanceof ICalculationUnit1D )
    {
      invariant_CheckForBLExist( invariantErrorMessages, (ICalculationUnit1D) calcUnit );
      invariant_CheckBC_On_EndNode( invariantErrorMessages, (ICalculationUnit1D) calcUnit );
    }

    return invariantErrorMessages;
  }

  private void invariant_CheckBC_On_EndNode( final List<IProblem> invariantErrorMessages, final ICalculationUnit1D calc )
  {
    final List<IFENetItem> elements = calc.getElements();
    final List<IFE1D2DNode> endNodes = new ArrayList<IFE1D2DNode>();
    for( final IFENetItem element : elements )
    {
      if( element instanceof IElement1D )
      {
        final IElement1D thisElement = (IElement1D) element;
        if( EdgeOps.find1DEdgeEndNode( thisElement.getEdge() ) != null )
        {
          endNodes.add( EdgeOps.find1DEdgeEndNode( thisElement.getEdge() ) );
        }
      }
    }

    final List<IBoundaryCondition> bConditions_ = CalcUnitOps.getBoundaryConditions( m_flowRelationship, calc );
    boolean foundEndBLine = false;
    for( final IBoundaryCondition condition_ : bConditions_ )
    {
      for( final IFE1D2DNode endNode : endNodes )
      {
        // TODO: set an definite constant value for this grab distance!
        if( endNode.getPoint().distance( condition_.getPosition() ) < 0.5 )
        {
          foundEndBLine = true;
        }
      }
      if( !foundEndBLine )
      {
        invariantErrorMessages.add( new ProblemDescriptor( null, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.invariants.InvariantBConditionWithBLine.0" ) + calc.getName(), calc, calc ) ); //$NON-NLS-1$
      }
      foundEndBLine = false;
    }

  }

  private void invariant_CheckForBLExist( final List<IProblem> invariantErrorMessages, final ICalculationUnit1D calc )
  {
    final List<IFELine> continuityLine2Ds = calc.getContinuityLines();
    if( continuityLine2Ds.size() == 0 )
      invariantErrorMessages.add( new ProblemDescriptor( null, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.invariants.InvariantBConditionWithBLine.1" ) + calc.getName(), calc, calc ) ); //$NON-NLS-1$
  }

  private void invariant_CheckEachBLhasBC( final List<IProblem> invariantErrorMessages, final ICalculationUnit calcUnit )
  {
    final List<IFELine> continuityLine2Ds = calcUnit.getContinuityLines();
    final List<IBoundaryCondition> boundaryConditions = CalcUnitOps.getBoundaryConditions( m_flowRelationship, calcUnit );

    for( final IFELine line : continuityLine2Ds )
    {
      boolean hasBc = false;
      try
      {
        for( final IBoundaryCondition bc : boundaryConditions )
        {
          // TODO: set an definite constant value for this grab distance!
          if( line.recalculateElementGeometry().distance( bc.getPosition() ) < 0.5 )
          {
            hasBc = true;
          }
        }
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        hasBc = true;
      }
      if( !hasBc )
      {
        invariantErrorMessages.add( new ProblemDescriptor( null, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.invariants.InvariantBConditionWithBLine.2", calcUnit.getName() ), calcUnit, line ) ); //$NON-NLS-1$
      }
    }
  }
}
