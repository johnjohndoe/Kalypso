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
import org.kalypso.kalypsomodel1d2d.ui.map.IGrabDistanceProvider;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;
import org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Provides Validating Conditions for Checking if every boundary line has atleast one boundary condition
 * 
 * @author Madanagopal
 * 
 */
public class InvariantBConditionWithBLine implements ICalculationValidateInterface
{
  private final ICalculationUnit m_calculationUnit;

  private final CalculationUnitDataModel m_dataModel;

  private final List<IProblem> m_invariantErrorMessages = new ArrayList<IProblem>();

  public InvariantBConditionWithBLine( final ICalculationUnit calc, final CalculationUnitDataModel dataModel )
  {
    m_calculationUnit = calc;
    m_dataModel = dataModel;
  }

  public List<IBoundaryCondition> getBoundaryConditions( )
  {
    final CommandableWorkspace workspace = KeyBasedDataModelUtil.getBCWorkSpace( m_dataModel );// dataModel.getData(
    // CommandableWorkspace.class,
    // ICommonKeys.KEY_BOUNDARY_CONDITION_CMD_WORKSPACE
    // );
    final Feature bcHolderFeature = workspace.getRootFeature();
    final IFlowRelationshipModel flowRelationship = (IFlowRelationshipModel) bcHolderFeature.getAdapter( IFlowRelationshipModel.class );
    final List<IBoundaryCondition> conditions = new ArrayList<IBoundaryCondition>( (List) flowRelationship );
    return conditions;
  }

  public double getGrabDistance( )
  {
    final IGrabDistanceProvider grabDistanceProvider = m_dataModel.getData( IGrabDistanceProvider.class, ICommonKeys.KEY_GRAB_DISTANCE_PROVIDER );
    return grabDistanceProvider.getGrabDistance();
  }

  /**
   * Runs the validating Checks
   * 
   * @see org.kalypso.kalypsomodel1d2d.validate.test.calculation_unit.ICalculationValidateInterface#checkAllInvariants()
   */
  @SuppressWarnings("unchecked")
  public void checkAllInvariants( )
  {
    invariant_CheckEachBLhasBC();
    if( m_calculationUnit instanceof ICalculationUnit1D )
    {
      invariant_CheckForBLExist( (ICalculationUnit1D) m_calculationUnit );
      invariant_CheckBC_On_EndNode( (ICalculationUnit1D) m_calculationUnit );
    }
  }

  @SuppressWarnings("unchecked")
  private void invariant_CheckBC_On_EndNode( final ICalculationUnit1D calc )
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

    final List<IBoundaryCondition> bConditions_ = CalcUnitOps.getBoundaryConditions( getBoundaryConditions(), calc );
    boolean foundEndBLine = false;
    for( final IBoundaryCondition condition_ : bConditions_ )
    {
      for( final IFE1D2DNode endNode : endNodes )
      {
        if( endNode.getPoint().distance( condition_.getPosition() ) < getGrabDistance() )
        {
          foundEndBLine = true;
        }
      }
      if( !foundEndBLine )
      {
        m_invariantErrorMessages.add( new ProblemDescriptor( null, "Add Boundary Line & BC on End Node " + m_calculationUnit.getName(), m_calculationUnit, m_calculationUnit ) );
      }
      foundEndBLine = false;
    }

  }

  @SuppressWarnings("unchecked")
  private void invariant_CheckForBLExist( final ICalculationUnit1D calc )
  {
    final List<IFELine> continuityLine2Ds = calc.getContinuityLines();
    if( continuityLine2Ds.size() == 0 )
    {
      m_invariantErrorMessages.add( new ProblemDescriptor( null, "Boundary Line must be present or yet to be assigned " + calc.getName(), calc, calc ) );
    }
  }

  @SuppressWarnings("unchecked")
  private void invariant_CheckEachBLhasBC( )
  {
    final List<IFELine> continuityLine2Ds = m_calculationUnit.getContinuityLines();
    final List<IBoundaryCondition> boundaryConditions = CalcUnitOps.getBoundaryConditions( getBoundaryConditions(), m_calculationUnit );

    for( final IFELine line : continuityLine2Ds )
    {
      boolean hasBc = false;
      try
      {
        for( final IBoundaryCondition bc : boundaryConditions )
        {
          if( line.recalculateElementGeometry().distance( bc.getPosition() ) < getGrabDistance() )
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
        m_invariantErrorMessages.add( new ProblemDescriptor( null, "Boundary Line may not have Boundary condition or have not yet been assigned to " + m_calculationUnit.getName(), m_calculationUnit, line ) );
      }
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.validate.test.calculation_unit.ICalculationValidateInterface#getBrokenInvariantMessages()
   */
  public List<IProblem> getBrokenInvariantMessages( )
  {
    return m_invariantErrorMessages;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.validate.test.calculation_unit.ICalculationValidateInterface#getCalculationUnit()
   */
  public ICalculationUnit getCalculationUnit( )
  {
    return m_calculationUnit;
  }
}
