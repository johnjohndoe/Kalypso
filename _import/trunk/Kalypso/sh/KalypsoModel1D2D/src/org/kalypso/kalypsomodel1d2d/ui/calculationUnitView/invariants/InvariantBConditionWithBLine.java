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

import org.kalypso.kalypsomodel1d2d.ops.CalUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IBoundaryLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.IProblem;
import org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.ProblemDescriptor;
import org.kalypso.kalypsomodel1d2d.ui.map.IGrabDistanceProvider;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Provides Validating Conditions for Checking if every boundary line has atleast one boundary condition
 * @author Madanagopal
 * 
 */
public class InvariantBConditionWithBLine implements ICalculationValidateInterface
{

  private ICalculationUnit calculationUnit;

//  private List<IBoundaryLine> boundaryLines;

  private CalculationUnitDataModel dataModel;

//  private List<IBoundaryCondition> boundaryConditions;
  private List<IProblem> invariantErrorMessages = new ArrayList<IProblem>();

  public InvariantBConditionWithBLine( ICalculationUnit calc, CalculationUnitDataModel dataModel )
  {
    this.calculationUnit = calc;
    this.dataModel = dataModel;
  }

   public List<IBoundaryCondition> getBoundaryConditions( )
  {
    final CommandableWorkspace workspace = dataModel.getData( CommandableWorkspace.class, ICommonKeys.KEY_BOUNDARY_CONDITION_CMD_WORKSPACE );
    final Feature bcHolderFeature = workspace.getRootFeature();
    IFlowRelationshipModel flowRelationship = (IFlowRelationshipModel) bcHolderFeature.getAdapter( IFlowRelationshipModel.class );
    List<IBoundaryCondition> conditions = new ArrayList<IBoundaryCondition>( (List) flowRelationship );
    return conditions;
  }

  public double getGrabDistance()
  {
    IGrabDistanceProvider grabDistanceProvider = dataModel.getData( IGrabDistanceProvider.class, ICommonKeys.KEY_GRAB_DISTANCE_PROVIDER );
    return grabDistanceProvider.getGrabDistance();
  }

  /**
   * Runs the validating Checks
   * @see org.kalypso.kalypsomodel1d2d.validate.test.calculation_unit.ICalculationValidateInterface#checkAllInvariants()
   */
  public void checkAllInvariants( )
  {
    final double grabDistance = getGrabDistance();
    final List<IBoundaryLine> boundaryLines = CalUnitOps.getBoundaryLines( calculationUnit );
    final List<IBoundaryCondition> boundaryConditions = CalUnitOps.getBoundaryConditions( getBoundaryConditions(), calculationUnit, getGrabDistance() );
    for (IBoundaryLine line:boundaryLines)
    {
      boolean hasBc=false;
      try
      {
        for( IBoundaryCondition bc: boundaryConditions )
        {
          if( line.recalculateElementGeometry().distance( bc.getPosition() )<grabDistance ) 
          {
            hasBc=true;
          }
        }
      }
      catch (Exception e) 
      {
        e.printStackTrace();
        hasBc = true;
      }
      if( !hasBc )
      {
        invariantErrorMessages.add( new ProblemDescriptor(null, "Boundary Condition with out boundary Line "+calculationUnit, calculationUnit, line) );        
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
    return calculationUnit;
  }
}
