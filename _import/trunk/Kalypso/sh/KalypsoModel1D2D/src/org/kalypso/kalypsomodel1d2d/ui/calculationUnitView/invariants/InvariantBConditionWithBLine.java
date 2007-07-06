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
import org.kalypso.kalypsomodel1d2d.ops.EdgeOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IBoundaryLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IBoundaryLine1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.IProblem;
import org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.ProblemDescriptor;
import org.kalypso.kalypsomodel1d2d.ui.map.IGrabDistanceProvider;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;
import org.kalypso.kalypsosimulationmodel.core.discr.IFENode;
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
  private CalculationUnitDataModel dataModel;
  private List<IProblem> invariantErrorMessages = new ArrayList<IProblem>();

  public InvariantBConditionWithBLine( ICalculationUnit calc, CalculationUnitDataModel dataModel )
  {
    this.calculationUnit = calc;
    this.dataModel = dataModel;
  }

   public List<IBoundaryCondition> getBoundaryConditions( )
  {
    final CommandableWorkspace workspace = 
      KeyBasedDataModelUtil.getBCWorkSpace( dataModel );// dataModel.getData( CommandableWorkspace.class, ICommonKeys.KEY_BOUNDARY_CONDITION_CMD_WORKSPACE );
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
  @SuppressWarnings("unchecked")
  public void checkAllInvariants( )
  {
    invariant_CheckEachBLhasBC();
    if (calculationUnit instanceof ICalculationUnit1D)
    {
      invariant_CheckForBLExist((ICalculationUnit1D)calculationUnit);
      invariant_CheckBC_On_EndNode((ICalculationUnit1D)calculationUnit);
    }   
  }
      

  @SuppressWarnings("unchecked")
  private void invariant_CheckBC_On_EndNode( ICalculationUnit1D calc )
  {
    
    final List<IBoundaryLine> bLines = CalUnitOps.getBoundaryLines( calc );
    
    List <IFE1D2DElement> elements = calc.getElements();
    List <IFE1D2DNode> endNodes = new ArrayList<IFE1D2DNode>();
    for (IFE1D2DElement element: elements){
      if (element instanceof IElement1D)
      {
        IElement1D thisElement = (IElement1D)element;
        if (EdgeOps.find1DEdgeEndNode(thisElement.getEdge())!= null)
        {
        endNodes.add( EdgeOps.find1DEdgeEndNode(thisElement.getEdge()));    
        }
        System.out.println("Node :"+EdgeOps.find1DEdgeEndNode(thisElement.getEdge()));
      }
    }    
    
    List<IBoundaryCondition> bConditions_ = CalUnitOps.getBoundaryConditions( getBoundaryConditions(), calc, getGrabDistance() );
    boolean foundEndBLine = false;
    for (IBoundaryCondition condition_ : bConditions_)
    {
      for (IFE1D2DNode endNode: endNodes)
      {
        if (endNode.getPoint().distance( condition_.getPosition() ) < getGrabDistance())
        {
         foundEndBLine = true; 
        }
      }
      if (!foundEndBLine)            
      {
        invariantErrorMessages.add( new ProblemDescriptor(null,
            "Add Boundary Line & BC on End Node "+calculationUnit.getName(), calculationUnit, calculationUnit) );
      }
      foundEndBLine = false;
    }
      
  }

  @SuppressWarnings("unchecked")
  private void invariant_CheckForBLExist(ICalculationUnit1D calc )
  {
    final List<IBoundaryLine> boundaryLines = CalUnitOps.getBoundaryLines( calc );
    if (boundaryLines.size() == 0)
    {
      invariantErrorMessages.add( new ProblemDescriptor(null,
          "Boundary Line must be present or yet to be assigned "+calc.getName(), calc,calc) );
    }    
  }

  @SuppressWarnings("unchecked")
  private void invariant_CheckEachBLhasBC()
  {    
    final List<IBoundaryLine> boundaryLines = CalUnitOps.getBoundaryLines( calculationUnit );
    final List<IBoundaryCondition> boundaryConditions = CalUnitOps.getBoundaryConditions( getBoundaryConditions(), calculationUnit, getGrabDistance() );
    
    for (IBoundaryLine line:boundaryLines)
    {
      boolean hasBc=false;
      try
      {
        for( IBoundaryCondition bc: boundaryConditions )
        {
          if( line.recalculateElementGeometry().distance( bc.getPosition() )<getGrabDistance() ) 
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
        invariantErrorMessages.add( new ProblemDescriptor(null, 
            "Boundary Line may not have Boundary condition or have not yet been assigned to "+calculationUnit.getName(),
            calculationUnit, line) );        
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
