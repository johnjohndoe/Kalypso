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
package org.kalypso.kalypsomodel1d2d.ops;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * Provide utility methods around calculation units
 * 
 * @author Patrice Congo
 */

public class CalcUnitOps
{

  private CalcUnitOps( )
  {
    // Do not instantiate this class!
  }

  public static final IFE1D2DElement[] toAddableElements( final ICalculationUnit calculationUnit, final IFE1D2DElement[] elementsToAdd )
  {
    final ArrayList<IFE1D2DElement> eleList = new ArrayList<IFE1D2DElement>( elementsToAdd.length );

    if( calculationUnit instanceof ICalculationUnit1D )
    {
      for( final IFE1D2DElement ele : elementsToAdd )
      {
        if( ele instanceof IElement1D )
        {
          eleList.add( ele );
        }
      }

    }
    else if( calculationUnit instanceof ICalculationUnit2D )
    {
      for( final IFE1D2DElement ele : elementsToAdd )
      {
        if( ele instanceof IPolyElement )
        {
          eleList.add( ele );
        }
      }
    }
    else
    {
      for( final IFE1D2DElement ele : elementsToAdd )
      {
        if( ele != null )
        {
          eleList.add( ele );
        }
      }
    }
    final IFE1D2DElement[] array = eleList.toArray( new IFE1D2DElement[eleList.size()] );
    return array;
  }

  public static final IFE1D2DElement[] toAddableElements( final ICalculationUnit calculationUnit, final Feature[] elementsToAdd )
  {
    final ArrayList<IFE1D2DElement> eleList = new ArrayList<IFE1D2DElement>( elementsToAdd.length );
    final Class adapterCls;
    if( calculationUnit instanceof ICalculationUnit1D )
    {
      adapterCls = IElement1D.class;
    }
    else if( calculationUnit instanceof ICalculationUnit2D )
    {
      adapterCls = IPolyElement.class;
    }
    else
    {
      adapterCls = IFE1D2DElement.class;
    }

    for( final Feature f : elementsToAdd )
    {
      final IFE1D2DElement ele = f == null ? null : (IFE1D2DElement) f.getAdapter( adapterCls );
      if( ele != null )
      {
        eleList.add( ele );
      }
    }

    final IFE1D2DElement[] array = eleList.toArray( new IFE1D2DElement[eleList.size()] );
    return array;
  }

  /**
   * To get the parent units of the given unit inside the specified model
   * 
   * @param calculationUnit
   *            the calculation unit which parents are to be search
   * @param model1d2d
   *            the model where to look for the parent units
   * @return a collection containing the parent unit of the given model1d2d
   * @throws IllegalArgumentException
   *             if any argument is null
   */
  public static final Collection<ICalculationUnit1D2D> getParentUnit( final ICalculationUnit calculationUnit, final IFEDiscretisationModel1d2d model1d2d ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNullParam( calculationUnit, "calculationUnit" ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( model1d2d, "model1d2d" ); //$NON-NLS-1$
    final IFeatureWrapperCollection<IFE1D2DComplexElement> complexElements = model1d2d.getComplexElements();
    final Collection<ICalculationUnit1D2D> parents = new ArrayList<ICalculationUnit1D2D>();
    for( final IFE1D2DComplexElement ce : complexElements )
    {
      if( ce instanceof ICalculationUnit1D2D )
      {
        final ICalculationUnit1D2D parent = (ICalculationUnit1D2D) ce;
        if( parent.getChangedSubUnits().contains( calculationUnit ) )
        {
          parents.add( parent );
        }
      }
    }
    return parents;
  }

  /**
   * To get the all calculation units of the given discretisation model
   * 
   * @param model1d2d
   *            the discretisation model
   * @return a collection containing the calculation unit of the dicretisation model
   * @throws IllegalArgumentException
   *             if the argument model1d2d is null
   */
  public static final List<ICalculationUnit> getModelCalculationUnits( final IFEDiscretisationModel1d2d model1d2d ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNullParam( model1d2d, "model1d2d" ); //$NON-NLS-1$
    final List<ICalculationUnit> calUnits = new ArrayList<ICalculationUnit>();
    final IFeatureWrapperCollection<IFE1D2DComplexElement> complexElements = model1d2d.getComplexElements();
    for( final IFE1D2DComplexElement ce : complexElements )
    {
      if( ce instanceof ICalculationUnit )
      {
        calUnits.add( (ICalculationUnit) ce );
      }
    }
    return calUnits;
  }

  /**
   * Tests whether a given boundary is a boundary line of the specified calculation unit.
   * 
   * @param boundaryLine
   *            the boundary line to assert
   * @param calUnit
   *            the calculation unit which stream boundary is to be tested
   * @return true if the provided boundary line is a boundary of the given calculation unit otherwise false.
   */
  public static final boolean isBoundaryLineOf( final IFELine continuityLine, final ICalculationUnit calUnit )
  {
    return calUnit.getContinuityLines().contains( continuityLine );
  }

  /**
   * To get the bounding box of the given calculation unit.
   * 
   * @param calUnit
   *            the calculation which bounding box is to be get
   * @return an {@link GM_Envelope} representing the bounding box of the calculation unit.
   * 
   */
  public static final GM_Envelope getBoundingBox( final ICalculationUnit calUnit )
  {
    Assert.throwIAEOnNullParam( calUnit, "calUnit" ); //$NON-NLS-1$
    final LinkedList<GM_Envelope> contributingBBox = new LinkedList<GM_Envelope>();

    // collect all contributing bboxes
    contributingBBox.add( calUnit.getElements().getWrappedList().getBoundingBox() );
    if( calUnit instanceof ICalculationUnit1D2D )
    {
      final LinkedList<ICalculationUnit> subUnits = new LinkedList<ICalculationUnit>( ((ICalculationUnit1D2D) calUnit).getChangedSubUnits() );
      while( !subUnits.isEmpty() )
      {
        final ICalculationUnit removed = subUnits.remove( 0 );
        contributingBBox.add( removed.getElements().getWrappedList().getBoundingBox() );
        if( removed instanceof ICalculationUnit1D2D )
        {
          subUnits.addAll( ((ICalculationUnit1D2D) removed).getChangedSubUnits() );
        }
      }
    }

    GM_Envelope boundingBox = null;
    // find the first non null
    find_first_non_null: while( !contributingBBox.isEmpty() )
    {
      final GM_Envelope removeBB = contributingBBox.removeFirst();
      if( removeBB != null )
      {
        boundingBox = removeBB;
        break find_first_non_null;
      }
    }

    // widden box the include following non nulls
    while( !contributingBBox.isEmpty() )
    {
      final GM_Envelope removeBB = contributingBBox.removeFirst();
      if( removeBB != null )
      {
        boundingBox = boundingBox.getMerged( removeBB );
      }
    }

    return boundingBox;

  }

  /**
   * Answer whether an element is part of the calculation unit.
   * 
   * @param unit
   *            the calculation unit
   * @param element
   */
  public static final boolean isFiniteElementOf( final ICalculationUnit unit, final IFE1D2DElement element )
  {
    Assert.throwIAEOnNullParam( unit, "unit" ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( element, "element" ); //$NON-NLS-1$
    final IFeatureWrapperCollection<IFE1D2DComplexElement> containers = element.getContainers();
    final List list = new ArrayList<String>();
    for( int i = 0; i < containers.size(); i++ )
      list.add( (containers.get( i )).getGmlID() );
    // return containers.contains( unit );

    final LinkedList<ICalculationUnit> subUnits = new LinkedList<ICalculationUnit>();
    subUnits.add( unit );
    while( !subUnits.isEmpty() )
    {
      final ICalculationUnit currentSubUnit = subUnits.remove( 0 );
      if( currentSubUnit instanceof ICalculationUnit1D2D )
        subUnits.addAll( ((ICalculationUnit1D2D) currentSubUnit).getChangedSubUnits() );
      if( list.contains( currentSubUnit.getGmlID() ) )
        return true;
    }
    return false;
  }

  /**
   * Answer whether a boundary condition is assign to the given calculation unit
   * 
   * @param unit
   *            the possible target calculation unit
   * @param bCondition
   *            the boundary condition to test for assignment
   * @return true if the boundary condition is assign to the calculation unit otherwise false.
   * @throws IllegalArgumentException
   *             if unit or bCondition is null or unit does not have a model 1d 2d as parent feature
   * 
   */
  public static final boolean isBoundaryConditionOf( final ICalculationUnit unit, final IBoundaryCondition bCondition )
  {
    final List parents = (List) bCondition.getFeature().getProperty( Kalypso1D2DSchemaConstants.OP1D2D_PROP_PARENT_CALCUNIT );
    return parents.contains( unit.getGmlID() );
  }

  /**
   * Returns all boundary condition assign to the specified unit found in the passed list of boundary conditions
   * 
   * @param conditions
   *            the list of boundary condition
   * @param unit
   *            the calculation unit which boundary conditions are being collected
   * @return a list of boundary condition assigned to the calculation unit
   * @throws IllegalArgumentException
   *             if condition or unit is null or grabDistance is less than 0
   */
  public static final List<IBoundaryCondition> getBoundaryConditions( final Collection<IFlowRelationship> conditions, final ICalculationUnit unit )
  {
    final List<IBoundaryCondition> assignedConditions = new ArrayList<IBoundaryCondition>();
    for( final IFlowRelationship condition : conditions )
      if( condition instanceof IBoundaryCondition && isBoundaryConditionOf( unit, (IBoundaryCondition) condition ) )
        assignedConditions.add( (IBoundaryCondition) condition );
    return assignedConditions;
  }

  /**
   * Counts the number of boundary conditions assigned to the calculation unit.
   * 
   * @param conditions
   *            the collection condition to test
   * @param unit
   *            the target calculation unit
   * @param grabDistance
   *            the grab distance for geometry searching
   * @return a integer representing the number of boundary contions in the collection which has been assigned to the
   *         calculation unit
   * @throws IllegalArgumentException
   *             if condition or unit is null or grabDistance is less than 0
   * 
   */
  public static final int countAssignedBoundaryConditions( final Collection<IBoundaryCondition> conditions, final ICalculationUnit unit, final IBoundaryCondition.BOUNDARY_TYPE typeToCount )
  {
    int count = 0;
    for( final IBoundaryCondition condition : conditions )
    {
      if( isBoundaryConditionOf( unit, condition ) && typeToCount.equals( condition.getBoundaryType() ) )
        count++;
    }

    return count;
  }
  
  public static final int countAssignedBoundaryConditions( final Collection<IBoundaryCondition> conditions, final ICalculationUnit unit  ){
    return countAssignedBoundaryConditions( conditions, unit, IBoundaryCondition.BOUNDARY_TYPE.HydroBoundary );
  }

  public static ICalculationUnit findSubUnit( final ICalculationUnit calcUnit, IFE1D2DElement element )
  {
    if( calcUnit instanceof ICalculationUnit1D2D )
    {
      final ICalculationUnit1D2D calcUnit1d2d = (ICalculationUnit1D2D) calcUnit;
      IFeatureWrapperCollection<ICalculationUnit> subUnits = calcUnit1d2d.getChangedSubUnits();
      for( final ICalculationUnit subUnit : subUnits )
      {
        ICalculationUnit unit = findSubUnit( subUnit, element );
        if( unit != null )
          return unit;
      }

      return null;
    }

    if( calcUnit.contains( element ) )
      return calcUnit;

    return null;
  }

}
