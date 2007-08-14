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
import java.util.LinkedList;
import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IBoundaryLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IEdgeInv;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IJunctionContext1DToCLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ILineElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiPoint;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * Provide utility methods around calculation units
 * 
 * @author Patrice Congo
 */

@SuppressWarnings( { "unchecked", "hiding" })
public class CalcUnitOps
{

  private CalcUnitOps( )
  {
    // Do not instantiate this class!
  }

  public static final boolean isNodeOf( final ICalculationUnit unit, final IFE1D2DNode<IFE1D2DEdge> node )
  {
    final IFeatureWrapperCollection<IFE1D2DEdge> containers = node.getContainers();
    for( final IFE1D2DEdge edge : containers )
    {
      if( isEdgeOf( unit, edge ) )
      {
        return true;
      }
    }
    return false;
  }

  public static final boolean isEdgeOf( final ICalculationUnit unit, final IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge )
  {

    IFeatureWrapperCollection<IFE1D2DElement> containers = edge.getContainers();
    for( final IFE1D2DElement ele : containers )
    {
      if( isFiniteElementOf( unit, ele ) )
      {
        return true;
      }
    }
    // test the inverted
    containers = null;
    if( edge instanceof IEdgeInv )
    {
      final IFE1D2DEdge inverted = ((IEdgeInv) edge).getInverted();
      if( inverted != null )
      {
        containers = inverted.getContainers();
      }
    }
    else
    {
      final IFE1D2DEdge inverted = edge.getEdgeInv();
      if( inverted != null )
      {
        containers = inverted.getContainers();
      }
    }
    if( containers != null )
    {
      for( final IFE1D2DElement ele : containers )
      {
        if( isFiniteElementOf( unit, ele ) )
        {
          return true;
        }
      }
    }

    return false;
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
    Assert.throwIAEOnNullParam( calculationUnit, "calculationUnit" );
    Assert.throwIAEOnNullParam( model1d2d, "model1d2d" );
    final IFeatureWrapperCollection<IFE1D2DComplexElement> complexElements = model1d2d.getComplexElements();
    final Collection<ICalculationUnit1D2D> parents = new ArrayList<ICalculationUnit1D2D>();
    for( final IFE1D2DComplexElement ce : complexElements )
    {
      if( ce instanceof ICalculationUnit1D2D )
      {
        final ICalculationUnit1D2D parent = (ICalculationUnit1D2D) ce;
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
   * 
   * @param model1d2d
   *            the discretisation model
   * @return a collection containing the calculation unit of the dicretisation model
   * @throws IllegalArgumentException
   *             if the argument model1d2d is null
   */
  public static final List<ICalculationUnit> getModelCalculationUnits( final IFEDiscretisationModel1d2d model1d2d ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNullParam( model1d2d, "model1d2d" );
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
   * Answer the number of the 2d elements the given calculation unit has
   * 
   * @param calUnit
   *            the calUnit to inspect
   * @return a 0 or positive integer representing the calculation unit 2d element
   * @throws IllegalArgumentException
   *             if calUnit is null
   */
  public static int getNum2DElement( final ICalculationUnit calUnit )
  {
    Assert.throwIAEOnNullParam( calUnit, "calUnit" );
    if( calUnit instanceof ICalculationUnit1D )
    {
      return 0;
    }
    else if( calUnit instanceof ICalculationUnit2D )
    {
      return calUnit.getElements().countFeatureWrappers( IPolyElement.class );
    }
    else if( calUnit instanceof ICalculationUnit1D2D )
    {
      final LinkedList<ICalculationUnit> calUnitsToCheck = new LinkedList<ICalculationUnit>();
      calUnitsToCheck.add( calUnit );
      int num = 0;
      for( final ICalculationUnit<IFE1D2DElement> currentUnit : calUnitsToCheck )
      {
        num = num + currentUnit.getElements().countFeatureWrappers( IPolyElement.class );
        if( currentUnit instanceof ICalculationUnit1D2D )
        {
          calUnitsToCheck.addAll( ((ICalculationUnit1D2D) currentUnit).getSubUnits() );
        }
      }
      return num;
    }
    else
    {
      throw new IllegalArgumentException( "Cannot handle this calculation unit:" + calUnit );
    }
  }

  public static final int countUnitElements( final ICalculationUnit calUnit, final Class eleClass )
  {
    final LinkedList<ICalculationUnit> calUnitsToCheck = new LinkedList<ICalculationUnit>();
    calUnitsToCheck.add( calUnit );
    int num = 0;
    for( final ICalculationUnit<IFE1D2DElement> currentUnit : calUnitsToCheck )
    {
      num = num + currentUnit.getElements().countFeatureWrappers( eleClass );
      if( currentUnit instanceof ICalculationUnit1D2D )
      {
        calUnitsToCheck.addAll( ((ICalculationUnit1D2D) currentUnit).getSubUnits() );
      }
    }
    return num;
  }

  /**
   * To get the number of boundary line the specified calculation unit contains.
   * 
   * @param calUnit
   *            the calculation unit which boundary line are to be count
   * @return an int representing the number of boundary lines belonging to calUnit
   */
  public static int getNumBoundaryLine( final ICalculationUnit calUnit )
  {
    Assert.throwIAEOnNullParam( calUnit, "calUnit" );
    int num = 0;
    final IFeatureWrapperCollection<IFE1D2DElement> elements = calUnit.getElements();
    for( final IFE1D2DElement ele : elements )
    {
      if( ele instanceof IBoundaryLine )
      {
        num++;
      }
    }
    return num;
  }

  /**
   * Answer the number of the 1d elements the given calculation unit has
   * 
   * @param calUnit
   *            the calUnit to inspect
   * @return a 0 or positive integer representing the calculation unit 1d element
   * @throws IllegalArgumentException
   *             if calUnit is null
   */
  public static int getNum1DElement( final ICalculationUnit calUnit ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNullParam( calUnit, "calUnit" );
    if( calUnit instanceof ICalculationUnit2D )
    {
      return 0;
    }
    else if( calUnit instanceof ICalculationUnit1D )
    {
      return calUnit.getElements().countFeatureWrappers( IElement1D.class );
    }
    else if( calUnit instanceof ICalculationUnit1D2D )
    {
      System.out.println( "Getting element of 1d2d calunit" );
      int num = 0;
      for( final Object ele : calUnit.getElements() )
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
      throw new IllegalArgumentException( "Cannot handle this calculation unit:" + calUnit );
    }
  }

  // /**
  // * Answer whether the given calculation unit has an up-stream
  // * boundary line
  // *
  // * @param calUnit the calculation unit to assert
  // * @return true if the calculation unit has an upstream boundary
  // * line otherwise false
  // * @throws IllegalArgumentException if the argument calUnit is null
  // */
  // public static boolean hasUpBoundary(
  // ICalculationUnit calUnit )
  // throws IllegalArgumentException
  // {
  // Assert.throwIAEOnNullParam( calUnit, "calUnit" );
  // return calUnit.getUpStreamBoundaryLine() !=null;
  // }

  // /**
  // * Answer whether the given calculation unit has a down-stream
  // * boundary line
  // *
  // * @param calUnit the calculation unit to assert
  // * @return true if the calculation unit has a boundary line
  // * otherwise false
  // * @throws IllegalArgumentException if the argument calUnit is null
  // */
  // public static boolean hasDownBoundary(
  // ICalculationUnit calUnit )
  // throws IllegalArgumentException
  // {
  // Assert.throwIAEOnNullParam( calUnit, "calUnit" );
  // return calUnit.getDownStreamBoundaryLine() !=null;
  // }

  // public static final boolean isCalculationUnitBoundaryRelationType(
  // QName relationType )
  // {
  // Assert.throwIAEOnNullParam( relationType, "relationType" );
  // if( relationType.equals(
  // Kalypso1D2DSchemaConstants.WB1D2D_PROP_BOUNDARY_LINE_DOWNSTREAM)||
  // relationType.equals(
  // Kalypso1D2DSchemaConstants.WB1D2D_PROP_BOUNDARY_LINE_UPSTREAM))
  // {
  // return true;
  // }
  // else
  // {
  // return false;
  // }
  // }

  // /**
  // * To get the boundary line linked to the given calculation
  // * unit by the specified relation.
  // * @param calUnit the calculation unit which boundary is to be
  // * found
  // * @param relationType the type of the relation between boundary
  // * line and calculation unit
  // */
  // public static final IBoundaryLine getLinkedBoundaryLine(
  // ICalculationUnit calUnit,
  // QName relationType )
  // {
  // Assert.throwIAEOnNullParam( calUnit, "calUnit" );
  // Assert.throwIAEOnNullParam( relationType, "relaytionType" );
  //
  // if( relationType.equals(
  // Kalypso1D2DSchemaConstants.WB1D2D_PROP_BOUNDARY_LINE_DOWNSTREAM))
  // {
  // return calUnit.getDownStreamBoundaryLine();
  // }
  // else if( relationType.equals(
  // Kalypso1D2DSchemaConstants.WB1D2D_PROP_BOUNDARY_LINE_UPSTREAM))
  // {
  // return calUnit.getUpStreamBoundaryLine();
  // }
  // else
  // {
  // throw new RuntimeException(
  // "Unable to get related boundary: relationType="+relationType);
  // }
  //
  //
  // }

  // /**
  // * Tests whether a given boundary is the upstream boundary line of the specified
  // * calculation unit.
  // * @param calUnit the calculation unit which up stream boundary is to be tested
  // * @param boundaryLine the boundary line to assert
  // * @return true if the provided boundary line is the upstream boundary of the given
  // * calculation unit otherwise false.
  // */
  // public static final boolean isUpStreamBoundaryLine(
  // ICalculationUnit calUnit,
  // IBoundaryLine boundaryLine)
  // {
  // Assert.throwIAEOnNullParam( calUnit, "calUnit" );
  // Assert.throwIAEOnNullParam( boundaryLine, "boundaryLine" );
  // IBoundaryLine upLine = calUnit.getUpStreamBoundaryLine();
  // if( upLine == null )
  // {
  // return false;
  // }
  // else
  // {
  // return boundaryLine.equals( upLine );
  // }
  // }

  // /**
  // * Tests whether a given boundary is the downstream boundary line of the specified
  // * calculation unit.
  // * @param calUnit the calculation unit which down stream boundary is to be tested
  // * @param boundaryLine the boundary line to assert
  // * @return true if the provided boundary line is the down stream boundary of the given
  // * calculation unit otherwise false.
  // */
  // public static final boolean isDownStreamBoundaryLine(
  // ICalculationUnit calUnit,
  // IBoundaryLine boundaryLine)
  // {
  // Assert.throwIAEOnNullParam( calUnit, "calUnit" );
  // Assert.throwIAEOnNullParam( boundaryLine, "boundaryLine" );
  // IBoundaryLine upLine = calUnit.getDownStreamBoundaryLine();
  // if( upLine == null )
  // {
  // return false;
  // }
  // else
  // {
  // return boundaryLine.equals( upLine );
  // }
  // }

  // public static final QName getRelationType(
  // ICalculationUnit calUnit,
  // IBoundaryLine bLine)
  // {
  // Assert.throwIAEOnNullParam( calUnit, "calUnit" );
  // Assert.throwIAEOnNullParam( bLine, "bLine" );
  // if( isDownStreamBoundaryLine( calUnit, bLine ))
  // {
  // return Kalypso1D2DSchemaConstants.WB1D2D_PROP_BOUNDARY_LINE_DOWNSTREAM;
  // }
  // else if( isUpStreamBoundaryLine( calUnit, bLine ))
  // {
  // return Kalypso1D2DSchemaConstants.WB1D2D_PROP_BOUNDARY_LINE_UPSTREAM;
  // }
  // else
  // {
  // return null;
  // }
  // }

  /**
   * Tests whether a given boundary is a boundary line of the specified calculation unit.
   * 
   * @param boundaryLine
   *            the boundary line to assert
   * @param calUnit
   *            the calculation unit which stream boundary is to be tested
   * @return true if the provided boundary line is a boundary of the given calculation unit otherwise false.
   */
  public static final boolean isBoundaryLineOf( final IBoundaryLine boundaryLine, final ICalculationUnit calUnit )
  {
    Assert.throwIAEOnNullParam( calUnit, "calUnit" );
    Assert.throwIAEOnNullParam( boundaryLine, "boundaryLine" );
    final IFeatureWrapperCollection containers = boundaryLine.getContainers();
    final boolean answer = containers.contains( calUnit );
    return answer;
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
    Assert.throwIAEOnNullParam( calUnit, "calUnit" );
    final LinkedList<GM_Envelope> contributingBBox = new LinkedList<GM_Envelope>();

    // collect all contributing bboxes
    contributingBBox.add( calUnit.getElements().getWrappedList().getBoundingBox() );
    if( calUnit instanceof ICalculationUnit1D2D )
    {
      final LinkedList<ICalculationUnit> subUnits = new LinkedList<ICalculationUnit>( ((ICalculationUnit1D2D) calUnit).getSubUnits() );
      while( !subUnits.isEmpty() )
      {
        final ICalculationUnit removed = subUnits.remove( 0 );
        contributingBBox.add( removed.getElements().getWrappedList().getBoundingBox() );
        if( removed instanceof ICalculationUnit1D2D )
        {
          subUnits.addAll( ((ICalculationUnit1D2D) removed).getSubUnits() );
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
    Assert.throwIAEOnNullParam( unit, "unit" );
    Assert.throwIAEOnNullParam( element, "element" );
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
        subUnits.addAll( ((ICalculationUnit1D2D) currentSubUnit).getSubUnits() );
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
    final List parents = (List) bCondition.getWrappedFeature().getProperty( Kalypso1D2DSchemaConstants.OP1D2D_PROP_PARENT_CALCUNIT );
    return parents.contains( unit.getGmlID() );
  }

  /**
   * To get the boundary line used in the assignment of the given boundary condition in the provided unit
   * 
   * @param unit
   *            the unit given the assignement contex
   * @param bCondition
   *            the boundary condition which line is to be get
   * @return the assigned boundary condition line or null if there is none
   * @throws IllegalArgumentException
   *             if unit or bCondition is null or grabDistance is null
   */
  public static final IBoundaryLine getAssignedBoundaryConditionLine( final ICalculationUnit unit, final IBoundaryCondition bCondition )
  {
    final List<IBoundaryLine> boundaryLines = unit.getBoundaryLines();
    if( bCondition.getType().equals( IBoundaryCondition.PARENT_TYPE_LINE1D2D ) )
    {
      final String parentGmlID = bCondition.getParentElementID();
      for( final IBoundaryLine boundaryLine : boundaryLines )
        if(boundaryLine.getGmlID().equals( parentGmlID ))
          return boundaryLine;
    }
    return null;
  }

  /**
   * To select the calculation unit line element given the provided position. The nearest line in the proximity of the
   * selection position is selected.
   * 
   * @param unit
   *            the unit which line element is to be selected
   * @param bcPosition
   *            the selection location
   * @param lineType
   *            the type of line to be selected
   * @return a {@link ILineElement} representing the nearest line element in the proximity of the given position.
   */
  public static final <T extends ILineElement> T getLineElement( final ICalculationUnit<IFE1D2DElement> unit, final GM_Point bcPosition, final double grabDistance, final Class<T> lineType )
  {
    Assert.throwIAEOnNullParam( unit, "unit" );
    Assert.throwIAEOnNullParam( bcPosition, "bcPosition" );
    Assert.throwIAEOnNullParam( lineType, "lineType" );
    Assert.throwIAEOnLessThan0( grabDistance, "grab distance must be greater or equals to 0" );
    final GM_Envelope env = GeometryUtilities.grabEnvelopeFromDistance( bcPosition, grabDistance );

    final List<IFE1D2DElement> targetLines = unit.getElements().query( env );
    double minDistance = Double.MAX_VALUE;
    T targetLine = null;
    for( int i = targetLines.size() - 1; i >= 0; i-- )
    {
      final IFE1D2DElement ele = targetLines.get( i );

      if( lineType.isAssignableFrom( ele.getClass() ) )
      {
        try
        {
          final GM_Object line = ele.recalculateElementGeometry();
          final double currentDist = bcPosition.distance( line );
          if( currentDist < minDistance )
          {
            minDistance = currentDist;
            targetLine = (T) ele;
          }
        }
        catch( final GM_Exception e )
        {
          e.printStackTrace();
          throw new RuntimeException( e );
        }

      }
    }
    return targetLine;
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
  public static final List<IBoundaryCondition> getBoundaryConditions( final Collection<IBoundaryCondition> conditions, final ICalculationUnit<IFE1D2DElement> unit )
  {
    final List<IBoundaryCondition> assignedConditions = new ArrayList<IBoundaryCondition>();
    for( final IBoundaryCondition condition : conditions )
      if( isBoundaryConditionOf( unit, condition ) )
        assignedConditions.add( condition );
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
  public static final int countAssignedBoundaryConditions( final Collection<IBoundaryCondition> conditions, final ICalculationUnit<IFE1D2DElement> unit )
  {
    int count = 0;
    for( final IBoundaryCondition condition : conditions )
      if( isBoundaryConditionOf( unit, condition ) )
        count++;
    return count;
  }

  /**
   * Answer whether the provided junction contect is joining subunits of the given combined unit 1d2d. The test is base
   * on the inclusion ofthe juntion 1d element in a subunit
   * 
   * @param unit
   *            the combined unit which gives the test context
   * @param jContext
   *            the junction context to text for being part of combined unit
   * @return true if jContext is joining sub unit of the combined unit otherwise false
   */
  public static boolean isJunctionContextOf( final ICalculationUnit1D2D unit, final IJunctionContext1DToCLine jContext )
  {
    final IElement1D element1D = jContext.getElement1D();
    if( element1D == null )
      return false;
    return isFiniteElementOf( unit, element1D );
  }
}
