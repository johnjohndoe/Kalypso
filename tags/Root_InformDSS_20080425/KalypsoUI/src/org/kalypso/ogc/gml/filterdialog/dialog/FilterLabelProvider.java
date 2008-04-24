/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.filterdialog.dialog;

import java.util.List;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.kalypso.ogc.gml.filterdialog.model.FilterRootElement;
import org.kalypsodeegree.filterencoding.ElseFilter;
import org.kalypsodeegree.filterencoding.Expression;
import org.kalypsodeegree.filterencoding.Filter;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree_impl.filterencoding.ComplexFilter;
import org.kalypsodeegree_impl.filterencoding.FeatureFilter;
import org.kalypsodeegree_impl.filterencoding.Literal;
import org.kalypsodeegree_impl.filterencoding.LogicalOperation;
import org.kalypsodeegree_impl.filterencoding.OperationDefines;
import org.kalypsodeegree_impl.filterencoding.PropertyIsBetweenOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyIsCOMPOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyIsLikeOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyIsNullOperation;
import org.kalypsodeegree_impl.filterencoding.SpatialOperation;

/**
 * @author kuepferle
 */
public class FilterLabelProvider extends LabelProvider
{

  /**
   * @see org.eclipse.jface.viewers.ILabelProvider#getImage(java.lang.Object)
   */
  @Override
  public Image getImage( final Object element )
  {
// ImageDescriptor imageDesc = null;
// if( element != null )
// {
// if( element instanceof Operation )
// {
// Operation operation = (Operation) element;
// int id = operation.getOperatorId();
// int type = OperationDefines.getTypeById( id );
// if( operation instanceof SpatialOperation )
// {
// if( type == OperationDefines.UNKNOWN )
// imageDesc = ImageProvider.IMAGE_FILTERDIALOG_WARNING;
// }
// }
// if( imageDesc != null )
// return imageDesc.createImage();
// }
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ILabelProvider#getText(java.lang.Object)
   */

  @Override
  public String getText( final Object element )
  {
    if( element != null )
    {
      if( element instanceof FilterRootElement )
      {
        return ((FilterRootElement) (element)).getName();
      }
      else if( element instanceof ComplexFilter )
      {
        return "COMPLEX_FILTER";
      }
      else if( element instanceof FeatureFilter )
      {
        return "FEATURE_FILTER";
      }
      else if( element instanceof ElseFilter )
      {
        return "ELSE_FILTER";
      }
      else if( element instanceof Filter )
      {
        return "FILTER";
      }
      else if( element instanceof Operation )
      {
        final Operation operation = (Operation) element;
        final int id = operation.getOperatorId();
        final int type = OperationDefines.getTypeById( id );
        // Spatial
        if( element instanceof SpatialOperation )
        {
          if( type == OperationDefines.UNKNOWN )
            return "Empty Spatial Operation *";

          return operation.getOperatorName();
        }
        // Logical
        if( type == OperationDefines.TYPE_LOGICAL )
        {
          String label = operation.getOperatorName().toUpperCase();
          final List<Operation> args = ((LogicalOperation) element).getArguments();
          if( id == OperationDefines.AND || id == OperationDefines.OR )
          {
            if( args == null || args.size() < 2 )
              label = label + " *"; //$NON-NLS-1$
          }
          if( id == OperationDefines.NOT && (args == null || args.size() == 0) )
            label = label + " *"; //$NON-NLS-1$
          return label;
        }
        // Comparision
        if( operation instanceof PropertyIsCOMPOperation )
        {
          final Expression firstExpression = ((PropertyIsCOMPOperation) operation).getFirstExpression();
          final Expression secondExpression = ((PropertyIsCOMPOperation) operation).getSecondExpression();
          if( operation.getOperatorId() == OperationDefines.UNKNOWN )
            return "unkown Comparsion Operation *";
          else if( firstExpression == null || secondExpression == null )
            return OperationDefines.getNameById( id ) + " *";// "X = VALUE"; //$NON-NLS-1$
          return OperationDefines.getNameById( id );

          // if( id == OperationDefines.PROPERTYISEQUALTO )
          // {}
          // if( id == OperationDefines.PROPERTYISGREATERTHAN )
          // return OperationDefines.getNameById( id );//"X > VALUE";
          //
          // if( id == OperationDefines.PROPERTYISGREATERTHANOREQUALTO )
          // return OperationDefines.getNameById( id );//"X >= VALUE";
          //
          // if( id == OperationDefines.PROPERTYISLESSTHAN )
          // return OperationDefines.getNameById( id );//"X < VALUE";
          //
          // if( id == OperationDefines.PROPERTYISLESSTHANOREQUALTO )
          // return OperationDefines.getNameById( id );//"X <= VALUE";

        }
        if( operation instanceof PropertyIsLikeOperation )
        {
          final Literal literal = ((PropertyIsLikeOperation) operation).getLiteral();
          if( operation.getOperatorId() == OperationDefines.UNKNOWN )
            return "unkown Comparsion Operation *";
          else if( literal == null )
            return OperationDefines.getNameById( id ) + " *";// "TEXT isLike"; //$NON-NLS-1$
          return OperationDefines.getNameById( id );
        }

        if( operation instanceof PropertyIsNullOperation )
        {
          final Expression expression = ((PropertyIsNullOperation) operation).getExpression();
          if( operation.getOperatorId() == OperationDefines.UNKNOWN )
            return "unkown Comparsion Operation *";
          else if( expression == null )
            return OperationDefines.getNameById( id ) + " *";// "X = NULL"; //$NON-NLS-1$
          return OperationDefines.getNameById( id );
        }
        if( operation instanceof PropertyIsBetweenOperation )
        {
          final Expression upperBoundary = ((PropertyIsBetweenOperation) operation).getUpperBoundary();
          final Expression lowerBoundary = ((PropertyIsBetweenOperation) operation).getLowerBoundary();
          if( operation.getOperatorId() == OperationDefines.UNKNOWN )
            return "unkown Comparsion Operation *";
          else if( upperBoundary == null || lowerBoundary == null )
            return OperationDefines.getNameById( id ) + " *"; //$NON-NLS-1$
          return OperationDefines.getNameById( id );// "VALUE1 < X < VALUE2";
        }
      }
      else
        throw unknownElement( element );
    }
    return "EMPTY_NODE";
  }

  protected RuntimeException unknownElement( final Object element )
  {
    return new RuntimeException( "Unknown type of element in tree of type " + element.getClass().getName() ); //$NON-NLS-1$
  }

}