/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.filterencoding;

import org.kalypsodeegree.filterencoding.Expression;
import org.kalypsodeegree.filterencoding.FilterConstructionException;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.xml.ElementList;
import org.kalypsodeegree.xml.XMLTools;
import org.w3c.dom.Element;

/**
 * Encapsulates the information of a <Add>/ <Sub>/ <Mul>or <DIV>element as defined in the Expression DTD.
 * 
 * @author Markus Schneider
 * @version 07.08.2002
 */
public class ArithmeticExpression extends Expression_Impl
{
  /** The first operand. */
  Expression m_expr1;

  /** The second operand. */
  Expression m_expr2;

  /** Constructs a new ArithmeticExpression. */
  public ArithmeticExpression( final int id, final Expression expr1, final Expression expr2 )
  {
    m_id = id;
    m_expr1 = expr1;
    m_expr2 = expr2;
  }

  /**
   * Given a DOM-fragment, a corresponding Expression-object is built. This method recursively calls other buildFromDOM () -
   * methods to validate the structure of the DOM-fragment.
   * 
   * @throws FilterConstructionException
   *             if the structure of the DOM-fragment is invalid
   */
  public static Expression buildFromDOM( final Element element ) throws FilterConstructionException
  {
    // check if root element's name is 'Add' / 'Sub' / 'Mul' or 'Div'
    final String name = element.getLocalName();
    final int id = EXPRESSION_DEFINES.getIdByName( name );
    switch( id )
    {
      case ExpressionDefines.ADD:
      case ExpressionDefines.SUB:
      case ExpressionDefines.MUL:
      case ExpressionDefines.DIV:
      {
        break;
      }
      default:
      {
        throw new FilterConstructionException( "Element's name does not match 'Add' / 'Sub' / 'Mul' or 'Div'!" );
      }
    }

    // determine the arguments
    final ElementList children = XMLTools.getChildElements( element );
    if( children.getLength() != 2 )
      throw new FilterConstructionException( "'" + name + "' requires exactly 2 elements!" );

    final Expression expr1 = Expression_Impl.buildFromDOM( children.item( 0 ) );
    final Expression expr2 = Expression_Impl.buildFromDOM( children.item( 1 ) );

    return new ArithmeticExpression( id, expr1, expr2 );
  }

  /** Produces an indented XML representation of this object. */
  @Override
  public StringBuffer toXML( )
  {
    final StringBuffer sb = new StringBuffer();

    sb.append( "<ogc:" ).append( getExpressionName() ).append( ">" );
    sb.append( m_expr1.toXML() );
    sb.append( m_expr2.toXML() );
    sb.append( "</ogc:" ).append( getExpressionName() ).append( ">" );
    return sb;
  }

  /**
   * Returns this <tt>ArithmeticExpression/tt>'s value (to be used in the
   * evaluation of complex <tt>Expression</tt>s).
   * TODO: Improve datatype handling.
   * @param feature that determines the concrete values of
   *                <tt>PropertyNames</tt> in the expression
   * @return the resulting value (as <tt>Double</tt>)
   * @throw FilterEvaluationException if the expressions are not numerical
   */
  public Object evaluate( final Feature feature ) throws FilterEvaluationException
  {
    final Object o1 = m_expr1.evaluate( feature );
    final Object o2 = m_expr2.evaluate( feature );

    // If any of the arguments evaluates to null return null
    if( o1 == null || o2 == null )
      return null;

    if( !(o1 instanceof Number && o2 instanceof Number) )
    {
      throw new FilterEvaluationException( "ADD/SUB/DIV/MUL may only be applied to numerical expressions." );
    }
    final double d1 = ((Number) o1).doubleValue();
    final double d2 = ((Number) o2).doubleValue();
    switch( m_id )
    {
      case ExpressionDefines.ADD:
        return new Double( d1 + d2 );
      case ExpressionDefines.SUB:
        return new Double( d1 - d2 );
      case ExpressionDefines.MUL:
        return new Double( d1 * d2 );
      case ExpressionDefines.DIV:
        return new Double( d1 / d2 );
      default:
      {
        throw new FilterEvaluationException( "Unknown ArithmeticExpression: '" + getExpressionName() + "'!" );
      }
    }
  }

  /**
   * returns the first expression
   */
  public Expression getFirstExpression( )
  {
    return m_expr1;
  }

  /**
   * returns the second expression
   */
  public Expression getSecondExpression( )
  {
    return m_expr1;
  }
}