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
import org.w3c.dom.Element;

/**
 * Abstract superclass representing expr-entities (as defined in the Expression DTD).
 * 
 * @author Markus Schneider
 * @version 06.08.2002
 */
abstract public class Expression_Impl implements Expression
{
  public static ExpressionDefines EXPRESSION_DEFINES = new ExpressionDefines();

  /**
   * The underlying expression's id.
   * 
   * @see ExpressionDefines
   */
  int m_id;

  /**
   * Given a DOM-fragment, a corresponding Expression-object is built. This method recursively calls other buildFromDOM () -
   * methods to validate the structure of the DOM-fragment.
   * 
   * @throws FilterConstructionException
   *             if the structure of the DOM-fragment is invalid
   */
  public static Expression buildFromDOM( final Element element ) throws FilterConstructionException
  {
    // check if root element's name is a known expression
    final String name = element.getLocalName();

    final int id = EXPRESSION_DEFINES.getIdByName( name );
    Expression expression = null;

    switch( id )
    {
      case ExpressionDefines.EXPRESSION:
      {
        break;
      }
      case ExpressionDefines.PROPERTYNAME:
      {
        expression = PropertyName.buildFromDOM( element );
        break;
      }
      case ExpressionDefines.LITERAL:
      {
        expression = Literal.buildFromDOM( element );
        break;
      }
      case ExpressionDefines.FUNCTION:
      {
        expression = Function.buildFromDOM( element );
        break;
      }
      case ExpressionDefines.ADD:
      case ExpressionDefines.SUB:
      case ExpressionDefines.MUL:
      case ExpressionDefines.DIV:
      {
        expression = ArithmeticExpression.buildFromDOM( element );
        break;
      }
      default:
      {
        throw new FilterConstructionException( "Unknown expression '" + name + "'!" );
      }
    }
    return expression;
  }

  /** Returns the name of the expression. */
  public String getExpressionName( )
  {
    return EXPRESSION_DEFINES.getNameById( m_id );
  }

  /**
   * Returns the expression's id.
   * 
   * @see ExpressionDefines
   */
  public int getExpressionId( )
  {
    return m_id;
  }

  /** Produces an indented XML representation of this object. */
  public abstract StringBuffer toXML( );
}