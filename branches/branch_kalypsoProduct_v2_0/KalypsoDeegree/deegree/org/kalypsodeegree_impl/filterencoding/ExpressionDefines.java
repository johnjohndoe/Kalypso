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

import java.util.HashMap;

/**
 * Defines codes and constants for easy coping with the different kinds of Expressions (both XML-Entities &
 * JavaObjects). Change: the static, lazily initialized members where NOT thread safe. Better: use the class in a non
 * static way
 * 
 * @author Markus Schneider
 * @version 06.08.2002
 */
public class ExpressionDefines
{
  // expression codes
  public static final int EXPRESSION = 0;

  public static final int PROPERTYNAME = 1;

  public static final int LITERAL = 2;

  public static final int FUNCTION = 3;

  public static final int ADD = 4;

  public static final int SUB = 5;

  public static final int MUL = 6;

  public static final int DIV = 7;

  public static final int UNKNOWN = -1;

  /** used to associate names with the expressions */
  private static HashMap<String, ExpressionInfo> names = new HashMap<String, ExpressionInfo>();

  /** used to associate ids (Integers) with the expressions */
  private static HashMap<Integer, ExpressionInfo> ids = new HashMap<Integer, ExpressionInfo>();

  public ExpressionDefines( )
  {
    addExpression( EXPRESSION, "Expression" );
    addExpression( PROPERTYNAME, "PropertyName" );
    addExpression( LITERAL, "Literal" );
    addExpression( FUNCTION, "Function" );
    addExpression( ADD, "Add" );
    addExpression( SUB, "Sub" );
    addExpression( MUL, "Mul" );
    addExpression( DIV, "Div" );
  }

  private void addExpression( final int id, final String name )
  {
    final ExpressionInfo expressionInfo = new ExpressionInfo( id, name );
    names.put( name.toLowerCase(), expressionInfo );
    ids.put( new Integer( id ), expressionInfo );
  }

  /**
   * Returns the id of an expression for a given name.
   * 
   * @return EXPRESSION / PROPERTYNAME / LITERAL / ...
   */
  public int getIdByName( final String name )
  {
    final ExpressionInfo expression = names.get( name.toLowerCase() );
    if( expression == null )
      return UNKNOWN;
    return expression.m_id;
  }

  /**
   * Returns the name of an expression for a given id.
   * 
   * @return null / Name of expression
   */
  public String getNameById( int id )
  {
    final ExpressionInfo expression = ids.get( new Integer( id ) );
    if( expression == null )
      return null;
    return expression.m_name;
  }

  private static final class ExpressionInfo
  {
    public final int m_id;

    public final String m_name;

    public ExpressionInfo( final int id, final String name )
    {
      m_id = id;
      m_name = name;
    }
  }
}
