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
package org.kalypsodeegree_impl.model.feature.xpath;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author doemming
 */
public class FeaturePathConditionFactory
{
  private IOperation[] m_operations;

  final IFunction[] m_functions;

  public FeaturePathConditionFactory( )
  {

    // 12 +
    // 11 -
    // 10 *
    // Multiplikation - jedoch nur, wenn vor dem Operator keines der folgenden Zeichen bzw. keine der folgenden
    // Zeichenfolgen steht: @ :: ( [ ) ,
    // 9 =
    // 8 !=
    // 7 < bzw. &lt;
    // 6 > bzw. &&gt;
    // 5 <= bzw. &lt;=
    // 4 >= bzw. &gt;=
    // 3 and
    // 2 or
    // 1 div
    // 0 mod
    final IOperation op1 = new Operation( "(.+)( mod )(.+)" )
    {
      // "mod";
      public Object operate( Object value1, Object value2 )
      {
        // TODO Auto-generated method stub
        return null;
      }
    };

    final IOperation op2 = new Operation( "(.+)( div )(.+)" )
    {
      public Object operate( Object value1, Object value2 )
      {
        // TODO Auto-generated method stub
        return null;
      }
    };

    final IOperation op3 = new Operation( "(.+)( or )(.+)" )
    {
      public Object operate( Object value1, Object value2 )
      {
        boolean b1;
        if( value1 instanceof Boolean )
          b1 = ((Boolean) value1).booleanValue();
        else
          b1 = (value1 != null);
        boolean b2;
        if( value2 instanceof Boolean )
          b2 = ((Boolean) value2).booleanValue();
        else
          b2 = (value2 != null);
        return b1 || b2;
      }
    };

    final IOperation op4 = new Operation( "(.+)( and )(.+)" )
    {
      public Object operate( Object value1, Object value2 )
      {
        boolean b1;
        if( value1 instanceof Boolean )
          b1 = ((Boolean) value1).booleanValue();
        else
          b1 = (value1 != null);
        boolean b2;
        if( value2 instanceof Boolean )
          b2 = ((Boolean) value2).booleanValue();
        else
          b2 = (value2 != null);
        return b1 && b2;
      }
    };
    final IOperation op5 = new MathOperation( "(.+)( >= )(.+)" )
    {

      @Override
      public Object mathOperate( double n1, double n2 )
      {
        return n1 >= n2;
      }

    };
    final IOperation op6 = new MathOperation( "(.+)( <= )(.+)" )
    {

      @Override
      public Object mathOperate( double n1, double n2 )
      {
        return n1 <= n2;
      }
    };
    final IOperation op7 = new MathOperation( "(.+)( > )(.+)" )
    {

      @Override
      public Object mathOperate( double n1, double n2 )
      {
        return n1 > n2;
      }
    };
    final IOperation op8 = new MathOperation( "(.+)( < )(.+)" )
    {

      @Override
      public Object mathOperate( double n1, double n2 )
      {
        return n1 >= n2;
      }
    };
    final IOperation op9 = new MathOperation( "(.+)( != )(.+)" )
    {

      @Override
      public Object operate( Object value1, Object value2 ) throws FeaturePathException
      {
        if( value1 instanceof String && value2 instanceof String )
          return !value1.equals( value2 );
        if( value1 instanceof Boolean && value2 instanceof Boolean )
          return !value1.equals( value2 );
        return super.operate( value1, value2 );
      }

      @Override
      public Object mathOperate( double n1, double n2 )
      {
        return n1 != n2;
      }
    };
    final IOperation op10 = new MathOperation( "(.+)( = )(.+)" )
    {

      @Override
      public Object operate( Object value1, Object value2 ) throws FeaturePathException
      {
        if( value1 instanceof String && value2 instanceof String )
          return value1.equals( value2 );
        if( value1 instanceof Boolean && value2 instanceof Boolean )
          return value1.equals( value2 );
        return super.operate( value1, value2 );
      }

      @Override
      public Object mathOperate( double n1, double n2 )
      {
        return n1 == n2;
      }
    };
    IOperation op11 = new MathOperation( "(.+)( \\\\* )(.+)" )
    {

      @Override
      public Object mathOperate( double n1, double n2 )
      {
        return n1 * n2;
      }

    };
    IOperation op12 = new MathOperation( "(.+)( - )(.+)" )
    {
      @Override
      public Object mathOperate( double n1, double n2 )
      {
        return n1 - n2;
      }
    };
    IOperation op13 = new MathOperation( "(.+)( \\\\+ )(.+)" )
    {
      @Override
      public Object mathOperate( double n1, double n2 )
      {
        return n1 + n2;
      }

    };

    final IFunction f1 = new Function( "not" )
    {

      public Boolean evaluate( GMLWorkspace contextWS, Feature contextFE, IXElement element ) throws FeaturePathException
      {
        Object object = element.evaluate( contextWS, contextFE );
        if( object instanceof String )
          return !Boolean.parseBoolean( (String) object );
        return null;
      }
    };
    final IFunction f2 = new Function( "name" )
    {

      public String evaluate( GMLWorkspace contextWS, Feature contextFE, IXElement element ) throws FeaturePathException
      {
        final Feature fe;
        if( element != null )
          fe = (Feature) element.evaluate( contextWS, contextFE );
        else
          fe = contextFE;
        return fe.getFeatureType().getQName().getLocalPart();
      }
    };
    final IFunction f3 = new Function( "position" )
    {

      public String evaluate( GMLWorkspace contextWS, Feature context, IXElement element )
      {
        throw new UnsupportedOperationException();
      }
    };
    final IFunction f4 = new Function( "id" )
    {

      public Object evaluate( GMLWorkspace contextWS, Feature contextFE, IXElement element ) throws FeaturePathException
      {
        final String fId = (String) element.evaluate( contextWS, contextFE );
        return contextWS.getFeature( fId );
      }
    };

    m_operations = new IOperation[] { op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11, op12, op13 };
    m_functions = new IFunction[] { f1, f2, f3, f4 };
  }

  public IXElement create( final Cond cond )
  {
    final String condition = cond.getCond();
    final String marked = cond.getMarked();
    // operations...
    for( int i = 0; i < m_operations.length; i++ )
    {
      final IOperation operation = m_operations[i];
      final Pattern pattern = operation.getPattern();
      final Matcher matcher = pattern.matcher( marked );
      if( matcher.matches() )
      {
        final int gc = matcher.groupCount(); // TODO sollte 4 sein
        final String preMarked = matcher.group( 1 );
        final String pre = condition.substring( 0, preMarked.length() );
        final Cond preCond = new Cond( pre );
        // matcher.group(2) is operation
        final String postMarked = matcher.group( 3 );
        final String post = condition.substring( condition.length() - postMarked.length() );
        final Cond postCond = new Cond( post );
        final IXElement preXElement = create( preCond );
        final IXElement postXElement = create( postCond );
        return new XElementFromOperation( preXElement, operation, postXElement );
      }
    }
    // functions
    for( int i = 0; i < m_functions.length; i++ )
    {
      final IFunction function = m_functions[i];
      final Pattern p = function.getPattern();
      final Matcher matcher = p.matcher( marked );
      if( matcher.matches() )
      {
        final String argument = function.getArgument( matcher, cond );
        if( argument != null && argument.length() > 0 )
        {
          final IXElement element = create( new Cond( argument ) );
          return new XElementFromFunction( function, element );
        }
        return new XElementFromFunction( function );
      }
    }
    // attributes
    String markedTrim= marked.trim();
    if( markedTrim.startsWith( "@" ) )
      return new XElementFormAttribute(condition);
    // other xpathes
    // strings
    if( markedTrim.startsWith( "'" ) )
      return new XElementFormString( condition );
    return new XElementFormProperty( condition );
  }

}
