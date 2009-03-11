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
package org.kalypsodeegree_impl.model.feature.gmlxpath;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.namespace.NamespaceContext;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.gmlxpath.xelement.IXElement;
import org.kalypsodeegree_impl.model.feature.gmlxpath.xelement.XElementFormAttribute;
import org.kalypsodeegree_impl.model.feature.gmlxpath.xelement.XElementFormPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.xelement.XElementFormString;
import org.kalypsodeegree_impl.model.feature.gmlxpath.xelement.XElementFromFunction;
import org.kalypsodeegree_impl.model.feature.gmlxpath.xelement.XElementFromOperation;

/**
 * Factory to create XElements that a part of complied GMLXPathes
 * 
 * @author doemming
 */
public class XElementFactory
{
  private final IGMLXPathOperation[] m_operations;

  final IGMLXPathFunction[] m_functions;

  public XElementFactory( )
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
    final IGMLXPathOperation op1 = new GMLXPathOperation( "(.+)( mod )(.+)" )
    {
      // "mod";
      public Object operate( Object value1, Object value2 )
      {
        // TODO Auto-generated method stub
        return null;
      }
    };

    final IGMLXPathOperation op2 = new GMLXPathOperation( "(.+)( div )(.+)" )
    {
      public Object operate( Object value1, Object value2 )
      {
        // TODO Auto-generated method stub
        return null;
      }
    };

    final IGMLXPathOperation op3 = new GMLXPathOperation( "(.+)( or )(.+)" )
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

    final IGMLXPathOperation op4 = new GMLXPathOperation( "(.+)( and )(.+)" )
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
    final IGMLXPathOperation op5 = new GMLXPathMathOperation( "(.+)( >= )(.+)" )
    {

      @Override
      public Object mathOperate( double n1, double n2 )
      {
        return n1 >= n2;
      }

    };
    final IGMLXPathOperation op6 = new GMLXPathMathOperation( "(.+)( <= )(.+)" )
    {

      @Override
      public Object mathOperate( double n1, double n2 )
      {
        return n1 <= n2;
      }
    };
    final IGMLXPathOperation op7 = new GMLXPathMathOperation( "(.+)( > )(.+)" )
    {

      @Override
      public Object mathOperate( double n1, double n2 )
      {
        return n1 > n2;
      }
    };
    final IGMLXPathOperation op8 = new GMLXPathMathOperation( "(.+)( < )(.+)" )
    {

      @Override
      public Object mathOperate( double n1, double n2 )
      {
        return n1 >= n2;
      }
    };
    final IGMLXPathOperation op9 = new GMLXPathMathOperation( "(.+)( != )(.+)" )
    {

      @Override
      public Object operate( Object value1, Object value2 ) throws GMLXPathException
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
    final IGMLXPathOperation op10 = new GMLXPathMathOperation( "(.+)( = )(.+)" )
    {

      @Override
      public Object operate( Object value1, Object value2 ) throws GMLXPathException
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
    final IGMLXPathOperation op11 = new GMLXPathMathOperation( "(.+)( \\\\* )(.+)" )
    {

      @Override
      public Object mathOperate( double n1, double n2 )
      {
        return n1 * n2;
      }

    };
    final IGMLXPathOperation op12 = new GMLXPathMathOperation( "(.+)( - )(.+)" )
    {
      @Override
      public Object mathOperate( double n1, double n2 )
      {
        return n1 - n2;
      }
    };
    final IGMLXPathOperation op13 = new GMLXPathMathOperation( "(.+)( \\\\+ )(.+)" )
    {
      @Override
      public Object mathOperate( double n1, double n2 )
      {
        return n1 + n2;
      }

    };

    final IGMLXPathFunction f1 = new AbstractGMLXPathFunction( "not" )
    {

      public Boolean evaluate( Feature contextFE, IXElement argumentXEelement, boolean isFeatureTypeLevel ) throws GMLXPathException
      {
        Object object = argumentXEelement.evaluate( contextFE, isFeatureTypeLevel );
        if( object instanceof String )
          return !Boolean.parseBoolean( (String) object );
        return null;
      }
    };
    final IGMLXPathFunction f2 = new AbstractGMLXPathFunction( "name" )
    {

      public Object evaluate( Feature contextFE, IXElement argumentXEelement, boolean isFeatureTypeLevel ) throws GMLXPathException
      {
        final Feature fe;
        if( argumentXEelement != null )
          fe = (Feature) argumentXEelement.evaluate( contextFE, isFeatureTypeLevel );
        else
          fe = contextFE;
        return fe.getFeatureType().getQName().getLocalPart();
      }
    };
    final IGMLXPathFunction f3 = new AbstractGMLXPathFunction( "position" )
    {
      public Object evaluate( Feature contextFE, IXElement argumentXEelement, boolean isFeatureTypeLevel )
      {
        throw new UnsupportedOperationException();
      }
    };
    final IGMLXPathFunction f4 = new AbstractGMLXPathFunction( "id" )
    {

      public Object evaluate( Feature contextFE, IXElement argumentXEelement, boolean isFeatureTypeLevel ) throws GMLXPathException
      {
        final GMLWorkspace workspace = contextFE.getWorkspace();
        final String fId = (String) argumentXEelement.evaluate( contextFE, isFeatureTypeLevel );
        final Feature feature = workspace.getFeature( fId );
        return feature;
      }
    };

    m_operations = new IGMLXPathOperation[] { op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11, op12, op13 };
    m_functions = new IGMLXPathFunction[] { f1, f2, f3, f4 };
  }

  public IXElement create( final String string, final NamespaceContext namespaceContext )
  {
    final GMLXPathString cond = new GMLXPathString( string );
    return create( cond, namespaceContext );
  }

  public IXElement create( final GMLXPathString cond, final NamespaceContext namespaceContext )
  {
    final String condition = cond.getCond();
    final String marked = cond.getMarked();
    // operations...
    for( final IGMLXPathOperation operation : m_operations )
    {
      final Pattern pattern = operation.getPattern();
      final Matcher matcher = pattern.matcher( marked );
      if( matcher.matches() )
      {
        final String preMarked = matcher.group( 1 );
        final String pre = condition.substring( 0, preMarked.length() );
        // matcher.group(2) is operation
        final String postMarked = matcher.group( 3 );
        final String post = condition.substring( condition.length() - postMarked.length() );

        final IXElement preXElement = create( pre, namespaceContext );
        final IXElement postXElement = create( post, namespaceContext );
        return new XElementFromOperation( preXElement, operation, postXElement );
      }
    }
    // functions
    for( final IGMLXPathFunction function : m_functions )
    {
      final Pattern p = function.getPattern();
      final Matcher matcher = p.matcher( marked );
      if( matcher.matches() )
      {
        final String argument = function.getArgument( matcher, cond );
        if( argument != null && argument.length() > 0 )
        {
          final IXElement element = create( argument, namespaceContext );
          return new XElementFromFunction( function, element );
        }
        return new XElementFromFunction( function );
      }
    }
    // attributes
    final String markedTrim = marked.trim();
    if( markedTrim.startsWith( "@" ) )
      return new XElementFormAttribute( condition );
    // other xpathes
    // strings
    if( markedTrim.startsWith( "'" ) )
      return new XElementFormString( condition );
    return new XElementFormPath( condition, namespaceContext );
  }

}
