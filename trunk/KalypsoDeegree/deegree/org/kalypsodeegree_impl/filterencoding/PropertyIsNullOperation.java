/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

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
 
 
 history:
  
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
     
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.filterencoding;

import org.kalypsodeegree.filterencoding.Expression;
import org.kalypsodeegree.filterencoding.FilterConstructionException;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.xml.ElementList;
import org.kalypsodeegree.xml.XMLTools;
import org.w3c.dom.Element;

/**
 * Encapsulates the information of a <PropertyIsNull>-element (as defined in
 * Filter DTD). The DTD defines the properties type to be tested as PropertyName
 * or Literal.
 * 
 * @author Markus Schneider
 * @version 07.08.2002
 */
public class PropertyIsNullOperation extends ComparisonOperation
{

  // PropertyName / Literal
  private Expression expression;

  public PropertyIsNullOperation( Expression expression )
  {
    super( OperationDefines.PROPERTYISNULL );
    this.expression = expression;
  }

  public Expression getExpression()
  {
    return expression;
  }

  /**
   * Given a DOM-fragment, a corresponding Operation-object is built. This
   * method recursively calls other buildFromDOM () - methods to validate the
   * structure of the DOM-fragment.
   * 
   * @throws FilterConstructionException
   *           if the structure of the DOM-fragment is invalid
   */
  public static Operation buildFromDOM( Element element ) throws FilterConstructionException
  {

    // check if root element's name equals 'PropertyIsNull'
    if( !element.getLocalName().equals( "PropertyIsNull" ) )
      throw new FilterConstructionException( "Name of element does not equal 'PropertyIsNull'!" );

    ElementList children = XMLTools.getChildElements( element );
    if( children.getLength() != 1 )
      throw new FilterConstructionException( "'PropertyIsNull' requires exactly 1 element!" );

    Element child = children.item( 0 );
    Expression expr = null;

    switch( ExpressionDefines.getIdByName( child.getLocalName() ) )
    {
    case ExpressionDefines.PROPERTYNAME:
    {
      expr = PropertyName.buildFromDOM( child );
      break;
    }
    case ExpressionDefines.LITERAL:
    {
      expr = Literal.buildFromDOM( child );
      break;
    }
    default:
    {
      throw new FilterConstructionException( "Name of element does not equal 'PropertyIsNull'!" );
    }
    }

    return new PropertyIsNullOperation( expr );
  }

  /** Produces an indented XML representation of this object. */
  public StringBuffer toXML()
  {
    StringBuffer sb = new StringBuffer( 500 );
    sb.append( "<ogc:" ).append( getOperatorName() ).append( ">" );
    sb.append( expression.toXML() );
    sb.append( "</ogc:" ).append( getOperatorName() ).append( ">" );
    return sb;
  }

  /**
   * Calculates the <tt>PropertyIsNull</tt> -Operation's logical value based
   * on the certain property values of the given <tt>Feature</tt>.
   * 
   * @param feature
   *          that determines the property values
   * @return true, if the <tt>PropertyIsNull</tt> -Operation evaluates to
   *         true, else false
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  public boolean evaluate( Feature feature ) throws FilterEvaluationException
  {
    Object value = expression.evaluate( feature );
    if( value == null )
      return true;
    return false;
  }
}