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
package org.deegree_impl.filterencoding;

import org.deegree.filterencoding.Expression;
import org.deegree.filterencoding.FilterConstructionException;
import org.deegree.filterencoding.FilterEvaluationException;
import org.deegree.filterencoding.Operation;
import org.deegree.model.feature.Feature;
import org.deegree.xml.ElementList;
import org.deegree.xml.XMLTools;
import org.w3c.dom.Element;

/**
 * Encapsulates the information of a <PropertyIsBetween>-element (as defined in
 * Filter DTD).
 * 
 * @author Markus Schneider
 * @version 07.08.2002
 */
public class PropertyIsBetweenOperation extends ComparisonOperation
{

  private PropertyName propertyName;

  private Expression lowerBoundary;

  private Expression upperBoundary;

  public PropertyIsBetweenOperation( PropertyName propertyName, Expression lowerBoundary,
      Expression upperBoundary )
  {
    super( OperationDefines.PROPERTYISBETWEEN );
    this.propertyName = propertyName;
    this.lowerBoundary = lowerBoundary;
    this.upperBoundary = upperBoundary;
  }

  /**
   * Given a DOM-fragment, a corresponding Operation-object is built. This
   * method recursively calls other buildFromDOM () - methods to validate the
   * structure of the DOM-fragment.
   * 
   * @throws FilterConstructionException
   *                   if the structure of the DOM-fragment is invalid
   */
  public static Operation buildFromDOM( Element element ) throws FilterConstructionException
  {

    // check if root element's name equals 'PropertyIsBetween'
    if( !element.getLocalName().equals( "PropertyIsBetween" ) )
      throw new FilterConstructionException( "Name of element does not equal 'PropertyIsBetween'!" );

    ElementList children = XMLTools.getChildElements( element );
    if( children.getLength() != 3 )
      throw new FilterConstructionException( "'PropertyIsBetween' requires exactly 3 elements!" );

    PropertyName propertyName = (PropertyName)PropertyName.buildFromDOM( children.item( 0 ) );
    Expression lowerBoundary = buildLowerBoundaryFromDOM( children.item( 1 ) );
    Expression upperBoundary = buildUpperBoundaryFromDOM( children.item( 2 ) );

    return new PropertyIsBetweenOperation( propertyName, lowerBoundary, upperBoundary );
  }

  /**
   * Given a DOM-fragment, a corresponding Expression-object (for the
   * LowerBoundary-element) is built. This method recursively calls other
   * buildFromDOM () - methods to validate the structure of the DOM-fragment.
   * 
   * @throws FilterConstructionException
   *                   if the structure of the DOM-fragment is invalid
   */
  private static Expression buildLowerBoundaryFromDOM( Element element )
      throws FilterConstructionException
  {

    // check if root element's name equals 'LowerBoundary'
    if( !element.getLocalName().equals( "LowerBoundary" ) )
      throw new FilterConstructionException( "Name of element does not equal 'LowerBoundary'!" );

    ElementList children = XMLTools.getChildElements( element );

    if( children.getLength() != 1 )
    {
      if( element.getChildNodes().getLength() == 1 )
        return new BoundaryExpression( XMLTools.getStringValue( element ) );
      throw new FilterConstructionException( "'LowerBoundary' requires exactly 1 element!" );
    }
    return Expression_Impl.buildFromDOM( children.item( 0 ) );
  }

  /**
   * Given a DOM-fragment, a corresponding Expression-object (for the
   * UpperBoundary-element) is built. This method recursively calls other
   * buildFromDOM () - methods to validate the structure of the DOM-fragment.
   * 
   * @throws FilterConstructionException
   *                   if the structure of the DOM-fragment is invalid
   */
  private static Expression buildUpperBoundaryFromDOM( Element element )
      throws FilterConstructionException
  {

    // check if root element's name equals 'UpperBoundary'
    if( !element.getLocalName().equals( "UpperBoundary" ) )
      throw new FilterConstructionException( "Name of element does not equal 'UpperBoundary'!" );

    ElementList children = XMLTools.getChildElements( element );
    if( children.getLength() != 1 )
    {
      if( element.getChildNodes().getLength() == 1 )
        return new BoundaryExpression( XMLTools.getStringValue( element ) );
      throw new FilterConstructionException( "'UpperBoundary' requires exactly 1 element!" );
    }
    return Expression_Impl.buildFromDOM( children.item( 0 ) );
  }

  /**
   * returns the name of the property that shall be compared to the boundaries
   */
  public PropertyName getPropertyName()
  {
    return propertyName;
  }

  /**
   * returns the lower boundary of the operation as an <tt>Expression</tt>
   */
  public Expression getLowerBoundary()
  {
    return lowerBoundary;
  }

  /**
   * returns the upper boundary of the operation as an <tt>Expression</tt>
   */
  public Expression getUpperBoundary()
  {
    return upperBoundary;
  }

  /** Produces an indented XML representation of this object. */
  public StringBuffer toXML()
  {
    StringBuffer sb = new StringBuffer( 500 );
    sb.append( "<ogc:" ).append( getOperatorName() ).append( ">" );
    sb.append( propertyName.toXML() );
    sb.append( "<ogc:LowerBoundary>" );
    sb.append( lowerBoundary.toXML() );
    sb.append( "</ogc:LowerBoundary>" );
    sb.append( "<ogc:UpperBoundary>" );
    sb.append( upperBoundary.toXML() );
    sb.append( "</ogc:UpperBoundary>" );
    sb.append( "</ogc:" ).append( getOperatorName() ).append( ">" );
    return sb;
  }  

  /**
   * Calculates the <tt>PropertyIsBetween</tt> -Operation's logical value
   * based on the certain property values of the given <tt>Feature</tt>.
   * TODO: Improve datatype handling.
   * 
   * @param feature
   *                   that determines the property values
   * @return true, if the <tt>Operation</tt> evaluates to true, else false
   * @throws FilterEvaluationException
   *                   if the evaluation fails
   */
  public boolean evaluate( Feature feature ) throws FilterEvaluationException
  {

    Object lowerValue = lowerBoundary.evaluate( feature );
    Object upperValue = upperBoundary.evaluate( feature );
    Object thisValue = propertyName.evaluate( feature );

    if( !( lowerValue instanceof Number && upperValue instanceof Number && thisValue instanceof Number ) )
      throw new FilterEvaluationException(
          "PropertyIsBetweenOperation can only be applied to numerical " + "expressions!" );

    double d1 = ( (Number)lowerValue ).doubleValue();
    double d2 = ( (Number)upperValue ).doubleValue();
    double d3 = ( (Number)thisValue ).doubleValue();
    return d1 <= d3 && d3 <= d2;

  }
}