//$HeadURL: svn+ssh://rbezema@svn.wald.intevation.org/deegree/base/branches/2.2_testing/src/org/deegree/model/filterencoding/PropertyIsInstanceOfOperation.java $
/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001-2008 by:
 Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/deegree/
 lat/lon GmbH
 http://www.lat-lon.de

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

 Andreas Poth
 lat/lon GmbH
 Aennchenstraße 19
 53177 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Prof. Dr. Klaus Greve
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: greve@giub.uni-bonn.de

 ---------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.filterencoding;

import org.deegree.datatypes.QualifiedName;
import org.deegree.framework.xml.NamespaceContext;
import org.deegree.framework.xml.XMLParsingException;
import org.deegree.ogcbase.CommonNamespaces;
import org.kalypsodeegree.filterencoding.FilterConstructionException;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree.filterencoding.visitor.FilterVisitor;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_MultiPoint;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree.xml.ElementList;
import org.kalypsodeegree.xml.XMLTools;
import org.w3c.dom.Element;

/**
 * deegree-specific <code>ComparisonOperation</code> that allows to check the type of a property.
 * <p>
 * This is useful if the property has an abstract type with several concrete implementations, for example
 * 'gml:_Geometry'.
 * <p>
 * NOTE: Currently supported types to test are:
 * <ul>
 * <li>gml:Point</li>
 * <li>gml:_Curve</li>
 * <li>gml:_Surface</li>
 * </ul>
 * 
 * @author <a href="mailto:schneider@lat-lon.de">Markus Schneider</a>
 * @author last edited by: $Author: apoth $
 * @version $Revision: 9343 $, $Date: 2007-12-27 14:30:32 +0100 (Do, 27 Dez 2007) $
 */
public class PropertyIsInstanceOfOperation extends ComparisonOperation
{

  private final PropertyName propertyName;

  private final QualifiedName typeName;

  private static NamespaceContext nsContext = CommonNamespaces.getNamespaceContext();

  /**
   * Creates a new instance of <code>PropertyIsInstanceOfOperation</code>.
   * 
   * @param propertyName
   * @param typeName
   */
  public PropertyIsInstanceOfOperation( final PropertyName propertyName, final QualifiedName typeName )
  {
    super( OperationDefines.PROPERTYISINSTANCEOF );
    this.propertyName = propertyName;
    this.typeName = typeName;
  }

  /**
   * Produces an XML representation of this object.
   */
  public StringBuffer toXML( )
  {
    final StringBuffer sb = new StringBuffer();
    sb.append( "<deegreeogc:" ).append( getOperatorName() ).append( " xmlns:deegreeogc=\"http://www.deegree.org/ogc\">" );
    sb.append( propertyName.toXML() );
    sb.append( "<ogc:Literal>" ).append( typeName.getPrefixedName() ).append( "</ogc:Literal>" );
    sb.append( "</deegreeogc:" ).append( getOperatorName() ).append( ">" );
    return sb;
  }

  /**
   * Calculates the <code>Operation</code>'s logical value based on the certain property values of the given feature.
   * 
   * @param feature
   *          that determines the values of <code>PropertyNames</code> in the expression
   * @return true, if the <code>Operation</code> evaluates to true, else false
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  @SuppressWarnings("deprecation")
  public boolean evaluate( final Feature feature ) throws FilterEvaluationException
  {
    boolean equals = false;
    final Object propertyValue = feature.getProperty( propertyName.getValue() );

    if( CommonNamespaces.GMLNS.equals( this.typeName.getNamespace() ) )
    {
      final String localName = this.typeName.getLocalName();
      if( "Point".equals( localName ) )
      {
        equals = propertyValue instanceof GM_Point || propertyValue instanceof GM_MultiPoint;
      }
      else if( "_Curve".equals( localName ) )
      {
        equals = propertyValue instanceof GM_Curve || propertyValue instanceof GM_MultiCurve;
      }
      else if( "_Surface".equals( localName ) )
      {
        equals = propertyValue instanceof GM_Surface || propertyValue instanceof GM_MultiSurface || propertyValue instanceof GM_SurfacePatch;
      }
      else
      {
        final String msg = "Error evaluating PropertyIsInstanceOf operation: " + this.typeName + " is not a supported type to check for.";
        throw new FilterEvaluationException( msg );
      }
    }
    else
    {
      final String msg = "Error evaluating PropertyIsInstanceOf operation: " + this.typeName + " is not a supported type to check for.";
      throw new FilterEvaluationException( msg );
    }
    
    return equals;
  }

  /**
   * Given a DOM-fragment, a corresponding Operation-object is built. This method recursively calls other buildFromDOM
   * () - methods to validate the structure of the DOM-fragment.
   * 
   * @param element
   *          to build from
   * @return the Bean of the DOM
   * @throws FilterConstructionException
   *           if the structure of the DOM-fragment is invalid
   */
  public static Operation buildFromDOM( final Element element ) throws FilterConstructionException
  {
    // check if root element's name equals 'PropertyIsInstanceOf'
    if( !element.getLocalName().equals( "PropertyIsInstanceOf" ) )
      throw new FilterConstructionException( "Name of element does not equal 'PropertyIsInstanceOf'!" );

    final ElementList children = XMLTools.getChildElements( element );
    if( children.getLength() != 2 )
      throw new FilterConstructionException( "'PropertyIsInstanceOf' requires exactly 2 elements!" );

    final PropertyName propertyName = (PropertyName) PropertyName.buildFromDOM( children.item( 0 ) );
    QualifiedName typeName = null;

    try
    {
      typeName = org.deegree.framework.xml.XMLTools.getRequiredNodeAsQualifiedName( element, "ogc:Literal/text()", nsContext );
    }
    catch( final XMLParsingException e )
    {
      throw new FilterConstructionException( e.getMessage() );
    }

    return new PropertyIsInstanceOfOperation( propertyName, typeName );
  }

  /**
   * @return the propertyName of this Operation
   */
  public PropertyName getPropertyName( )
  {
    return propertyName;
  }

  /**
   * @see org.kalypsodeegree.filterencoding.Operation#accept(org.kalypsodeegree.filterencoding.visitor.FilterVisitor,
   *      org.kalypsodeegree.filterencoding.Operation, int)
   */
  @Override
  public void accept( final FilterVisitor fv, final Operation operation, final int depth )
  {
    fv.visit( this );
  }
}
