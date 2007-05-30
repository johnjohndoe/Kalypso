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

import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;

import org.kalypso.contribs.java.xml.XMLUtilities;
import org.kalypsodeegree.filterencoding.Expression;
import org.kalypsodeegree.filterencoding.FilterConstructionException;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathException;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;
import org.w3c.dom.Element;

/**
 * Encapsulates the information of a <PropertyName>element as defined in the FeatureId DTD.
 * 
 * @author Markus Schneider
 * @version 07.08.2002
 */
public class PropertyName extends Expression_Impl
{
  /** The PropertyName's value (as an XPATH expression). */
  private final GMLXPath m_path;

  public PropertyName( final String xpath, final NamespaceContext namespaceContext )
  {
    this( new GMLXPath( xpath, namespaceContext ) );
  }

  /**
   * Constructs a new PropertyName.
   * 
   * @deprecated Use either {@link #PropertyName(QName)} ord {@link #PropertyName(String, NamespaceContext)} instead.
   */
  @Deprecated
  public PropertyName( final String xpath )
  {
    this( xpath, null );
  }

  public PropertyName( final QName qname )
  {
    this( new GMLXPath( qname ) );
  }

  public PropertyName( final GMLXPath path )
  {
    m_id = ExpressionDefines.PROPERTYNAME;

    m_path = path;
  }

  /**
   * Given a DOM-fragment, a corresponding Expression-object is built.
   * 
   * @throws FilterConstructionException
   *             if the structure of the DOM-fragment is invalid
   */
  public static Expression buildFromDOM( final Element element ) throws FilterConstructionException
  {
    // check if root element's name equals 'PropertyName'
    if( !element.getLocalName().toLowerCase().equals( "propertyname" ) )
    {
      throw new FilterConstructionException( "Name of element does not equal 'PropertyName'!" );
    }

    final String elementValue = XMLTools.getValue( element );
    final NamespaceContext namespaceContext = XMLUtilities.createNamespaceContext( element );
    return new PropertyName( elementValue, namespaceContext );
  }

  /**
   * Returns the last two parts of the XPATH-Expression in the format TABLENAME.VALUE.
   */
  public String getSQLFieldQualifier( )
  {
    // return m_value;
    throw new UnsupportedOperationException();
  }

  /**
   * Returns the PropertyName's value.
   */
  public String getValue( )
  {
    return m_path.toString();
  }

  /** Produces an indented XML representation of this object. */
  @Override
  public StringBuffer toXML( )
  {
    final StringBuffer sb = new StringBuffer( 200 );
    sb.append( "<ogc:PropertyName>" );
    sb.append( m_path.toString() );
    sb.append( "</ogc:PropertyName>" );
    return sb;
  }

  /**
   * Returns the <tt>PropertyName</tt>'s value (to be used in the evaluation of a complexer <tt>Expression</tt>).
   * If the value is a geometry, an instance of <tt>GM_Object</tt> is returned, if it appears to be numerical, a
   * <tt>Double</tt>, else a <tt>String</tt>.
   * <p>
   * TODO: Improve datatype handling.
   * <p>
   * 
   * @param feature
   *            that determines the value of this <tt>PropertyName</tt>
   * @return the resulting value
   */
  public Object evaluate( final Feature feature ) throws FilterEvaluationException
  {
    try
    {
      final Object object = GMLXPathUtilities.query( m_path, feature );
      if( object == null )
        return null;
      else if( object instanceof Number )
        return object;
      else if( object instanceof GM_Object )
        return object;
      else if( object instanceof Boolean )
        return object.toString();
      return FilterElementLabelProvider.toString( object );
    }
    catch( final GMLXPathException e )
    {
      final String msg = String.format( "Bad path: %s (%s)", m_path, e.getLocalizedMessage() );
      throw new FilterEvaluationException( msg );
    }
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return getValue();
  }
}