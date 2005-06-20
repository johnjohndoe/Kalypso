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
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.xml.XMLTools;
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
  private String value;

  /** Constructs a new PropertyName. */
  public PropertyName( String value )
  {
    id = ExpressionDefines.PROPERTYNAME;
    setValue( value );
  }

  /**
   * Given a DOM-fragment, a corresponding Expression-object is built.
   * 
   * @throws FilterConstructionException
   *           if the structure of the DOM-fragment is invalid
   */
  public static Expression buildFromDOM( Element element ) throws FilterConstructionException
  {
    // check if root element's name equals 'PropertyName'
    if( !element.getLocalName().toLowerCase().equals( "propertyname" ) )
    {
      throw new FilterConstructionException( "Name of element does not equal 'PropertyName'!" );
    }

    return new PropertyName( XMLTools.getValue( element ) );
  }

  /**
   * Returns the last two parts of the XPATH-Expression in the format TABLENAME.VALUE.
   */
  public String getSQLFieldQualifier()
  {
    return value;
  }

  /**
   * Returns the PropertyName's value.
   */
  public String getValue()
  {
    return value;
  }

  /**
   * @see org.kalypsodeegree_impl.filterencoding.PropertyName#getValue()
   */
  public void setValue( String value )
  {
    this.value = value;

    if( value.startsWith( "/" ) )
    {
      this.value = value.substring( 1 );
    }
  }

  /** Produces an indented XML representation of this object. */
  public StringBuffer toXML()
  {
    StringBuffer sb = new StringBuffer( 200 );
    sb.append( "<ogc:PropertyName>" ).append( value ).append( "</ogc:PropertyName>" );
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
   *          that determines the value of this <tt>PropertyName</tt>
   * @return the resulting value
   * @throws FilterEvaluationException
   *           if the <Feature>has no <tt>Property</tt> with a matching name
   */
  public Object evaluate( Feature feature ) throws FilterEvaluationException
  {
    //        FeatureTypeProperty[] ftp = feature.getFeatureType().getProperties();
    Object object = getProperty( feature, value );

    //        if (feature.getFeatureType ().getProperty (value) == null) {
    //            throw new FilterEvaluationException (
    //                "FeatureType '" + feature.getFeatureType ().getName () +
    //                "' has no property with name '" + value + "'!");
    //        }
    //        Object object = feature.getProperty (value);
    if( object == null )
    {
      return null;
    }

    if( object instanceof Number )
    {
      return object;
    }

    return object.toString();
  }

  /**
   * Method getProperty
   * 
   * @param feature
   *          a Feature
   * @param value
   *          a String
   * 
   * @return an Object
   */
  private Object getProperty( Feature feature, String value ) throws FilterEvaluationException
  {
    if( feature.getFeatureType().getProperty( value ) != null )
    {
      return feature.getProperty( value );
    }
    throw new FilterEvaluationException( "FeatureType '" + feature.getFeatureType().getName()
        + "' has no property with name '" + value + "'!" );
  }
}