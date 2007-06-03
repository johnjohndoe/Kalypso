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
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.xml.XMLTools;
import org.w3c.dom.Element;

/**
 * Encapsulates the information of a <Literal>element as defined in the FeatureId DTD.
 * 
 * @author Markus Schneider
 * @version 07.08.2002
 */
public class Literal extends Expression_Impl
{

  /** The literal's value. */
  String m_value;

  /** Constructs a new Literal. */
  public Literal( final String value )
  {
    m_id = ExpressionDefines.LITERAL;
    m_value = value;
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
    final String localNameFrom = element.getLocalName();

    // check if root element's name equals 'Literal'
    if( !localNameFrom.equals( "Literal" ) )
      throw new FilterConstructionException( "Name of element does not equal 'Literal'!" );

    return new Literal( XMLTools.getValue( element ) );
  }

  /**
   * Returns the literal's value (as String).
   */
  public String getValue( )
  {
    return m_value;
  }

  /**
   * @see org.kalypsodeegree_impl.filterencoding.Literal#getValue()
   */
  public void setValue( final String value )
  {
    this.m_value = value;
  }

  /** Produces an indented XML representation of this object. */
  @Override
  public StringBuffer toXML( )
  {
    final StringBuffer sb = new StringBuffer( 200 );
    sb.append( "<ogc:Literal>" ).append( m_value ).append( "</ogc:Literal>" );
    return sb;
  }

  /**
   * Returns the <tt>Literal</tt>'s value (to be used in the evaluation of a complexer <tt>Expression</tt>). If
   * the value appears to be numerical, a <tt>Double</tt> is returned, else a <tt>String</tt>. TODO: Improve
   * datatype handling.
   * 
   * @param feature
   *            that determines the values of <tt>PropertyNames</tt> in the expression (no use here)
   * @return the resulting value
   */
  public Object evaluate( final Feature feature )
  {
    try
    {
      return new Double( m_value );
    }
    catch( final NumberFormatException e )
    {
      //  
    }
    return m_value;
  }
}