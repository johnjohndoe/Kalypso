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
package org.kalypsodeegree_impl.graphics.sld;

import org.kalypsodeegree.graphics.sld.Extent;
import org.kalypsodeegree.xml.Marshallable;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * 
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version $Revision$ $Date$
 */
public class Extent_Impl implements Extent, Marshallable
{
  private String name = null;

  private String value = null;

  /**
   * constructor initializing the class with the <Extent>
   */
  Extent_Impl( String value, String name )
  {
    setName( name );
    setValue( value );
  }

  /**
   * returns the name of the extent
   * 
   * @return the name of the extent
   */
  public String getName()
  {
    return name;
  }

  /**
   * sets the name of the extent
   * 
   * @param name
   *          the name of the extent
   */
  public void setName( String name )
  {
    this.name = name;
  }

  /**
   * returns the value of the extent
   * 
   * @return the value of the extent
   */
  public String getValue()
  {
    return value;
  }

  /**
   * sets the value of the extent
   * 
   * @param value
   *          the value of the extent
   */
  public void setValue( String value )
  {
    this.value = value;
  }

  /**
   * exports the content of the FeatureTypeConstraint as XML formated String
   * 
   * @return xml representation of the FeatureTypeConstraint
   */
  public String exportAsXML()
  {
    Debug.debugMethodBegin();

    StringBuffer sb = new StringBuffer( 1000 );
    sb.append( "<Extent>" );
    sb.append( "<Name>" ).append( name ).append( "</Name>" );
    sb.append( "<Value>" ).append( name ).append( "</Value>" );
    sb.append( "</Extent>" );

    Debug.debugMethodEnd();
    return sb.toString();
  }

}