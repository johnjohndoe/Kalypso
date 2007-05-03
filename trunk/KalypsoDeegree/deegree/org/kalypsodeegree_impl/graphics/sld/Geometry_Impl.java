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
package org.kalypsodeegree_impl.graphics.sld;

import org.kalypsodeegree.graphics.sld.Geometry;
import org.kalypsodeegree.xml.Marshallable;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * The Geometry element is optional and if it is absent then the default geometry property of the feature type that is
 * used in the containing FeatureStyleType is used. The precise meaning of default geometry property is
 * system-dependent. Most frequently, feature types will have only a single geometry property.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version $Revision$ $Date$
 */
public class Geometry_Impl implements Geometry, Marshallable
{
  private String m_propertyName = null;

  /**
   * constructor initializing the class with the <Geometry>
   */
  public Geometry_Impl( String propertyName )
  {
    setPropertyName( propertyName );
  }

  /**
   * returns the name of the geometry property
   * 
   * @return the name of the geometry property
   */
  public String getPropertyName( )
  {
    return m_propertyName;
  }

  /**
   * sets the name of the geometry property
   * 
   * @param propertyName
   *          the name of the geometry property
   */
  public void setPropertyName( String propertyName )
  {
    this.m_propertyName = propertyName;
  }

  // /**
  // * In principle, a fixed geometry could be defined using GML or operators could be defined for computing a geometry
  // * from references or literals. This enbales the calling client to submitt the geometry to be rendered by the WMS
  // * directly. (This is not part of the SLD XML-schema)
  // *
  // * @return the GMLGeometry
  // */
  // public GMLGeometry getGeometryAsGML()
  // {
  // return geometryAsGML;
  // }

  /**
   * // * sets the <GMLGeometry> // * // *
   * 
   * @param geometryAsGML // *
   *          the GMLGeometry //
   */
  // public void setGeometryAsGML( GMLGeometry geometryAsGML )
  // {
  // this.geometryAsGML = geometryAsGML;
  // }
  /**
   * exports the content of the Geometry as XML formated String
   * 
   * @return xml representation of the Geometry
   */
  public String exportAsXML( )
  {
    Debug.debugMethodBegin();

    StringBuffer sb = new StringBuffer( 1000 );
    if( m_propertyName != null && !m_propertyName.equals( "" ) )
    {
      sb.append( "<Geometry>" );
      sb.append( "<ogc:PropertyName>" ).append( m_propertyName );
      sb.append( "</ogc:PropertyName>" );
      sb.append( "</Geometry>" );
    }
    // else
    // {
    // String s = DOMPrinter.nodeToString( ((GMLGeometry_Impl) geometryAsGML).getAsElement(), "UTF-8" );
    // sb.append( s );
    // }

    Debug.debugMethodEnd();
    return sb.toString();
  }
}