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
package org.kalypsodeegree_impl.gml;

import org.kalypsodeegree.gml.GMLException;
import org.kalypsodeegree.gml.GMLFeature;
import org.kalypsodeegree.gml.GMLFeatureProperty;
import org.kalypsodeegree.ogcbasic.CommonNamespaces;
import org.kalypsodeegree.xml.DOMPrinter;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.tools.Debug;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * 
 * 
 * <p>
 * ----------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 07.02.2001
 *          <p>
 */
public class GMLFeatureProperty_Impl extends GMLProperty_Impl implements GMLFeatureProperty
{
  /**
   * Creates a new GMLFeatureProperty_Impl object.
   * 
   * @param element
   */
  public GMLFeatureProperty_Impl( Element element )
  {
    super( element );
  }

  /**
   * factory method to create a GMLFeatureProperty. the property that will be return doesn't contain a value.
   */
  public static GMLFeatureProperty createGMLFeatureProperty( Document doc, String propName )
  {
    Debug.debugMethodBegin( "", "createGMLFeatureProperty(Document,String)" );

    if( !propName.startsWith( "gml:" ) )
    {
      propName = "gml:" + propName;
    }

    Element elem = doc.createElementNS( CommonNamespaces.GMLNS, propName );

    GMLFeatureProperty ls = new GMLFeatureProperty_Impl( elem );

    Debug.debugMethodEnd();
    return ls;
  }

  /**
   * factory method to create a GMLFeatureProperty. the property that will be return doesn't contain a value.
   */
  public static GMLFeatureProperty createGMLFeatureProperty( Document doc, String propName, GMLFeature feature )
      throws GMLException
  {
    Debug.debugMethodBegin( "", "createGMLFeatureProperty(Document, GMLFeature)" );

    GMLFeatureProperty ls = createGMLFeatureProperty( doc, propName );

    ls.setFeaturePropetryValue( feature );

    Debug.debugMethodEnd();
    return ls;
  }

  /**
   * returns the value of the property
   */
  public GMLFeature getFeaturePropertyValue()
  {
    Debug.debugMethodBegin( this, "getFeaturePropertyValue()" );

    GMLFeature result = null;

    Debug.debugMethodEnd();
    return result;
  }

  /**
   * @see org.kalypsodeegree_impl.gml.GMLFeatureProperty_Impl#getFeaturePropertyValue()
   */
  public void setFeaturePropetryValue( GMLFeature value )
  {
    Debug.debugMethodBegin();

    Element elem = ( (GMLFeature_Impl)value ).getAsElement();

    XMLTools.insertNodeInto( elem, m_element );

    Debug.debugMethodEnd();
  }

  /**
   * will be overwritten by this class in a way that an exception will be thrown if the method is called. Because it
   * doesn't make any sense to set the value of a geo property as a String.
   */
  public void setPropertyValue( String value )
  {
    throw new NoSuchMethodError( "It doesn't make any sense to set the value " + "of a feature property as a String" );
  }

  public String toString()
  {
    return DOMPrinter.nodeToString( m_element, "" );
  }
}