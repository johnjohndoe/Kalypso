/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
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
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.gml;

import org.deegree.gml.GMLException;
import org.deegree.gml.GMLFeature;
import org.deegree.gml.GMLFeatureProperty;
import org.deegree.xml.DOMPrinter;
import org.deegree.xml.XMLTools;
import org.deegree_impl.tools.Debug;
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
   * factory method to create a GMLFeatureProperty. the property that will be
   * return doesn't contain a value.
   */
  public static GMLFeatureProperty createGMLFeatureProperty( Document doc, String propName )
      throws GMLException
  {
    Debug.debugMethodBegin( "", "createGMLFeatureProperty(Document,String)" );

    if( !propName.startsWith( "gml:" ) )
    {
      propName = "gml:" + propName;
    }

    Element elem = doc.createElementNS( GMLGeometricMapping.GMLNS, propName );

    GMLFeatureProperty ls = new GMLFeatureProperty_Impl( elem );

    Debug.debugMethodEnd();
    return ls;
  }

  /**
   * factory method to create a GMLFeatureProperty. the property that will be
   * return doesn't contain a value.
   */
  public static GMLFeatureProperty createGMLFeatureProperty( Document doc, String propName,
      GMLFeature feature ) throws GMLException
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
   * @see org.deegree_impl.gml.GMLFeatureProperty_Impl#getFeaturePropertyValue()
   */
  public void setFeaturePropetryValue( GMLFeature value ) throws GMLException
  {
    Debug.debugMethodBegin();

    Element elem = ( (GMLFeature_Impl)value ).getAsElement();

    XMLTools.insertNodeInto( elem, element );

    Debug.debugMethodEnd();
  }

  /**
   * will be overwritten by this class in a way that an exception will be thrown
   * if the method is called. Because it doesn't make any sense to set the value
   * of a geo property as a String.
   */
  public void setPropertyValue( String value )
  {
    throw new NoSuchMethodError( "It doesn't make any sense to set the value "
        + "of a feature property as a String" );
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    return DOMPrinter.nodeToString( element, "" );
  }
}