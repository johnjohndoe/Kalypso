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
package org.deegree_impl.gml;

import java.util.ArrayList;

import org.deegree.gml.GMLBox;
import org.deegree.gml.GMLCoord;
import org.deegree.gml.GMLFeature;
import org.deegree.gml.GMLFeatureCollection;
import org.deegree.ogcbasic.CommonNamespaces;
import org.deegree.xml.DOMPrinter;
import org.deegree.xml.XMLTools;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

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
public class GMLFeatureCollection_Impl extends GMLFeature_Impl implements GMLFeatureCollection
{
  /**
   * Creates a new GMLFeatureCollection_Impl object.
   * 
   * @param name
   */
  public GMLFeatureCollection_Impl( String name )
  {
    Document doc = XMLTools.create();
    Element elem = doc.createElement( name );
    elem.setAttribute( "xmlns:gml", CommonNamespaces.GMLNS );

    Element el = doc.createElementNS( CommonNamespaces.GMLNS, "gml:boundedBy" );
    elem.appendChild( el );
    doc.appendChild( elem );
    setElement( elem );
  }

  /**
   * Creates a new GMLFeatureCollection_Impl object.
   * 
   * @param name
   * @param prefix
   * @param nameSpace
   */
  public GMLFeatureCollection_Impl( String name, String prefix, String nameSpace )
  {
    Document doc = XMLTools.create();
    Element elem = doc.createElementNS( nameSpace, prefix + ":" + name );
    elem.setAttribute( "xmlns:" + prefix, nameSpace );

    if( !nameSpace.equals( CommonNamespaces.GMLNS ) )
    {
      elem.setAttribute( "xmlns:gml", CommonNamespaces.GMLNS );
    }

    Element el = doc.createElementNS( CommonNamespaces.GMLNS, "gml:boundedBy" );
    elem.appendChild( el );
    doc.appendChild( elem );
    setElement( elem );
  }

  /**
   * Creates a new GMLFeatureCollection_Impl object.
   * 
   * @param element
   */
  public GMLFeatureCollection_Impl( Element element )
  {
    super( element );
  }

  /**
   * 
   * 
   * @return
   */
  public Element getAsElement()
  {
    return element;
  }

  /**
   * returns all features of the collection
   */
  public GMLFeature[] getFeatures()
  {
    Debug.debugMethodBegin( this, "getFeatures" );

    ArrayList list = new ArrayList();

    NodeList nl = element.getChildNodes();

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      list.ensureCapacity( nl.getLength() );

      for( int i = 0; i < nl.getLength(); i++ )
      {
        if( nl.item( i ) instanceof Element )
        {
          Element elem = (Element)nl.item( i );

          // check if the current node is a feature member
          // if so get the first subnode for creating a feature
          if( isFeatureMember( elem ) )
          {
            list.add( new GMLFeature_Impl( XMLTools.getFirstElement( elem ) ) );
          }
        }
      }
    }

    Debug.debugMethodEnd();

    if( list.size() == 0 )
    {
      return null;
    }
    return (GMLFeature[])list.toArray( new GMLFeature[list.size()] );
  }

  /**
   * in a future release this check has to be performed against the schema
   */
  private boolean isFeatureMember( Node node )
  {
    // check if its gml definied property or a feature
    if( !XMLTools.toLocalName( node.getNodeName() ).equals( "name" )
        && !XMLTools.toLocalName( node.getNodeName() ).equals( "description" )
        && !XMLTools.toLocalName( node.getNodeName() ).equals( "boundedBy" ) )
    {
      if( !isProperty( node ) )
      {
        return true;
      }

      return false;
    }

    return false;
  }

  /**
   * returns the feature that matches the submitted id
   */
  public GMLFeature getFeature( String id )
  {
    Debug.debugMethodBegin( this, "getFeature" );

    GMLFeature feature = null;

    NodeList nl = element.getChildNodes();

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      for( int i = 0; i < nl.getLength(); i++ )
      {
        if( nl.item( i ) instanceof Element )
        {
          Element elem = (Element)nl.item( i );

          // check if the current node is a feature member
          // if so get the first subnode for creating a feature
          if( isFeatureMember( elem ) )
          {
            feature = new GMLFeature_Impl( XMLTools.getFirstElement( elem ) );

            if( feature.getId().equals( id ) )
            {
              break;
            }
          }
        }
      }
    }

    Debug.debugMethodEnd();

    return feature;
  }

  /**
   * returns the features that matvhes the submitted name
   */
  public GMLFeature[] getFeatures( String name )
  {
    Debug.debugMethodBegin( this, "getFeatures(String)" );

    ArrayList list = new ArrayList();

    NodeList nl = element.getChildNodes();

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      for( int i = 0; i < nl.getLength(); i++ )
      {
        Element elem = (Element)nl.item( i );

        // check if the current node is a feature member
        // if so get the first subnode for creating a feature
        if( isFeatureMember( elem ) )
        {
          GMLFeature feature = new GMLFeature_Impl( XMLTools.getFirstElement( elem ) );

          if( feature.getName().equals( name ) )
          {
            list.add( feature );
          }
        }
      }
    }

    Debug.debugMethodEnd();

    if( list.size() == 0 )
    {
      return null;
    }
    return (GMLFeature[])list.toArray( new GMLFeature[list.size()] );
  }

  /**
   * adds a feature to the collection
   */
  public void addFeature( GMLFeature feature )
  {
    Debug.debugMethodBegin( this, "addFeature" );

    //    	if ( feature.getId() != null ) {
    //    		removeFeature( feature.getId() );
    //    	}
    Element elem = element.getOwnerDocument().createElementNS( CommonNamespaces.GMLNS,
        "gml:featureMember" );
    element.appendChild( elem );

    Element el = ( (GMLFeature_Impl)feature ).getAsElement();
    XMLTools.insertNodeInto( el, elem );

    Debug.debugMethodEnd();
  }

  /**
   * removes a feature from the collection
   */
  public void removeFeature( String id )
  {
    Debug.debugMethodBegin( this, "removeFeature" );

    GMLFeature feature = null;

    NodeList nl = element.getChildNodes();

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      for( int i = 0; i < nl.getLength(); i++ )
      {
        if( nl.item( i ) instanceof Element )
        {
          Element elem = (Element)nl.item( i );

          // check if the current node is a feature member
          // if so get the first subnode for creating a feature
          if( isFeatureMember( elem ) )
          {
            feature = new GMLFeature_Impl( XMLTools.getFirstElement( elem ) );

            if( feature.getId().equals( id ) )
            {
              element.removeChild( elem );
              break;
            }
          }
        }
      }
    }

    Debug.debugMethodEnd();
  }

  /**
   * sets the bounding box of the feature collection
   */
  public void setBoundingBox( double minx, double miny, double maxx, double maxy )
  {
    Debug.debugMethodBegin( this, "setBoundingBox(double,double,double,double)" );

    GMLBox box = GMLBox_Impl.createGMLBox( element.getOwnerDocument() );

    GMLCoord min = GMLCoord_Impl.createGMLCoord( element.getOwnerDocument() );
    min.setCoord( minx, miny );
    box.setMin( min );

    GMLCoord max = GMLCoord_Impl.createGMLCoord( element.getOwnerDocument() );
    max.setCoord( maxx, maxy );
    box.setMax( max );

    setBoundingBox( box );

    Debug.debugMethodEnd();
  }

  /**
   * sets the bounding box of the feature collection
   */
  public void setBoundingBox( GMLBox box )
  {
    Debug.debugMethodBegin( this, "setBoundingBox(GMLBox)" );

    Element elem = XMLTools.getNamedChild( element, CommonNamespaces.GMLNS, "boundedBy" );
    Element el = XMLTools.getFirstElement( elem );

    if( el != null )
    {
      elem.removeChild( el );
    }

    elem.appendChild( ( (GMLBox_Impl)box ).getAsElement() );

    Debug.debugMethodEnd();
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

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.6  2005/02/08 18:43:59  belger
 * *** empty log message ***
 *
 * Revision 1.5  2005/01/18 12:50:42  doemming
 * *** empty log message ***
 *
 * Revision 1.4  2004/10/07 14:09:13  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:56:58  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31 13:03:30
 * doemming *** empty log message *** Revision 1.8 2004/07/09 07:16:56 poth no
 * message
 * 
 * Revision 1.7 2004/04/07 06:43:48 poth no message
 * 
 * Revision 1.6 2004/02/23 07:47:48 poth no message
 * 
 * Revision 1.5 2003/04/23 15:44:39 poth no message
 * 
 * Revision 1.4 2002/11/18 17:15:35 poth no message
 * 
 * Revision 1.3 2002/11/12 10:21:28 poth no message
 * 
 * Revision 1.2 2002/10/21 08:19:02 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:04 poth no message
 * 
 * Revision 1.9 2002/08/19 15:59:29 ap no message
 * 
 * Revision 1.8 2002/08/05 16:11:02 ap no message
 * 
 * Revision 1.7 2002/08/01 08:56:56 ap no message
 *  
 */
