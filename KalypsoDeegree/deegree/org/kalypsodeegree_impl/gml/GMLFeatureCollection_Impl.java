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

import java.util.*;

import org.deegree.gml.*;
import org.deegree.xml.*;

import org.deegree_impl.tools.*;

import org.w3c.dom.*;


/**
 *
 *
 * <p>----------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 07.02.2001
 * <p>
 */
public class GMLFeatureCollection_Impl extends GMLFeature_Impl implements GMLFeatureCollection {
    /**
     * Creates a new GMLFeatureCollection_Impl object.
     *
     * @param name 
     */
    public GMLFeatureCollection_Impl( String name ) {
        Document doc = XMLTools.create();
        Element elem = doc.createElement( name );
        elem.setAttribute( "xmlns:gml", GMLGeometricMapping.GMLNS );

        Element el = doc.createElementNS( GMLGeometricMapping.GMLNS, "gml:boundedBy" );
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
    public GMLFeatureCollection_Impl( String name, String prefix, String nameSpace ) {
        Document doc = XMLTools.create();
        Element elem = doc.createElementNS( nameSpace, prefix + ":" + name );
        elem.setAttribute( "xmlns:" + prefix, nameSpace );

        if ( !nameSpace.equals( GMLGeometricMapping.GMLNS ) ) {
            elem.setAttribute( "xmlns:gml", GMLGeometricMapping.GMLNS );
        }

        Element el = doc.createElementNS( GMLGeometricMapping.GMLNS, "gml:boundedBy" );
        elem.appendChild( el );
        doc.appendChild( elem );
        setElement( elem );
    }

    /**
     * Creates a new GMLFeatureCollection_Impl object.
     *
     * @param element 
     */
    public GMLFeatureCollection_Impl( Element element ) {
        super( element );
    }

    /**
     *
     *
     * @return 
     */
    public Element getAsElement() {
        return element;
    }

    /**
     * creates a GMLFeatureCollection that doesn't contain a property and that
     * hasn't an id.
     */
    public static GMLFeatureCollection createGMLFeatureCollection( Document doc, 
                                                                   String collectionName ) {
        Debug.debugMethodBegin( "GMLFeatureCollection_Impl", "createGMLFeatureCollection" );

        Element elem = doc.createElement( collectionName );
        elem.setAttribute( "xmlns:gml", "http://www.opengis.net/gml" );

        Element el = doc.createElementNS( GMLGeometricMapping.GMLNS, "gml:boundedBy" );
        elem.appendChild( el );

        GMLFeatureCollection feature = new GMLFeatureCollection_Impl( elem );

        Debug.debugMethodEnd();
        return feature;
    }

    /**
     * returns all features of the collection
     */
    public GMLFeature[] getFeatures() {
        Debug.debugMethodBegin( this, "getFeatures" );

        ArrayList list = new ArrayList();

        NodeList nl = element.getChildNodes();

        if ( ( nl != null ) && ( nl.getLength() > 0 ) ) {
            list.ensureCapacity( nl.getLength() );

            for ( int i = 0; i < nl.getLength(); i++ ) {
                if ( nl.item( i ) instanceof Element ) {
                    Element elem = (Element)nl.item( i );

                    // check if the current node is a feature member
                    // if so get the first subnode for creating a feature
                    if ( isFeatureMember( elem ) ) {
                        list.add( new GMLFeature_Impl( XMLTools.getFirstElement( elem ) ) );
                    }
                }
            }
        }

        Debug.debugMethodEnd();

        if ( list.size() == 0 ) {
            return null;
        } else {
            return (GMLFeature[])list.toArray( new GMLFeature[list.size()] );
        }
    }

    /**
     * in a future release this check has to be performed against
     * the schema
     */
    private boolean isFeatureMember( Node node ) {
        // check if its gml definied property or a feature
        if ( !XMLTools.toLocalName( node.getNodeName() ).equals( "name" ) && 
                 !XMLTools.toLocalName( node.getNodeName() ).equals( "description" ) && 
                 !XMLTools.toLocalName( node.getNodeName() ).equals( "boundedBy" ) ) {
            if ( !isProperty( node ) ) {
                return true;
            }
	    
            return false;
	}
	
        return false;
    }

    /**
     * returns the feature that matches the submitted id
     */
    public GMLFeature getFeature( String id ) {
        Debug.debugMethodBegin( this, "getFeature" );

        GMLFeature feature = null;

        NodeList nl = element.getChildNodes();

        if ( ( nl != null ) && ( nl.getLength() > 0 ) ) {
            for ( int i = 0; i < nl.getLength(); i++ ) {
                if ( nl.item( i ) instanceof Element ) {
                    Element elem = (Element)nl.item( i );

                    // check if the current node is a feature member
                    // if so get the first subnode for creating a feature
                    if ( isFeatureMember( elem ) ) {
                        feature = new GMLFeature_Impl( XMLTools.getFirstElement( elem ) );

                        if ( feature.getId().equals( id ) ) {
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
    public GMLFeature[] getFeatures( String name ) {
        Debug.debugMethodBegin( this, "getFeatures(String)" );

        ArrayList list = new ArrayList();

        NodeList nl = element.getChildNodes();

        if ( ( nl != null ) && ( nl.getLength() > 0 ) ) {
            for ( int i = 0; i < nl.getLength(); i++ ) {
                Element elem = (Element)nl.item( i );

                // check if the current node is a feature member
                // if so get the first subnode for creating a feature
                if ( isFeatureMember( elem ) ) {
                    GMLFeature feature = new GMLFeature_Impl( XMLTools.getFirstElement( elem ) );

                    if ( feature.getName().equals( name ) ) {
                        list.add( feature );
                    }
                }
            }
        }

        Debug.debugMethodEnd();

        if ( list.size() == 0 ) {
            return null;
        } else {
            return (GMLFeature[])list.toArray( new GMLFeature[list.size()] );
        }
    }

    /**
     * adds a feature to the collection
     */
    public void addFeature( GMLFeature feature ) {
        Debug.debugMethodBegin( this, "addFeature" );

        //    	if ( feature.getId() != null ) {
        //    		removeFeature( feature.getId() );
        //    	}
     
        Element elem = element.getOwnerDocument()
                              .createElementNS( GMLGeometricMapping.GMLNS, "gml:featureMember" );
        element.appendChild( elem );

        Element el = ( (GMLFeature_Impl)feature ).getAsElement();
        XMLTools.insertNodeInto( el, elem );

        Debug.debugMethodEnd();
    }

    /**
     * removes a feature from the collection
     */
    public void removeFeature( String id ) {
        Debug.debugMethodBegin( this, "removeFeature" );

        GMLFeature feature = null;

        NodeList nl = element.getChildNodes();

        if ( ( nl != null ) && ( nl.getLength() > 0 ) ) {
            for ( int i = 0; i < nl.getLength(); i++ ) {
                if ( nl.item( i ) instanceof Element ) {
                    Element elem = (Element)nl.item( i );

                    // check if the current node is a feature member
                    // if so get the first subnode for creating a feature
                    if ( isFeatureMember( elem ) ) {
                        feature = new GMLFeature_Impl( XMLTools.getFirstElement( elem ) );

                        if ( feature.getId().equals( id ) ) {
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
    public void setBoundingBox( double minx, double miny, double maxx, double maxy ) {
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
    public void setBoundingBox( GMLBox box ) {
        Debug.debugMethodBegin( this, "setBoundingBox(GMLBox)" );

        Element elem = XMLTools.getNamedChild( element, GMLGeometricMapping.GMLNS, "boundedBy" );
        Element el = XMLTools.getFirstElement( elem );

        if ( el != null ) {
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
    public String toString() {
        return DOMPrinter.nodeToString( element, "" );
    }
}

/*
 * Changes to this class. What the people haven been up to:
 *
 * $Log$
 * Revision 1.2  2004/08/11 11:20:16  doemming
 * *** empty log message ***
 *
 * Revision 1.1.1.1  2004/05/11 16:43:24  doemming
 * backup of local modified deegree sources
 *
 * Revision 1.6  2004/02/23 07:47:48  poth
 * no message
 *
 * Revision 1.5  2003/04/23 15:44:39  poth
 * no message
 *
 * Revision 1.4  2002/11/18 17:15:35  poth
 * no message
 *
 * Revision 1.3  2002/11/12 10:21:28  poth
 * no message
 *
 * Revision 1.2  2002/10/21 08:19:02  poth
 * no message
 *
 * Revision 1.1.1.1  2002/09/25 16:01:04  poth
 * no message
 *
 * Revision 1.9  2002/08/19 15:59:29  ap
 * no message
 *
 * Revision 1.8  2002/08/05 16:11:02  ap
 * no message
 *
 * Revision 1.7  2002/08/01 08:56:56  ap
 * no message
 *
 */
