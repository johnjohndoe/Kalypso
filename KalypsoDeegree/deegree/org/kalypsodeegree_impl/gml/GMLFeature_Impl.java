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
public class GMLFeature_Impl implements GMLFeature {
    protected Element element = null;

    /**
     * Creates a new GMLFeature_Impl object.
     */
    protected GMLFeature_Impl() {
    }

    /**
     * Creates a new GMLFeature_Impl object.
     *
     * @param element 
     */
    public GMLFeature_Impl( Element element ) {
        setElement( element );
    }

    /**
     *
     *
     * @param element 
     */
    protected void setElement( Element element ) {
        this.element = element;
    }

    /**
     * creates a GMLFeature that doesn't contain a property and that
     * hasn't an id.
     */
    public static GMLFeature createGMLFeature( Document doc, String featureTypeName ) {
        Debug.debugMethodBegin( "", "createGMLFeature(Document, String)" );

        Element elem = doc.createElement( featureTypeName );
        GMLFeature feature = new GMLFeature_Impl( elem );

        Debug.debugMethodEnd();
        return feature;
    }

    /**
     * creates a GMLFeature.
     */
    public static GMLFeature createGMLFeature( Document doc, String featureTypeName, String id, 
                                               GMLProperty[] properties ) throws GMLException {
        Debug.debugMethodBegin( "", "createGMLFeature(Document, String, String, GMLProperty[])" );

        Element elem = doc.createElement( featureTypeName );
        GMLFeature feature = new GMLFeature_Impl( elem );
        feature.setId( id );

        for ( int i = 0; i < properties.length; i++ ) {
            feature.addProperty( properties[i] );
        }

        Debug.debugMethodEnd();
        return feature;
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
     * returns the ID of the feature
     */
    public String getId() {
        Debug.debugMethodBegin( this, "getId" );
        
        String s = XMLTools.getAttrValue( element, "fid" );     
        if ( s != null ) {
            s = s.replace(' ','_');
        } else {
            s ="";
        }
        Debug.debugMethodEnd();
        return s;
    }

    /**
     * @see org.deegree_impl.gml.GMLFeature_Impl#getId()
     */
    public void setId( String id ) {
        Debug.debugMethodBegin();
        try {
            Double.parseDouble( id );
            element.setAttribute( "fid", "ID" + id);
        } catch (Exception e) {
            element.setAttribute( "fid", id.replace(' ', '_') );
        }        
        Debug.debugMethodEnd();
    }

    /**
     * returns the description of the feature
     */
    public String getDescription() {
        Debug.debugMethodBegin( this, "getDescription" );

        String description = null;

        NodeList nl = element.getElementsByTagName( "gml:description" );

        if ( ( nl != null ) && ( nl.getLength() > 0 ) ) {
            Element elem = (Element)nl.item( 0 );
            description = elem.getFirstChild().getNodeValue();
        }

        Debug.debugMethodEnd();
        return description;
    }

    /**
     * @see org.deegree_impl.gml.GMLFeature_Impl#getDescription()
     */
    public void setDescription( String description ) {
        Debug.debugMethodBegin( );

        NodeList nl = element.getElementsByTagName( "gml:description" );

        // remove description if already exists
        if ( ( nl != null ) && ( nl.getLength() > 0 ) ) {
            element.removeChild( nl.item( 0 ) );
        }

        // create new description
        Element elem = element.getOwnerDocument()
                              .createElementNS( GMLGeometricMapping.GMLNS, "gml:description" );
        Text text = element.getOwnerDocument().createTextNode( description );
        elem.appendChild( text );
        element.appendChild( elem );

        Debug.debugMethodEnd();
    }

    /**
     * returns the name of the Geometry.
     */
    public String getName() {
        Debug.debugMethodBegin( this, "getName" );

        String name = element.getTagName();

        NodeList nl = element.getElementsByTagName( "gml:name" );

        if ( ( nl != null ) && ( nl.getLength() > 0 ) ) {
            Element elem = (Element)nl.item( 0 );
            name = elem.getFirstChild().getNodeValue();
        }

        Debug.debugMethodEnd();
        return name;
    }

    /**
     * @see org.deegree_impl.gml.GMLFeature_Impl#getName()
     */
    public void setName( String name ) {
        Debug.debugMethodBegin( );

        NodeList nl = element.getElementsByTagName( "gml:name" );

        // remove name if already exists
        if ( ( nl != null ) && ( nl.getLength() > 0 ) ) {
            element.removeChild( nl.item( 0 ) );
        }

        // create new description
        Element elem = element.getOwnerDocument()
                              .createElementNS( GMLGeometricMapping.GMLNS, "gml:name" );
        Text text = element.getOwnerDocument().createTextNode( name );
        elem.appendChild( text );
        element.appendChild( elem );

        Debug.debugMethodEnd();
    }

    /**
     * return the name of the feature type the feature based on
     */
    public String getFeatureTypeName() {
        return element.getNodeName();
    }

    /**
     * returns the boundingbox of the feature
     */
    public GMLBox getBoundedBy() {
        Debug.debugMethodBegin( this, "getBoundedBy" );

        GMLBox box = null;

        NodeList nl = element.getElementsByTagNameNS( GMLGeometricMapping.GMLNS, "boundedBy" );

        if ( ( nl != null ) && ( nl.getLength() > 0 ) ) {
            Element elem = (Element)nl.item( 0 );            
			box = new GMLBox_Impl( XMLTools.getFirstElement( elem ) );
        }

        Debug.debugMethodEnd();
        return box;
    }

    /**
     * returns all properties of the feature
     */
    public GMLProperty[] getProperties() {
        Debug.debugMethodBegin( this, "getProperties" );

        ArrayList list = new ArrayList();

        NodeList nl = element.getChildNodes();

        if ( ( nl != null ) && ( nl.getLength() > 0 ) ) {
            for ( int i = 0; i < nl.getLength(); i++ ) {
                if ( nl.item( i ) instanceof Element ) {
                    Element elem = (Element)nl.item( i );

                    // make sure that's a property node
                    if ( !XMLTools.toLocalName( elem.getNodeName() ).equals( "name" ) && 
                         !XMLTools.toLocalName( elem.getNodeName() ).equals( "description" ) && 
                         !XMLTools.toLocalName( elem.getNodeName() ).equals( "boundedBy" ) ) {
                        //is it a geometry property
                        if ( isGeometryProperty( elem ) ) {
                            list.add( new GMLGeoProperty_Impl( elem ) );
                        } else if ( isComplexProperty( elem ) ) {
                            list.add( new GMLComplexProperty_Impl( elem ) );
                        } else {
                            list.add( new GMLProperty_Impl( elem ) );
                        }
                    }
                }
            }
        }

        Debug.debugMethodEnd();

        return (GMLProperty_Impl[])list.toArray( new GMLProperty_Impl[list.size()] );
    }

    /**
     * returns true if the submitted element contains just excatly one
     * childelement that have to be a geometry
     */
    private boolean isGeometryProperty( Element elem ) {
        Debug.debugMethodBegin( this, "isChildAGeometry" );

        boolean flag = false;
        NodeList nl = elem.getChildNodes();

        if ( nl != null ) {
            for ( int i = 0; i < nl.getLength(); i++ ) {
                if ( nl.item( i ) instanceof Element ) {
                    Element el = (Element)nl.item( i );
                    String name = XMLTools.toLocalName( el.getNodeName() );

                    if ( name.equals( "Point" ) || name.equals( "LineString" ) || 
                         name.equals( "Polygon" ) || name.equals( "MultiPoint" ) || 
                         name.equals( "MultiLineString" ) || name.equals( "MultiPolygon" ) || 
                         name.equals( "Box" ) || name.equals( "MultiGeometry" ) ) {
                        flag = true;
                    } else {
                        flag = false;
                        break;
                    }
                }
            }
        }

        Debug.debugMethodEnd();
        return flag;
    }

    /**
     * returns alls properties that are a GMLGeometry
     */
    public GMLGeoProperty[] getGeoProperties() {
        Debug.debugMethodBegin( this, "getGeoProperties" );

        ArrayList list = new ArrayList();

        NodeList nl = element.getChildNodes();

        if ( ( nl != null ) && ( nl.getLength() > 0 ) ) {
            for ( int i = 0; i < nl.getLength(); i++ ) {
                if ( nl.item( i ) instanceof Element ) {
                    Element elem = (Element)nl.item( i );

                    // make sure that's a property node
                    if ( !XMLTools.toLocalName( elem.getNodeName() ).equals( "name" ) && 
                         !XMLTools.toLocalName( elem.getNodeName() ).equals( "description" ) && 
                         !XMLTools.toLocalName( elem.getNodeName() ).equals( "boundedBy" ) ) {
                        //is it a geometry property
                        if ( isGeometryProperty( elem ) ) {
                            list.add( new GMLGeoProperty_Impl( elem ) );
                        }
                    }
                }
            }
        }

        Debug.debugMethodEnd();

        return (GMLGeoProperty_Impl[])list.toArray( new GMLGeoProperty_Impl[list.size()] );
    }

    /**
     * returns alls properties that are not a GMLGeometry
     */
    public GMLProperty[] getNoneGeoProperties() {
        Debug.debugMethodBegin( this, "getNoneGeoProperties" );

        ArrayList list = new ArrayList();

        NodeList nl = element.getChildNodes();

        if ( ( nl != null ) && ( nl.getLength() > 0 ) ) {
            for ( int i = 0; i < nl.getLength(); i++ ) {
                if ( nl.item( i ) instanceof Element ) {
                    Element elem = (Element)nl.item( i );

                    // make sure that's a property node
                    if ( !XMLTools.toLocalName( elem.getNodeName() ).equals( "name" ) && 
                         !XMLTools.toLocalName( elem.getNodeName() ).equals( "description" ) && 
                         !XMLTools.toLocalName( elem.getNodeName() ).equals( "boundedBy" ) ) {
                        // if it's not a geometry property
                        if ( isProperty( elem ) ) {
                            if ( !isGeometryProperty( elem ) ) {
                                list.add( new GMLProperty_Impl( elem ) );
                            }
                        } else {
                            if ( isComplexProperty( elem ) ) {
                                if ( !isGeometryProperty( elem ) ) {
                                    list.add( new GMLComplexProperty_Impl( elem ) );
                                }
                            }
                        }
                    }
                }
            }
        }

        Debug.debugMethodEnd();

        if ( list.size() == 0 ) {
            return null;
        } else {
            return (GMLProperty[])list.toArray( new GMLProperty[list.size()] );
        }
    }

    /**
     * returns a specific property identified by its name
     */
    public GMLProperty getProperty( String name ) {
        Debug.debugMethodBegin( this, "getProperty" );

        GMLProperty prop = null;

        NodeList nl = element.getChildNodes();

        if ( ( nl != null ) && ( nl.getLength() > 0 ) ) {
            for ( int i = 0; i < nl.getLength(); i++ ) {
                if ( nl.item( i ) instanceof Element ) {
                    Element elem = (Element)nl.item( i );

                    // check if the properties name is equal to the
                    // submitted name
                    if ( elem.getNodeName().equals( name ) ) {
                        //is it a geometry property
                        if ( GMLGeometricMapping.getFormalName( elem.getNodeName() ) != null ) {
                            prop = new GMLGeoProperty_Impl( elem );
                            break;
                        } else if ( isComplexProperty( elem ) ) {
                            prop = new GMLComplexProperty_Impl( elem );
                        } else {
                            prop = new GMLProperty_Impl( elem );
                            break;
                        }
                    }
                }
            }
        }

        Debug.debugMethodEnd();

        return prop;
    }

    /**
     * adds a property to the feature. <p>
     * This method should throw an exection if the submitted node
     * doesn't belong to the underlying Feature Type definition.
     * At the moment validiation against the feature type.
     */
    public void addProperty( GMLProperty property ) throws GMLException {
        Debug.debugMethodBegin( this, "addProperty" );

        //    	NodeList nl = element.getElementsByTagName( property.getName() );
        //
        //    	// remove property if it alread exists
        //    	if (nl != null && nl.getLength() > 0) {
        //    		element.removeChild( nl.item(0) );
        //    	}
        //
        // insert new property
        XMLTools.insertNodeInto( ( (GMLProperty_Impl)property ).getAsElement(), element );

        Debug.debugMethodEnd();
    }

    /**
     * returns true if the submitted node represents a property.
     * in a future release this check has to be performed against
     * the schema
     */
    protected boolean isProperty( Node node ) {
        if ( ( node.getFirstChild() == null ) ) {
            return true;
        }

        if ( node.getFirstChild() instanceof Text ) {
            String s = node.getFirstChild().getNodeValue();

            if ( s != null ) {
                s = s.trim();
            }

            return s.length() > 0;
        } else {
            return false;
        }
    }

    /**
     * returns true if the submitted node represents a complex property.
     * in a future release this check has to be performed against
     * the schema
     */
    protected boolean isComplexProperty( Node node ) {
        if ( ( node.getFirstChild() == null ) ) {
            return false;
        }

        NodeList nl = node.getChildNodes();

        for ( int i = 0; i < nl.getLength(); i++ ) {
            if ( nl.item( i ) instanceof Element ) {
                return true;
            }
        }

        return false;
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
 * Revision 1.1  2004/05/11 16:43:24  doemming
 * Initial revision
 *
 * Revision 1.12  2004/03/02 07:38:14  poth
 * no message
 *
 * Revision 1.11  2004/02/11 08:06:05  poth
 * no message
 *
 * Revision 1.10  2004/02/06 13:49:08  axel_schaefer
 * use of XMLTools
 *
 * Revision 1.9  2003/10/24 07:29:46  poth
 * no message
 *
 * Revision 1.8  2003/08/27 07:53:34  poth
 * no message
 *
 * Revision 1.7  2003/07/10 08:38:46  poth
 * no message
 *
 * Revision 1.6  2003/06/03 15:56:39  poth
 * no message
 *
 * Revision 1.5  2003/05/15 09:37:40  poth
 * no message
 *
 * Revision 1.4  2003/04/23 15:44:39  poth
 * no message
 *
 * Revision 1.3  2002/12/02 09:48:12  poth
 * no message
 *
 * Revision 1.2  2002/11/13 16:57:09  poth
 * no message
 *
 * Revision 1.1.1.1  2002/09/25 16:01:01  poth
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
 */
