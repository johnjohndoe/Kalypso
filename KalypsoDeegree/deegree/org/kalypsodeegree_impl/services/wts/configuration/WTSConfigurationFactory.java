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
package org.deegree_impl.services.wts.configuration;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;

import javax.media.jai.JAI;
import javax.media.jai.RenderedOp;

import org.deegree.model.coverage.CVDescriptor;
import org.deegree.services.OGCWebService;
import org.deegree.services.wfs.capabilities.WFSCapabilities;
import org.deegree.services.wts.configuration.WTSConfiguration;
import org.deegree.xml.XMLParsingException;
import org.deegree.xml.XMLTools;
import org.deegree_impl.model.cv.CVDescriptorDirFactory;
import org.deegree_impl.model.cv.CVDescriptorFactory;
import org.deegree_impl.model.cv.InvalidAxisDefinitionException;
import org.deegree_impl.services.wcs.RemoteWCService;
import org.deegree_impl.services.wcs.WCService_Impl;
import org.deegree_impl.services.wfs.WFSFactory;
import org.deegree_impl.services.wfs.capabilities.WFSCapabilitiesFactory;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.sun.media.jai.codec.MemoryCacheSeekableStream;
import com.sun.media.jai.codec.SeekableStream;


/**
 * This class parses a WTS configuration (capabilities) file and creates a 
 * <tt>WTSConfiguration</tt> object from it.
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public class WTSConfigurationFactory {
    private static String WTSNS = "http://www.lat-lon.de";

    /** Creates a new instance of ConfigurationFactory */
    public static WTSConfiguration createConfiguration( String configFileURL )
                                                throws MalformedURLException, XMLParsingException, 
                                                       IOException, SAXException, 
                                                       InvalidAxisDefinitionException {
        URL url = new URL( configFileURL );

        return createConfiguration( url );
    }

    /** Creates a new instance of ConfigurationFactory */
    public static WTSConfiguration createConfiguration( URL url ) throws MalformedURLException, 
                                                                         XMLParsingException, 
                                                                         IOException, SAXException, 
                                                                         InvalidAxisDefinitionException {
        Debug.debugMethodBegin( );

        // open XML document
        Document doc = null;

        try {
            Reader reader = new InputStreamReader( url.openStream() );
            doc = XMLTools.parse( reader );
        } catch ( Exception e ) {
            throw new XMLParsingException( e.toString() );
        }

        HashMap sourceTypes = new HashMap();
        HashMap formatNames = new HashMap();
        HashMap responsibleServices = new HashMap();
        HashMap featureTextures = new HashMap();

        NodeList nl = doc.getElementsByTagNameNS( WTSNS, "Layer" );

        if ( ( nl == null ) || ( nl.getLength() == 0 ) ) {
            throw new XMLParsingException( "no Layer specified" );
        }

        for ( int i = 0; i < nl.getLength(); i++ ) {
            createLayer( (Element)nl.item( i ), sourceTypes, formatNames, responsibleServices );
        }
        
        nl = doc.getElementsByTagNameNS( WTSNS, "DEM" );

        if ( ( nl == null ) || ( nl.getLength() == 0 ) ) {
            throw new XMLParsingException( "no Layer specified" );
        }

        for ( int i = 0; i < nl.getLength(); i++ ) {
            createLayer( (Element)nl.item( i ), sourceTypes, formatNames, responsibleServices );
        }
        
        nl = doc.getElementsByTagNameNS( WTSNS, "FeatureCollection" );
        for ( int i = 0; i < nl.getLength(); i++ ) {
            createFeatureCollection( (Element)nl.item( i ), sourceTypes, formatNames, 
                                     featureTextures, responsibleServices );
        }
        
        nl = doc.getElementsByTagNameNS( WTSNS, "Background" );
        HashMap backgrounds = createBackgrounds( nl );
        
        WTSConfiguration config = 
            new WTSConfiguration_Impl( sourceTypes, formatNames, featureTextures, 
                                       responsibleServices, backgrounds );

        Debug.debugMethodEnd();

        return config;
    }

    /**
     *
     *
     * @param sourceTypes 
     * @param formatNames 
     * @param responsibleServices 
     *
     * @throws XMLParsingException 
     */
    private static void createLayer( Element layer, HashMap sourceTypes, HashMap formatNames, 
                                     HashMap responsibleServices ) throws MalformedURLException, 
                                                                          XMLParsingException, 
                                                                          IOException, SAXException, 
                                                                          InvalidAxisDefinitionException {
        Debug.debugMethodBegin( "ConfigurationFactory", "createLayer" );

        String noSubSets = XMLTools.getAttrValue( layer, "noSubsets" );

        // if noSubsets == false then the layer is a 'directory' / 'node' just for
        // layer organization
        if ( ( noSubSets != null ) && 
                 ( noSubSets.equalsIgnoreCase( "false" ) || noSubSets.equals( "0" ) ) ) {
            return;
        }

        // get layer name
        String name = XMLTools.getRequiredStringValue( "Name", WTSNS, layer );

        Element dataurl = XMLTools.getRequiredChildByName( "DataURL", WTSNS, layer );

        // get format of the layer
        String format = XMLTools.getRequiredStringValue( "Format", WTSNS, dataurl );

        // get type of the associated data source
        Element elem = XMLTools.getRequiredChildByName( "Type", WTSNS, dataurl );
        String type = XMLTools.getRequiredAttrValue( "name", elem );

        // get the associated data source location
        elem = XMLTools.getRequiredChildByName( "OnlineResource", WTSNS, dataurl );

        String onlineResource = XMLTools.getRequiredAttrValue( "href", elem );

        // get the associated data source configuration
        elem = XMLTools.getRequiredChildByName( "Configuration", WTSNS, dataurl );

        String conf = XMLTools.getRequiredAttrValue( "href", elem );

        OGCWebService ows = null;

        Integer tp = null;
        if ( type.equalsIgnoreCase( "LOCALWCS" ) ) {
            CVDescriptor[] desc = new CVDescriptor[1];
            URL url = new URL( conf );

            if ( name.toUpperCase().startsWith( "DD" ) ) {
                desc[0] = CVDescriptorDirFactory.createCVDescriptor( url );
            } else {
                desc[0] = CVDescriptorFactory.createCVDescriptor( url );
            }
            ows = new WCService_Impl( desc );
            tp = new Integer( WTSConfiguration.LOCALWCS );
        } else if ( type.equalsIgnoreCase( "REMOTEWCS" ) ) {
            ows = new RemoteWCService( onlineResource );
            tp = new Integer( WTSConfiguration.REMOTEWCS );
        } else if ( type.equalsIgnoreCase( "LOCALWFS" ) ) {
            URL url = new URL( conf );
            WFSCapabilities capa = null;
            try {
                capa = WFSCapabilitiesFactory.createCapabilities( url );
                ows = WFSFactory.createWFSService( capa, null );                
            } catch(Exception e) {                
                throw new XMLParsingException( e.toString() );
            }            
            tp = new Integer( WTSConfiguration.LOCALWFS );
        } else if ( type.equalsIgnoreCase( "REMOTEWFS" ) ) {
            //ows = new RemoteWFService( onlineResource );
        	tp = new Integer( WTSConfiguration.REMOTEWFS );
        } else if ( type.equalsIgnoreCase( "LOCALWMS" ) ) {
        	tp = new Integer( WTSConfiguration.LOCALWMS );
        } else if ( type.equalsIgnoreCase( "REMOTEWMS" ) ) {
            //ows = new RemoteWMService( onlineResource );
        	tp = new Integer( WTSConfiguration.REMOTEWMS );
        }

        responsibleServices.put( name.toUpperCase(), ows );
        sourceTypes.put( name.toUpperCase(), tp );
        formatNames.put( name.toUpperCase(), format );

        Debug.debugMethodEnd();
    }
    
    /**
     *
     *
     * @param sourceTypes 
     * @param formatNames 
     * @param responsibleServices 
     *
     * @throws XMLParsingException 
     */
    private static void createFeatureCollection( Element layer, HashMap sourceTypes,
                                                 HashMap formatNames, HashMap textures,
                                                 HashMap responsibleServices ) throws MalformedURLException, 
                                                                          XMLParsingException, 
                                                                          IOException, SAXException, 
                                                                          InvalidAxisDefinitionException {
        Debug.debugMethodBegin( "ConfigurationFactory", "createFeatureCollection" );

        String noSubSets = XMLTools.getAttrValue( layer, "noSubsets" );

        // if noSubsets == false then the layer is a 'directory' / 'node' just for
        // layer organization
        if ( ( noSubSets != null ) && 
                 ( noSubSets.equalsIgnoreCase( "false" ) || noSubSets.equals( "0" ) ) ) {
            return;
        }

        // get layer name
        String name = XMLTools.getRequiredStringValue( "Name", WTSNS, layer );

        Element dataurl = XMLTools.getRequiredChildByName( "DataURL", WTSNS, layer );

        // get format of the layer
        String format = XMLTools.getRequiredStringValue( "Format", WTSNS, dataurl );

        // get type of the associated data source
        Element elem = XMLTools.getRequiredChildByName( "Type", WTSNS, dataurl );
        String type = XMLTools.getRequiredAttrValue( "name", elem );

        // get the associated data source location
        elem = XMLTools.getRequiredChildByName( "OnlineResource", WTSNS, dataurl );

        String onlineResource = XMLTools.getRequiredAttrValue( "href", elem );

        // get the associated data source configuration
        elem = XMLTools.getRequiredChildByName( "Configuration", WTSNS, dataurl );

        String conf = XMLTools.getRequiredAttrValue( "href", elem );

        OGCWebService ows = null;

        // get and create responsible Services
        if ( type.equalsIgnoreCase( "LOCALWFS" ) ) {
            URL url = new URL( conf );
            WFSCapabilities capa = null;
            try {
                capa = WFSCapabilitiesFactory.createCapabilities( url );
                ows = WFSFactory.createWFSService( capa, null );
            } catch(Exception e) {                
                throw new XMLParsingException( e.toString() );
            }            
        } else if ( type.equalsIgnoreCase( "REMOTEWFS" ) ) {
            //ows = new RemoteWFService( onlineResource );
        }

        responsibleServices.put( name.toUpperCase(), ows );
        sourceTypes.put( name.toUpperCase(), type );
        formatNames.put( name.toUpperCase(), format );
        
        // load texture for each feature
        NodeList nl = layer.getElementsByTagNameNS( WTSNS, "Texture" );
        HashMap textTmp = new HashMap(100);
        for (int i = 0; i < nl.getLength(); i++) {
            String associatedFeature = 
                XMLTools.getRequiredAttrValue ( "associatedFeature", nl.item(i) );
            String texture = XMLTools.getStringValue (nl.item(i));
            BufferedImage img = (BufferedImage)textTmp.get( texture );
            if ( img == null ) {
                // load texture image if not already loaded
                try {
                    URL url = new URL( texture );
                    SeekableStream sst = new MemoryCacheSeekableStream( url.openStream() );
                    RenderedOp rop = JAI.create("stream", sst);
                    img = rop.getAsBufferedImage();
                    sst.close();     
                } catch(Exception e) {
                    System.out.println(e);	
                    throw new XMLParsingException( e.toString() );
                }
                textTmp.put( texture, img);
            }
            textures.put(associatedFeature.toUpperCase(), img);
        }

        Debug.debugMethodEnd();
    }
    
    /**
     * creates a map of background images that can be accessed by an associated
     * name (not the image name)
     */
    private static HashMap createBackgrounds(NodeList nl) throws MalformedURLException, 
                                                           XMLParsingException, 
                                                           IOException, SAXException {
        Debug.debugMethodBegin( "ConfigurationFactory", "createBackgrounds" );
        
        HashMap bkgr = new HashMap();
        
        for (int i = 0; i < nl.getLength(); i++) {
            String name = XMLTools.getRequiredAttrValue( "name", nl.item(i) );
            String tmp = XMLTools.getStringValue( nl.item(i) );            
            BufferedImage img = null;
            try {
                URL url = new URL( tmp );
                SeekableStream sst = new MemoryCacheSeekableStream( url.openStream() );
                RenderedOp rop = JAI.create("stream", sst);
                img = rop.getAsBufferedImage();
                sst.close();     
            } catch(Exception e) {
                System.out.println(e);	
                throw new XMLParsingException( e.toString() );
            }
            bkgr.put( name, img );
        }
        
        Debug.debugMethodEnd();
        return bkgr;
    }
    
    public static void main(String[] args) {
        try {
            WTSConfigurationFactory.createConfiguration( "file:///C:/java/projekte/wts/WEB-INF/xml/wts_capabilities.xml" );
        } catch(Exception e) {
            System.out.println(e);	
        }
    }
    
}