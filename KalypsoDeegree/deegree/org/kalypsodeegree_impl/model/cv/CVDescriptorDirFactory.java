/*----------------    FILE HEADER  ------------------------------------------
 
This file is part of deegree (Java Framework for Geospatial Solutions).
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
package org.deegree_impl.model.cv;



import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.RandomAccessFile;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;

import javax.media.jai.JAI;
import javax.media.jai.RenderedOp;

import org.deegree.model.coverage.CVDescriptor;
import org.deegree.model.coverage.CoverageLayer;
import org.deegree.model.coverage.Directory;
import org.deegree.model.coverage.ExtentType;
import org.deegree.model.coverage.Format;
import org.deegree.model.coverage.Grid;
import org.deegree.model.coverage.GridAxis;
import org.deegree.model.coverage.GridAxisDescription;
import org.deegree.model.coverage.GridCoverageLayer;
import org.deegree.model.coverage.GridExtentDescription;
import org.deegree.model.coverage.GridRange;
import org.deegree.model.coverage.GridRangeDescription;
import org.deegree.model.coverage.Level;
import org.deegree.model.coverage.RangeSetDescription;
import org.deegree.model.coverage.SpatialExtent;
import org.deegree.model.coverage.Tile;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.tools.ParameterList;
import org.deegree.xml.ElementList;
import org.deegree.xml.XMLTools;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.ParameterList_Impl;
import org.deegree_impl.tools.StringExtend;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.SAXException;

import com.sun.media.jai.codec.ByteArraySeekableStream;

/**
 *
 * <p> --------------------------------------------------------------------- </p>
 * @author Andreas Poth
 * @version 31.10.2002
 */
public class CVDescriptorDirFactory  {
    
    public static boolean logToFile     = false;
    public static String logFile        = "log_descriptor.txt";
    private static String NSWCS         = "http://www.opengis.net/wcs";
    private static String NSCV          = "http://www.deegree.org/gc";
    
    /**
     * create a <tt>CVDescriptor</tt> instance from an URL referencing a XML-
     * document that's conform to CVDescriptor schema definition.
     */
    public static CVDescriptor createCVDescriptor(URL resource)
                                        throws IOException, SAXException, InvalidAxisDefinitionException
    {
        Reader reader = new InputStreamReader( resource.openStream() );
        Document doc = XMLTools.parse(  reader );
        return createCVDescriptor( doc );
    }
    
    /**
     * create a <tt>CVDescriptor</tt> instance from an XML-document that's
     * conform to CVDescriptor schema definition.
     */
    public static CVDescriptor createCVDescriptor(Document document)
                                    throws IOException, InvalidAxisDefinitionException
    {
        Debug.debugMethodBegin( "CVDescriptorFactory",  "createCVDescriptor" );
        
        Element root = document.getDocumentElement();
        
        ParameterList pl = new ParameterList_Impl();
        NodeList nl = root.getChildNodes();
        for (int i = 0; i < nl.getLength(); i++) {
            if (nl.item(i) instanceof Element &&
                nl.item(i).getNodeName().endsWith( "Property" ) ) {
                String name = XMLTools.getAttrValue( nl.item(i), "name" );
                String value = XMLTools.getAttrValue( nl.item(i), "value" );
                pl.addParameter( name, value );
            }
        }
        
        nl 			= root.getElementsByTagNameNS( NSWCS, "GridCoverageLayer" );
        Node node 	= nl.item(0);
        GridCoverageLayer gcl = createGridCoverageLayer( (Element)node );
        node 		= document.getElementsByTagNameNS( NSCV, "CVDescriptor" ).item(0);

		// ETJ Ext ... ->
        Node  levelNode = XMLTools.getNamedChild( node, NSCV, "Level" );
        Level level 	= createLevel( levelNode );
		System.out.println("Level(DIR):: "+level.toString()); // ETJ ext level

        ElementList rangeList = XMLTools.getChildElementsByName( "Range", NSCV, node );
		ArrayList   ranges 	  = new ArrayList(rangeList.getLength());
		for(int i=0; i<rangeList.getLength(); i++)
			ranges.add(createRange(rangeList.item(i)));
		
        CVDescriptor cvd = new CVDescriptor_Impl(gcl, null /*preview URL*/, level, ranges, pl );
        
        Debug.debugMethodEnd();
        return cvd;
    }

    /**
     * Creates a <tt>CVRange</tt> instance from a node of the CVDescriptor xml
     * document.
	 * @author ETj
     */
    private static CVRange createRange(Element node) throws IOException
    {
        Debug.debugMethodBegin( "CVDescriptorDirFactory",  "createRange" );
        
        String name 	= XMLTools.getAttrValue( node, "name" );
		String value  	= XMLTools.getAttrValue( node, "value" );

        Node  levelNode = XMLTools.getNamedChild( node, NSCV, "Level" );
        Level level 	= createLevel( levelNode );
		System.out.println("Level_DIR_:: "+level.toString()); // ETJ ext level
		
        ElementList rangeList = XMLTools.getChildElementsByName( "Range", NSCV, node );
		ArrayList   subranges = new ArrayList(rangeList.getLength());
		for(int i=0; i<rangeList.getLength(); i++)
			subranges.add(createRange(rangeList.item(i)));
		
        CVRange range = CVRangeFactory.createRange( name, value, level, subranges );
		
        Debug.debugMethodEnd();
        return range;
    }
	
    /**
     * creates a <tt>Level</tt> instance from a node of the CVDescriptor xml
     * document.
     */
    private static Level createLevel(Node node) throws IOException
    {
        Debug.debugMethodBegin( "CVDescriptorFactory",  "createLevel" );
        
        String s = XMLTools.getAttrValue( node, "minScale" );
        double minScale = Double.parseDouble(s);
        s = XMLTools.getAttrValue( node, "maxScale" );
        double maxScale = Double.parseDouble(s);
        
        Tile[] tiles = null;
        Directory[] directories = null;
        ParameterList properties = new ParameterList_Impl();
        
        NodeList dirs = node.getChildNodes();
                
        if ( dirs != null ) {
            directories = createDirectories( dirs );
        } else {
            ArrayList list = new ArrayList();
            NodeList nl = node.getChildNodes();
            for (int i = 0; i < nl.getLength(); i++) {
                // is Tile
                if ( nl.item(i) instanceof Element &&
                     XMLTools.toLocalName(nl.item(i).getNodeName()).equals("Tile") ) {
                     list.add( nl.item(i) );
                } else {
                    // is property
                    if ( nl.item(i) instanceof Element &&
                         XMLTools.toLocalName(nl.item(i).getNodeName()).equals("Property") ) {
                        String name = XMLTools.getAttrValue( nl.item(i), "name" );
                        String value = XMLTools.getAttrValue( nl.item(i), "value" );
                        properties.addParameter(name, value);
                    }
                }
            }
            Element[] elements = new Element[ list.size() ];
            elements = (Element[])list.toArray( elements );
            tiles = createTiles( elements );
        }
        
        Level level = null;
        if ( directories != null ) {
            level = new Level_Impl(minScale, maxScale, properties, directories );
        } else {
            level = new Level_Impl(minScale, maxScale, properties, tiles );
        }
        
        Debug.debugMethodEnd();
        return level;
    }
    
    /**
     * creates an array of <tt>Tile</tt> instances from an array of Tile-elements.
     * Each element may represent one single Tile (file) or a set of Tiles using
     * '*' as wild card for file (not directory) name
     */
    private static Tile[] createTiles(Element[] elements) throws IOException
    {
        Debug.debugMethodBegin( "CVDescriptorFactory",  "createTiles" );
        
        double minx = 0;
        double miny = 0;
        double maxx = 0;
        double maxy = 0;
        
        ArrayList list = new ArrayList(elements.length);        
        
        for (int i = 0; i < elements.length; i++) {

            NodeList tileList = elements[i].getChildNodes();
            ArrayList tlist = new ArrayList();
            for (int j = 0; j < tileList.getLength(); j++ ) {
                if ( tileList.item(j) instanceof Element &&
                     tileList.item(j).getNodeName().equals("Tile") ) {
                    tlist.add( tileList.item(j) );
                }
            }
            
            Tile[] tls = null;
            if ( tlist.size() > 0 ) {
                // tile contains sub-tiles with smaller bounding boxes
                Element[] elems = (Element[])tlist.toArray( new Element[tlist.size()] );
                tls = createTiles( elems );
            }
                        
            String s = XMLTools.getAttrValue( elements[i], "resource" );
            File file = null;
            if ( s.startsWith( "FILE" ) || s.startsWith( "file" ) ) {
                // resource is a file
                s = s.substring(7);
                file = new File( s );
            }
                
            if ( file == null && s != null ) {
                //TODO
                // handle regular expressions
            } else {
                s = XMLTools.getAttrValue( elements[i], "minx" );
                if ( s != null ) minx = Double.parseDouble(s);
                s = XMLTools.getAttrValue( elements[i], "miny" );
                if ( s != null ) miny = Double.parseDouble(s);
                s = XMLTools.getAttrValue( elements[i], "maxx" );
                if ( s != null ) maxx = Double.parseDouble(s);
                s = XMLTools.getAttrValue( elements[i], "maxy" );
                if ( s != null ) maxy = Double.parseDouble(s);
                //get world file if no bounding box is defined
                if ( s == null ) {
                    double[] bbox = worldFile( s, -1, -1 );
                    minx = bbox[0]; miny = bbox[1];
                    maxx = bbox[2]; miny = bbox[3];
                }
                GM_Envelope env = GeometryFactory.createGM_Envelope( minx, miny, maxx, maxy );
                Level level = null;
                Node node = XMLTools.getNamedChild( elements[i], NSCV, "Level" );
                if ( node != null ) level = createLevel( node );

                list.add( new Tile_Impl( new URL("file:///"+file.getPath()), env, level, tls ) );
            }
        }
        
        Debug.debugMethodEnd();
        return (Tile[])list.toArray( new Tile[list.size()] );
    }
    
    /**
     * creats an array of <tt>Tile</tt> instances from a node that represents
     * a directory at the local or a remote file system
     */
    private static Directory[] createDirectories(NodeList nodeList) throws IOException
    {
        Debug.debugMethodBegin( "CVDescriptorFactory",  "createDirectories" );

        double minx = 0;  double miny = 0;
        double maxx = 0;  double maxy = 0;
        
        ArrayList list = new ArrayList(nodeList.getLength());
        for (int k = 0; k < nodeList.getLength(); k++) {
            if ( nodeList.item(k) instanceof Element &&
                 nodeList.item(k).getNodeName().equals( "Directory" ) ) {
                String dir = "";

                NodeList nl = nodeList.item(k).getChildNodes();
                for (int i = 0; i < nl.getLength(); i++) {
                    if ( nl.item(i) instanceof Text ) {
                        dir = nl.item(i).getNodeValue().trim();
                        break;
                    }
                }

                String s = null;

                s = XMLTools.getAttrValue( nodeList.item(k), "extensions" );
                String[] extensions = null;
                if ( s != null ) {
                    extensions = StringExtend.toArray( s, ",;", true );
                }
                
                s = XMLTools.getAttrValue( nodeList.item(k), "widthCRS" );
                double widthCRS = Double.parseDouble( s );
                s = XMLTools.getAttrValue( nodeList.item(k), "heightCRS" );
                double heightCRS = Double.parseDouble( s );

                s = XMLTools.getAttrValue( nodeList.item(k), "minx" );
                if ( s != null ) minx = Double.parseDouble(s);
                s = XMLTools.getAttrValue( nodeList.item(k), "miny" );
                if ( s != null ) miny = Double.parseDouble(s);
                s = XMLTools.getAttrValue( nodeList.item(k), "maxx" );
                if ( s != null ) maxx = Double.parseDouble(s);
                s = XMLTools.getAttrValue( nodeList.item(k), "maxy" );
                if ( s != null ) maxy = Double.parseDouble(s);
                //get world file if no bounding box is defined
                if ( s == null ) {
                    double[] bbox = worldFile( s, -1, -1 );
                    minx = bbox[0]; miny = bbox[1];
                    maxx = bbox[2]; miny = bbox[3];
                }
                GM_Envelope env = GeometryFactory.createGM_Envelope( minx, miny, maxx, maxy );

                Node lnode = XMLTools.getNamedChild( nodeList.item(k), NSCV, "Level" );
                Level level = null;
                if ( lnode != null ) level = createLevel( lnode );

                list.add( new Directory_Impl(dir, env, level, extensions, widthCRS, heightCRS ) );

            }
        }
                
        Debug.debugMethodEnd();
        return (Directory[])list.toArray( new Directory[list.size()] );
    }
    
    /**
     * Gets the bounding box of the image.
     */
    private static double[] worldFile(String filename, int width, int height) throws IOException {
        
        Debug.debugMethodBegin( "CVDescriptorFactory",  "worldFile" );

        if ( width < 0 || height < 0 ) {
            RandomAccessFile raf = new RandomAccessFile( filename, "r" );
            byte[] b = new byte[ (int)raf.length() ];
            raf.read(b);
            raf.close();
            ByteArraySeekableStream bass = new ByteArraySeekableStream( b );
            RenderedOp ro = JAI.create( "stream", bass );
            bass.close();
            bass = null;
            b = null;

            width = ro.getWidth();
            height = ro.getHeight();
            ro = null;
        }
        
        /*
         * Gets the substring beginning at the specified beginIndex (0) - the
         *   beginning index, inclusive - and extends to the character at
         *	index endIndex (position of '.') - the ending index, exclusive.
         */
        String fname = null;
        int pos = filename.lastIndexOf(".");
        filename = filename.substring(0,pos);
        
       /*
        * Looks for corresponding worldfiles.
        */
        if ( (new File(filename + ".tfw")).exists() ){
            fname = filename + ".tfw";
        }
        else
            if ( (new File(filename + ".wld")).exists() ){
                fname = filename + ".wld";
            }
            else
                if ( (new File(filename + ".jgw")).exists() ){
                    fname = filename + ".jgw";
                }
                else
                    if ( (new File(filename + ".jpgw")).exists() ){
                        fname = filename + ".jpgw";
                    }
                    else
                        if ( (new File(filename + ".gfw")).exists() ){
                            fname = filename + ".gfw";
                        }
                        else
                            if ( (new File(filename + ".gifw")).exists() ){
                                fname = filename + ".gifw";
                            }
                            else
                                if ( (new File(filename + ".pgw")).exists() ){
                                    fname = filename + ".pgw";
                                }
                                else
                                    if ( (new File(filename + ".pngw")).exists() ){
                                        fname = filename + ".pngw";
                                    }
                                    else {
                                        throw new IOException("Not a world file for: " +filename);
                                    }
        /*
         * Reads character files.
         * The constructors of this class (FileReader) assume that the default character
         *	 encoding and the default byte-buffer size are appropriate.
         * The BufferedReader reads text from a character-input stream, buffering characters so as
         *	to provide for the efficient reading of characters
         */
        BufferedReader br = new BufferedReader( new FileReader(fname) );
        
        String s = null;
        
        int cnt = 0;
        double d1 = 0; double d2 = 0;
        double d3 = 0; double d4 = 0;
        while ( (s = br.readLine()) != null) {
            cnt++;
            s = s.trim();
            switch ( cnt ) {
                case 1: d1 = Double.parseDouble(s); break;
                case 4: d2 = Double.parseDouble(s); break;
                case 5: d3 = Double.parseDouble(s); break;
                case 6: d4 = Double.parseDouble(s); break;
            }
        }
        
        br.close();
        
        double d5 = d3 + width * d1;
        double d6 = d4 + height * d2;

        double[] bbox = new double[]{ d3, d6, d5, d4 };
        
        Debug.debugMethodEnd();
        
        return bbox;
        
    }
    
    /**
     * creates an array of <tt>CoverageLayer</tt>s from the CoverageLayerList
     * element of the capabilities<p>
     * TODO
     * support for other layer types than grid coverages
     */
    public static CoverageLayer[] createCoverageLayer(Element element)
                                                  throws InvalidAxisDefinitionException,
                                                         MalformedURLException
    {
        Debug.debugMethodBegin( "WCSCapabilitiesFactory",  "createCoverageLayer" );
        
        ArrayList list = new ArrayList();
         
        NodeList nl = element.getChildNodes();
        for (int i = 0; i < nl.getLength(); i++) {
            if ( nl.item(i) instanceof Element ) {
                list.add( createGridCoverageLayer( (Element)nl.item(i) ) );
            }
        }
        
        CoverageLayer[] cl = (CoverageLayer[])list.toArray( new CoverageLayer[list.size()] );
        
        Debug.debugMethodEnd();
        return cl;
    }
    
    /**
     * creates a <tt>GridCoverageLayer</tt> instance from a <tt>Element</tt>
     */
    public static GridCoverageLayer createGridCoverageLayer(Element element)
                                            throws InvalidAxisDefinitionException,
                                                   MalformedURLException
    {
        Debug.debugMethodBegin( "WCSCapabilitiesFactory",  "createGridCoverageLayer" );
        
        String s = XMLTools.getAttrValue( element, "descriptorResource" );
        URL descriptorResource = null;
        if ( s != null ) {
            descriptorResource = new URL( s );
        }
        
        // get unique ID of the GridCoverageLayer
        Node node = element.getElementsByTagNameNS( NSWCS, "LayerID" ).item(0);
        String id = node.getFirstChild().getNodeValue().trim();
        
        // get title of the GridCoverageLayer
        node = element.getElementsByTagNameNS( NSWCS, "Title" ).item(0);
        String title = node.getFirstChild().getNodeValue().trim();
        
        // get abstract of the GridCoverageLayer
        String abstract_ = null;
        NodeList list = element.getElementsByTagNameNS( NSWCS, "Title" );
        if ( list != null && list.getLength() > 0 ) {
            abstract_ = list.item(0).getFirstChild().getNodeValue().trim();
        }
        
        // get keywords assigned to the GridCoverageLayer
        String[] keywords = null;
        list = element.getElementsByTagNameNS( NSWCS, "KeywordList" );
        if ( list != null && list.getLength() > 0 ) {
            String tmp = list.item(0).getFirstChild().getNodeValue().trim();
            keywords = StringExtend.toArray( tmp, ",;", true );
        }
        
        // get metadata URLs assigned to the GridCoverageLayer
        list = element.getElementsByTagNameNS( NSWCS, "MetadataURL" );
        URL[] metadataURLs = createMetadataURLs( list );
                
        // get latlon bounding box of the GridCoverageLayer
        node = element.getElementsByTagNameNS( NSWCS, "LatLonBoundingBox" ).item(0);
        GM_Envelope llbbox = createLatLonBoundingBox( node );
        
        // get CRSs assigned to the GridCoverageLayer
        String[] crs = null;
        list = element.getElementsByTagNameNS( NSWCS, "SRS" );
        if ( list != null && list.getLength() > 0) {
            String tmp = list.item(0).getFirstChild().getNodeValue().trim();
            crs = StringExtend.toArray( tmp, ",;", true );
        } else {
            list = element.getElementsByTagNameNS( NSWCS, "QuerySRS" );
            list = element.getElementsByTagNameNS( NSWCS, "ResponseSRS" );
            //TODO
        }
        
        // get get formats supported by the GridCoverageLayer
        list = element.getElementsByTagNameNS( NSWCS, "SupportedFormatList" );
        Format[] formats = createSupportedFormatList( list );
        
        // get keywords assigned to the GridCoverageLayer
        String[] interpolationMethods = new String[] { "nearest neighbor" };
        list = element.getElementsByTagNameNS( NSWCS, "InterpolationMethod" );
        if ( list != null && list.getLength() > 0 ) {
            String tmp = list.item(0).getFirstChild().getNodeValue().trim();
            interpolationMethods = StringExtend.toArray( tmp, ",;", true );
        }
        
        // get get formats supported by the GridCoverageLayer
        list = element.getElementsByTagNameNS( NSWCS, "GridExtentDescription" );
        GridExtentDescription ged = createGridExtentDescription( (Element)list.item(0) );
        
        // create the description of the grids range
        node = element.getElementsByTagNameNS( NSWCS, "RangeSetDescription" ).item(0);
        RangeSetDescription rsd = createRangeSetDescription( (Element)node );
        
        GridCoverageLayer gcl =
            new GridCoverageLayer_Impl( id, title, abstract_, keywords, llbbox,
                                        crs, null, metadataURLs, formats,
                                        interpolationMethods, ged, descriptorResource,
                                        rsd);
        
        Debug.debugMethodEnd();
        return gcl;
    }
    
    /**
     * creates a list URLs referencing metadata assigned to a <tt>GridCoverageLayer</tt>
     */
    private static URL[] createMetadataURLs(NodeList list )
    {
        Debug.debugMethodBegin( "WCSCapabilitiesFactory",  "createMetadataURLs" );
        
        //TODO
        
        Debug.debugMethodEnd();
        return null;
    }
    
    /**
     * creates the latlon bounding box of the <tt>GridCoverageLayer</tt>
     */
    private static GM_Envelope createLatLonBoundingBox(Node node)
    {
        Debug.debugMethodBegin( "WCSCapabilitiesFactory",  "createLatLonBoundingBox" );
        
        String s = XMLTools.getAttrValue( node, "minx" );
        double minx = Double.parseDouble( s );
        
        s = XMLTools.getAttrValue( node, "miny" );
        double miny = Double.parseDouble( s );
        
        s = XMLTools.getAttrValue( node, "maxx" );
        double maxx = Double.parseDouble( s );
        
        s = XMLTools.getAttrValue( node, "maxy" );
        double maxy = Double.parseDouble( s );
        
        GM_Envelope bb = GeometryFactory.createGM_Envelope( minx, miny, maxx, maxy );
        
        Debug.debugMethodEnd();
        return bb;
    }
    
    /**
     * creates a list of formats that are supported by the <tt>GridCoverageLayer</tt>
     */
    private static Format[] createSupportedFormatList(NodeList list )
    {
        Debug.debugMethodBegin( "WCSCapabilitiesFactory",  "createSupportedFormatList" );
        
        Format[] formats = new Format[list.getLength()];
        
        for (int i = 0; i < list.getLength(); i++) {
            NodeList nl = ((Element)list.item(i)).getElementsByTagNameNS( NSWCS, "FormatName");
            String name = nl.item(0).getFirstChild().getNodeValue().trim();
            
            nl = ((Element)list.item(i)).getElementsByTagNameNS( NSWCS, "description");
            String description = null;
            if ( nl != null && nl.getLength() > 0 ) {
                description = nl.item(0).getFirstChild().getNodeValue().trim();
            }
            
            nl = ((Element)list.item(i)).getElementsByTagNameNS( NSWCS, "MIMEType");
            String mime = nl.item(0).getFirstChild().getNodeValue().trim();
            
            formats[i] = new Format_Impl(name, description, mime );
        }
        
        
        
        Debug.debugMethodEnd();
        return formats;
    }
    
    /**
     * creates the <tt>GridExtentDescription</tt> for the <tt>GridCoverageLayer</tt>
     */
    private static GridExtentDescription createGridExtentDescription(Element element)
                                            throws InvalidAxisDefinitionException
    {
        Debug.debugMethodBegin( "WCSCapabilitiesFactory",  "createGridExtentDescription" );
        
        String s = XMLTools.getAttrValue( element, "temporal" );
        boolean temporal = "true".equals(s) || "1".equals(s);
        
        s = XMLTools.getAttrValue( element, "dimension" );
        int dim = 0;
        if ( s != null ) {
            dim = Integer.parseInt(s);
        }
        
        Node node = element.getElementsByTagNameNS( NSWCS, "SpatialExtent" ).item(0);
        SpatialExtent se = createSpatialExtent( (Element)node );
        
        //TODO
        // create temporal extent
        // create elevation extent
        
        // create the description of the grids axis
        node = element.getElementsByTagNameNS( NSWCS, "GridAxisDescription" ).item(0);
        GridAxisDescription gad = createGridAxisDescription( (Element)node );
        
        //TODO
        // createGridSpacing( .. )
        
        node = element.getElementsByTagNameNS( NSWCS, "Grid" ).item(0);
        Grid grid = createGrid( (Element)node );
        
        //TODO
        // alternative to Grid createRectified
        
        //
        GridExtentDescription ged =
            new GridExtentDescription_Impl( se, dim, null, null, grid, gad, null );
        
        Debug.debugMethodEnd();
        return ged;
    }
    
    /**
     * creates an instance of a <tt>SpatialExtent</tt>
     */
    private static SpatialExtent createSpatialExtent(Element element)
    {
        Debug.debugMethodBegin( "WCSCapabilitiesFactory",  "createSpatialExtent" );
        
        String crs = XMLTools.getAttrValue( element, "srsName" );
        
        Element elem = (Element)element.getElementsByTagNameNS( NSWCS, "XExtent" ).item(0);
        ExtentType xExtent = createExtent( elem );
        
        elem = (Element)element.getElementsByTagNameNS( NSWCS, "YExtent" ).item(0);
        ExtentType yExtent = createExtent( elem );
        
// TODO
//        elem = (Element)element.getElementsByTagNameNS( NSWCS, "ZExtent" ).item(0);
//        ExtentType zExtent = createExtent( elem );
        
        SpatialExtent se = new SpatialExtent_Impl( crs, xExtent, yExtent, null);
        
        Debug.debugMethodEnd();
        return se;
    }
    
    /**
     * creates an <tt>ExtentType</tt> instance (without uom parameter)
     */
    private static ExtentType createExtent(Element element)
    {
        Debug.debugMethodBegin( "WCSCapabilitiesFactory",  "createExtent" );
        
        String s = element.getElementsByTagNameNS( NSWCS, "min" ).item(0).getFirstChild().getNodeValue().trim();
        double min = Double.parseDouble( s );
        s = element.getElementsByTagNameNS( NSWCS, "max" ).item(0).getFirstChild().getNodeValue().trim();
        double max = Double.parseDouble( s );
        double res = 0;
        try {
            s = element.getElementsByTagNameNS( NSWCS, "res" ).item(0).getFirstChild().getNodeValue().trim();
            res = Double.parseDouble( s );
        } catch(Exception e){}
        
        ExtentType et = new ExtentType_Impl( min, max, res, null );
        
        Debug.debugMethodEnd();
        return et;
    }
    
    /**
     * creates a description of a axis of the <tt>GridCoverageLayer</tt>
     */
    private static GridAxisDescription createGridAxisDescription(Element element)
                                            throws InvalidAxisDefinitionException
    {
        Debug.debugMethodBegin( "WCSCapabilitiesFactory",  "createGridAxisDescription" );
        
        NodeList nl = element.getElementsByTagNameNS( NSWCS, "GridAxis" );
        GridAxis[] ga = new GridAxis[ nl.getLength() ];
        for (int i = 0; i < ga.length; i++) {
            Node node = ((Element)nl.item(i)).getElementsByTagNameNS( NSWCS, "Name" ).item(0);
            String name = node.getFirstChild().getNodeValue().trim();
            
            String description = null;
            NodeList nl_ = ((Element)nl.item(i)).getElementsByTagNameNS( NSWCS, "description" );
            if ( nl_ != null && nl_.getLength() > 0) {
                description = nl_.item( 0 ).getFirstChild().getNodeValue().trim();
            }
            
            node = ((Element)nl.item(i)).getElementsByTagNameNS( NSWCS, "orientation" ).item(0);
            String s = node.getFirstChild().getNodeValue().trim();
            int orientation = getOrientation(s);
            
            ga[i] = new GridAxis_Impl( name, description, orientation );
        }
        
        GridAxisDescription gad = new GridAxisDescription_Impl( ga );
        
        Debug.debugMethodEnd();
        return gad;
    }
    
    private static int getOrientation(String s) {
        int orientation = -1;
        if ( s.equals("up") ) {
            orientation = GridAxis.UP;
        } else
            if ( s.equals("down") ) {
            orientation = GridAxis.DOWN;
        } else
            if ( s.equals("left") ) {
            orientation = GridAxis.LEFT;
        } else
            if ( s.equals("right") ) {
            orientation = GridAxis.RIGHT;
        } else
            if ( s.equals("back") ) {
            orientation = GridAxis.BACK;
        } else
            if ( s.equals("front") ) {
            orientation = GridAxis.FRONT;
        }
        return orientation;
    }
    
    /**
     * creates the Grid(description) of the <tt>GridCoverageLayer</tt>
     */
    private static Grid createGrid( Element element)
    {
        Debug.debugMethodBegin( "WCSCapabilitiesFactory",  "createGrid" );
        
        String type = XMLTools.getAttrValue( element, "type" );
        String s = XMLTools.getAttrValue( element, "dimension" );
        int dimension = Integer.parseInt(s);
        
        Node node = element.getElementsByTagNameNS( NSWCS, "low" ).item(0);
        NodeList nl = ((Element)node).getElementsByTagNameNS( NSWCS, "ordinate" );
        double[] low = new double[nl.getLength()];
        for (int i = 0; i < low.length; i++) {
            s = nl.item(i).getFirstChild().getNodeValue().trim().trim();
            low[i] = Double.parseDouble(s);
        }
        
        node = element.getElementsByTagNameNS( NSWCS, "high" ).item(0);
        nl = ((Element)node).getElementsByTagNameNS( NSWCS, "ordinate" );
        double[] high = new double[nl.getLength()];
        for (int i = 0; i < low.length; i++) {
            s = nl.item(i).getFirstChild().getNodeValue().trim().trim();
            high[i] = Double.parseDouble(s);
        }
        
        GridRange gr = new GridRange_Impl( low, high );
        
        Grid grid = new Grid_Impl( dimension, type, gr );
        
        Debug.debugMethodEnd();
        return grid;
    }
    
    /**
     * creates the range set description for the <tt>GridCoverageLayer</tt>
     */
    private static RangeSetDescription createRangeSetDescription( Element element)
    {
        Debug.debugMethodBegin( "WCSCapabilitiesFactory",  "createRangeSetDescription" );
        
        NodeList nl = element.getElementsByTagNameNS( NSWCS, "GridRangeDescription" );
        
        GridRangeDescription[] grd = createGridRangeDescription( nl );
        
        RangeSetDescription rsd = new RangeSetDescription_Impl( grd );
        
        Debug.debugMethodEnd();
        return rsd;
    }
    
    /**
     * creates the grid range description for <tt>GridCoverageLayer</tt>s
     * <tt>RangeSetDescription</tt>
     */
    private static GridRangeDescription[] createGridRangeDescription(NodeList nl)
    {
        Debug.debugMethodBegin( "WCSCapabilitiesFactory",  "createGridRangeDescription" );
        
        GridRangeDescription[] grd = new GridRangeDescription[ nl.getLength() ];
        for (int i = 0; i < grd.length; i++) {
            Node node = ((Element)nl.item(i)).getElementsByTagNameNS( NSWCS, "RangeID" ).item(0);
            String rangeID = node.getFirstChild().getNodeValue();
            node = ((Element)nl.item(i)).getElementsByTagNameNS( NSWCS, "title" ).item(0);
            String title = node.getFirstChild().getNodeValue();
            //TODO
            // optional parameter
            grd[i] = new GridRangeDescription_Impl( rangeID, title, null, null, null,
                                                    null, null, null, null );
        }
        
        Debug.debugMethodEnd();
        return grd;
    }
    
}
