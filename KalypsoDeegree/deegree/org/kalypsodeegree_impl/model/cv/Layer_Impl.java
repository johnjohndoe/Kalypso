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

import java.net.URL;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.coverage.Format;
import org.deegree.model.coverage.Layer;

/**
 * The Layer(Type), is meant to be shared with the Web Map Service and Web 
 * Feature Service and defines access basic to common elements.
 *
 * <p>-----------------------------------------------------------------------</p>
 *
 * @author Andreas Poth
 * @version $Revision$ $Date$
 * <p>
 */
abstract class Layer_Impl implements Layer {
    
    private String abstract_                = null;
    private String[] crs                    = null;
    private String[] keywordList            = null; 
    private GM_Envelope latLonBoundingBox   = null; 
    private String layerID                  = null;
    private URL[] metadataURLs              = null;
    private String nativeCRS                = null;
    private String[] queryCRS               = null;
    private String[] responseCRS            = null;
    private Format[] supportedFormatList    = null;
    private String title                    = null;
    
    Layer_Impl(String layerID, String title, String abstract_, String[] keywordList,
               GM_Envelope latLonBoundingBox, String[] crs, String nativeCRS,
               URL[] metadataURLs, Format[] supportedFormatList)
    {
        this.layerID = layerID;
        this.title = title;
        this.abstract_ = abstract_;
        this.keywordList = keywordList;
        this.latLonBoundingBox = latLonBoundingBox;
        this.crs = crs;
        queryCRS = new String[crs.length];
        responseCRS = new String[crs.length];
        for (int i = 0; i < crs.length; i++) {
            queryCRS[i] = crs[i];
            responseCRS[i] = crs[i];
        }
        this.nativeCRS = nativeCRS;
        this.metadataURLs = metadataURLs;
        this.supportedFormatList = supportedFormatList;
    }
        
    Layer_Impl(String layerID, String title, String abstract_, String[] keywordList,
               GM_Envelope latLonBoundingBox, String[] queryCRS, String[] responseCRS, 
               String nativeCRS, URL[] metadataURLs, Format[] supportedFormatList)
    {
        this.layerID = layerID;
        this.title = title;
        this.abstract_ = abstract_;
        this.keywordList = keywordList;
        this.latLonBoundingBox = latLonBoundingBox;
        this.queryCRS = queryCRS;
        this.responseCRS = responseCRS;
        this.nativeCRS = nativeCRS;
        this.metadataURLs = metadataURLs;
        this.supportedFormatList = supportedFormatList;
    }
               
    
    
    /** returns a narrative description of the map layer
     *
     */
    public String getAbstract() {
        return abstract_;
    }    
    
    /** For each Coverage Layer, the Capabilities XML description specifies (i)
     * the native Spatial Reference System (SRS) of the data; (ii) the SRSs in
     * which it understands incoming GetCoverage requests; and (iii) the SRSs in
     * which it can produce coverages in response to GetCoverage requests
     * <p>
     * Every CoverageLayer must have either one or more SRS elements, or both a
     * QuerySRS and a ResponseSRS element (which contain one or more SRS elements).
     * A server may choose to detail query and response SRSs separately, or just
     * advertise SRSs in which it can both accept requests and deliver coverage
     * responses
     * <p>
     * The content of the SRS element may be any of the EPSG: or AUTO: coordinate
     * systems defined in the Web Map Service Implementation Specification; it may
     * be undefined (“OGC:none”); or it may be an embedded “swath” referencing
     * system (“OGC:swath”) that lets the client reconstruct ground coordinates
     * from tie-points or sensor metadata
     *
     */
    public String[] getCRS() {
        return crs;
    }
    
    /** returns a list that contains keywords to aid in catalog searches
     *
     */
    public String[] getKeywordList() {
        return keywordList;
    }
    
    /** The required LatLonBoundingBox element, has attributes indicating the edges
     * of an enclosing rectangle in longitude/latitude decimal degrees.
     * LatLonBoundingBox must be supplied regardless of what SRS the map server
     * may support for coverage requests and responses. However, it may be
     * approximate if EPSG:4326 (WGS84 geographic) is not a supported request SRS.
     * Its chief purpose is to populate registries for geographic search
     *
     */
    public GM_Envelope getLatLonBoundingBox() {
        return latLonBoundingBox;
    }
    
    /**
     * The required LayerID is a unique identifier (not used for any other
     * coverage layer). Using the GetCoverage or DescribeCoverage operation,
     * clients request coverages from that Coverage Layer (or a description of
     * the Coverage Layer) by using this LayerID value in the LAYERS parameter
     * (for requests expressed as key-value pairs) or in the LayerID element
     * (for XML requests).
     *
     */
    public String getLayerID() {
        return layerID;
    }
    
    /** The optional MetadataURL element is recommended for access to detailed,
     * standardized metadata about the data underneath a particular layer. The
     * type attribute indicates the standard to which the metadata complies.
     * Two types are defined at present: 'TC211' = ISO TC211 19115; 'FGDC' =
     * FGDC Content Standard for Digital Geospatial Metadata
     *
     */
    public URL[] getMetadataURLs() {
        return metadataURLs;
    }
    
    /** The optional NativeSRS element states the native SRS of a Coverage Layer.
     * (This, along with the native resolution stated in the BoundingBox,
     * facilitates client access to unretouched coverage values.
     *
     */
    public String getNativeCRS() {
        return nativeCRS;
    }
    
    /** The QuerySRS element states the SRS(s) in which GetCoverage requests may
     * be expressed against that coverage layer. One of these should be the
     * coverage layer’s native SRS.
     * <p>
     * Requests expressed in the special SRS codes “OGC:none” or “OGC:swath”
     * refer to subsets defined according the layer’s pixel row/column coordinate
     * system (for imagery) or its internal / local coordinate system (for
     * non-gridded data).
     *
     */
    public String[] getQueryCRS() {
        return queryCRS;
    }
    
    /** The ResponseSRS element states the SRS(s) in which coverage replies to
     * GetCoverage requests may be expressed. One of these should be the Layer’s
     * native SRS. Coverages served in the special SRS codes “OGC:none” or
     * “OGC:swath” are expressed in pixel row/column coordinate system (for
     * imagery) or an internal / local coordinate system (for non-gridded data).
     * Coverage replies with the SRS “OGC:swath” may embed sensor-model or
     * tie-point information to let a thick client georeference the returned
     * coverage
     *
     */
    public String[] getResponseCRS() {
        return responseCRS;
    }
    
    /** The required SupportedFormatList element advertises the output format(s)
     * in which coverages may be requested from this Coverage Layer (e.g.,
     * GeoTIFF, HDF-EOS, NITF, DTED, etc.). Both a format name and a MIME type
     * identify these formats. Several OGC-specific types have been defined to
     * distinguish various types of XML documents; these are listed in WMS 1.1.
     *
     */
    public Format[] getSupportedFormatList() {
        return supportedFormatList;
    }
    
    /** The required Title element contains a human-readable string for p
     * resentation in a menu.
     *
     */
    public String getTitle() {
        return title;
    }
    
}
