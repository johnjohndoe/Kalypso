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
package org.deegree.services.wts.protocol;

import java.awt.Color;

import java.net.URL;

import java.util.Calendar;

import org.deegree.graphics.sld.StyledLayerDescriptor;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Position;
import org.deegree.services.OGCWebServiceRequest;


/**
 * This interface describes the access to the parameters of a GetView request.<p></p>
 * Even it is possible to access the values of a HTTP GET request throught their
 * bean accessor methods the request shall be mapped to a SLD data structure that
 * is accessible using the <tt>getSLD()</tt>.
 * <p>--------------------------------------------------------------------</p>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 2002-03-01
 */
public interface WTSGetViewRequest extends OGCWebServiceRequest {
    /**
     * returns the angle representing the breadth of landscape in the 
     * viewer's scene. The AOV (Angle of View) parameter defines the angle 
     * representing the breadth of landscape in the viewer's scene
     */
    double getAOV();

    /**
     * Distance between the viewer and the POI in meters. The DISTANCE parameter 
     * defines the distance between the viewer and the POI in meters. This
     * implies that setting DISTANCE = zero will make the POI coincident with 
     * the viewer.
     */
    double getDistance();

    /**
     * returns the azimuth angle
     */
    double getYAW();

    /**
     * returns Angle of inclination
     */
    double getPitch();

    /**
     * The POI (Point of Interest) parameter defines the x, y, z point in SRS units.
     */
    GM_Position getPointOfInterest();

    /**
     * The FORMAT parameter specifies the output format of the response to an
     * operation.<p></p>
     * An OGC Web Service may offer only a subset of the formats known for that
     * type of operation, but the server shall advertise in its Capabilities XML
     * those formats it does support and shall accept requests for any format it
     * advertises. A Service Instance may optionally offer a new format not
     * previously offered by other instances, with the recognition that clients
     * are not required to accept or process an unknown format. If a request
     * contains a Format not offered by a particular server, the server shall
     * throw a Service Exception (with code "InvalidFormat").
     */
    String getFormat();

    /**
     *
     *
     * @return 
     */
    /**
     * The required LAYERS parameter lists the map layer(s) to be returned by
     * this GetMap request. The value of the LAYERS parameter is a comma-separated
     * list of one or more valid layer names. Allowed layer names are the character
     * data content of any <Layer><Name> element in the Capabilities XML.<p></p>
     * A WMS shall render the requested layers by drawing the leftmost in the
     * list bottommost, the next one over that, and so on.<p></p>
     * Each layer is associated to a style. Styles are also is encoded as a comma-
     * seperated list within the GetMap request.<p></p>
     * The required STYLES parameter lists the style in which each layer is to be
     * rendered. There is a one-to-one correspondence between the values in the
     * LAYERS parameter and the values in the STYLES parameter. Because of this
     * layer-style combinations are returned coupled within an array of Layer-
     * objects. Each map in the list of LAYERS is drawn using the corresponding
     * style in the same position in the list of STYLES. Each style Name shall be
     * one that was defined in the <Name> element of a <Style> element that is
     * either directly contained within, or inherited by, the associated <Layer>
     * element in Capabilities XML.
     */
    Layer[] getLayers();

    /**
     * The required SRS parameter states which Spatial Reference System applies
     * to the values in the BBOX parameter. The value of the SRS parameter shall
     * be one of the values defined in the character data section of an <SRS>
     * element defined or inherited by the requested layer. The same SRS applies
     * to all layers in a single request. <p></p>
     * If the WMS server has declared SRS=NONE
     * for a Layer, as discussed in the Basic Service Elements section, then the
     * Layer does not have a well-defined spatial reference system and should not
     * be shown in conjunction with other layers. The Client shall specify SRS=NONE
     * (case-insensitive) in the GetMap request and the Server may issue a Service
     * Exception otherwise.
     */
    String getSrs();

    /**
     * WIDTH specifies the number of pixels to be used between the minimum and
     * maximum X values (inclusive) in the BBOX parameter. The returned picture,
     * regardless of its return format, shall have exactly the specified width
     * and height in pixels. In the case where the aspect ratio of the BBOX and
     * the ratio width/height are different, the WMS shall stretch the returned
     * map so that the resulting pixels could themselves be rendered in the
     * aspect ratio of the BBOX. In other words, it should be possible using this
     * definition to request a map for a device whose output pixels are themselves
     * non-square, or to stretch a map into an image area of a different aspect ratio.
     */
    int getWidth();

    /**
     * HEIGHT specifies the number of pixels between the minimum and maximum Y
     * values. The returned picture, regardless of its return format, shall have
     * exactly the specified width and height in pixels. In the case where the
     * aspect ratio of the BBOX and the ratio width/height are different, the WMS
     * shall stretch the returned map so that the resulting pixels could
     * themselves be rendered in the aspect ratio of the BBOX. In other words, it
     * should be possible using this definition to request a map for a device
     * whose output pixels are themselves non-square, or to stretch a map into an
     * image area of a different aspect ratio.
     */
    int getHeight();

    /**
     * The optional BGCOLOR parameter specifies the color to be used as the
     * background of the map. The general format of BGCOLOR is a hexadecimal
     * encoding of an RGB value where two hexadecimal characters are used for
     * each of Red, Green, and Blue color values. The values can range between 00
     * and FF for each (0 and 255, base 10). The format is 0xRRGGBB; either upper
     * or lower case characters are allowed for RR, GG, and BB values. The "0x"
     * prefix shall have a lower case 'x'. The default value is 0xFFFFFF
     * (corresponding to the color white) if this parameter is absent from the
     * request.
     */
    Color getBGColor();

    /**
     * The optional EXCEPTIONS parameter states the manner in which errors are to
     * be reported to the client. The default value is application/vnd.ogc.se_xml
     * if this parameter is absent from the request.<p></p>
     * A Web Map Service shall offer one or more of the following exception
     * reporting formats by listing them in separate <Format> elements inside the
     * <Exceptions> element of its Capabilities XML. The entire MIME type string
     * in <Format> is used as the value of the EXCEPTIONS parameter. The first of
     * these formats is required to be offered by every WMS; the others are optional.
     */
    String getExceptions();

    /**
     * URL of Web Feature Service providing features to be symbolized using SLD.
     * This parameter is optional. If no WFS URL is defined <tt>null</tt> will be
     * returned.
     */
    URL getWFS_URL();

    /**
     * returns the SLD the request is made of. This implies that a 'simple'
     * HTTP GET-Request will be transformed into a valid SLD. This is mandatory
     * within a JaGo WMS.<p></p>
     * This mean even if a GetMap request is send using the HTTP GET method, an
     * implementing class has to map the request to a SLD data sructure.
     */
    StyledLayerDescriptor getStyledLayerDescriptor();

    /**
     * returns the date/time for which a scene shall be rendered
     */
    Calendar getDate();

    /**
     * returns the material behavior of the DEM and features
     */
    Object getMaterial();

    /**
     * returns the scale of the DEM. 1 = original scale, > 1 make DEM higher
     * < 1 make DEM lower
     */
    double getScale();

    /**
     * returns the Color, Image or Shape to fill the scene background
     */
    Object getBackground();

    /**
     *
     *
     * @return 
     */
    /**
     * returns a list of the DEMs to be used
     */
    String[] getElevationModels();

    /**
     *
     *
     * @return 
     */
    /**
     * returns a list of named Faetuter collections and associated styles to
     * put into the scene.
     */
    Layer[] getFeatureCollections();

    /**
     * returns a list of named Faetuter collections and associated styles to
     * put into the scene as a SLD-document.
     */
    StyledLayerDescriptor getFeatureCollectionsAsSLD();

    /**
     *
     *
     * @return 
     */
    /**
     * retruns a list of atmospheric paramteres to be considered for rendering
     */
    Object[] getAtmosphericParameters();

    /**
     * returns the trajectory of the camera movement through the scene. This replaces
     * the viewers position
     */
    GM_Object getTrajectory();

    /**
     *
     *
     * @return 
     */
    /**
     * returns descriptions of light sources in addtion to the sun
     */
    Object[] getLight();

    /**
     * 
     *
     * @version $Revision$
     * @author $author$
     */
    public interface Layer {
        /**
         *
         *
         * @return 
         */
        String getName();

        /**
         *
         *
         * @return 
         */
        String getStyleName();
    }
}