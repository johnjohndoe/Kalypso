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
package org.deegree_impl.clients.wmsclient.control;

import java.net.MalformedURLException;
import java.util.HashMap;

import org.deegree.enterprise.control.FormEvent;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.services.InconsistentRequestException;
import org.deegree.services.wms.protocol.WMSGetMapRequest;
import org.deegree.xml.XMLParsingException;
import org.deegree_impl.clients.wmsclient.configuration.Format;
import org.deegree_impl.clients.wmsclient.configuration.MapSize;
import org.deegree_impl.clients.wmsclient.configuration.WMSClientConfiguration;
import org.deegree_impl.clients.wmsclient.model.Constants;
import org.deegree_impl.graphics.transformation.WorldToScreenTransform;
import org.deegree_impl.services.wms.protocol.WMSProtocolFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.StringExtend;


/**
 * will be called if the client forces a zoomin action. the zoomin will
 * be performed by setting the map boundaries to the rectangle selected
 * by the client or centering the map onto the  point the user had
 * mouse-clicked to. the point or the rectangle is defined within the
 * settings that are contained within the <tt>MapClientEvent</tt>.
 * <p>-------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp</a>
 * @version $Revision$ $Date$
 */
public class ZoomInListener extends AbstractMapListener {
    /**
     * the method will be called by the <tt>MapListener</tt>
     * if a zoomin action/event occurs.
     */
    public void actionPerformed( FormEvent event ) {
        Debug.debugMethodBegin( this, "actionPerformed" );

        // default actions 
        super.actionPerformed( event );

        zoomIn( event );

        Debug.debugMethodEnd();
    }

    /**
     * the method perfomrs a "zoom in" on the map model.
     * This means the map will be zoomed and recentered onto a
     * specified point or rectangle . Usually this will be the point the user
     * clicked on the map or a rectangle he had drag with the mouse.
     */
    private void zoomIn( FormEvent event ) {
        Debug.debugMethodBegin( this, "zoomInByPoint" );

        HashMap model = this.toModel();
       
        // modify the GetMap Request
        WMSGetMapRequest gmr = null;

        try {
            WMSClientConfiguration config = 
                (WMSClientConfiguration)getRequest().getAttribute( Constants.WMSCLIENTCONFIGURATION );
            String action = (String)model.get( "ACTION" );
            if ( "ZOOMIN".equalsIgnoreCase( action ) ) {
                gmr = modifyModelToPointZoom( config, model );
            } else {
                gmr = modifyModelToZoomInByRect( config, model );
            }
            config.setSelectedMapOperation( Constants.WMS_OPERATION_ZOOMIN );
        } catch ( Exception e ) {
            e.printStackTrace();
        }

        this.getRequest().setAttribute( Constants.WMSGETMAPREQUEST, gmr );

        Debug.debugMethodEnd();
    }

    /**
     * 
     */
    private WMSGetMapRequest modifyModelToPointZoom( WMSClientConfiguration config, HashMap model )
                                             throws InconsistentRequestException, 
                                                    XMLParsingException, MalformedURLException {
                                                       
        Debug.debugMethodBegin( this, "modifyModelToPointZoom" );
        
        // get GetMap request
        String tmp = (String)model.get( Constants.WMSGETMAPREQUEST );
        HashMap getMR = toMap( tmp );

        // set new map size
        MapSize ms = config.getSelectedMapSize();
        getMR.put( "WIDTH", "" + ms.getWidth() );
        getMR.put( "HEIGHT", "" + ms.getHeight() );

        tmp = (String)getMR.get( "BBOX" );

        String[] bbox = StringExtend.toArray( tmp, ",", false );
        double xmin = Double.parseDouble( bbox[0] );
        double ymin = Double.parseDouble( bbox[1] );
        double xmax = Double.parseDouble( bbox[2] );
        double ymax = Double.parseDouble( bbox[3] );

        // calc new map center
        GeoTransform gti = new WorldToScreenTransform( xmin, ymin, xmax, ymax, 0, 0, 
                                                       ms.getWidth() - 1, ms.getHeight() - 1 );
        double x = Double.parseDouble( (String)model.get( Constants.WMS_X ) );
        double y = Double.parseDouble( (String)model.get( Constants.WMS_Y ) );
        x = gti.getSourceX( x );
        y = gti.getSourceY( y );

        // rescale the map (zoomin)
        double wi = ( xmax - xmin ) / 100d * ( 100 - config.getSelectedZoomFactor().getFactor() );
        double he = ( ymax - ymin ) / 100d * ( 100 - config.getSelectedZoomFactor().getFactor() );
        xmin = x - ( wi / 2d );
        xmax = x + ( wi / 2d );
        ymin = y - ( he / 2d );
        ymax = y + ( he / 2d );
        getMR.put( "BBOX", xmin + "," + ymin + "," + xmax + "," + ymax );

        // set new image/map format
        Format format = config.getSelectedMapFormat();
        getMR.put( "FORMAT", format.getName() );

        // set Layers & styles
        // invert layer order
        tmp = (String)model.get( Constants.LAYERLIST );

        String[] v = StringExtend.toArray( tmp, ",", false );
        StringBuffer lay = new StringBuffer( 200 );
        StringBuffer sty = new StringBuffer( 200 );

        for ( int i = 0; i < v.length; i++ ) {
            tmp = v[v.length - 1 - i];

            int pos = tmp.indexOf( '|' );
            lay.append( tmp.substring( 0, pos ) );
            sty.append( tmp.substring( pos + 1, tmp.length() ) );

            if ( i < ( v.length - 1 ) ) {
                lay.append( "," );
                sty.append( "," );
            }
        }

        getMR.put( "LAYERS", lay.toString() );
        getMR.put( "STYLES", sty.toString() );
        
        Debug.debugMethodEnd();

        return WMSProtocolFactory.createGetMapRequest( "1", getMR );
    }   

    /**
     * The method performs a "zoom in by rectangle" to the map model.
     * This means that the map is set to a new bounding box that is
     * smaller then the previous one. Usually the user defines the
     * new bounding box by dragging a rectangle on the map.
     */
    private WMSGetMapRequest modifyModelToZoomInByRect( WMSClientConfiguration config, 
                                                        HashMap model)
                                                        throws InconsistentRequestException, 
                                                               XMLParsingException, 
                                                               MalformedURLException {
        Debug.debugMethodBegin( this, "modifyModelToZoomInByRect" );

        // get GetMap request
        String tmp = (String)model.get( Constants.WMSGETMAPREQUEST );
        HashMap getMR = toMap( tmp );

        // set new map size
        MapSize ms = config.getSelectedMapSize();
        getMR.put( "WIDTH", "" + ms.getWidth() );
        getMR.put( "HEIGHT", "" + ms.getHeight() );

        tmp = (String)getMR.get( "BBOX" );

        String[] bbox = StringExtend.toArray( tmp, ",", false );
        double xmin = Double.parseDouble( bbox[0] );
        double ymin = Double.parseDouble( bbox[1] );
        double xmax = Double.parseDouble( bbox[2] );
        double ymax = Double.parseDouble( bbox[3] );

        // calc new map center
        GeoTransform gti = new WorldToScreenTransform( xmin, ymin, xmax, ymax, 0, 0, 
                                                       ms.getWidth() - 1, ms.getHeight() - 1 );

        //get new bbox
        double x = Double.parseDouble( (String)model.get( "MINX" ) );
        double y = Double.parseDouble( (String)model.get( "MINY" ) );
        double x2 = Double.parseDouble( (String)model.get( "MAXX" ) );
        double y2 = Double.parseDouble( (String)model.get( "MAXY" ) );
        // rescale the map (zoomin)
        double wi = ( x2 - x );
        double he = ( y2 - y );
        
        // perform zooin by point if the zoom rectangle is smaller than 10
        // pixel at one side
        if ( wi < 10 || he < 10 ) {
            model.put( Constants.WMS_X, "" + (x+wi/2) );
            model.put( Constants.WMS_Y, "" + (y+he/2) );
            Debug.debugMethodEnd();
            return modifyModelToPointZoom( config, model );
        }

        if ( model.get( "TRANSFORM" ) == null || 
            "true".equalsIgnoreCase( (String)model.get( "TRANSFORM" ) ) ) {
            xmin = gti.getSourceX( x );
            xmax = gti.getSourceX( x2 );
            ymax = gti.getSourceY( y );
            ymin = gti.getSourceY( y2 );        
        } else {
            xmin = x;
            xmax = x2;
            ymax = y2;
            ymin = y;        
        }

        getMR.put( "BBOX", xmin + "," + ymin + "," + xmax + "," + ymax );

        // set new image/map format
        Format format = config.getSelectedMapFormat();
        getMR.put( "FORMAT", format.getName() );

        // set Layers & styles
        // invert layer order
        tmp = (String)model.get( Constants.LAYERLIST );

        String[] v = StringExtend.toArray( tmp, ",", false );
        StringBuffer lay = new StringBuffer( 200 );
        StringBuffer sty = new StringBuffer( 200 );

        for ( int i = 0; i < v.length; i++ ) {
            tmp = v[v.length - 1 - i];

            int pos = tmp.indexOf( '|' );
            lay.append( tmp.substring( 0, pos ) );
            sty.append( tmp.substring( pos + 1, tmp.length() ) );

            if ( i < ( v.length - 1 ) ) {
                lay.append( "," );
                sty.append( "," );
            }
        }

        getMR.put( "LAYERS", lay.toString() );
        getMR.put( "STYLES", sty.toString() );

        Debug.debugMethodEnd();
        return WMSProtocolFactory.createGetMapRequest( "1", getMR );
    }
}