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
package org.deegree_impl.graphics.displayelements;

import java.awt.*;
import java.awt.image.*;

import java.io.Serializable;

import org.deegree.graphics.displayelements.PointDisplayElement;
import org.deegree.graphics.sld.*;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.*;
import org.deegree.model.geometry.*;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;

import org.deegree_impl.graphics.sld.*;
import org.deegree_impl.tools.*;


/**
 * DisplayElement that encapsulates a point geometry (<tt>GM_Point</tt>)
 * and a <tt>PointSymbolizer</tt>.
 * <p>
* @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider</a>
 * @version $Revision$ $Date$
 */
class PointDisplayElement_Impl extends GeometryDisplayElement_Impl implements PointDisplayElement,
                                                                              Serializable {
    /** Use serialVersionUID for interoperability. */
    private final static long serialVersionUID = -2979559276151855757L;
    private transient static Image defaultImg = 
                        new BufferedImage( 7, 7, BufferedImage.TYPE_INT_ARGB );

    static {
        Graphics g = defaultImg.getGraphics();
        g.setColor( Color.LIGHT_GRAY );
        g.fillRect( 0, 0,  9, 9 );
        g.dispose();
    }
    /**
     * Creates a new PointDisplayElement_Impl object.
     *
     * @param feature 
     * @param geometry 
     */
    PointDisplayElement_Impl( Feature feature, GM_Point geometry ) {
        super( feature, geometry, null );

        Symbolizer defaultSymbolizer = new PointSymbolizer_Impl();
        this.setSymbolizer( defaultSymbolizer );
    }

    /**
     * Creates a new PointDisplayElement_Impl object.
     *
     * @param feature 
     * @param geometry 
     * @param symbolizer 
     */
    PointDisplayElement_Impl( Feature feature, GM_Point geometry, PointSymbolizer symbolizer ) {
        super( feature, geometry, symbolizer );
    }

    /**
     * Creates a new PointDisplayElement_Impl object.
     *
     * @param feature 
     * @param geometry 
     */
    PointDisplayElement_Impl( Feature feature, GM_MultiPoint geometry ) {
        super( feature, geometry, null );

        Symbolizer defaultSymbolizer = new PointSymbolizer_Impl();
        this.setSymbolizer( defaultSymbolizer );
    }

    /**
     * Creates a new PointDisplayElement_Impl object.
     *
     * @param feature 
     * @param geometry 
     * @param symbolizer 
     */
    PointDisplayElement_Impl( Feature feature, GM_MultiPoint geometry, PointSymbolizer symbolizer ) {
        super( feature, geometry, symbolizer );
    }

    /**
     *  renders the DisplayElement to the submitted graphic context
     */
    public void paint( Graphics g, GeoTransform projection ) {
        try {
            Image image = defaultImg;

            if ( ( (PointSymbolizer)symbolizer ).getGraphic() != null ) {
                image = ( (PointSymbolizer)symbolizer ).getGraphic().getAsImage( feature );
            }
            Graphics2D g2D = (Graphics2D)g;

            if ( geometry instanceof GM_Point ) {
                drawPoint( g2D, (GM_Point)geometry, projection, image );
            } else {
                GM_MultiPoint mp = (GM_MultiPoint)geometry;

                for ( int i = 0; i < mp.getSize(); i++ ) {
                    drawPoint( g2D, mp.getPointAt( i ), projection, image );
                }
            }
        } catch ( FilterEvaluationException e ) {
            Debug.debugException( e, "Exception caught evaluating an Expression!" );
        }
    }

    /**
     * renders one point to the submitted graphic context considering
     * the also submitted projection
     */
    private void drawPoint( Graphics2D g, GM_Point point, GeoTransform projection, Image image ) {
        GM_Position source = point.getPosition();
        int x = (int)( projection.getDestX( source.getX() ) + 0.5 );
        int y = (int)( projection.getDestY( source.getY() ) + 0.5 );

        int x_ = x - ( image.getWidth( null ) >> 1 );
        int y_ = y - ( image.getHeight( null ) >> 1 );
        g.drawImage( image, x_, y_, null );
    }        
}