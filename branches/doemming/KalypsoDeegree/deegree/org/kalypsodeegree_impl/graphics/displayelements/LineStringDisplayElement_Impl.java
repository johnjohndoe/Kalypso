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

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.io.Serializable;

import org.deegree.graphics.displayelements.LineStringDisplayElement;
import org.deegree.graphics.sld.LineSymbolizer;
import org.deegree.graphics.sld.Symbolizer;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.Feature;
import org.deegree.model.geometry.GM_Curve;
import org.deegree.model.geometry.GM_LineString;
import org.deegree.model.geometry.GM_MultiCurve;
import org.deegree.model.geometry.GM_Position;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;
import org.deegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.deegree_impl.tools.Debug;
import de.tuhh.wb.jm.tools.Tools;


/**
 * DisplayElement that encapsulates a linestring (<tt>GM_Curve</tt>) or
 * multi-linestring geometry (<tt>GM_MultiCurve</tt>) and a
 * <tt>LineStringSymbolizer</tt>.
 * <p>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider</a>
 * @version $Revision$ $Date$
 */
class LineStringDisplayElement_Impl extends GeometryDisplayElement_Impl
    implements LineStringDisplayElement, Serializable {
    /** Use serialVersionUID for interoperability. */
    private final static long serialVersionUID = -4657962592230618248L;

    /**
     * Creates a new LineStringDisplayElement_Impl object.
     *
     * @param feature 
     * @param geometry 
     */
    protected LineStringDisplayElement_Impl( Feature feature, GM_Curve geometry ) {
        super( feature, geometry, null );

        Symbolizer defaultSymbolizer = new LineSymbolizer_Impl();
        this.setSymbolizer( defaultSymbolizer );
    }

    /**
     * Creates a new LineStringDisplayElement_Impl object.
     *
     * @param feature 
     * @param geometry 
     * @param symbolizer 
     */
    protected LineStringDisplayElement_Impl( Feature feature, GM_Curve geometry, 
                                             LineSymbolizer symbolizer ) {
        super( feature, geometry, symbolizer );        
    }

    /**
     * Creates a new LineStringDisplayElement_Impl object.
     *
     * @param feature 
     * @param geometry 
     */
    protected LineStringDisplayElement_Impl( Feature feature, GM_MultiCurve geometry ) {
        super( feature, geometry, null );

        Symbolizer defaultSymbolizer = new LineSymbolizer_Impl();
        this.setSymbolizer( defaultSymbolizer );
    }

    /**
     * Creates a new LineStringDisplayElement_Impl object.
     *
     * @param feature 
     * @param geometry 
     * @param symbolizer 
     */
    protected LineStringDisplayElement_Impl( Feature feature, GM_MultiCurve geometry, 
                                             LineSymbolizer symbolizer ) {
        super( feature, geometry, symbolizer );
    }

    /**
     *  renders the DisplayElement to the submitted graphic context
     */
    public void paint( Graphics g, GeoTransform projection ) {
        Debug.debugMethodBegin( this, "paint" );

        try {
            int[][] pos = null;
	    if(de.tuhh.wb.jm.Debug.showDisplayEnv())
		Tools.paint(g,geometry.getEnvelope());
            if ( geometry instanceof GM_Curve ) {
                pos = calcTargetCoordinates( projection, (GM_Curve)geometry );
                drawLine( g, pos );
            } else {
                GM_MultiCurve mc = (GM_MultiCurve)geometry;

                for ( int i = 0; i < mc.getSize(); i++ ) {
                    pos = calcTargetCoordinates( projection, mc.getCurveAt( i ) );
                    drawLine( g, pos );
                }                
            }
        } catch ( Exception e ) {
            System.out.println( e );
        }

        Debug.debugMethodEnd();
    } 

    /**
     * calculates the coordintes (image or screen coordinates) where to draw
     * the curve.
     */
    private int[][] calcTargetCoordinates( GeoTransform projection, GM_Curve curve )
                                   throws Exception {
        GM_LineString lineString = curve.getAsLineString();
        int count = lineString.getNumberOfPoints();
        int[][] pos = new int[3][];
        pos[0] = new int[count];
        pos[1] = new int[count];
        pos[2] = new int[1];
        
        int k = 0;
        for ( int i = 0; i < count; i++ ) {
            GM_Position position = lineString.getPositionAt( i );  
            double tx = projection.getDestX( position.getX() );
            double ty = projection.getDestY( position.getY() );
            if ( i > 0 ) {                
                if ( distance( tx, ty, pos[0][k-1], pos[1][k-1] ) > 1 ) {
                    pos[0][k] = (int)( tx + 0.5 );
                    pos[1][k] = (int)( ty + 0.5 );
                    k++;
                }
            } else {                
                pos[0][k] = (int)( tx + 0.5 );
                pos[1][k] = (int)( ty + 0.5 );
                k++;
            }            
        }
        pos[2][0] = k;        

        return pos;
    }

    /**
     * Renders one curve to the submitted graphic context.
     *
     * TODO: Calculate miterlimit.
     */
    private void drawLine( Graphics g, int[][] pos ) throws FilterEvaluationException {
        LineSymbolizer sym = (LineSymbolizer)symbolizer;
        org.deegree.graphics.sld.Stroke stroke = sym.getStroke();

        // no stroke defined -> don't draw anything
        if ( stroke == null ) {
            return;
        }

        // Color & Opacity
        Graphics2D g2 = (Graphics2D)g;
        setColor( g2, stroke.getStroke( feature ), stroke.getOpacity( feature ) );

        float[] dash = stroke.getDashArray( feature );

        // use a simple Stroke if dash == null or its length < 2
        // that's faster
        float width = (float)stroke.getWidth( feature );
        int cap = stroke.getLineCap( feature );
        int join = stroke.getLineJoin( feature );
        BasicStroke bs2 = null;

        if ( ( dash == null ) || ( dash.length < 2 ) ) {
            bs2 = new BasicStroke( width, cap, join );
        } else {
            bs2 = new BasicStroke( width, cap, join, 10.0f, dash, stroke.getDashOffset( feature ) );
        }

        g2.setStroke( bs2 );                
        
        g2.drawPolyline( pos[0], pos[1], pos[2][0] );
                
    }
    
    private double distance( double x1, double y1, double x2, double y2 ) {
        return Math.sqrt( (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1) );
    }

    /**
     *
     *
     * @param g2 
     * @param color 
     * @param opacity 
     *
     * @return 
     */
    private Graphics2D setColor( Graphics2D g2, Color color, double opacity ) {
        if ( opacity < 0.999 ) {
            final int alpha = (int)Math.round( opacity * 255 );
            final int red = color.getRed();
            final int green = color.getGreen();
            final int blue = color.getBlue();
            color = new Color( red, green, blue, alpha );
        }

        g2.setColor( color );
        return g2;
    }      
}
