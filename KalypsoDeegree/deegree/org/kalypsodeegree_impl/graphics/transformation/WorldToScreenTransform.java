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

package org.deegree_impl.graphics.transformation;

import org.deegree.model.geometry.*;
import org.deegree_impl.model.geometry.*;
import org.deegree.graphics.transformation.*;

/**
 * the class <code>WorldToScreenTransform</code> implements
 * <code>GeoTransformInterface</code> and defines a transformation
 * to a linear coordinat system with its orgin on top/left. this can be
 * used for realising a screen mapping of geometries.
 *
 * @author Andreas Poth poth@lat-lon.de
 * @version 28.12.2000
 */

public class WorldToScreenTransform implements GeoTransform {
     
    private double qx = 0;
    private double qy = 0;
    private GM_Envelope sourceRect = null;
    private GM_Envelope destRect = null;
    
    /**
     * constructor
     *
     * initialices the transfromation rectangles with unique values
     * (origin 0/0; widht 1; height 1)
     */
    public WorldToScreenTransform() {
        setSourceRect( 0, 0, 1, 1 );
        setDestRect( 0, 0, 1, 1 );
    }
    
    /**
     * constructor
     *
     * initialices the transformation rectangle using the submitted
     * source- and destination rectangle.
     *
     * @param sourceRect is the boundary of the source geometry.
     * @param destRect is the boundary of the destination rectangle (for
     *        example a region on the screen)
     */
    public WorldToScreenTransform(GM_Envelope sourceRect,GM_Envelope destRect) {
        setSourceRect( sourceRect );
        setDestRect( destRect );
    }
    
    /**
     * constrctor
     *
     * @param sourceXMin minimum x-coordinate (source system) of the map.
     * @param sourceYMin minimum y-coordinate (source system) of the map.
     * @param sourceXMax maximum x-coordinate (source system) of the map.
     * @param sourceYMax maximum y-coordinate (source system) of the map.
     * @param destXMin minimum x-coordinate (destination system) of the map.
     * @param destYMin minimum y-coordinate (destination system) of the map.
     * @param destXMax maximum x-coordinate (destination system) of the map.
     * @param destYMax maximum y-coordinate (destination system) of the map.
     */
    public WorldToScreenTransform(double sourceXMin, double sourceYMin, double sourceXMax,
                                  double sourceYMax, double destXMin, double destYMin,
                                  double destXMax, double destYMax) {
        setSourceRect( sourceXMin, sourceYMin, sourceXMax, sourceYMax );
        setDestRect( destXMin, destYMin, destXMax, destYMax );
    }
    
    /**
     * sets the source rectangle
     *
     * @param rect is the boundary of the source geometry.
     */
    public void setSourceRect(GM_Envelope rect) {
        
        sourceRect = rect;
        
        if ( (sourceRect != null) && (destRect != null) ) {
            calculateQX();
            calculateQY();
        }
    }
    
    /**
     * sets the source rectangle
     *
     * @param xMin minimum x-coordinate (source system) of the map.
     * @param yMin minimum y-coordinate (source system) of the map.
     * @param xMax maximum x-coordinate (source system) of the map.
     * @param yMax maximum y-coordinate (source system) of the map.
     */
    public void setSourceRect(double xMin, double yMin, double xMax, double yMax) {
        
        double dum = 0;
        
        if ( xMin > xMax ) {
            dum = xMax;
            xMax = xMin;
            xMin = dum;
        }
        
        if ( yMin > yMax ) {
            dum = yMax;
            yMax = yMin;
            yMin = dum;
        }
        
        sourceRect = GeometryFactory.createGM_Envelope( xMin, yMin, xMax, yMax );
        
        if ( destRect != null ) {
            calculateQX();
            calculateQY();
        }
        
    }
    
    public GM_Envelope getSourceRect() {
        return sourceRect;
    }
    
    /**
     * sets the destination rectangle.
     *
     * @param rect is the boundary of the destination rectangle (for
     *        example a region on the screen)
     */
    public void setDestRect(GM_Envelope rect) {
        destRect = rect;
        if ( (sourceRect != null) && (destRect != null) ) {
            calculateQX();
            calculateQY();
        }
    }
    
    /**
     * sets the destination rectangle
     *
     * @param xMin minimum x-coordinate (destination system) of the map.
     * @param yMin minimum y-coordinate (destination system) of the map.
     * @param xMax maximum x-coordinate (destination system) of the map.
     * @param yMax maximum y-coordinate (destination system) of the map.
     */
    public void setDestRect(double xMin, double yMin, double xMax, double yMax) {
        
        double dum = 0;
        
        if ( xMin > xMax ) {
            dum = xMax;
            xMax = xMin;
            xMin = dum;
        }
        
        if ( yMin > yMax ) {
            dum = yMax;
            yMax = yMin;
            yMin = dum;
        }
        
        destRect = GeometryFactory.createGM_Envelope( xMin, yMin, xMax, yMax );
        
        if ( sourceRect != null ) {
            calculateQX();
            calculateQY();
        }
        
    }
    
    public GM_Envelope getDestRect() {
        return destRect;
    }
    
    
    /**
     * executes a coordinat transformation for the submitted
     * x-coordinate of the source coordinat system.
     *
     * @param xsource, x-coordinate of a point in the source
     *        coordinate system.
     * @return the x-coordinate of the submitted value in the
     *         destination coordinate system.
     */
    public double getDestX(double xsource) {
        return destRect.getMin().getX() + (xsource - sourceRect.getMin().getX()) * qx;
    }
    
    /**
     * executes a coordinat transformation for the submitted
     * y-coordinate of the source coordinat system.
     *
     * @param ysource, y-coordinate of a point in the source
     *        coordinate system.
     * @return the y-coordinate of the submitted value in the
     *         destination coordinate system.
     */
    public double getDestY(double ysource) {
        return destRect.getMin().getY() + destRect.getHeight()
        - (ysource - sourceRect.getMin().getY()) * qy;
    }
    
    /**
     * executes a coordinat transformation for the submitted
     * point of the source coordinat system.
     *
     * @param point in the source coordinate system.
     * @return the location of the submitted point in the destination
     *         coordinate system.
     */
    public GM_Position getDestPoint(GM_Position point) {
        double x = getDestX(point.getX());
        double y = getDestY(point.getY());
        return GeometryFactory.createGM_Position(x,y);
    }
    
    /**
     * executes a coordinat transformation for the submitted
     * x-coordinate of the destination coordinate system.
     *
     * @param xdest, x-coordinate of a point in the destination
     *        coordinate system.
     * @return the x-coordinate of the submitted value in the
     *         source coordinate system.
     */
    public double getSourceX(double xdest) {
        return (xdest - destRect.getMin().getX()) / qx + sourceRect.getMin().getX();
    }
    
    /**
     * executes a coordinat transformation for the submitted
     * y-coordinate of the destination coordinate system.
     *
     * @param ydest, y-coordinate of a point in the destination
     *        coordinate system.
     * @return the y-coordinate of the submitted value in the
     *         source coordinate system.
     */
    public double getSourceY(double ydest) {        
        double d = ( destRect.getHeight() - (ydest - destRect.getMin().getY()) )/
            qy + sourceRect.getMin().getY();
        return d;
        
    }
    
    /**
     * executes a coordinat transformation for the submitted
     * point of the destination coordinate system.
     *
     * @param point in the destination coordinate system.
     * @return the location of the submitted point in the source
     *         coordinate system.
     */
    public GM_Position getSourcePoint(GM_Position point) {
        double x = getSourceX(point.getX());
        double y = getSourceY(point.getY());
        return GeometryFactory.createGM_Position(x,y);
    }
    
    /**
     * calculates the relation between the width of the
     * destination and the source coordinate system.
     */
    protected void calculateQX() {
        qx = destRect.getWidth()/sourceRect.getWidth();
    }
    
    /**
     * calculates the relation between the height of the
     * destination and the source coordinate system.
     */
    protected void calculateQY() {
        qy = destRect.getHeight()/sourceRect.getHeight();
    }
    
}
