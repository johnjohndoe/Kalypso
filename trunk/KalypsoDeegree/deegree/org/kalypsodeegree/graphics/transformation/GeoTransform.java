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
package org.deegree.graphics.transformation;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Position;


/**
 * <code>GeoTransformInterface</code> declares the methods which have to
 * be implemented by each class that executes a geographical coordinat
 * transformation.
 *
 * @author Andreas Poth poth@lat-lon.de
 * @version 28.12.2000
 */
public interface GeoTransform {
    /**
     *
     *
     * @param xdest 
     *
     * @return 
     */
    public double getSourceX( double xdest );

    /**
     *
     *
     * @param xsource 
     *
     * @return 
     */
    public double getDestX( double xsource );

    /**
     *
     *
     * @param ydest 
     *
     * @return 
     */
    public double getSourceY( double ydest );

    /**
     *
     *
     * @param ysource 
     *
     * @return 
     */
    public double getDestY( double ysource );

    /**
     *
     *
     * @param rect 
     */
    public void setSourceRect( GM_Envelope rect );

    /**
     *
     *
     * @param xMin 
     * @param yMin 
     * @param xMax 
     * @param yMax 
     */
    public void setSourceRect( double xMin, double yMin, double xMax, double yMax );

    /**
     *
     *
     * @return 
     */
    public GM_Envelope getSourceRect();

    /**
     *
     *
     * @param rect 
     */
    public void setDestRect( GM_Envelope rect );

    /**
     *
     *
     * @param xMin 
     * @param yMin 
     * @param xMax 
     * @param yMax 
     */
    public void setDestRect( double xMin, double yMin, double xMax, double yMax );

    /**
     *
     *
     * @return 
     */
    public GM_Envelope getDestRect();

    /**
     *
     *
     * @param point 
     *
     * @return 
     */
    public GM_Position getSourcePoint( GM_Position point );

    /**
     *
     *
     * @param point 
     *
     * @return 
     */
    public GM_Position getDestPoint( GM_Position point );
}