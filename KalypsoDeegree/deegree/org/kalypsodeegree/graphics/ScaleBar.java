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
package org.deegree.graphics;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Font;


/**
 * 
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public interface ScaleBar {
    
    public static int L_NONE = -1;
    public static int L_SCALE = 0;
    public static int L_SCALEDENOMINATOR = 1;
    
    /**
     * will paint the scale bar to the passed graphic context
     *
     * @param g graphic context
     */
    void paint( Graphics g );
    
    /**
     * sets the type of the label above the scale bar
     *
     * @param labelType lable type
     */
    void setTopLabel(int labelType);
    
    /**
     * sets the type of the label below the scale bar
     *
     * @param labelType lable type
     */
    void setBottomLabel(int labelType);  

    /**
     * sets the scale as defined in the OGC WMS 1.1.1 specification. Scale is
     * defined as the diagonal size of a pixel in the center of a map measured
     * in meter. The setting of the scale will affect the value of the scale 
     * denominator
     *
     * @parameter scale map scale
     */
    void setScale( double scale );

    /**
     * sets the scale denominator for the scale bar. The scale denominator is
     * the scale expression as we know it for printed maps (e.g. 1:10000 1:5000).
     * The passed value is expressed in meters.
     * The setting of the scale denominator will affect the value of the scale
     *
     * @param scaleDen scale denominator value
     */
    void setScaleDenominator( int scaleDen );
    
    /**
     * sets the units the scale and the scale denominater will be expressed at. 
     * Settings other than meter will cause that the passed values for scale
     * and scale denominater will be recalculated for painting. it depends on
     * the implementation what units are supported.
     *
     * @param units name units (meter, miles, feet etc.)
     */
    void setUnits(String units);
    
    /**
     * sets the front color of the scale bar
     */
    void setColor(Color color);
    
    /**
     * sets the label color of the scale bar
     */
    void setLabelColor(Color color);
    
    /**
     * sets the style of the scale bar. default style is |--------|
     * the list of known styles depends on the implementation
     *
     * @param style style name
     */
    void setStyle(String style);
    
    /**
     * sets the font for label rendering
     *
     * @param font awt font object
     */
    void setFont(Font font);
    
}