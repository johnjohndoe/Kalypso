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
package org.deegree.graphics.legend;

import java.awt.image.BufferedImage;

import org.deegree.graphics.legend.LegendException;


/**
 * The interface describes the basic legend element. a legend element may has a 
 * label that can be set to eight positions relative to the legend graphic. A
 * <tt>LegendElement</tt> can be activated or deactivated. It depends on the
 * using application what effect this behavior will have. It depends on 
 * implementing classes how a <tt>LegendElement</tt> is realized. E.g. as 
 * 'simple' image or maybe as composition of <tt>Component</tt>s.<p>
 * <tt>LegendElement</tt>s can be collected in a <tt>LegendElementCollection</tt>
 * which also is a <tt>LegendElement</tt> to group elements or to create more
 * complex elements.<p>
 * Each <tt>LegendElement</tt> is able to paint itself as <tt>BufferedImage</tt>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public interface LegendElement {
    
    public static int LP_NOLABEL = -1;
    public static int LP_TOPCENTER = 0;
    public static int LP_TOPLEFT = 1;
    public static int LP_TOPRIGHT = 2;
    public static int LP_RIGHT = 3;
    public static int LP_LEFT = 4;
    public static int LP_BOTTOMCENTER = 5;
    public static int LP_BOTTOMRIGHT = 6;
    public static int LP_BOTTOMLEFT = 7;

    /**
     * sets the label of the <tt>LegendElement</tt>
     *
     * @param label label of the <tt>LegendElement</tt>
     */
    void setLabel( String label );

    /**
     * returns the label set to <tt>LegendElement</tt>. If no label is set, the
     * method returns <tt>null</tt>
     *
     * @return label of the <tt>LegendElement</tt> or <tt>null</tt>
     */
    String getLabel();

    /**
     * sets the orientation of the label of the <tt>LegendElement</tt>. A label
     * can have an orientation from -90° to 90° expressed in radians, where 0°
     * is horizontal
     *
     * @param orientation 
     */
    void setLabelOrientation( double orientation );

    /**
     * returns the current orientation of the label of the <tt>LegendElement</tt>
     * in radians. If the element hasn't a label <tt>Double.NEGATIVE_INFINITY</tt>
     * will be returned.
     *
     * @return orientation of the label of the <tt>LegendElement</tt> in radians
     */
    double getLabelOrientation();

    /**
     * sets the placement of the label relative to the legend symbol. Possible 
     * values are:
     * <ul>
     *  <li>LP_TOPCENTER
     *  <li>LP_TOPLEFT
     *  <li>LP_TOPRIGHT
     *  <li>LP_RIGHT
     *  <li>LP_LEFT
     *  <li>LP_BOTTOMCENTER
     *  <li>LP_BOTTOMRIGHT
     *  <li>LP_BOTTOMLEFT
     * </ul>
     * <pre>
       +---+---+---+
       | 1 | 0 | 2 |
       +---+---+---+
       | 4 |LEG| 3 |
       +---+---+---+
       | 7 | 5 | 6 |
       +---+---+---+
     </pre>
     * An implementation of the interface may not supoort all positions.
     *
     * @param lablePlacement
     */
    void setLabelPlacement( int lablePosition );

    /**
     * returns the placement of the label relative to the legend symbol. If the
     * element hasn't a label <tt>LegendElement.LP_NOLABEL</tt> will be returned.
     * Otherwise possible values are:
     * <ul>
     *  <li>LP_TOPCENTER
     *  <li>LP_TOPLEFT
     *  <li>LP_TOPRIGHT
     *  <li>LP_RIGHT
     *  <li>LP_LEFT
     *  <li>LP_BOTTOMCENTER
     *  <li>LP_BOTTOMRIGHT
     *  <li>LP_BOTTOMLEFT
     * </ul>
     *
     * @return coded placement of the label relative to the legend symbol
     */
    int getLabelPlacement();

    /**
     * activates or deactivates the label
     *
     * @param active 
     */
    void setActive( boolean active );

    /**
     * exports the <tt>LegendElement</tt> as </tt>BufferedImage</tt>
     *
     * @return 
     */
    BufferedImage exportAsImage() throws LegendException;
}