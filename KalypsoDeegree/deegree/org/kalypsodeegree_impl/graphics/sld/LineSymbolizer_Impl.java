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
package org.deegree_impl.graphics.sld;

import org.deegree.graphics.sld.Geometry;
import org.deegree.graphics.sld.LineSymbolizer;
import org.deegree.graphics.sld.Stroke;
import org.deegree.xml.Marshallable;
import org.deegree_impl.tools.Debug;


/**
 * Used to render a "stroke" along a linear geometry. If a point geometry is
 * used, it should be interpreted as a line of zero length and two end caps. If
 * a polygon is used, then its closed outline is used as the line string (with
 * no end caps). A missing Geometry element selects the default geometry. A
 * missing Stroke element means that nothing will be plotted.
 * <p>----------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp</a>
 * @version $Revision$ $Date$
 */
public class LineSymbolizer_Impl extends Symbolizer_Impl 
                                 implements LineSymbolizer, Marshallable {
    private Stroke stroke = null;

    /**
     * Creates a new LineSymbolizer_Impl object.
     */
    public LineSymbolizer_Impl() {
        super( );

        Stroke stroke = new Stroke_Impl();
        setStroke( stroke );
    }
 
    /**
     * constructor initializing the class with the <LineSymbolizer>
     */
    LineSymbolizer_Impl( Stroke stroke, Geometry geometry, double min, double max ) {
        super( geometry );
        setStroke( stroke );
        setMinScaleDenominator( min );
        setMaxScaleDenominator( max );
    }

    /**
     * A Stroke allows a string of line segments (or any linear geometry) to be
     * rendered. There are three basic types of strokes: solid Color, GraphicFill
     * (stipple), and repeated GraphicStroke. A repeated graphic is plotted
     * linearly and has its graphic symbol bended around the curves of the line
     * string. The default is a solid black line (Color "#000000").
     * @return the Stroke
     */
    public Stroke getStroke() {
        return stroke;
    }

    /**
     * sets the <Stroke>
     * @param stroke the Stroke
     */
    public void setStroke( Stroke stroke ) {
        this.stroke = stroke;
    }
    
    /**
     * exports the content of the LineSymbolizer as XML formated String
     *
     * @return xml representation of the LineSymbolizer
     */
    public String exportAsXML() {
        Debug.debugMethodBegin();
        
        StringBuffer sb = new StringBuffer(1000);
        sb.append( "<LineSymbolizer>" );
        if ( geometry != null ) {
            sb.append( ((Marshallable)geometry).exportAsXML() );
        }  
        if ( stroke != null ) {
            sb.append( ((Marshallable)stroke).exportAsXML() );
        }
        sb.append( "</LineSymbolizer>" );
        
        Debug.debugMethodEnd();
        return sb.toString();
    }
}