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

import java.awt.Color;

import java.util.HashMap;
import java.util.Iterator;

import org.deegree.xml.Marshallable;
import org.deegree_impl.tools.Debug;
import org.deegree.graphics.sld.*;
import org.deegree.model.feature.Feature;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;


/**
 * A Fill allows area geometries to be filled. There are two types of fills:
 * solid-color and repeated GraphicFill. In general, if a Fill element is omitted
 * in its containing element, no fill will be rendered. The default is a solid
 * 50%-gray (color "#808080") opaque fill.
 * <p>
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp</a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider</a>
 * @version $Revision$ $Date$
 */
public class Fill_Impl extends Drawing_Impl implements Fill, Marshallable {   
    
    /**
     * Constructs a new <tt>Fill_Impl</tt>.
     */
    protected Fill_Impl() {
        super( new HashMap(), null );
    }

    /**
     * Constructs a new <tt>Fill_Impl</tt>.
     */
    protected Fill_Impl( HashMap cssParams, GraphicFill graphicFill ) {
        super( cssParams, graphicFill );
    }

    /**
     * Returns the (evaluated) value of the fill's CssParameter 'fill'.
     * <p>
     * @param feature specifies the <tt>Feature</tt> to be used for evaluation
     *        of the underlying 'sld:ParameterValueType'
     * @return the (evaluated) value of the parameter
     * @throws FilterEvaluationException if the evaluation fails or the value
     *         is invalid
     */
    public Color getFill(Feature feature) throws FilterEvaluationException {
        Color awtColor = FILL_DEFAULT;

        CssParameter cssParam = (CssParameter)cssParams.get( "fill" );
 
        if ( cssParam != null ) {
            String s = cssParam.getValue( feature );

            try {
                awtColor = Color.decode( s );
            } catch ( NumberFormatException e ) {
                throw new FilterEvaluationException( "Given value ('" + s + 
                                                     "') for CSS-Parameter 'fill' " + 
                                                     "does not denote a valid color!" );
            }
        }

        return awtColor;
    }
    
    /**
     *  sets the value of the fill's CssParameter 'fill' as a simple color
     *
     * @param color color to be set
     */
    public void setFill( Color color ) {
        
        String hex = StyleFactory.getColorAsHex( color ) ;
        CssParameter fill = StyleFactory.createCssParameter( "fill", hex );
        
        cssParams.put( "fill", fill ); 
    }

    /**
     * Returns the (evaluated) value of the fill's CssParameter 'fill-opacity'.
     * <p>
     * @param feature specifies the <tt>Feature</tt> to be used for evaluation
     *        of the underlying 'sld:ParameterValueType'
     * @return the (evaluated) value of the parameter
     * @throws FilterEvaluationException if the evaluation fails or the value
     *         is invalid
     */
    public double getOpacity( Feature feature ) throws FilterEvaluationException {
        double opacity = OPACITY_DEFAULT;

        CssParameter cssParam = (CssParameter)cssParams.get( "fill-opacity" );

        if ( cssParam != null ) {
            String value = cssParam.getValue( feature );

            try {
                opacity = Double.parseDouble( value );
            } catch ( NumberFormatException e ) {
                throw new FilterEvaluationException( "Given value for parameter 'fill-opacity' ('" + 
                                                     value + "') has invalid format!" );
            }

            if ( ( opacity < 0.0 ) || ( opacity > 1.0 ) ) {
                throw new FilterEvaluationException( "Value for parameter 'fill-opacity' (given: '" + 
                                                     value + "') must be between 0.0 and 1.0!" );
            }
        }

        return opacity;
    }
    
    /**
     * sets the value of the opacity's CssParameter 'opacity' as a value. Valid
     * values ranges from 0 .. 1. If a value < 0 is passed it will be set to 0.
     * If a value > 1 is passed it will be set to 1.
     *
     * @param opacity opacity to be set
     */
    public void setOpacity( double opacity ) {
        
        if ( opacity > 1 ) {
            opacity = 1;
        } else if ( opacity < 0 ) {
            opacity = 0;
        }
        
        CssParameter fillOp = StyleFactory.createCssParameter( "fill-opacity", "" + opacity );
        cssParams.put( "fill-opacity", fillOp );
    }
    
    /**
     * exports the content of the CssParameter as XML formated String
     *
     * @return xml representation of the CssParameter
     */
    public String exportAsXML() { 
        Debug.debugMethodBegin();
        
        StringBuffer sb = new StringBuffer("<Fill>");
        
        if ( graphicFill != null ) {
            sb.append( ((Marshallable)graphicFill).exportAsXML() );
        }
        Iterator iterator = cssParams.values().iterator();
        while ( iterator.hasNext() ) {
            sb.append( ((Marshallable)iterator.next()).exportAsXML() );
        }
        
        sb.append( "</Fill>" );
        
        Debug.debugMethodEnd();
        return sb.toString();
    }
        
}