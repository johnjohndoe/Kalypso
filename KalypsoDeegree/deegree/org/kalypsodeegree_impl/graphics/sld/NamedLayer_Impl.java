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


import org.deegree.xml.Marshallable;
import org.deegree_impl.tools.Debug;
import org.deegree.graphics.sld.*;


/**
 * A NamedLayer uses the "name" attribute to identify a layer known to the WMS
 * and can contain zero or more styles, either NamedStyles or UserStyles. In the
 * absence of any styles the default style for the layer is used.
 * <p>----------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp</a>
 * @version $Revision$ $Date$
 */
public class NamedLayer_Impl extends Layer_Impl implements NamedLayer, Marshallable {
    /**
    * constructor initializing the class with the <NamedLayer>
    */
    NamedLayer_Impl( String name, LayerFeatureConstraints layerFeatureConstraints, 
                     Style[] styles ) {
        super( name, layerFeatureConstraints, styles );
    }
    
    /**
     * exports the content of the Font as XML formated String
     *
     * @return xml representation of the Font
     */
    public String exportAsXML() {
        Debug.debugMethodBegin();
        
        StringBuffer sb = new StringBuffer(1000);
        sb.append( "<NamedLayer>" );
        sb.append( "<Name>" ).append( name ).append( "</Name>" );
        
        if (layerFeatureConstraints != null) {
        	sb.append( ((Marshallable)layerFeatureConstraints).exportAsXML() );
        } 
        
        for (int i = 0; i < styles.size(); i++) {
            sb.append( ((Marshallable)styles.get(i)).exportAsXML() );
        }
        sb.append( "</NamedLayer>" );
        
        Debug.debugMethodEnd();
        return sb.toString();
    }
}