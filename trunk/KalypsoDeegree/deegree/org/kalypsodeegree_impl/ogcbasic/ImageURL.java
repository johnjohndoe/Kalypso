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
package org.deegree_impl.ogcbasic;

import java.net.URL;


/**
 * 
 * <p>----------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp</a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider</a>
 * @version $Revision$
 */
public class ImageURL extends BaseURL_Impl {
    private int height = 0;
    private int width = 0;

    /**
    * constructor initializing the class with the <LegendURL>
    */
    public ImageURL(int width, int height, String format, URL onlineResource) {
        super( format, onlineResource );
        setWidth( width );
        setHeight( height );
    }

    /**
     * returns the width of the logo image
     */
    public int getWidth() {
        return width;
    }

    /**
    * sets the width of the logo image
    */
    public void setWidth( int width ) {
        this.width = width;
    }

    /**
     * returns the height of the logo image
     */
    public int getHeight() {
        return height;
    }

    /**
    * sets the height of the logo image
    */
    public void setHeight( int height ) {
        this.height = height;
    }

    /**
     *
     *
     * @return 
     */
    public String toString() {
        String ret = null;
        ret = "width = " + width + "\n";
        ret += ( "height = " + height + "\n" );
        return ret;
    }
   
}
