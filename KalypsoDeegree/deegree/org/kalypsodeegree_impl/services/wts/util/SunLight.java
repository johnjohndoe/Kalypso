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
package org.deegree_impl.services.wts.util;

import javax.vecmath.*;

/**
 *
 * @author  Katharina Lupp
 * @author Andreas Poth
 */
public class SunLight {
    
    public static final float BASE_LIGHT_INTENSITY = 0.5f;    
    
    public synchronized static Vector3f calculateSunlight(double latitude, int year, 
                                                          int month, int date, 
                                                          int hour,  int minute,
                                                          float cloudFactor) {
                                                              
        double vDir = SunPosition.calcVerticalSunposition( latitude, year, month, 
                                                           date, hour, minute );
        float c = 7.25f*((float)Math.sin( vDir ));

        float r = (float)(BASE_LIGHT_INTENSITY + ((c)/16.0) + 0.05)*0.8f;
        float g = (float)(BASE_LIGHT_INTENSITY + ((c)/18.5) + 0.05)*0.8f;
        float b = (float)(BASE_LIGHT_INTENSITY  +((c)/17.0) + 0.05)*0.75f;
        if ( r > 1 ) r = 1;
        if ( g > 1 ) g = 1;
        if ( b > 1 ) b = 1;
        
        Vector3f vec = new Vector3f( r, g, b );
System.out.println(vec);        
        return vec;
    }
    
    public synchronized static float calcSunlightIntensity(double latitude, int year, 
                                                          int month, int date, 
                                                          int hour,  int minute,
                                                          float cloudFactor) {
        Vector3f vec = calculateSunlight( latitude, year, month, date, hour, minute,
                                          cloudFactor );
        return (vec.x + vec.y + vec.z)/3.0f;
    }
        
}
