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
package org.deegree_impl.services.wts;

import java.util.Calendar;

import javax.media.j3d.Group;
import javax.media.j3d.Light;
import javax.media.j3d.Shape3D;

import org.deegree.services.wts.AtmosphericCondition;
import org.deegree.services.wts.ViewPoint;
import org.deegree.services.wts.WTSScene;

/**
 * 
 * <p>-----------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public class WTSFactory {

    /**
     * creates a WTS scene from its components
     * @param terrain Shape3D object(s) representing the scenes terrain.
     * @param feature feature that shall be published in the scene
     * @param viewPoint object that describes the viewers position and the point he looks at
     * @param calendar describtion of the date and time for which the scene shall be rendered --> light
     * conditions
     * @param conditions atmospheric conditions like fog, dust, rain etc. of the scene
     * @param lights lights in addition to sun and ambient light (e.g. street lights, spots
     * etc.)
     */
    public static WTSScene createWTSScene(Shape3D[] terrain, Group[] feature, 
                                          ViewPoint viewPoint, Calendar calendar, 
                                          AtmosphericCondition[] conditions, 
                                          Light[] lights, Object background) {
                             
        return new WTSScene_Impl( terrain, feature, viewPoint, calendar, 
                                  conditions, lights, background );
    }
        
}
