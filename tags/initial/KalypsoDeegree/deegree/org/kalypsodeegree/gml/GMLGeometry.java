
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

package org.deegree.gml;

/**
*
*
* <p>----------------------------------------------------------</p>
* 
* @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
* @version 07.02.2001
* <p>
*/	
public interface GMLGeometry {
    /*#GMLProperty lnkGMLProperty;*/
        
   /**
    * returns the name of the Geometry. As default a geometry is named
    * using the types specified within the geometry.xsd schema file. 
    * But the user is allowed to extend this types to create his
    * own geometries with its own names
    */ 
    public String getName();
    
   /**
    * returns the ID of the geometry
    */ 
    public String getId();
    
   /**
    * @see #getId
    */ 
    public void setId(String id);
    
   /**
    * returns the spatial reference system of the geometry
    */ 
    public String getSrs();
    
   /**
    * @see #getSrs
    */ 
    public void setSrs(String srs);
    
}
/*
 * Changes to this class. What the people haven been up to:
 *
 * $Log$
 * Revision 1.1  2004/05/11 16:43:22  doemming
 * Initial revision
 *
 * Revision 1.1.1.1  2002/09/25 16:01:45  poth
 * no message
 *
 * Revision 1.2  2002/08/19 15:59:20  ap
 * no message
 *
 * Revision 1.1  2002/04/04 16:17:15  ap
 * no message
 *
 * Revision 1.3  2001/11/23 10:40:53  axel
 * as: CVS change-log comment added
 *
 *
 */
