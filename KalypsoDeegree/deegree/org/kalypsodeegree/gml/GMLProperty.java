
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
public interface GMLProperty {
    
    public static final int STRING              = 0;
    public static final int GEOMETRY            = 1;
    public static final int POINT               = 2;
    public static final int LINESTRING          = 3;
    public static final int POLYGON             = 4;
    public static final int MULTIPOINT          = 5;
    public static final int MULTILINESTRING     = 6;
    public static final int MULTIPOLYGON        = 7;
    public static final int MULTIGEOMETRY       = 8;
    public static final int FEATURE             = 9;
    public static final int FEATURECOLLECTION   = 10;
    public static final int BOX                 = 11;
    public static final int UNKNOWNTYPE         = 12;
    
    /**
     * returns the name of the property
     */
    public String getName();
    
    /**
     * returns the type of a property. <p>this method may be removed
     * in future definitions of this interface because this can be
     * determined by validiating the property name against the
     * describing schema.</p>
     */
    public int getPropertyType();
    
    /**
     * returns the value of the property
     */
    public Object getPropertyValue();
    
    /**
     * @see #getPropertyValue<p>
     * This set-method only supports text propetries. More complex
     * property types must be inherited from this interface and
     * define additional set-methods.</p>
     */
    public void setPropertyValue(String value);
    
}
/*
 * Changes to this class. What the people haven been up to:
 *
 * $Log$
 * Revision 1.1  2004/05/11 16:43:22  doemming
 * Initial revision
 *
 * Revision 1.2  2004/01/26 08:15:36  poth
 * no message
 *
 * Revision 1.1.1.1  2002/09/25 16:01:46  poth
 * no message
 *
 * Revision 1.4  2002/08/19 15:59:20  ap
 * no message
 *
 * Revision 1.3  2002/07/13 13:54:47  ap
 * no message
 *
 * Revision 1.2  2002/06/03 16:01:42  ap
 * no message
 *
 * Revision 1.1  2002/04/04 16:17:15  ap
 * no message
 *
 * Revision 1.4  2001/12/04 17:22:22  ap
 * no message
 *
 * Revision 1.3  2001/11/23 10:40:53  axel
 * as: CVS change-log comment added
 *
 *
 */
