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
package org.deegree_impl.model.cv;

import org.deegree.tools.ParameterList;
import org.deegree.model.coverage.Tile;
import org.deegree.model.coverage.Level;
import org.deegree.model.coverage.Directory;


/**
 *
 * <p> --------------------------------------------------------------------- </p>
 * @author Andreas Poth
 * @version 31.10.2002
 */
class Level_Impl implements Level {
       
    private double minScale             = 0;
    private double maxScale             = 0;
    private ParameterList properties    = null;
    private Tile[] tiles                = new Tile[0];  
    private Directory[] directories     = new Directory[0];

    Level_Impl(double minScale, double maxScale, ParameterList properties,
               Tile[] tiles) {
         this.minScale = minScale;
         this.maxScale = maxScale;
         this.properties = properties;
         this.tiles = tiles;
    }
    
    Level_Impl(double minScale, double maxScale, ParameterList properties,
               Directory[] directories) {
         this.minScale = minScale;
         this.maxScale = maxScale;
         this.properties = properties;
         this.directories = directories;
    }
    
    /**
     * returns the minimum valid scale of the <tt>Level</tt> and the embeded
     * <tt>Levels</tt>
     */
    public double getMinScale()
    {
        return minScale;
    }
    
    /**
     * returns the maximum valid scale of the <tt>Level</tt> and the embeded
     * <tt>Levels</tt>
     */
    public double getMaxScale()
    {
        return maxScale;
    }
    
    /**
     * returns a list of properties assigned to the <tt>Level</tt>. The properties
     * may be overwritten by the embeded <tt>Level</tt>s.
     */
    public ParameterList getProperties()
    {
        return properties;
    }
    
    /**
     * returns the Tiles assigned to a <tt>Level</tt> if the <tt>Level</tt> is
     * of TILE type.
     */
    public Tile[] getTiles()
    {
        return tiles;
    }    
    
    /** returns the directories contained within the level
     *
     */
    public Directory[] getDirectories() {
        return directories;
    }
    
}
