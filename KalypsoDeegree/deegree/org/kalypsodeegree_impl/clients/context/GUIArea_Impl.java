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
package org.deegree_impl.clients.context;

import java.util.HashMap;

import org.deegree.clients.context.*;
import org.deegree.xml.Marshallable;


/**
 * this interface describes the content of an area of a GUI. a GUI area contains
 * zero ... n modules described by the <tt>Module</tt> interface. A GUI area may
 * be can be switched to be invisible. indicated by the hidden attribute.
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public class GUIArea_Impl implements GUIArea, Marshallable {
    private HashMap modules = new HashMap();
    private boolean hidden = false;
    private int area = 0;

    /**
     * Creates a new GUIArea_Impl object.
     *
     * @param area 
     * @param hidden 
     * @param modules 
     */
    public GUIArea_Impl( int area, boolean hidden, Module[] modules ) {
        setArea( area );
        setHidden( hidden );
        setModules( modules );
    }

    /**
     * returns area (north, west, east ...)  assigned to an instance
     *
     * @return 
     */
    public int getArea() {
        return area;
    }

    /**
     * sets the name of a module
     *
     * @param area 
     */
    public void setArea( int area ) {
        this.area = area;
    }

    /**
     * returns true if the GUIArea is hidden. 
     *
     * @return 
     */
    public boolean isHidden() {
        return hidden;
    }

    /**
     * sets the GUIArea to be hidden or visible. 
     *
     * @param hidden 
     */
    public void setHidden( boolean hidden ) {
        this.hidden = hidden;
    }

    /**
     * returns a module identified by its name
     *
     * @param name 
     *
     * @return 
     */
    public Module getModule( String name ) {
        return (Module)modules.get( name );
    }

    /**
     * returns all modules of a GUIArea
     *
     * @return 
     */
    public Module[] getModules() {
        Module[] mo = new Module[modules.size()];
        return (Module[])modules.values().toArray( mo );
    }

    /**
     * sets the modules of a GUIArea
     *
     * @param modules 
     */
    public void setModules( Module[] modules ) {
        this.modules.clear();

        if ( modules != null ) {
            for ( int i = 0; i < modules.length; i++ ) {
                this.modules.put( modules[i].getName(), modules[i] );
            }
        }
    }

    /**
     * adds a module to a GUIArea
     *
     * @param module 
     */
    public void addModul( Module module ) {
        modules.put( module.getName(), module );
    }

    /**
     * reomes a module identified by its name from the GUIArea
     *
     * @param name 
     *
     * @return 
     */
    public Module removeModule( String name ) {
        return (Module)modules.remove( name );
    }
    
    public String exportAsXML() {
        return null;
    }
    
}