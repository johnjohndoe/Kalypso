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

import org.deegree.clients.context.Frontend;
import org.deegree.xml.Marshallable;


/**
 * this class encapsulates the deegree specific extensions of the general 
 * section of a web map context document. this is a description of the GUI 
 * including the used modules (<tt>Frontend</tt>) and the parameters to control 
 * the map view (<tt>Marshallable</tt>).
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public class GeneralExtension implements Marshallable {
    private Frontend frontend = null;
    private MapParameter mapParameter = null;
    private IOSettings iOSettings = null;

    /**
     * Creates a new GeneralExtension object.
     *
     * @param frontend 
     * @param mapParameter 
     */
    public GeneralExtension( IOSettings iOSettings, Frontend frontend, 
    					     MapParameter mapParameter ) {
        setFrontend( frontend );
        setMapParameter( mapParameter );
        setIOSettings(iOSettings);
    }

    /**
     * returns the frontend (GUI) description encapsulating objekt
     *
     * @return 
     */
    public Frontend getFrontend() {
        return frontend;
    }

    /**
     * sets the frontend (GUI) description encapsulating objekt
     *
     * @param frontend <tt>Frontend</tt>
     */
    public void setFrontend( Frontend frontend ) {
        this.frontend = frontend;
    }

    /**
     * returns the parameters describing the control options for the map
     *
     * @return <tt>MapParameter</tt> encapsulating several control params
     */
    public MapParameter getMapParameter() {
        return mapParameter;
    }

    /**
     * sets the parameters describing the control options for the map
     *
     * @param mapParameter <tt>MapParameter</tt> encapsulating several control 
     *                     params
     */
    public void setMapParameter( MapParameter mapParameter ) {
        this.mapParameter = mapParameter;
    }
  
	/**
	 * @return Returns the iOSettings.
	 */
	public IOSettings getIOSettings() {
		return iOSettings;
	}

	/**
	 * @param settings The iOSettings to set.
	 */
	public void setIOSettings(IOSettings settings) {
		iOSettings = settings;
	}
	
	/**
	 *
	 *
	 * @return 
	 */
	public String exportAsXML() {
		return null;
	}

}