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
package org.deegree.services.gazetteer;

import java.net.URL;
import java.util.Date;

import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.services.wcas.metadatadesc.CitedResponsibleParty;

/**
 * 
 * @version $Revision$
 * @author AxxL
 *
 */
public interface SI_LocationInstance {
	
	/**
	 * 
	 * @return
	 */
	String getGeographicIdentifier();
	
	/**
	 * equivalent to RT in a thesaurus.
	 * @return
	 */
	String getAlternativeGeographicIdentifier();
    
    /**
     * returns a unique identifier for a location instance
     */
    String getIdentifier();
	
	/**
	 * for the TemporalExtent
	 * @return
	 */
	Date getBegin();
	
	/**
	 * for the TemporalExtent
	 * @return
	 */
	Date getEnd();
	
	
	/**
	 * TDB - should we support optional multiple bounding boxes or a single
	 * one? Restricted to one to meet UML and minimise semantic variability
	 * between implementations.
	 * @return
	 */
	GM_Point[] getPosition();
	
	/**
	 * 
	 * @return
	 */
	// TODO: GeographicExtent
	GM_Object[] getGeographicExtent();
	
	/**
	 * 
	 * @return
	 */
	CitedResponsibleParty getAdministrator();
	
	/**
	 * 
	 * @return
	 */
	SI_LocationInstance[] getParents();
	
	/**
	 * 
	 * @return
	 */
	SI_LocationInstance[] getChildren();
	
	/**
	 * 
	 * @return
	 */
	SI_LocationType getLocationType();
    
    /**
     * returns an <tt>URL</tt> containing a request against a WFS returning
     * the feature that is the source of this <tt>SI_LocationInstance</tt>
     * 
     * @return
     */
    URL getSourceFeature();
		
}
