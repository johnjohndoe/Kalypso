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
package org.deegree_impl.services.gazetteer.capabilities;

import java.util.ArrayList;

import org.deegree.model.geometry.GM_Object;
import org.deegree.services.gazetteer.capabilities.SI_Gazetteer;
import org.deegree.services.gazetteer.SI_LocationType;
import org.deegree.services.wcas.metadatadesc.CitedResponsibleParty;

/**
 * 
 * @version $Revision$
 * @author AxxL
 *
 */
public class SI_Gazetteer_Impl implements SI_Gazetteer {

	String identifier = null;
	GM_Object territoryOfUse = null;
	CitedResponsibleParty custodian = null;
	ArrayList locationTypes = null;
	String scope = null;
	String coordinateSystem = null;

	/**
	 * 
	 *
	 */
	private SI_Gazetteer_Impl() {
		this.locationTypes = new ArrayList();
	}

	/**
	 * 
	 * @param identifier
	 * @param territoryOfUse
	 * @param custodian
	 * @param locationTypes
	 * @param scope
	 * @param coordinateSystem
	 */
	public SI_Gazetteer_Impl(
		String identifier,
		GM_Object territoryOfUse,
		CitedResponsibleParty custodian,
		SI_LocationType[] locationTypes,
		String scope,
		String coordinateSystem) {

		this();
		setIdentifier(identifier);
		setTerritoryOfUse(territoryOfUse);
		setCustodian(custodian);
		setLocationType(locationTypes);
		setScope(scope);
		setCoordinateSystem(coordinateSystem);

	}

	/* (non-Javadoc)
	 * @see org.deegree.services.gazetteer.capabilities.SI_Gazetteer#getIdentifier()
	 */
	public String getIdentifier() {
		return this.identifier;
	}

	/**
	 * 
	 * @param identifier
	 */
	public void setIdentifier(String identifier) {
		this.identifier = identifier;
	}

	/* (non-Javadoc)
	 * @see org.deegree.services.gazetteer.capabilities.SI_Gazetteer#getTerritoryOfUse()
	 */
	public GM_Object getTerritoryOfUse() {
		return this.territoryOfUse;
	}

	/**
	 * 
	 * @param territoryOfUse
	 */
	public void setTerritoryOfUse(GM_Object territoryOfUse) {
		this.territoryOfUse = territoryOfUse;
	}

	/* (non-Javadoc)
	 * @see org.deegree.services.gazetteer.capabilities.SI_Gazetteer#getCustodian()
	 */
	public CitedResponsibleParty getCustodian() {
		return this.custodian;
	}

	/**
	 * 
	 * @param custodian
	 */
	public void setCustodian(CitedResponsibleParty custodian) {
		this.custodian = custodian;
	}

	/* (non-Javadoc)
	 * @see org.deegree.services.gazetteer.capabilities.SI_Gazetteer#getLocationTypes()
	 */
	public SI_LocationType[] getLocationTypes() {
		return (SI_LocationType[])locationTypes.toArray(
			new SI_LocationType[locationTypes.size()]);
	}
	
	/**
	 * 
	 * @param locationType
	 */
	public void addLocationType(SI_LocationType locationType) {
		this.locationTypes.add(locationType);
	}

	/**
	 * 
	 * @param locationTypes
	 */
	public void setLocationType(SI_LocationType[] locationTypes) {
		this.locationTypes.clear();

		if (locationTypes != null) {
			for (int i = 0; i < locationTypes.length; i++) {
				this.locationTypes.add(locationTypes[i]);
			}
		}
	}

	/* (non-Javadoc)
	 * @see org.deegree.services.gazetteer.capabilities.SI_Gazetteer#getScope()
	 */
	public String getScope() {
		return this.scope;
	}

	/**
	 * 
	 * @param scope
	 */
	public void setScope(String scope) {
		this.scope = scope;
	}

	/* (non-Javadoc)
	 * @see org.deegree.services.gazetteer.capabilities.SI_Gazetteer#getCoordinateSystem()
	 */
	public String getCoordinateSystem() {
		return this.coordinateSystem;
	}

	/**
	 * 
	 * @param coordinateSystem
	 */
	public void setCoordinateSystem(String coordinateSystem) {
		this.coordinateSystem = coordinateSystem;
	}
}
