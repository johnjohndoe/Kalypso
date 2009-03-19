/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 * 
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 * 
 *  ---------------------------------------------------------------------------*/
package org.kalypso.gaja3d.service.impl;

import java.util.Calendar;

import org.globus.wsrf.ResourceKey;
import org.globus.wsrf.impl.ResourceHomeImpl;
import org.globus.wsrf.impl.SimpleResourceKey;
import org.globus.wsrf.impl.security.descriptor.ResourceSecurityDescriptor;
import org.globus.wsrf.security.SecurityManager;

public class Gaja3dResourceHome extends ResourceHomeImpl {

	public ResourceKey create() throws Exception {
		// Create a resource and initialize it
		final Gaja3dResource gaja3dResource = (Gaja3dResource) createNewInstance();
		gaja3dResource.initialize();

		// Get key
		final Object id = gaja3dResource.getID();
		final ResourceKey key = new SimpleResourceKey(keyTypeName, id);

		// Security
		final ResourceSecurityDescriptor desc = gaja3dResource
				.getSecurityDescriptor();
		final SecurityManager securityManager = SecurityManager.getManager();
		securityManager.setResourceOwnerFromContext(desc);

		// Add the resource to the list of resources in this home
		add(key, gaja3dResource);

		final Calendar termination = Calendar.getInstance();
		termination.add(Calendar.HOUR, 6);
		gaja3dResource.setTerminationTime(termination);
		gaja3dResource.store();
		return key;
	}

}