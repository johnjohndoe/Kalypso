package org.kalypso.gaja3d.service.impl;

import org.globus.wsrf.ResourceKey;
import org.globus.wsrf.impl.ResourceHomeImpl;
import org.globus.wsrf.impl.SimpleResourceKey;
import org.globus.wsrf.impl.security.descriptor.ResourceSecurityDescriptor;
import org.globus.wsrf.security.SecurityManager;

public class Gaja3dResourceHome extends ResourceHomeImpl {

	public ResourceKey create() throws Exception {
		// Create a resource and initialize it
		Gaja3dResource gaja3dResource = (Gaja3dResource) createNewInstance();
		gaja3dResource.initialize();
		// Get key
		ResourceKey key = new SimpleResourceKey(keyTypeName, gaja3dResource
				.getID());

		// Security
		ResourceSecurityDescriptor desc = gaja3dResource
				.getSecurityDescriptor();
		SecurityManager.getManager().setResourceOwnerFromContext(desc);

		// Add the resource to the list of resources in this home
		add(key, gaja3dResource);
		return key;
	}

}