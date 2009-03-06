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