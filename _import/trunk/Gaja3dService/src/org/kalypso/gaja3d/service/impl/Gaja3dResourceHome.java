package org.kalypso.gaja3d.service.impl;

import org.globus.wsrf.ResourceKey;
import org.globus.wsrf.impl.ResourceHomeImpl;
import org.globus.wsrf.impl.SimpleResourceKey;

public class Gaja3dResourceHome extends ResourceHomeImpl {

	public ResourceKey create() throws Exception {
		// Create a resource and initialize it
		Gaja3dResource mathResource = (Gaja3dResource) createNewInstance();
		mathResource.initialize();
		// Get key
		ResourceKey key = new SimpleResourceKey(keyTypeName, mathResource
				.getID());
		// Add the resource to the list of resources in this home
		add(key, mathResource);
		return key;
	}

}