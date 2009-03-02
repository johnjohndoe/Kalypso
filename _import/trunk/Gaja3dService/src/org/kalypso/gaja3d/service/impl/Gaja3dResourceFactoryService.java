package org.kalypso.gaja3d.service.impl;

import java.net.URL;
import java.rmi.RemoteException;

import org.apache.axis.AxisFault;
import org.apache.axis.MessageContext;
import org.apache.axis.message.addressing.EndpointReferenceType;
import org.globus.wsrf.ResourceContext;
import org.globus.wsrf.ResourceKey;
import org.globus.wsrf.container.ServiceHost;
import org.globus.wsrf.utils.AddressingUtils;
import org.kalypso.gaja3d.service.factory.stubs.CreateResource;
import org.kalypso.gaja3d.service.factory.stubs.CreateResourceResponse;

public class Gaja3dResourceFactoryService {

	/* Implementation of createResource Operation */
	public CreateResourceResponse createResource(CreateResource request)
			throws RemoteException {
		ResourceKey key = null;

		/* First, we create a new Gaja3dResource through the Gaja3dResourceHome */
		try {
			final ResourceContext ctx = ResourceContext.getResourceContext();
			final Gaja3dResourceHome home = (Gaja3dResourceHome) ctx
					.getResourceHome();
			key = home.create();
		} catch (final Throwable e) {
			throw new AxisFault("Could not create resource key.", e);
		}

		EndpointReferenceType epr = null;

		/*
		 * We construct the instance's endpoint reference. The instance's
		 * service path can be found in the WSDD file as a parameter.
		 */
		try {
			final URL baseURL = ServiceHost.getBaseURL();
			final String instanceService = (String) MessageContext
					.getCurrentContext().getService().getOption("instance");
			final String instanceURI = baseURL.toString() + instanceService;
			// The endpoint reference includes the instance's URI and the
			// resource key
			epr = AddressingUtils.createEndpointReference(instanceURI, key);
		} catch (final Exception e) {
			throw new AxisFault("Could not create EPR.", e);
		}

		/* Finally, return the endpoint reference in a CreateResourceResponse */
		final CreateResourceResponse response = new CreateResourceResponse();
		response.setEndpointReference(epr);
		return response;
	}
}