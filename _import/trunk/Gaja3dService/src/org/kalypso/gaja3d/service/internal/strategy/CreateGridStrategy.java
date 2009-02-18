package org.kalypso.gaja3d.service.internal.strategy;

import java.net.URI;
import java.rmi.RemoteException;

public interface CreateGridStrategy {
	public URI createGrid(final URI boundaryLocation,
			final URI demPointsLocation, double dx, double dy)
			throws RemoteException;
}
