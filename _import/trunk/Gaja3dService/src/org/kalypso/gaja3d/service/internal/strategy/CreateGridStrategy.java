package org.kalypso.gaja3d.service.internal.strategy;

import java.net.URL;
import java.rmi.RemoteException;

public interface CreateGridStrategy {
	public URL createGrid(final URL boundaryLocation,
			final URL demPointsLocation, double dx, double dy)
			throws RemoteException;
}
