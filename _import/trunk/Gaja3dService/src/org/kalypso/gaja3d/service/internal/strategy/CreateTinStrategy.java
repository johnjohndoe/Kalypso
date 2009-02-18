package org.kalypso.gaja3d.service.internal.strategy;

import java.net.URI;
import java.rmi.RemoteException;

public interface CreateTinStrategy {
	/**
	 * @param boundaryLocation
	 * @param breaklinesLocation
	 * @return
	 * @throws RemoteException
	 */
	public URI createTin(final URI boundaryLocation,
			final URI breaklinesLocation)
			throws RemoteException;
	

	/**
	 * @param minAngle
	 */
	public void setMinAngle(double minAngle);


	/**
	 * @param maxArea
	 */
	public void setMaxArea(double maxArea);
}
