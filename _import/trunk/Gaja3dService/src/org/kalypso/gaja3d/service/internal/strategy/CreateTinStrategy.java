package org.kalypso.gaja3d.service.internal.strategy;

import java.net.URL;
import java.rmi.RemoteException;

public interface CreateTinStrategy {
	/**
	 * @param boundaryLocation
	 * @param breaklinesLocation
	 * @return
	 * @throws RemoteException
	 */
	public URL createTin(final URL boundaryLocation,
			final URL breaklinesLocation)
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
