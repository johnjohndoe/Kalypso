package org.kalypso.gaja3d.service.internal.strategy;

import java.net.URI;
import java.rmi.RemoteException;

public interface CreateTinStrategy {
	/**
	 * @param boundaryLocation
	 * @return
	 * @throws RemoteException
	 */
	public URI createTin(final URI boundaryLocation) throws RemoteException;

	/**
	 * @param minAngle
	 */
	public void setMinAngle(final double minAngle);

	/**
	 * @param maxArea
	 */
	public void setMaxArea(final double maxArea);

	/**
	 * @param gridLocation
	 */
	void setDemGridLocation(final URI gridLocation);

	/**
	 * @param breaklinesLocation
	 */
	public void setBreaklinesLocation(final URI breaklinesLocation);
}
