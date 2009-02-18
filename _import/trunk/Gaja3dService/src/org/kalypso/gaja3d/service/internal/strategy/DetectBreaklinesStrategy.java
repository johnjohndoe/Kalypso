package org.kalypso.gaja3d.service.internal.strategy;

import java.net.URI;
import java.rmi.RemoteException;

public interface DetectBreaklinesStrategy {
	public URI detectBreaklines(final URI boundaryLocation,
			final URI demGridLocation) throws RemoteException;

	/**
	 * @param edgeMethod
	 *            the edgeMethod to set
	 */
	public void setEdgeMethod(String edgeMethod);

	/**
	 * @param smoothMethod
	 *            the smoothMethod to set
	 */
	public void setSmoothMethod(String smoothMethod);

	/**
	 * @param smooth
	 *            the smooth to set
	 */
	public void setSmooth(int smooth);

	/**
	 * @param featureMethod
	 *            the featureMethod to set
	 */
	public void setFeatureMethod(String featureMethod);

	/**
	 * @param lowThresh
	 *            the lowThresh to set
	 */
	public void setLowThresh(double lowThresh);

	/**
	 * @param highThresh
	 *            the highThresh to set
	 */
	public void setHighThresh(double highThresh);

	/**
	 * @param distanceTolerance
	 *            the distanceTolerance to set
	 */
	public void setDistanceTolerance(double distanceTolerance);

}
