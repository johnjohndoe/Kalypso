package org.kalypso.statistics.tools;

import org.kalypso.statistics.types.data.TimeserieProfile;

public class GumbelDistributionProcessor implements IDistributionParams {

	private final TimeserieProfile m_timeserieProfile;
	private final boolean m_isPartialSerie;

	public GumbelDistributionProcessor(final TimeserieProfile timeserieProfile, boolean isPartialSerie) {
		m_timeserieProfile = timeserieProfile;
		m_isPartialSerie = isPartialSerie;
	}

	@Override
	public double[] getParameters() {
		// TODO Auto-generated method stub

		// there are two different methods of calculation, depending on
		// m_isPartialSerie type; see Schema 7 and 8

		return null;
	}

}
