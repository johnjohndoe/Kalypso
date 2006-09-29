package org.kalypso.model.km;

/**
 * calculates KM values from 2 rows of 1D-simulation results (QW-table)
 * 
 * @author doemming
 * 
 */
public class KMValue extends AbstractKMValue
{
	private final double m_k;

	private final double m_n;

	private final double m_kForeland;

	private final double m_nForeland;

	private final double m_alpha;

	private final double m_length;

	private final double m_q;

	private final double m_qf;

    /*
     * Calculates the Kalinin-Miljukov parameters for one profile (one set per discharge) 
     */
	public KMValue(double length, Row row1, Row row2)
	{
		final double qm = (row1.getQ() + row2.getQ()) / 2d;
		final double qmForeland = (row1.getQforeland() + row2.getQforeland()) / 2d;
		final double dh = Math.abs(row2.getHNN() - row1.getHNN());
		final double dA = Math.abs(row2.getArea() - row1.getArea());
		final double dAForeland = Math.abs(row2.getAreaForeland()
				- row1.getAreaForeland());
		final double dq = Math.abs(row2.getQ() - row1.getQ());
		final double dqForeland = Math.abs(row2.getQforeland()
				- row1.getQforeland());
// a problem occures, if the slope is negative (first solution: take the absulute values - bacause negative values are very small)
		final double slope = (Math.abs( row1.getSlope() ) + Math.abs( row2.getSlope() )) / 2d;
		final double li = qm * dh / (slope * dq);
		final double ki = (li * dA / dq) / 3600d;
		final double n = length / li;

		final double liForeland = qmForeland * dh / (slope * dqForeland);
		if (Double.isNaN(liForeland))
		{
			m_kForeland = 0;
			m_nForeland = 0;
		} else
		{
			final double kiForeland = (liForeland * dAForeland / dqForeland) / 3600d;
			final double nForeland = length / liForeland;

			m_kForeland = kiForeland;
			m_nForeland = nForeland;
		}
		m_alpha = 1 - qmForeland / (qm + qmForeland);
		m_length = length;
		m_q = qm;
		m_qf = qmForeland;
		m_k = ki;
		m_n = n;
	}

	public double getAlpha()
	{
		return m_alpha;
	}

	public double getKForeland()
	{
		return m_kForeland;
	}

	public double getNForeland()
	{
		return m_nForeland;
	}

	public double getK()
	{
		return m_k;
	}

	public double getN()
	{
		return m_n;
	}

	public double getLength()
	{
		return m_length;
	}

	public double getQ()
	{
		return m_q;
	}

	public double getQForeland()
	{
		return m_qf;
	}

}
