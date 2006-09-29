package org.kalypso.model.km;

/**
 * 
 * interpolates KM for some profiles at same discharge
 * 
 * @author doemming
 * 
 */
public class MulitKMValue extends AbstractKMValue
{
	private final double m_alpha;

	private final double m_length;

	private final double m_k;

	private final double m_kForeland;

	private final double m_n;

	private final double m_nForeland;

	private AbstractKMValue m_innerKM;

    /*
     * Aggregation of Kalinin-Miljukov parameters for one river strand.
     */
	public MulitKMValue(AbstractKMValue[] values)
	{
		m_innerKM = values[0];
		double length = 0;
		double alpha = 0;
		double k = 0;
		double kForeland = 0;
		double n = 0;
		double nForeland = 0;
		for (int i = 0; i < values.length; i++)
		{
			final AbstractKMValue km = values[i];
			length += km.getLength();
			alpha += km.getAlpha() * km.getLength();
			k += km.getK() * km.getLength();
			kForeland += km.getKForeland() * km.getLength();
			n += km.getN();
			nForeland += km.getNForeland();
		}
		m_alpha = alpha / length;
		m_length = length;
		m_k = k / length;
		m_kForeland = kForeland / length;
		m_n = n;
		m_nForeland = nForeland;

		System.out.println("\nBerechnung KM über " + values.length
				+ " Profile konstanter Abfluss ");
		for (int i = 0; i < values.length; i++)
		{
			System.out.println("Profil #" + i);
			final AbstractKMValue value = values[i];
			System.out.println(value);
		}
		System.out.println("Ergebnis:" + this);
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

	@Override
	public double getQ()
	{
		return m_innerKM.getQ();
	}

	@Override
	public double getQForeland()
	{
		return m_innerKM.getQForeland();
	}

}
