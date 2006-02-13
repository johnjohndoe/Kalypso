package org.kalypso.model.km;

public abstract class AbstractKMValue
{
	public abstract double getLength();

	public abstract double getAlpha();

	public abstract double getK();

	public abstract double getN();

	public abstract double getKForeland();

	public abstract double getNForeland();

	public abstract double getQ();

	public abstract double getQForeland();

	public double getQSum()
	{
		return getQ() + getQForeland();
	}

	public String toString()
	{
		return "\t qges:" + getQSum() + "\t length:" + getLength() + "\t Q:"
				+ getQ() + "\t k:" + getK() + "\t n:" + getN()
				+ "\t Vorland:  k:" + getKForeland() + "\t n:" + getNForeland();
	}
}
