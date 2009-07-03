package org.kalypso.convert.dwd;


public class KrigingRelation
{
	
	private double m_factor;
	final private String m_id;

	public KrigingRelation(final double factor,final String id)
	{
		m_factor=factor;
		m_id=id;
	}
	
	public String getId()
	{
	return m_id;	
	}
	public double getFactor()
	{
		return m_factor;
	}

	/**
	 * @param d
	 */
	public void setFactor(final double factor) 
	{
	   m_factor=factor;
	}
}