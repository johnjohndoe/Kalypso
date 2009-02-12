package de.tuhh.kalypso.data;

/** The class Wc holds the data for a watercours, it consists of the wcIndex and
 * a user defined wcName. It is used to make a strand unique in a watercouse system.
 * If there are watercourses with the same WcIndex and a differne wcName they are
 * considered different watercourse.
 *
 * @author <A HREF="mailto:c.kuepferle@tuhh.de">Christoph Küpferle</A>
 * @version Wc.java,v 1.0 2002/07/01
 */
public class Wc
{
	/** This string holds the official watercourse index of this watercourse.
	 * @param m_wcIndex*/
	private String m_wcIndex = null;
	/** This string holds the user defined name of this watercourse.
	 * @param m_wcName*/
	private String m_wcName = null;
	
	public Wc( String wcIndex )
	{
		this.m_wcIndex = wcIndex;
	}
	public Wc( String wcName, String wcIndex )
	{
		this.m_wcName = wcName;
		this.m_wcIndex = wcIndex;
	}
	// Standardkonstruktor
	/** Returns the wcIndex of this watercourse.
	 * @return m_wcIndex*/
	public String getWcIndex() { return m_wcIndex; }
	/** Returns the wcName of this watercourse.
	 * @return m_wcName*/
	public String getWcName() { return m_wcName; }
	/** Returns a String from this watercourse as wcIndex and wcName.
	 * @return string */
	public String toString()
	{
		return getWcIndex() + getWcName();
	}// toString
	/** This method cenerates a unique Hash Code for a watercourse object. It is
	 * composed of the watercourse index puls the watercourse name string.
	 * @return hashCode The unique hash code of a watercourse object.
	 */
	public int hashCode()
	{
		String hashString = m_wcIndex + m_wcName;
		return hashString.hashCode();
	}// hashCode
	
} // class Watercours Wc



