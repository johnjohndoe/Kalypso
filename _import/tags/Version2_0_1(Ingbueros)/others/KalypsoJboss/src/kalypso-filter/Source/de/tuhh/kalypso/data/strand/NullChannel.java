package de.tuhh.kalypso.data.strand;




/** The class NullChannel.java is only created for consistancy. It has no data,
 * meaning there is no retention in this type of strand and the flood function
 * at the in node equals the flood function at the out node.
 * The Type number is 0.
 *
 * @author <A HREF="mailto:c.kuepferle@tuhh.de">Christoph Küpferle</A>
 * @version NullChannel.java,v 1.0 2002/07/01
 */


public class NullChannel extends StrandData
{

	public StrandData getStrandData()
	{
		NullChannel f = this;
		return f;
	}
	/** Returns type number of this type NullChannel. The type number is O.
	 * @return int Type number 0*/
	public int getType() { return 0; }
}

