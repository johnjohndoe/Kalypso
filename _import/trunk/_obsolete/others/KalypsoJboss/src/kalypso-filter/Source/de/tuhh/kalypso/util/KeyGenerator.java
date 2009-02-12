/**
 * KeyGenerator.java
 *
 * @author Christoph Küpferle
 */

package de.tuhh.kalypso.util;

public class KeyGenerator
{
	private static KeyGenerator keyGen = null;
	
	private Integer m_key = null;
	
	private static KeyGenerator getInstance()
	{
		if( keyGen == null )
			keyGen = new KeyGenerator();
		
		return keyGen;
	} // getInstance
	public static String getKey()
	{
		
		if( getInstance().m_key == null )
		{
			Integer key = new Integer( 0 );
			getInstance().m_key = key;
			return key.toString();
		}
		else
		{
			Integer key = getInstance().m_key;
			key = new Integer( key.intValue() + 1 );
			getInstance().m_key = key;
			return key.toString();
		}
	}
}

