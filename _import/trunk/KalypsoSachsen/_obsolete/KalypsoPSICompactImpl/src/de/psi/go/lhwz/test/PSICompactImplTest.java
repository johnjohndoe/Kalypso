package de.psi.go.lhwz.test;

import junit.framework.TestCase;
import de.psi.go.lhwz.ECommException;
import de.psi.go.lhwz.PSICompact;
import de.psi.go.lhwz.PSICompactImpl;

/**
 * @author schlienger
 *
 */
public class PSICompactImplTest extends TestCase
{
	PSICompact psi = new PSICompactImpl();
	
	/**
	 * Constructor for PSICompactImplTest.
	 * @param arg0
	 */
	public PSICompactImplTest(String arg0)
	{
		super(arg0);
	}

	public void testDistributeFile()
	{
		// TODO
	}

	public void testRemoveFile()
	{
    // TODO
	}

	public void testGetInfo() throws ECommException
	{
		PSICompact.ObjectInfo[] info = psi.getInfo( PSICompact.TYPE_MEASUREMENT);
		
		for (int i = 0; i < info.length; i++)
		{
			System.out.println( info[i].getId() + " - " + info[i].getDescription() );
		}
	}

	public void testGetArchiveData()
	{
		// TODO
	}

	public void testSetArchiveData()
	{
		// TODO
	}
}
