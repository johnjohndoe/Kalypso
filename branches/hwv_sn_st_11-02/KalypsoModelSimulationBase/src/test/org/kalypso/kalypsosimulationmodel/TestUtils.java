package test.org.kalypso.kalypsosimulationmodel;

import java.io.PrintWriter;
import java.io.StringWriter;

/**
 * Holds utily methods for testing
 * 
 * @author Patrice Congo
 *
 */
public class TestUtils
{
	/**
	 * To get the stack trace as string
	 * 
	 * @param th -- the throwable which stack trace is to be  converted
	 * 			into  a string 
	 * @return
	 */
	public static final String getStackTraceAsString(Throwable th)
	{
		if(th==null)
		{
			return null;
		}
		
		StringWriter writer= new StringWriter();
		th.printStackTrace(new PrintWriter(writer));
		return writer.toString();
	}
}
