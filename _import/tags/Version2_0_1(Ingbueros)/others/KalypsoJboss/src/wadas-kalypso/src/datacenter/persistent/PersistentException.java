package datacenter.persistent;

/**
 * 
 * Persistent Exception for persistent related exceptions
 * 
 * @author Marc Schlienger
 */
public class PersistentException extends Exception
{
	private String m_ErrorString;

	public PersistentException(String str)
	{
		super(str);
	}

	public PersistentException(int id)
	{
		super("Object invalid exception: object id=" + id);
	}
}
