package timeserieSelection;

/**
 * @author ingres
 *
 */
public class CTSStruct
{
    public String m_name;
    public String m_tableName;
	
	public CTSStruct(String name, String tableName)
	{
		m_name = name;
		m_tableName = tableName;
	}
	
	public String toString()
	{
		return m_name + '[' + m_tableName + ']';
	}
}
