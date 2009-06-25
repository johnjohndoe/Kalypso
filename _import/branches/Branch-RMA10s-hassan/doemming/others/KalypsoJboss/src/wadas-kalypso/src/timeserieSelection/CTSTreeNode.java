package timeserieSelection;

/**
 * @author ingres
 *
 */
public class CTSTreeNode
{
        private CTSStruct m_tsInfo;
	
	public CTSTreeNode( CTSStruct ts )
	{
		m_tsInfo = ts;
	};
	
	public CTSTreeNode(String name, String tableName)
	{
		m_tsInfo = new CTSStruct(name, tableName);
	};
	
	public CTSStruct getTimeserieInfo() { return m_tsInfo; }
	
	public String toString() { return m_tsInfo.m_name; }
};
