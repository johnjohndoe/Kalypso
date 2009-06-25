package timeserieSelection;

public class BceBrowser
{
    private static BceBrowser me=null;
    private CSelectTSFrame browser=null;
    
    private BceBrowser()
    {
	this.browser=new CSelectTSFrame();
	try
	    {
		browser.setIcon(true);
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }
    
    public static BceBrowser getInstance()
    {
	if(me==null)
	    me=new BceBrowser();	      	
	return me;
    }

    public boolean isIcon()
    {
	return browser.isIcon();
    }
  
    public void show()
    {
	try
	    {
		browser.setIcon(false);
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	browser.show();
    }

    public String getSelectedNode()
    {
	if(browser.getSelectedTS()!=null)
	    return browser.getSelectedTSPath()+","+browser.getSelectedTS().m_tableName;
	else
	    return null;
    }
}
