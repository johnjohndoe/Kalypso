package de.tuhh.wb.tools.event;
import java.awt.event.ActionListener;
import java.util.Hashtable;

import javax.swing.JButton;
import javax.swing.JMenuItem;
//import javax.swing.JToolBar;
//import de.tuhh.wb.tools.prefs.Prefs;
//import de.tuhh.wb.jm.Debug;

public class ActionManager
{
    private static ActionManager instance=null;
    //    private File menuFile=new File("jsim-menu.conf");
    private Hashtable topicHash=new Hashtable();

    private ActionManager()
    {}

    public static ActionManager getInstance()
    {
	if(instance==null)
	    instance=new ActionManager();
	return instance;
    }

    private Topic getTopic(String topicName)
    {
	if(!topicHash.containsKey(topicName))
	    topicHash.put(topicName,new Topic(topicName));
	return (Topic)topicHash.get(topicName);
    }

    public void register(String topicName,String actionCommand,ActionListener listener)
    {
	getTopic(topicName).add(actionCommand,listener);
    }
    
    public void unregister(String topicName,String actionCommand,ActionListener listener)
    {
	getTopic(topicName).remove(actionCommand,listener);
    }

    public JButton getJButton(String topicName,String actionCommand)
    {
	return getTopic(topicName).getJButton(actionCommand);
    }

    public JMenuItem getJMenuItem(String topicName,String actionCommand)
    {
	return getTopic(topicName).getJMenuItem(actionCommand);
    }

    /*public JToolBar getJToolBar(String toolBarName)
    {
	String entry=Prefs.get("ToolBar_"+toolBarName);
	String[] list=entry.split(":");
	JToolBar toolBar=new JToolBar();
	for(int i=0;i<list.length;i++)
	    {
		Debug.println("getJToolBar..."+list[i]);
		String[] part=list[i].split("\\."); 		
		toolBar.add(getJButton(part[0],part[1]));
	    }
	return toolBar;
    }*/
    
    
    /*
      public void addToolBar(JToolbar toolBar,String name)
      {}
    */
}
