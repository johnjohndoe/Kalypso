package de.tuhh.wb.javagis.view.tableview;

import java.io.FileWriter;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.util.Vector;
import java.util.Enumeration;
import javax.swing.JOptionPane;
import java.io.File;
import de.tuhh.wb.javagis.data.TrippelKeyHash;
import de.tuhh.wb.javagis.view.LogView;
import de.tuhh.wb.javagis.tools.I18n;

public class TableProfile
{
    private final static String CONF_FILE="profile.conf";
    private TrippelKeyHash toHide; // elementKey/propKey

    private static TableProfile myInstance=null;
    private TableProfile()
    {
	this.toHide=new TrippelKeyHash();
	load();
    }
    
    public static TableProfile getInstance()
    {
	if(myInstance==null)
	    myInstance=new TableProfile();
	return myInstance;
    }

    public Vector getProfiles()
    {
	Vector results=new Vector();
	results.add("all");
	for(Enumeration e=toHide.keys();e.hasMoreElements();)
	    results.add(e.nextElement());
	return results;
    }
    
    public void hide(String profile,String elementKey,String propKey)
    {
	//Object[] option = {"OK"};
	if("all".equals(profile))
		
	    JOptionPane.showMessageDialog(null,I18n.get("ProfileDia_hide_Message"),I18n.get("ProfileDia_hide_title"),JOptionPane.INFORMATION_MESSAGE);
	else
	    toHide.put(profile,elementKey,propKey,new Boolean(true));
    }
    
    public String newProfile()
    {
	String profileName=null;
	try
	    {
		profileName=(String)JOptionPane.showInputDialog(null,
							I18n.get("ProfileDia_new_Question"),
							I18n.get("ProfileDia_new_Title"),
							JOptionPane.QUESTION_MESSAGE);
		if(profileName!=null)
		    toHide.put(profileName,"-","-",new Boolean(false));
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
		return null;
	    }
	return profileName;
    }

    public void removeProfile(String profileName)
    {
	int n;
	Object[] options = {I18n.get("Dia_Yes"),I18n.get("Dia_No"),I18n.get("Dia_Cancel")};
	if(profileName!=null && !"all".equals(profileName)){
    n = JOptionPane.showOptionDialog(null,I18n.get("ProfileDia_removeW_Question"),I18n.get("ProfileDia_removeW_Title"),JOptionPane.YES_NO_CANCEL_OPTION,JOptionPane.QUESTION_MESSAGE,null,options,options[1]);

                switch (n)
                    {
                   case JOptionPane.NO_OPTION:
                    break;
                    case JOptionPane.YES_OPTION:
					 toHide.remove(profileName);
					case JOptionPane.CANCEL_OPTION:
					break;
                    default:
                        break ;

                    }
	   
				}
	else
	    JOptionPane.showMessageDialog(null,I18n.get("ProfileDia_removeMS_Message"),I18n.get("ProfileDia_removeW_Title"),JOptionPane.INFORMATION_MESSAGE);
    }

    public void clearProfile(String profile)
    {}
    
    public boolean shouldBeHidden(String profile,String elementKey,String propKey)
    {
	return toHide.containsKey(profile,elementKey,propKey);
    }

    public void load()
    {
	File file=new File(CONF_FILE);
	if(file.exists())
	    {
		LineNumberReader reader=null;
		try
		    {
			reader=new LineNumberReader(new FileReader(file));
			String line;
			while((line=reader.readLine())!=null)
			    {
				if(!line.startsWith("#"))
				    {
					try
					    {
						int trim1=line.indexOf(",");
						int trim2=line.indexOf(",",trim1+1);
						int trim3=line.lastIndexOf(",");
						if(trim1>=0 && trim1<trim2 && trim2<trim3 && trim3>=0)
						    {
							String profile=line.substring(0,trim1);
							String key1=line.substring(trim1+1,trim2);
							String key2=line.substring(trim2+1,trim3);
							hide(profile,key1,key2);
						    }
					    }
					catch(Exception e)
					    {
						e.printStackTrace();
						System.out.println(e.getMessage());
						System.out.println("ignored line: "+line);
					    }
				    }
			    }
			
			reader.close();
		    }
		catch(Exception e)
		    {}
	    }
    }
    
    public void save()
    {
	File file=new File(CONF_FILE);
	Vector lines=toHide.toStrings();
	FileWriter writer=null;
	try
	    {
		writer=new FileWriter(file);
		for(int i=0;i<lines.size();i++)
		    writeln(writer,(String)lines.elementAt(i));
		writer.close();
	    }
	catch(Exception e)
	    {
		JOptionPane.showMessageDialog(null," could not save profile :-(");
	    }
    }
    
    public void writeln(FileWriter writer,String line) throws IOException
    {
	line=line+System.getProperty("line.separator");
	writer.write(line,0,line.length());
    }
}
