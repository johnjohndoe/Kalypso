package de.tuhh.wb.javagis.tools;
//import de.tuhh.wb.jm.Debug;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.Properties;
import javax.swing.JOptionPane;

public class I18n
{
    private static I18n instance=null;
    private Properties props=null;
    private static String lang=null;
    private File langFile=null;

    public static void setLanguage(String lang)
    {
	I18n.lang=lang;
	getInstance().setLangFile();
	getInstance().load();
    }

    public static String getLanguage()
    {
	return I18n.lang;
    }
    
    private I18n()
    {
	setLangFile();
	load();
    }
    
    private static I18n getInstance()
    {
	if(instance==null)
	    instance=new I18n();
	return instance;
    }

	
    public static String get(String key)
    {
	String html=getHTML(key);
	StringBuffer stBuff1=new StringBuffer(html);
	int index1 = html.indexOf("<br>");
	if(index1!=-1)
	    {
		stBuff1.replace(index1,index1+4,"\n");
		int index2 = html.indexOf("<br>",(index1+4));
		if(index2!=-1)
		    {
			stBuff1.replace(index2-3,index2+1,"\n");
		    }
	    }
	return stBuff1.toString();
    }
    
    public static String getHTML(String key)
    {
	I18n instance=getInstance();
	String prop=instance.props.getProperty(key);
	if(prop==null)
	    {
		String value=JOptionPane.showInputDialog(null,"enter value for \""+key+"\"");
		instance.props.setProperty(key,"\""+value+"\"");
		instance.save();
		instance.load();
		return value;
	    }
	else
	    {
		StringBuffer stBuff = new StringBuffer(prop);
		stBuff.deleteCharAt(0);
		stBuff.deleteCharAt((stBuff.length()-1));
		return stBuff.toString();
	    }
    }
    
    private void load()
    {
        props=new Properties();
        try
            {
		FileInputStream in=new FileInputStream(langFile);
                props.load(in);
		in.close();
            }
        catch(Exception e)
            {
		try
		    {
			System.out.println("could not read config :-(, use default values");
			props.setProperty("title","java-simulation");
			FileOutputStream out=new FileOutputStream(langFile);
			props.store(out,"jsim");
			out.close();
		    }
		catch(Exception e2)
		    {
			e2.printStackTrace();
		    }
            }
    }

    private void setLangFile()
    {
	if(lang!=null)
	    langFile=new File("i18n/","jm-i18n-"+lang+".conf");
	else
	    langFile=new File("i18n/","jm-i18n-"+Prefs.getString("LangDefault")+".conf");
    }
    
    private void save()
    {
	try
	    {
		FileOutputStream out=new FileOutputStream(langFile);
		props.store(out,"jsim");
		out.close();
	    }
	catch(Exception e)
	    {
		//		Debug.exception(e);
	    }
    }
}
