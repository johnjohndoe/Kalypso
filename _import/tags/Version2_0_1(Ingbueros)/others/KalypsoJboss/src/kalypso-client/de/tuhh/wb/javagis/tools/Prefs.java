package de.tuhh.wb.javagis.tools;
//import de.tuhh.wb.jm.Debug;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.Properties;

import javax.swing.JOptionPane;

public class Prefs
{
    private static Prefs instance=null;
    private Properties props=null;
    private File propFile=null;
    private Prefs()
    {
	propFile=new File("jm.conf");
	load();
    }
    
    private static Prefs getInstance()
    {
	if(instance==null)
	    instance=new Prefs();
	return instance;
    }
    
    public static String get(String key)
    {
	return getString(key);
    }
    public static String getString(String key)
    {
	Prefs instance=getInstance();
	String prop=instance.props.getProperty(key);
	if(prop==null)
	    {
		String value=JOptionPane.showInputDialog(null,"enter value for \""+key+"\"");
		instance.props.setProperty(key,value);
		instance.save();
		instance.load();
		return value;
	    }
	else
	    return prop;
    }
    

    public static int getInt(String key)
    {
	return Integer.parseInt(getString(key));
    }

    private void load()
    {
        props=new Properties();
        try
            {
		FileInputStream in=new FileInputStream(propFile);
                props.load(in);
		in.close();
            }
        catch(Exception e)
            {
		try
		    {
			System.out.println("could not read config :-(, use default values");
			save();
		    }
		catch(Exception e2)
		    {
			e2.printStackTrace();
		    }
            }
    }

    private void save()
    {
	try
	    {
		FileOutputStream out=new FileOutputStream(propFile);
		props.store(out,"jsim");
		out.close();
	    }
	catch(Exception e)
	    {
		//Debug.exception(e);
	    }
    }
}
