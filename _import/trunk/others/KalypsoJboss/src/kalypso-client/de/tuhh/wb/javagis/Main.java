package de.tuhh.wb.javagis;

import de.tuhh.wb.javagis.view.ViewManager;
import de.tuhh.wb.javagis.view.LogView;
import de.tuhh.wb.javagis.tools.I18n;
import de.tuhh.wb.javagis.view.ConfigurationView;

import de.tuhh.wb.javagis.data.VersionClass;
import de.tuhh.wb.javagis.data.Version;
import de.tuhh.wb.javagis.data.VersionAccessImpl;
import de.tuhh.wb.javagis.data.VersionAccess;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileInputStream;
import java.util.Properties;
import datacenter.persistent.Database;
import timeserieSelection.CSelectTSFrame;
import timeserieSelection.CTSStruct;

import ejb.util.Lookup;

public class Main
{
    public static ViewManager viewManager=null;
    public static VersionClass versionClass=null;
    public static CSelectTSFrame bceBrowser=null;
    public static Properties props=new Properties();
	
	public static ConfigurationView confView;
    
    public static void main(String[] args) throws Exception
    {
		File configFile=new File("kalypsoMain.conf");
		System.out.println("start");
		try
	    {
			props.load(new FileInputStream(configFile));
			Main main=new Main();
	    }
		catch(Exception e)
	    {
			System.out.println("could not read config :-(, use default values");
			//ConfigurationView.getInstance().show();
			
			props.setProperty("bce_driver","ca.edbc.jdbc.EdbcDriver");
			props.setProperty("bce_url","jdbc:edbc://134.28.87.20:WB7/elbe::abwb/INGRES");
			props.setProperty("bce_user","ingres");
			props.setProperty("bce_pass","ingres42");
			props.setProperty("jboss_host","elbe.wb.tu-harburg.de");
			props.setProperty("jboss_port","");
			props.setProperty("jboss_user","kalypso");
			props.setProperty("jboss_pass","kalypso");
			props.setProperty("template_simulation","/tmp/kalypso_template");
			confView = new ConfigurationView(I18n.get("CV_Title"),false);
			confView.show();
			
			/*props.store(new FileOutputStream(configFile),"kalypso_config");
			 System.out.println(props.toString());*/
	    }
		//Main main=new Main();
    }
    
	
    public Main()
    {
	    Lookup.useHost(props.getProperty("jboss_host"));
		viewManager=new ViewManager();
    }
	
    public static void connectBCE() throws Exception
    {
		LogView.print(I18n.get("LV_Main_connect1"));
		Database.init(props.getProperty("bce_driver"),
					  props.getProperty("bce_url"),
					  props.getProperty("bce_user"),
					  props.getProperty("bce_pass"));
		/*
		 Database.init("ca.edbc.jdbc.EdbcDriver",//driver
		 "jdbc:edbc://128.1.5.67:LP7/BCE_PC067::dbflomatis/INGRES",//url
		 "ingres",//user
		 "ingres");//password
		 */
		LogView.println(I18n.get("LV_Main_connect2"));
	    /*
		 catch(Exception e)
		 {
		 e.printStackTrace();
		 LogView.println(I18n.get("LV_Main_connect3"));
		 LogView.println(I18n.get("LV_Main_connect4"));
		 LogView.println(I18n.get("LV_Main_connect5")+props.getProperty("bce_url"));
		 }
		 */
    }
	
    public static void setViewManager(ViewManager viewManager)
    {
		Main.viewManager=viewManager;
    }
    
    public static String toHtml(String text,int width)
    {
		if(text==null)
			text="";
		for(int i=0;i<text.length();i+=width+4)
	    {
			int trim=text.indexOf(" ",i+width);
			if(trim>5)
				text=text.substring(0,trim)+"<br>"+text.substring(trim,text.length());
	    }
		return "<html>"+text+"</html>";
    }
}
