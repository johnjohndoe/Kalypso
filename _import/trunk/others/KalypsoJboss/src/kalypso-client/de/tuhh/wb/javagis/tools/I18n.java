package de.tuhh.wb.javagis.tools;
//import de.tuhh.wb.jm.Debug;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Properties;

import javax.swing.JOptionPane;

import de.tuhh.wb.javagis.Main;

public class I18n
{
    private static boolean SHOWKEY=false;
    private static boolean LEARN=false;
	//public String learnLangKey=null;
	
    private static I18n instance=null;
	private HashMap translationMap=new HashMap();  // (langKey,Properties)
	//    private Properties props=null;
    private String actualLangKey=null;
	private String originalLanguage="de";
    //private File langFile=null;
	/*
	 public static void setLanguage(String lang)
	 {
	 I18n.lang=lang;
	 getInstance().setLangFile();
	 getInstance().load();
	 }
	 */
	
    /*public static String getLanguage()
	 {
	 return I18n.lang;
	 }
	 */
    private I18n(String langKey)
    {
		load(langKey);
		if(LEARN)
		load(originalLanguage);
		actualLangKey=langKey;
    }
    
    public static I18n sce_getInstance(String langKey){
    	instance = new I18n(langKey);
    	System.out.println("Instance: "+instance.actualLangKey);
    	return instance;
    }
    
    private static I18n getInstance()
    {
		String user = Main.props.getProperty("user");
    	
		if(instance==null)
	    {
			//	String langKey;
			if(user!=null && user.equals("Ingenieurbuero")){
			instance = new I18n("de");
			return instance;
			} else {
			File parentFile=new File("i18n");
			String[] fileNames = parentFile.list();
			String[] possibilities = new String[fileNames.length];
			int count = 0;
			for (int i=0; i<fileNames.length;i++){
				//System.out.println("Files:"+fileNames[i]);
				if (fileNames[i].startsWith("jm-i18n-"))
				{
					possibilities[count] = fileNames[i];
					//System.out.println("Possibilities:"+possibilities[count]);
					count = count + 1;
				}
			}
			//System.out.println("Anzahl languages:"+count);
			Object[] languages = new Object[count+1];
			for (int i=0; i<count;i++)
			{
				//System.out.println(fileNames[i]);
			    String tempLang=possibilities[i].replaceAll("jm-i18n-","");//substring(8,11);
				languages[i]= tempLang.replaceAll("\\.conf","");//substring(8,11);
			}
			languages[count]="New Language";
			String language = (String)JOptionPane.showInputDialog(
				null,
				"Choose a language!",
				"Language",
				JOptionPane.PLAIN_MESSAGE,
				null,
				languages,
				languages[0]);
			if ((language != null) && (language.length() > 0) && !language.equals("New Language"))
			{
				instance=new I18n(language);
				Object[] options = {get("Dia_Yes"),get("Dia_No")};
				//				int n = JOptionPane.showOptionDialog(null,get("LearnDia_Question"),get("LearnDia_Title"),JOptionPane.YES_NO_OPTION,JOptionPane.QUESTION_MESSAGE,null,options,options[1]);
				int n = JOptionPane.showOptionDialog(null,"Do you want to start in LearnMode?","LearnMode?",JOptionPane.YES_NO_OPTION,JOptionPane.QUESTION_MESSAGE,null,options,options[1]);
				switch (n)
				{
					case JOptionPane.NO_OPTION:
						LEARN=false;
						break;
					case JOptionPane.YES_OPTION:
						LEARN=true;
					default:
						break ;
				}
				return instance;
				
			}
			
			if (language.equals("New Language"))
			{
				String newLanguage = null;
				while(newLanguage==null || newLanguage.length()>3)
				{
					newLanguage=(String)JOptionPane.showInputDialog(                  null,
																	"Enter name of language! (3 letter!!)",
																	"Language",
																	JOptionPane.PLAIN_MESSAGE,
																	null,
																	null,
																	null);
					
					if ((newLanguage == null) || (newLanguage.length() > 3))
						JOptionPane.showMessageDialog(null,"3 letter are necessary.","error", JOptionPane.ERROR_MESSAGE);
				}
				instance=new I18n(newLanguage);
				instance.learnAll(instance.originalLanguage,newLanguage);
				return instance;
			}
			System.exit(0);
			}
		}
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
		return getInstance().getHTMLIntern(key);
	}
	
	private String getHTMLIntern(String key)
	{
		
		String prop=((Properties)translationMap.get(actualLangKey)).getProperty(key);
		
		if(prop==null)
		{
			if(LEARN)
			{
				load(originalLanguage);
				String value=learn(key,((Properties)translationMap.get(originalLanguage)),
									   ((Properties)translationMap.get(actualLangKey)));
				save(actualLangKey);
				load(actualLangKey);
				return value;
			}
			else
			    return key+"...";
		}
		else
		{
			StringBuffer stBuff = new StringBuffer(prop);
			stBuff.deleteCharAt(0);
			stBuff.deleteCharAt((stBuff.length()-1));
			if(SHOWKEY)
				return stBuff.toString()+" ["+key+"]";
			else
				return stBuff.toString();
		}
	}
	
	private void load(String langKey)
	{
		Properties props=new Properties();
		File langFile=new File("i18n/","jm-i18n-"+langKey+".conf");
		try
		{
			FileInputStream in=new FileInputStream(langFile);
			props.load(in);
			translationMap.put(langKey,props);
			in.close();
		}
		catch(Exception e)
		{
			try
			{
				System.out.println("could not read config :-(, use default values");
				//props.setProperty("title","java-simulation");
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
	/*
	 private void setLangFile()
	 {
	 if(lang!=null)
	 langFile=new File("i18n/","jm-i18n-"+lang+".conf");
	 else
	 langFile=new File("i18n/","jm-i18n-"+Prefs.getString("LangDefault")+".conf");
	 System.out.println("LangFile:"+langFile.getAbsolutePath().toString());
	 }
	 */
	private void save(String langKey)
	{
		try
		{
			Properties props=(Properties)translationMap.get(langKey);
			File langFile=new File("i18n/","jm-i18n-"+langKey+".conf");
			FileOutputStream out=new FileOutputStream(langFile);
			props.store(out,"jsim");
			out.close();
		}
		catch(Exception e)
		{
			//		Debug.exception(e);
		}
	}
	
	private void learnAll(String orgLangKey,String learnLangKey)
	{
		load(learnLangKey);
		load(orgLangKey);
		Properties orgProps=(Properties)translationMap.get(orgLangKey);
		Properties learnProps=(Properties)translationMap.get(learnLangKey);
		for (Enumeration keys = orgProps.keys(); keys.hasMoreElements();)
		{
			String key=(String)keys.nextElement();
			if(!learnProps.containsKey(key))
			{
				learn(key,orgProps,learnProps);
				save(learnLangKey);
				load(learnLangKey);
				System.out.println(keys.nextElement());
			}
		}
	}
	
	private String learn(String key,Properties orgProps,Properties learnProps)
	{
		String text;
		if(orgProps==null||!orgProps.containsKey(key))
			text=key;
		else
			text="enter value for \""+
				orgProps.getProperty(key)+"\"";
		String value=JOptionPane.showInputDialog(null,text);
		if(value!=null && !value.equals(""))
			learnProps.setProperty(key,"\""+value+"\"");
		return value;
	}
}
