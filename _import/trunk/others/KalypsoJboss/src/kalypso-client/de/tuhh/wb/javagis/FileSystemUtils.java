package de.tuhh.wb.javagis;

import java.io.File;
import java.io.IOException;
import java.util.Properties;
import java.io.InputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import de.tuhh.wb.javagis.view.LogView;

public class FileSystemUtils
{
    private static final int Linux=1;
    private static final int Dos=2;
    int os;
  
    // template kalypso-files
    private static final File TemplateDir=new File("../kalypso_template");

    // database-export-dir
    private  static final String DbExportDir="db_export";
    private static File dbExportDir=null;
    private static File dbExportDirUnderTemp=null;


    // kalypso-dir
    private static final String KalypsoDir="kalypso";
    private static File kalypsoDir=null;
    private static File kalypsoDirUnderTemp=null;

    // na-work-dir
    private static final String NaWorkDir="simulation";
    private static File naWorkDir=null;

    private static File tempDir=null;

    public static File getTempDir()// throws java.io.IOException
    {
	return new File(System.getProperty("java.io.tmpdir"));
    }

    public static File getDbExportDir() throws java.io.IOException
    {
	if(dbExportDir==null)
	    dbExportDir=new File(getKalypsoDir(),DbExportDir);
	return dbExportDir;
    }

    public static File getDbExportDirUnderTemp() throws java.io.IOException
    {
	if(dbExportDirUnderTemp==null)
	    dbExportDirUnderTemp=new File(getKalypsoDirUnderTemp(),DbExportDir);
	return dbExportDirUnderTemp;
    }



    public static File getKalypsoDir() throws java.io.IOException
    {
	if(kalypsoDir==null)  
	    kalypsoDir=new File(getTempDir(),KalypsoDir);	
	return kalypsoDir;
    }

    public static File getKalypsoDirUnderTemp() throws java.io.IOException
    {
	if(kalypsoDirUnderTemp==null)  
	    kalypsoDirUnderTemp=new File(KalypsoDir);	
	return kalypsoDirUnderTemp;
    }


    public static File getNaWorkDir() throws IOException
    {
	if(naWorkDir==null)
	    naWorkDir=new File(getKalypsoDir(),NaWorkDir);
	return naWorkDir;
    }
  
    public static File getTemplateDir()
    {
	return TemplateDir;
    }

    public static void createTempDirs() throws IOException
    {
	if(!getKalypsoDir().exists())
	    getKalypsoDir().mkdirs();
	if(!getDbExportDir().exists())
	    getDbExportDir().mkdirs();
    }

    public static void removeTempDirs() throws IOException
    {
	recursiveDelete(getKalypsoDir());
    }

    public static void recursiveDelete(File file)
    {
	if(file.isDirectory())
	    {
		File files[]=file.listFiles();
		for(int i=0;i<files.length;i++)
		    recursiveDelete(files[i]);
	    }
	System.out.println("remove "+file);
	file.delete();
    }

    public static void copyRecursiveDir(File src,File dest)  throws java.io.IOException,java.io.FileNotFoundException
    {
       	if(src.isDirectory())
	    {
		if(!dest.exists())
		    dest.mkdirs();
		File files[]=src.listFiles();
		for(int i=0;i<files.length;i++)
		    {
			if(files[i].isDirectory())
			    {
				copyRecursiveDir(files[i],new File(dest,files[i].getName()));
			    }
			if(files[i].isFile())
			    {
				copyFile(files[i],new File(dest,files[i].getName()));
			    }			
		    }
	    }
    }

    public static void copyDir(File src,File dest)
    {}
    
    public static void copyFile(File src,File dest)
	throws java.io.IOException,java.io.FileNotFoundException
    {	
	//System.out.println("\nFrom: "+src.toString());
	byte buffer[]=new byte[65536];
	FileInputStream fis=new FileInputStream(src);
	dest.getParentFile().mkdirs();
	if(dest.createNewFile())
	    {
		FileOutputStream fos=new FileOutputStream(dest);
		int c;
		while( (c=fis.read(buffer))!=-1)
		    fos.write(buffer,0,c);
		fos.close();
		fos=null;
		//System.out.println("To:   "+dest.toString());	
	    }
	fis.close();
	fis=null;	
    }
    
    /*
      public static void execute(String command,File workingDir) // throws Exception
      {
      String commandLine=workingDir.getPath()+System.getProperty("file.separator")+command;
      System.out.println("EXECUTE-Dir: "+workingDir.getPath());
      System.out.println("EXECUTE:  "+commandLine);
      String dummy[]={""};	
      byte buffer[]=new byte[80];
      int c;
      try
      {
      Process process=(Runtime.getRuntime()).exec(commandLine,dummy,workingDir);
      InputStream stream=process.getInputStream();	
      while( (c=stream.read(buffer))!=-1)
      {
      String out=new String(buffer,0,c);
      LogView.print(out);
      }		
      process.waitFor();
      }
      catch(Exception e)
      {
      System.out.println(e.getMessage());
      }
      }
    */
    
    public static void executeNoWait(String comand,File workingDir) // throws Exception
    {
	execute(comand,workingDir);
    }

    public static void execute(String comand,File workingDir) // throws Exception
    {
        String comandLine=workingDir.getPath()+System.getProperty("file.separator")+comand;
        String dummy[]={""};	
	executeProcess(comandLine,dummy,workingDir);
    }

    public static void executeProcess(String comandLine,String args[],File workingDir)
    {
	String dummy[]={""};	
        System.out.println("EXECUTE:  "+comandLine);
	String[] comandArgs=new String[args.length+1];
	comandArgs[0]=comandLine;
	for(int ca=0;ca<args.length;ca++)
	    {
		comandArgs[ca+1]=args[ca];
		System.out.println("EXECUTE args: "+args[ca]);
	    }
        System.out.println("EXECUTE-Dir: "+workingDir.getPath());
	System.out.println("system is <"+System.getProperty("os.name")+">");

	if("Linux".equals(System.getProperty("os.name")))
	    {
		System.out.println("DEBUG: running under linux");		
		return;
	    }
        byte bufferInp[]=new byte[80];
        byte bufferErr[]=new byte[80];
        int cInp=0;
        int cErr=0;
        try
            {
                Process process=(Runtime.getRuntime()).exec(comandArgs,dummy,workingDir);
                System.out.println("inputStream errorStream test V2");

                InputStream streamInp=null;
                InputStream streamErr=null;
                streamInp=process.getInputStream();
                streamErr=process.getErrorStream();
                while(
                      (cInp=streamInp.read(bufferInp))!=-1 ||
                      (cErr=streamErr.read(bufferErr))!=-1)
                    {
                        //                      System.out.println("\n\n cInp:"+cInp+" cErr: "+cErr+"\n----------------");
                        // ERROR:
                        if(cErr>0)
                            {
                                String outErr=new String(bufferErr,0,cErr);
                                //                              System.out.println("ErrorStream: outErr");
                            }
                        // Standart
                        if(cInp>0)
                            {
                                String outInp=new String(bufferInp,0,cInp);
                                LogView.print(outInp);
                            }
                        if(cInp<0)
                            {
                                streamInp.close();
                                streamInp=process.getInputStream();
                                //                              System.out.println("reopened InputStream");
                            }
			if(cErr<0)
                            {
                                streamErr.close();
                                streamErr=process.getErrorStream();
                                //                              System.out.println("reopened ErrorStream");
                            }
                    }
                //              System.out.println("inputStream.closed && errorStream.closed");
                process.waitFor();
                streamInp.close();
                streamErr.close();
                process.destroy();
            }
        catch(Exception e)
            {
                System.out.println(e.getMessage());
            }
    }

    public static String file2String(File file)
    {
	byte bufferInp[]=new byte[10*1024];
        int cInp=0;
	InputStream streamInp=null;
	String result="";
	try
	    {
		streamInp=new FileInputStream(file);
		while((cInp=streamInp.read(bufferInp))!=-1)
		    {
			String outInp=new String(bufferInp,0,cInp);
			result=result+outInp;
		    }
		    streamInp.close();
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	return result;
    }

    public static void move(File dir,String srcName,String destName)
    {
	File srcFile=new File(dir,srcName);
	try
	    {
		if(srcFile.exists())
		    srcFile.renameTo(new File(dir,destName));
	    }
	catch(Exception e)
	    {
		// ignore
	    }
    }
}
