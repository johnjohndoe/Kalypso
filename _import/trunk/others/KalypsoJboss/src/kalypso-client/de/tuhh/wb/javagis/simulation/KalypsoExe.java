package de.tuhh.wb.javagis.simulation;

public class SystemExecute extends Thread
{
    private String myCommand=null;
    private File myWorkingDir=null;
    
    public void SystemExecute(String command,File workingDir)
    {
	super(command);
	this.myCommand=command;
	this.myWorkingDir=workingDir;
    }

    public void run()
    {
	try
	    {
		String commandLine=workingDir.getPath()+System.getProperty("file.separator")+command;
		System.out.println("EXECUTE-Dir: "+workingDir.getPath());
		System.out.println("EXECUTE:  "+commandLine);
		String dummy[]={""};
		byte bufferInp[]=new byte[80];
		byte bufferErr[]=new byte[80];
		int cInp=0;
		int cErr=0;
		try
		    {
			Process process=(Runtime.getRuntime()).exec(commandLine,dummy,workingDir);
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
			streamInp.close();
			streamErr.close();
			//              System.out.println("inputStream.closed && errorStream.closed");
			process.waitFor();
			process.destroy();
		    }
		catch(IOException e)
		    {
			System.out.println("IOException:"+e.getMessage());
		    }
	    }
	catch(InterruptedException e)
	    {
		System.out.println("InterruptedException: "+e.getMessage());
		return;
	    }	
    }       
}
