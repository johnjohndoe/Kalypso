package de.tuhh.wb.javagis.view.projectview;

import java.io.File;
import de.tuhh.wb.javagis.FileSystemUtils;
import de.tuhh.wb.javagis.Main;
import de.tuhh.wb.javagis.view.LogView;

import de.tuhh.wb.javagis.xml.KalypsoXmlImportListener;
import de.tuhh.wb.javagis.xml.XmlImport;
import de.tuhh.wb.javagis.xml.GisTransferObject;
import de.tuhh.wb.javagis.data.VersionAccess;
import de.tuhh.wb.javagis.data.VersionAccessImpl;
import de.tuhh.wb.javagis.view.ViewManager;
import de.tuhh.wb.javagis.tools.I18n;
import javax.swing.JOptionPane;

public class HwsSimulation implements KalypsoXmlImportListener
{
    private String newProject=null;
    private String newState=null;
    private String newName=null;
    private VersionAccess myVersionAccess=null;

    public HwsSimulation()
    {
	
    }
    private File xmlTempDir;

    public void startSimulation(VersionAccess versionAccess,String themeKey,Object vId) throws Exception
    {
	myVersionAccess=versionAccess;

	newProject=versionAccess.getProjectName(themeKey,vId);
	newState=versionAccess.getState(themeKey,vId);
	newName=versionAccess.getName(themeKey,vId);

	String fileName=Main.props.getProperty("hws_temp");
	if(fileName==null)
	    throw new Exception("hws temp dir is not defined (hws_temp in kalypsoMain.conf)");
	
	xmlTempDir=new File(fileName);

	//Check: targetDir exists
	if(!xmlTempDir.exists())
	    xmlTempDir.mkdirs();

	File systemFile=new File(xmlTempDir,"hwsSystem.xml");
	File szenarioFile=new File(xmlTempDir,"hwsSzenario.xml");
	File logisticsFile=new File(xmlTempDir,"hwsLogistics.xml");
	File combinationFile=new File(xmlTempDir,"hwsCombination.xml");
	File resultsFile=new File(xmlTempDir,"hwsResults.xml");
	if(systemFile.exists())
	    systemFile.delete();
	if(szenarioFile.exists())
	    szenarioFile.delete();
	if(logisticsFile.exists())
	    logisticsFile.delete();
	if(combinationFile.exists())
	    combinationFile.delete();
	if(resultsFile.exists())
	    resultsFile.delete();
	
	File hwsCombinationFile=new File(xmlTempDir,"hwsCombination.xml");
	versionAccess.xmlExport(themeKey,vId,hwsCombinationFile);

	XmlImport combinationImport=new XmlImport(hwsCombinationFile,this);
		
	System.out.println("start hws combination import");
	combinationImport.start();
    }

    public void importObject(GisTransferObject gto)
    {
	try
	    {
		if("combination".equals(gto.getTableName()))
		    {
			System.out.println("processing combination-gto");
			Integer systemVersion = new Integer(gto.getSimpleProperty("co_systemVersion"));
			Integer szenarioVersion = new Integer(gto.getSimpleProperty("co_szenarioVersion"));
			Integer logisticsVersion = new Integer(gto.getSimpleProperty("co_logisticsVersion"));
			
			File systemFile=new File(xmlTempDir,"hwsSystem.xml");
			File szenarioFile=new File(xmlTempDir,"hwsSzenario.xml");
			File logisticsFile=new File(xmlTempDir,"hwsLogistics.xml");
			File resultsFile=new File(xmlTempDir,"hwsResults.xml");

			myVersionAccess.xmlExport("HWS_System",systemVersion,systemFile);
			myVersionAccess.xmlExport("HWS_Szenario",szenarioVersion,szenarioFile);
			myVersionAccess.xmlExport("HWS_Logistics",logisticsVersion,logisticsFile);
			
			de.tuhh.wb.hws.HwsStart.hwsCalculation(systemFile,szenarioFile,logisticsFile,resultsFile);
			
			if(resultsFile.exists())
			    {
				String message="<html>results are stored in file: "+resultsFile.getAbsolutePath()
				    +"<br>create new \"HWS_Results\"-version and use xml import function</html>";
				JOptionPane.showMessageDialog(null,message,"calculation message",JOptionPane.INFORMATION_MESSAGE);
				//myVersionAccess.createVersion(newProject,"HWS_Results",newState,newName,"results of szenario calculation: /"+newProject+"/"+newState+"/"+newName);
				//myVersionAccess.xmlImport("HWS_Results",vId,resultsFile);
			    }
			else
			    {
				String message="result file does not exist";
				JOptionPane.showMessageDialog(null,message,"calculation message",JOptionPane.ERROR_MESSAGE);
			    }
		    }
	    }
	catch(Exception e)
	    {
		String message="sorry, calculation failed by: "+e.getMessage();
		JOptionPane.showMessageDialog(null,message,"calculation message",JOptionPane.ERROR_MESSAGE);
		e.printStackTrace();
	    }
    }
}
