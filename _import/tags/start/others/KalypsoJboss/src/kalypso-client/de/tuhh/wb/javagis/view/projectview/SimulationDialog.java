package de.tuhh.wb.javagis.view.projectview;

import java.util.Vector;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import javax.swing.JInternalFrame;
import javax.swing.*;
import javax.swing.tree.*;
import java.util.Date;
import java.io.File;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import java.awt.event.MouseListener;
import java.awt.event.MouseEvent;

import javax.swing.event.InternalFrameListener;
import javax.swing.event.InternalFrameEvent;

import de.tuhh.wb.javagis.FileSystemUtils;

import de.tuhh.wb.javagis.Main;

import de.tuhh.wb.javagis.simulation.KonfigWrite;
import de.tuhh.wb.javagis.simulation.TimeSeriesGenerator;

import de.tuhh.kalypso.data.I_FilterImpl;
import de.tuhh.wb.javagis.view.LogView;

import de.tuhh.wb.javagis.xml.KalypsoXmlImportListener;
import de.tuhh.wb.javagis.xml.XmlImport;
import de.tuhh.wb.javagis.xml.GisTransferObject;
import de.tuhh.wb.javagis.data.VersionAccess;
import de.tuhh.wb.javagis.data.VersionAccessImpl;
import de.tuhh.wb.javagis.view.ViewManager;
import de.tuhh.wb.javagis.data.event.VersionListener;
import de.tuhh.wb.javagis.data.event.KalypsoEventManager;
import ejb.event.EJBEvent;
import java.util.HashSet;
import java.util.Collection;
import java.util.Iterator;
import de.tuhh.wb.javagis.SystemExecute;

public class SimulationDialog extends JInternalFrame implements ActionListener,InternalFrameListener,KalypsoXmlImportListener
//, VersionListener
{
    public final String OUT_DIR="results_kalypso";
    private VersionAccess myVersionAccess;
    private String myThemeKey;
    private Object myVersionId;
    private JPanel panel=new JPanel();
    private File xmlTempDir;    
    private File myModelXmlFile;
    private File myNaModelDir;
    private Date myStartDate;
    private Date myEndDate;
    
    private SystemExecute kalypsoExe=null;
    
    static File targetDir=null;
    JButton jTargetDir=new JButton("target directory");
    JButton jStart=new JButton("run simulation");
    JButton jViewLog=new JButton("view log");
    JButton jViewBalance=new JButton("view balance");
    JButton jGraphic=new JButton("graphic");
    JButton jCancel=new JButton("close");
    JFileChooser fileChooser;    

    private HashSet rainStations=null;
    private HashSet tempStations=null;
    
    public SimulationDialog(VersionAccess versionAccess,String themeKey,Object vId)
    {
	super("run simulation on Version: "+versionAccess.getFullName(themeKey,vId),true,true,true,true);
	this.rainStations=new HashSet();
	this.tempStations=new HashSet();
	this.myVersionAccess=versionAccess;
	this.myThemeKey=themeKey;
	this.myVersionId=vId;	
	if(targetDir==null)
	    targetDir=new File(FileSystemUtils.getTempDir(),"kalypso_temp");
	this.fileChooser=new JFileChooser();
	fileChooser.setSelectedFile(targetDir);
	fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
	initMask();
	ViewManager.desktop.add(this);
	setVisible(true);
	setSize(400,300);
	moveToFront();
    }
    
    private void initMask()
    {
	jTargetDir.setText("result-dir:"+targetDir.getPath());
	jTargetDir.setToolTipText("<html>simulation files will be generated this directory <i>"+targetDir.getPath()+"</i></html>");

	panel.add(jTargetDir);
	panel.add(new JLabel(" -> "));
	panel.add(jStart);
	panel.add(new JLabel(" -> "));
	panel.add(jViewLog);
	panel.add(new JLabel(" -> "));
	//	panel.add(jViewBalance);
	panel.add(new JLabel("to visualize open graphic-tool or editor separately "));
	//	panel.add(jGraphic);
	panel.add(jCancel);
	jTargetDir.setActionCommand("chooseTargetDir");
	jTargetDir.addActionListener(this);

	jStart.setActionCommand("start");
	jStart.addActionListener(this);

	jViewLog.setActionCommand("viewLog");
	jViewLog.addActionListener(this);

	jViewBalance.setActionCommand("viewBalance");
	jViewBalance.addActionListener(this);

	jGraphic.setActionCommand("viewGraphic");
	jGraphic.addActionListener(this);

	jCancel.setActionCommand("cancel");
	jCancel.addActionListener(this);

	getContentPane().add(panel);
    }
    
    // ActionListener
    public void actionPerformed(ActionEvent e)
    {
	String command=e.getActionCommand();
	if("cancel".equals(command))
	    {
		dispose();
	    }
	if("chooseTargetDir".equals(command))
	    {
		int returnVal = fileChooser.showDialog(this, "choose target dir");
		if(returnVal == JFileChooser.APPROVE_OPTION)
		    {
			targetDir=fileChooser.getSelectedFile();
			jTargetDir.setText("result-dir:"+targetDir.getPath());
			jTargetDir.setToolTipText("<html>simulation files will be generated this directory <i>"+targetDir.getPath()+"</i></html>");
		    }
	    }
	if("start".equals(command))
	    {
		runSimulation();
	    }
	if("viewLog".equals(command))
	    {		
		LogView.getInstance().show();
		//
		//		File prgDir=new File(targetDir,"out_tis.eik");
		//		FileSystemUtils.execute("grafik.exe",);
		// na-modell/output.err
	    }
	if("viewBalance".equals(command))
	    {
		File resultDir=new File(targetDir,"na-modell");
		FileSystemUtils.executeNoWait("output.res",resultDir);
		// na-modell/output.res 	    
	    }
	if("viewGraphic".equals(command))
	    {
		File resultDir=new File(targetDir,OUT_DIR);
		FileSystemUtils.executeNoWait("grafik.exe",resultDir);
	    }
    }
    
    //VersionListener:
    public void onVersionChanged(EJBEvent event)
    {
	//
    }

    //          Invoked when an internal frame is activated.
    public void internalFrameActivated(InternalFrameEvent e) 
    {}

    //          Invoked when an internal frame has been closed.
    public void internalFrameClosed(InternalFrameEvent e) 
    {
	System.out.println("SimulationDialogClosed");
    }

    //          Invoked when an internal frame is in the process of being closed.
    public void internalFrameClosing(InternalFrameEvent e) 
    {
    }

    //          Invoked when an internal frame is de-activated.
    public void internalFrameDeactivated(InternalFrameEvent e) 
    {}

    //          Invoked when an internal frame is de-iconified.
    public void internalFrameDeiconified(InternalFrameEvent e) 
    {}
    
    //          Invoked when an internal frame is iconified.
    public void internalFrameIconified(InternalFrameEvent e) 
    {}
    
    public void internalFrameOpened(InternalFrameEvent e)  
    {}

    private void runSimulation()
    {
	try
	    {
		//Check: targetDir exists
		if(!targetDir.exists())
		    targetDir.mkdirs();
		File kalypsoTemplate=new File(Main.props.getProperty("template_simulation"));
		System.out.println("copy template from "+kalypsoTemplate.toString());
		LogView.println("preparing workdirectory...");
		LogView.print(" copy files from "+kalypsoTemplate.toString());
		LogView.print(" to "+targetDir.toString()+"... ");
		KonfigWrite.clearSimulationDir(targetDir);
		FileSystemUtils.copyRecursiveDir(kalypsoTemplate,targetDir);
		LogView.println(" copy done");
		this.xmlTempDir=new File(targetDir,"xml_temp");
		if(!xmlTempDir.exists())
		    xmlTempDir.mkdirs();
		File simulationCaseFile=new File(xmlTempDir,"simulationCase.xml");
		myVersionAccess.xmlExport(myThemeKey,myVersionId,simulationCaseFile);
		
		XmlImport simCaseImport=new XmlImport(simulationCaseFile,this);
		
		System.out.println("start simulationcase-import");
		LogView.println("processing simulationcase");
		simCaseImport.start();

		File resultDir=new File(targetDir,"out_tis.eik");
		if(!resultDir.exists())
		    resultDir.mkdirs();

		LogView.println(" starting kalypso-simulation");

		if(kalypsoExe!=null && kalypsoExe.isAlive())
		    {
			kalypsoExe.interrupt();
			kalypsoExe.join(1000l);
		    }
		//		kalypsoExe=new SystemExecute("kalypso.bat",myNaModelDir);
		//		kalypsoExe.run();
		//		kalypsoExe.join();
		
		FileSystemUtils.execute("kalypso.bat",myNaModelDir);
		KonfigWrite.renameOutputFiles(resultDir);
		File resultsDir=KonfigWrite.moveOutputDir(targetDir,"out_tis.eik",OUT_DIR);
		
		JOptionPane.showMessageDialog(null,"results are stored to \n"+resultsDir.toString());

		/*
		  try
		  {			
		  File dir=new File(targetDir,OUT_DIR);
		  if(dir.exists())
		  FileSystemUtils.recursiveDelete(dir);
		  }
		  catch(Exception e)
		  {}// nothing
		  FileSystemUtils.move(targetDir,"out_tis.eik",OUT_DIR);
		*/
	    }	
	catch(Exception e)
	    {
		e.printStackTrace();
		System.out.println("simulation run failed by \""+e.getMessage()+"\"");
	    }
    }

    private static final String InputFilesPrefix="tis_eik";    

    public void importObject(GisTransferObject gto)
    {
	try
	    {
		if("simCase".equals(gto.getTableName()))
		    {
			System.out.println("processing simulationcase-gto");
			Integer modelVersion = new Integer(gto.getSimpleProperty("m_modelVersion"));
			Integer controlVersion = new Integer(gto.getSimpleProperty("m_controlVersion"));
			
			File modelFile=new File(xmlTempDir,"model.xml");
			this.myModelXmlFile=modelFile;
			myVersionAccess.xmlExport("Modell",modelVersion,modelFile);
			XmlImport modelImport=new XmlImport(modelFile,this);
			System.out.println("start control-import");

			File controlFile=new File(xmlTempDir,"control.xml");
			myVersionAccess.xmlExport("Control",controlVersion,controlFile);
			XmlImport controlImport=new XmlImport(controlFile,this);

			LogView.println("start processing control-theme");
			controlImport.start();  // model exists as file
			
			LogView.println("start processing modell-theme");
			modelImport.start();  // controldata is known

		    }
		if("controlData".equals(gto.getTableName()))
		    {
			this.myStartDate=gto.getSimplePropertyAsDate("m_startDate");
			this.myEndDate=gto.getSimplePropertyAsDate("m_endDate");
			
			File startDir=new File(targetDir,"start");
			File naModelDir=new File(targetDir,"na-modell");
			this.myNaModelDir=naModelDir;
			File klimadatDir=new File(targetDir,"klima.dat");
			if(!startDir.exists())
			    startDir.mkdirs();
			if(!naModelDir.exists())
			    naModelDir.mkdirs();
			if(!klimadatDir.exists())
			    klimadatDir.mkdirs();
			KonfigWrite write=new KonfigWrite();

			//			if(Main.props.getProperty("lzpath")!=null)
			//			    gto.addSimpleProperty("lzpath",Main.props.getProperty("lzpath"));

			write.writeKonfig(new File(startDir,"nam.konfig"),gto);	       
			write.writeFalstart(new File(naModelDir,"falstart.lst"),targetDir,gto);
			System.out.println("processing control-gto");

			// generate ASCII-kalypso-Files
			I_FilterImpl filter=new I_FilterImpl();
			(new File(targetDir,"inp.dat")).mkdirs();
			File inpFiles=new File(targetDir,"inp.dat"+File.separator+InputFilesPrefix);
			
			Integer rootNode=new Integer(gto.getSimpleProperty("m_rootNode"));
			LogView.println("generate sub-network for given root-node");
			rainStations.clear();
			tempStations.clear();
			filter.exportASCIIFiles(myModelXmlFile.toString(),inpFiles.getPath(),rootNode,tempStations,rainStations);
		        LogView.println("\nRain-Stations: "+rainStations.toString());
			LogView.println("\nTemp-Stations: "+tempStations.toString());
		    }
		if("node".equals(gto.getTableName()))
		    {
			System.out.println("processing gauchingStation");
			File klimadatDir = new File(targetDir,"klima.dat");
			if(!klimadatDir.exists())
			    klimadatDir.mkdirs();
			TimeSeriesGenerator gauchingSeries=new TimeSeriesGenerator(klimadatDir,myStartDate,myEndDate);
			gauchingSeries.importObject(gto);
			//			rainSeries.generateLeftStations();
		    }
		if("rainStation".equals(gto.getTableName()))
		    {
			System.out.println("processing rainStation");
			File klimadatDir = new File(targetDir,"klima.dat");
			if(!klimadatDir.exists())
			    klimadatDir.mkdirs();
			String fileName=new String(gto.getSimpleProperty("m_fileName"));
			//			if(intersectsString(rainStations,fileName))
			if(fileName!=null && rainStations.contains(fileName))
			    {
				//				LogView.println(fileName+" IS part of network");
				//
				TimeSeriesGenerator rainSeries=new TimeSeriesGenerator(klimadatDir,myStartDate,myEndDate);
				rainSeries.importObject(gto);
				//
			    }
			else
			    LogView.println("rain station "+fileName+" not in network "+rainStations.toString());
		    }
		if("tempStation".equals(gto.getTableName()))
		    {
			System.out.println("processing tempStation");
			File klimadatDir = new File(targetDir,"klima.dat");
			if(!klimadatDir.exists())
			    klimadatDir.mkdirs();
			String fileName=new String(gto.getSimpleProperty("m_fileName"));
			
			//			if(intersectsString(tempStations,fileName))
			if(fileName!=null && tempStations.contains(fileName))			    
			    {
				//				LogView.println(fileName+" IS part of network");
				//
				TimeSeriesGenerator tempSeries=new TimeSeriesGenerator(klimadatDir,myStartDate,myEndDate);
				tempSeries.importObject(gto);
				//
			    }
			else
			    LogView.println("temp station "+fileName+" not in network "+rainStations.toString());

		    }		    
		/* work in progress...
		if("nullStrand".equals(gto.getTableName()) ||
		   "channel".equals(gto.getTableName())    || 
		   "rhb".equals(gto.getTableName())        ||
		   "rht".equals(gto.getTableName()))
		    {
			String strandNr=gto.getSimpleProperty("m_strandNr");
			String qInitial=gto.getSimpleProperty("m_qInitial");
			if(strandNr!= null && qInitial !=null)
			    {
				File lzsimDir = new File(targetDir,"lzsim");				
			    }

			System.out.println("processing tempStation");

			String fileName=new String(gto.getSimpleProperty("m_fileName"));
			
			//			if(intersectsString(tempStations,fileName))
			if(fileName!=null && tempStations.contains(fileName))			    
			    {
				//				LogView.println(fileName+" IS part of network");
				//
				TimeSeriesGenerator tempSeries=new TimeSeriesGenerator(klimadatDir,myStartDate,myEndDate);
				tempSeries.importObject(gto);
				//
			    }
			else
			    LogView.println("temp station "+fileName+" not in network "+rainStations.toString());

		    }		    
		*/
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }
}
