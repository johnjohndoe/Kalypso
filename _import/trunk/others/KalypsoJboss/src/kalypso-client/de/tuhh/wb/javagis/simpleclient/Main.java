package de.tuhh.wb.javagis.simpleclient;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.util.HashSet;
import java.util.Properties;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;

import com.bce.datacenter.ingres.Database;

import de.tuhh.kalypso.data.I_FilterImpl;
import de.tuhh.wb.javagis.FileSystemUtils;
import de.tuhh.wb.javagis.simulation.KonfigWrite;
import de.tuhh.wb.javagis.simulation.TimeSeriesGenerator;
import de.tuhh.wb.javagis.tools.I18n;
import de.tuhh.wb.javagis.view.LogView;
import de.tuhh.wb.javagis.xml.GisTransferObject;
import de.tuhh.wb.javagis.xml.KalypsoXmlImportListener;
import de.tuhh.wb.javagis.xml.XmlImport;

//import de.tuhh.wb.javagis.simulation.I_FilerImpl;

public class Main extends JFrame implements ActionListener,WindowListener,KalypsoXmlImportListener
{    
    public final String OUT_DIR="results_kalypso";
    
    private final static String CONF_FILE="kalypso.conf";
    private static Main main;
    private JButton start;
    private JButton jGraphic;
    private JButton jLog;

    private HashSet rainStationFileNames=null;
    private HashSet tempStationFileNames=null;

    FileDialog modelVersion;
    RootNodeDialog rootNode;
    FileDialog simulationDir;
    FileDialog lzPathDir;
    VisualizeNodesDialog visualizeNodes;
    VisualizeAreasDialog visualizeAreas;
    ResultDialog nodes;
    CatchmentDialog catchments;
    SimulationTimeDialog times;
    TimeStepDialog timeStep;
    StationFileDialog rainStations;
    StationFileDialog tempStations;
    String templateKalypsoDir;
    String bceDriver;
    String bceUrl;
    String bceUser;
    String bcePass;
    static Properties props;

    public static void main(String[] args) throws Exception
    {
	File configFile=new File("kalypsoMain.conf");
	
	props=new Properties();
	try
	    {
		props.load(new FileInputStream(configFile));
	    }
	catch(Exception e)
	    {
		System.out.println("could not read config :-(");
	    }

	main=new Main();	
    }
    
    public Main()
    {
	super("KalypsoForecast");
	this.rainStationFileNames=new HashSet();
	this.tempStationFileNames=new HashSet();
	start=new JButton("START");
  	start.setActionCommand("startSimulation");
	start.addActionListener(this);

	jGraphic=new JButton(I18n.get("KF_runGraphic"));//"graphic");
	jGraphic.setActionCommand("runGraphicTool");
	jGraphic.addActionListener(this);

	jLog=new JButton(I18n.get("KF_viewLog"));//"view");
  	jLog.setActionCommand("viewLog");
	jLog.addActionListener(this);
	
	getContentPane().setLayout(null);

	modelVersion=new FileDialog("model",I18n.get("KF_ChooseModel"),false);//"model",false);

	rootNode=new RootNodeDialog();
	simulationDir=new FileDialog("resultDir",I18n.get("KF_resultDir"),true);//"resultDir",true);
	lzPathDir=new FileDialog("lzpath",I18n.get("KF_lzSimPath"),true);//"lzpath",true);
	visualizeNodes=new VisualizeNodesDialog();
	visualizeAreas=new VisualizeAreasDialog();
 	nodes=new ResultDialog();
 	catchments=new CatchmentDialog();
	times=new SimulationTimeDialog();
	timeStep=new TimeStepDialog();
	rainStations=new StationFileDialog(StationFileDialog.RAIN_KEY,I18n.get("KF_rainSequence"));
	tempStations=new StationFileDialog(StationFileDialog.TEMP_KEY,I18n.get("KF_temperatureSequence"));

	add(I18n.get("KF_controlParameter"),modelVersion,10,10);
	add(I18n.get("KF_initialCondition"),lzPathDir,10,70);
	add(null,rootNode,10,130);
	add("     ",visualizeAreas,360,295);
	add(I18n.get("KF_parametersToVisualize:"),visualizeNodes,230,295);
	add(I18n.get("KF_resultCatchments"),catchments,360,70);
	add(I18n.get("KF_resultNodes"),nodes,260,70);
	add(I18n.get("KF_timeOfSimulation"),times,500,10);
	add(null,timeStep,500,120);
	add(I18n.get("KF_rainSequenceForecast"),rainStations,10,370);
	add(I18n.get("KF_temperatureSequenceForecast"),tempStations,430,370);
	add(I18n.get("KF_resultDirectory"),simulationDir,500,170);
	add(I18n.get("KF_runSimulation"),start,750,250);
	add(I18n.get("KF_Logging"),jLog,660,250);
	
	/*
        add("control parameter",modelVersion,10,10);
        add("initial condition (path to 'lzsim')",lzPathDir,10,70);
	add(null,rootNode,10,130);
	add("     ",visualizeAreas,360,295);
	add("parameters to visualize:",visualizeNodes,230,295);
	add("result catchments",catchments,350,70);
	add("result nodes",nodes,250,70);
	add("time of simulation",times,500,10);
	add(null,timeStep,500,120);
	add("rain sequence (forecast)",rainStations,10,370);
	add("temperature sequence (forecast)",tempStations,430,370);
	add("result directory",simulationDir,500,170);
	add("run simulation",start,750,250);
	add("logging",jLog,660,250);
	 */
	
	setSize(900,660);
	setVisible(true);	
	loadFromXml();
	addWindowListener(this);
    }

    public void add(String title,JComponent component,int x,int y)
    {
	Dimension dim;
	int height=0;
	if(title!=null)
	    {
		JLabel jTitle=new JLabel(title);
		getContentPane().add(jTitle);
		dim=jTitle.getPreferredSize();
		height=(int)dim.getHeight();
		jTitle.setBounds(x,y,(int)dim.getWidth(),height);
	    }
	dim=component.getPreferredSize();
	getContentPane().add(component);
	component.setBounds(x,y+(int)(1.5*height),(int)dim.getWidth(),(int)dim.getHeight());
    }

    public void actionPerformed(ActionEvent e)
    {
	String command=e.getActionCommand();
	if("startSimulation".equals(command))
	{
	    startSimulation();
	}
	if("viewLog".equals(command))
	{
	    LogView.getInstance().show();
	}

	/*
	  if("runGraphicTool".equals(command))
	  {
	  startSimulation();
	  }
	*/
    }

    //    private static final String InputFilesPrefix="inp.dat"+File.separator+"tis_eik";    
    private static final String InputFilesPrefix="tis_eik";    

    private void startSimulation()
    {
	try
	    {
		GisTransferObject gto=new GisTransferObject("simpleClient","1",true);
		storeToGto(gto);
		I_FilterImpl filter=new I_FilterImpl();
		String modelXmlFile=modelVersion.getFile().toString();
		simulationDir.getFile().mkdirs();
		String tempDir=props.getProperty("template_simulation");

		if(tempDir!=null)
		    {
			File tmpSrcDir=new File(tempDir);
			KonfigWrite.clearSimulationDir(simulationDir.getFile());
			FileSystemUtils.copyRecursiveDir(tmpSrcDir,simulationDir.getFile());
		    }
		
		(new File(simulationDir.getFile(),"inp.dat")).mkdirs();
		File inpFiles=new File(simulationDir.getFile(),"inp.dat"+File.separator+InputFilesPrefix);
		// generate ASCIIFiles

		rainStationFileNames.clear();
		tempStationFileNames.clear();

		filter.exportASCIIFiles(modelXmlFile,inpFiles.getPath(),rootNode.getNode(),tempStationFileNames,rainStationFileNames,rainStationFileNames);		
		
		LogView.println("\nRain-Stations: "+rainStationFileNames.toString());
		LogView.println("\nTemp-Stations: "+tempStationFileNames.toString());
		
		// generate Konfig
		//		GisTransferObject konfigGto=new GisTransferObject("konfigfile","1",true);
		//		GisTransferObject falstartGto=new GisTransferObject("falstartfile","1",true);

		File startDir=new File(simulationDir.getFile(),"start");
		File naModelDir=new File(simulationDir.getFile(),"na-modell");
		File klimadatDir=new File(simulationDir.getFile(),"klima.dat");
		File outputDir=new File(simulationDir.getFile(),"out_tis.eik");
		if(!startDir.exists())
		    startDir.mkdirs();
		if(!naModelDir.exists())
		    naModelDir.mkdirs();
		if(!klimadatDir.exists())
		    klimadatDir.mkdirs();
		if(!outputDir.exists())
		    outputDir.mkdirs();
		KonfigWrite write=new KonfigWrite();
		write.writeKonfig(new File(startDir,"nam.konfig"),gto);	       
		write.writeFalstart(new File(naModelDir,"falstart.lst"),simulationDir.getFile(),gto);
		TimeSeriesGenerator rainSeries=new TimeSeriesGenerator(klimadatDir,gto);

		// parse stations from Model:

		XmlImport modelImport=new XmlImport(modelXmlFile,this);
		modelImport.start();
		//

		rainSeries.generateLeftStations();

		FileSystemUtils.execute("kalypso.bat",naModelDir);

		Vector wFiles=KonfigWrite.q2wTransformation(simulationDir.getFile(),outputDir);

		KonfigWrite.renameOutputFiles(outputDir);

		File resultsDir=KonfigWrite.moveOutputDir(simulationDir.getFile(),"out_tis.eik",OUT_DIR);

		LogView.println("end of simulation: results are stored to "+resultsDir.toString());
		
		File graphicTemplate=new File(resultsDir,"template.tpl");
		KonfigWrite.writeGraphicTemplate(graphicTemplate,resultsDir,gto);
		
		// start graphic-tool
		String args[]=
		    {
			"/V"+graphicTemplate.toString()
		    };
		String comandLine=simulationDir.getFile()+System.getProperty("file.separator")+"tools"+System.getProperty("file.separator")+"grafik.exe";
		FileSystemUtils.executeProcess(comandLine,args,resultsDir);
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
		LogView.println("\n !! simulation run failed by \""+e.getMessage()+"\"");
	    }
    }
    
    private void saveToXml(GisTransferObject gto) throws Exception
    {
	File config=new File(CONF_FILE);
	if(config.exists())
	    config.delete();
	
	FileWriter writer=new FileWriter(config);
	//	writer.write("<?xml version=\"1.0\" encoding=\"utf-8\"?>");
	writer.write("<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>");
	writer.write(gto.toXmlString());
	writer.close();	
	System.out.println("stored forecast configuration");
    }

    private void loadFromXml()
    {
	try
	    {
		File config=new File(CONF_FILE);
		XmlImport configImport=new XmlImport(config,this);
		configImport.start();
            }
        catch(Exception e)
            {
		System.out.println(e.getMessage());
                System.out.println("could not load forecast configuration");
            }
    }
    
    public void importObject(GisTransferObject gto)
    {	
	System.out.println("DEBUG: GTO-TableName:"+gto.getTableName());
	if("KalypsoConfigutarion".equals(gto.getTableName()))
	    {
		modelVersion.loadFromGto(gto);
		rootNode.loadFromGto(gto);
		simulationDir.loadFromGto(gto);

		lzPathDir.loadFromGto(gto);

		rainStations.loadFromGto(gto);
		tempStations.loadFromGto(gto);
		nodes.loadFromGto(gto);
		catchments.loadFromGto(gto);
		visualizeNodes.loadFromGto(gto);
		visualizeAreas.loadFromGto(gto);
		timeStep.loadFromGto(gto);
		times.loadFromGto(gto);
		this.templateKalypsoDir=gto.getSimpleProperty("template_simulation");
		try
		    {
			LogView.print("connect to time series data base ...");
			Database.init(props.getProperty("bce_driver"),
				      props.getProperty("bce_url"),
				      props.getProperty("bce_user"),
				      props.getProperty("bce_pass"));
			LogView.println(" established");				
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
			LogView.println(" failed");		
			LogView.println(" check: properties in file: kalypsoMain.conf");		
			LogView.println(" maybe wrong url: "+props.getProperty("bce_url"));		
		    }
		
	    }
	try
	    {
		if("node".equals(gto.getTableName()))
		    {
			System.out.println("processing gauchingStation");
			File klimadatDir = new File(simulationDir.getFile(),"klima.dat");
			if(!klimadatDir.exists())
			    klimadatDir.mkdirs();
			TimeSeriesGenerator gauchingSeries=new TimeSeriesGenerator(klimadatDir,times.getStartDate(),times.getEndDate());
			gauchingSeries.importObject(gto);
			//			rainSeries.generateLeftStations();
		    }
		if("rainStation".equals(gto.getTableName()))
		    {
			System.out.println("processing rainStation");
			
			File klimadatDir = new File(simulationDir.getFile(),"klima.dat");
			if(!klimadatDir.exists())
			    klimadatDir.mkdirs();
			//			String fileName=new String(gto.getSimpleProperty("m_fileName"));
			String fileName=TimeSeriesGenerator.name2FileName(new String(gto.getSimpleProperty("m_stationName")));
			if(fileName!=null && rainStationFileNames.contains(fileName))
			    {
				TimeSeriesGenerator rainSeries=new TimeSeriesGenerator(klimadatDir,times.getStartDate(),times.getEndDate());
				rainSeries.importObject(gto);
			    }
		    }
		if("tempStation".equals(gto.getTableName()))
		    {
			System.out.println("processing tempStation");
			File klimadatDir = new File(simulationDir.getFile(),"klima.dat");
			if(!klimadatDir.exists())
			    klimadatDir.mkdirs();
			//			String fileName=new String(gto.getSimpleProperty("m_fileName"));
			String fileName=TimeSeriesGenerator.name2FileName(new String(gto.getSimpleProperty("m_stationName")));
			
			if(fileName!=null && tempStationFileNames.contains(fileName))
			    {
				TimeSeriesGenerator tempSeries=new TimeSeriesGenerator(klimadatDir,times.getStartDate(),times.getEndDate());
				tempSeries.importObject(gto);
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
		System.out.println("problem parsing "+gto.getTableName()
				   +"\n check your inputs");
	    }
    }
    
    private void storeToGto(GisTransferObject gto) throws Exception
    {
	gto.addSimpleProperty("template_simulation",this.templateKalypsoDir);
	modelVersion.storeToGto(gto);
	rootNode.storeToGto(gto);
	simulationDir.storeToGto(gto);
	lzPathDir.storeToGto(gto);
	rainStations.storeToGto(gto);
	tempStations.storeToGto(gto);
	nodes.storeToGto(gto);
	catchments.storeToGto(gto);
	visualizeNodes.storeToGto(gto);
	visualizeAreas.storeToGto(gto);
	timeStep.storeToGto(gto);
	times.storeToGto(gto);

	//	if(props.getProperty("lzpath")!=null)
	//	    gto.addSimpleProperty("lzpath",Main.props.getProperty("lzpath"));
    }
    
    public void windowActivated(WindowEvent e) 
    {}
    
    public void windowClosed(WindowEvent e) 
    {}
    
    public void windowClosing(WindowEvent e) 
    {
	try
	    {	
		GisTransferObject gto = new GisTransferObject("KalypsoConfigutarion","1",true);
		storeToGto(gto);
		saveToXml(gto);
	    }
	catch(Exception err)
	    {
		System.out.println(err.getMessage());
		System.out.println("unable to save configuration");
	    }
	System.exit(0);
    }
    
    public void windowDeactivated(WindowEvent e) 
    {}

    public void windowDeiconified(WindowEvent e) 
    {} 
    
    public void windowIconified(WindowEvent e) 
    {}       
    
    public void windowOpened(WindowEvent e) 
    {}
}
