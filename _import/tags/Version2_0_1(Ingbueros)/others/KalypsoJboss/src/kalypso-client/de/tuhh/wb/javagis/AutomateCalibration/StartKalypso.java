package de.tuhh.wb.javagis.AutomateCalibration;

import java.io.File;
import java.util.Date;
import java.util.HashSet;
import java.util.Vector;
import javax.swing.JOptionPane;
import java.io.FileWriter;

import de.tuhh.kalypso.data.I_FilterImpl;
import de.tuhh.wb.javagis.FileSystemUtils;
import de.tuhh.wb.javagis.Main;
import de.tuhh.wb.javagis.simulation.BlockTimeSeries;
import de.tuhh.wb.javagis.simulation.KonfigWrite;
import de.tuhh.wb.javagis.simulation.TimeSeriesGenerator;
import de.tuhh.wb.javagis.tools.I18n;
import de.tuhh.wb.javagis.xml.GisTransferObject;
import de.tuhh.wb.javagis.xml.KalypsoXmlImportListener;
import de.tuhh.wb.javagis.xml.XmlImport;
import de.tuhh.wb.javagis.tools.xml.ServiceTools;
//import java.io.FileInputStream;
//import java.io.BufferedReader;
//import java.io.FileReader;

public class StartKalypso implements KalypsoXmlImportListener {

	File modelFile = null;
	public File modelxml = null;
	File controlFile = null;
	//File simCaseFile = null;
	File targetDir = null;
	private File myNaModelDir;
	public final String OUT_DIR = "results_kalypso";
	private Date myStartDate;
	private Date myEndDate;

	private HashSet rainStations = null;
	private HashSet tempStations = null;

	private final boolean SHORTTERM = true;
	;
	private final boolean LONGTERM = false;
	private boolean simulationType = SHORTTERM; //shortterm is default

	public boolean isLongTermSimulation() {
		if (simulationType == LONGTERM)
			return true;
		else
			return false;
	}

	public StartKalypso(File modelFile, File controlFile, File targetDir) {
		this.modelFile = modelFile;
		this.controlFile = controlFile;
		//this.simCaseFile = simCaseFile;
		this.targetDir = targetDir;
		this.rainStations = new HashSet();
		this.tempStations = new HashSet();
		//runSimulation();
	}
	public Vector runSimulation(
		boolean saveAll,
		int rootNode,
		Date startDate_pegel,
		Date endDate_pegel) {
		try {
			//Check: targetDir exists
			if (!targetDir.exists())
				targetDir.mkdirs();
			File kalypsoTemplate =
				new File(Main.props.getProperty("template_simulation"));
			System.out.println(
				"copy template from " + kalypsoTemplate.toString());
			System.out.print(I18n.get("LV_SD_runSim1"));
			System.out.print(
				I18n.get("LV_SD_runSim2") + kalypsoTemplate.toString());
			System.out.print(
				I18n.get("LV_SD_runSim3") + targetDir.toString() + "... ");
			KonfigWrite.clearSimulationDir(targetDir);
			FileSystemUtils.copyRecursiveDir(kalypsoTemplate, targetDir);
			System.out.println(I18n.get("LV_SD_runSim4"));
			//this.xmlTempDir=new File(targetDir,"xml_temp");
			//if(!xmlTempDir.exists())
			//xmlTempDir.mkdirs();
			//File simulationCaseFile=new File(xmlTempDir,"simulationCase.xml");
			//myVersionAccess.xmlExport(myThemeKey,myVersionId,simulationCaseFile);

			/*XmlImport simCaseImport = new XmlImport(simCaseFile, this);
			
			System.out.println("start simulationcase-import");
			System.out.println(I18n.get("LV_SD_runSim5"));
			simCaseImport.start();*/

			XmlImport modelImport = new XmlImport(modelFile, this);
			System.out.println("start control-import");
			XmlImport controlImport = new XmlImport(controlFile, this);
			System.out.println(I18n.get("LV_SD_simCase1"));
			controlImport.start(); // model exists as file
			// model-xml-file exists...now generate hydrotofile with xsl-trafo

			System.out.println(I18n.get("LV_SD_simCase2"));
			modelImport.start(); // controldata is known

			File resultDir = new File(targetDir, "out_tis.eik");
			if (!resultDir.exists())
				resultDir.mkdirs();

			System.out.println(I18n.get("LV_SD_runSim6"));

			//modelxml = new File(resultDir,"modelData.xml");
			//FileSystemUtils.copyFile(modelFile,modelxml);

			/*if (kalypsoExe != null && kalypsoExe.isAlive()) {
				kalypsoExe.interrupt();
				kalypsoExe.join(1000l);
			}*/
			//		kalypsoExe=new SystemExecute("kalypso.bat",myNaModelDir);
			//		kalypsoExe.run();
			//		kalypsoExe.join();

			Vector resultData = null;
			File kalypsoInp = new File(kalypsoTemplate, "inp.dat");
			//File kalypsoOut = new File(targetDir, "out_tis.eik");
			File zftFile = new File(kalypsoInp, "tis_eik.zft");
			//File modelFile = new File(kalypsoOut, "model.xml");
			File[] inpFiles = kalypsoInp.listFiles();
			boolean hasZftFile = false;
			for (int i = 0; i < inpFiles.length; i++) {
				//System.out.println(inpFiles[i]);
				if (inpFiles[i].equals(zftFile)) {
					hasZftFile = true;
					System.out.println("tis_eik.zft vorhanden");
					resultData =
						startCalibration(
							saveAll,
							resultDir,
							rootNode,
							startDate_pegel,
							endDate_pegel);
				}
			}
			if (!hasZftFile) {
				System.out.println("tis_eik.zft nicht vorhanden");
				//File[] outFiles = kalypsoOut.listFiles();
				/*for (int i = 0; i < outFiles.length; i++) {
					System.out.println(outFiles[i]);
				}*/
				Object[] options = { I18n.get("Dia_Yes"), I18n.get("Dia_No")};
				int n =
					JOptionPane.showOptionDialog(
						null,
						I18n.get("SD_zft_Question"),
						I18n.get("SD_zft_Title"),
						JOptionPane.YES_NO_OPTION,
						JOptionPane.QUESTION_MESSAGE,
						null,
						options,
						options[0]);

				switch (n) {
					case JOptionPane.NO_OPTION :
						System.out.println(I18n.get("LV_SD_missZft"));
						break;
					case JOptionPane.YES_OPTION :
						File xslFile = new File("xsl", "xml_2_zft.xsl");
						String result =
							ServiceTools.xslTransform(modelFile, xslFile);
						// zft-Datei wird in template-Ordner geschrieben!!
						FileWriter out = new FileWriter(zftFile);
						out.write(result);
						out.close();
						resultData =
							startCalibration(
								saveAll,
								resultDir,
								rootNode,
								startDate_pegel,
								endDate_pegel);
					default :
						break;

				}
			}

			return resultData;

		} catch (Exception e) {
			Vector dummyVector = new Vector();
			e.printStackTrace();
			System.out.println(
				"simulation run failed by \"" + e.getMessage() + "\"");
			return dummyVector;
		}
	}

	private Vector startCalibration(
		boolean saveAll,
		File resultDir,
		int rootNode,
		Date startDate_pegel,
		Date endDate_pegel) {
		FileSystemUtils.execute("kalypso.bat", myNaModelDir);
		KonfigWrite.renameOutputFiles(resultDir);
		String myCommand = "node_discharge.dat";
		String commandLine = null;
		if (saveAll) {
			File resultsDir =
				KonfigWrite.moveOutputDir(targetDir, "out_tis.eik", OUT_DIR);
			modelxml = new File(resultsDir, "modelData.xml");
			try {
				FileSystemUtils.copyFile(modelFile, modelxml);
			} catch (Exception e) {
				e.printStackTrace();
				System.out.println("Can´t copy modelFile");
			}
			commandLine =
				resultsDir.getPath()
					+ System.getProperty("file.separator")
					+ myCommand;
			System.out.println(
				"results are stored to \n" + resultsDir.toString());
		} else {
			//JOptionPane.showMessageDialog(null,"results are stored to \n"+resultsDir.toString());
			commandLine =
				resultDir.getPath()
					+ System.getProperty("file.separator")
					+ myCommand;
			System.out.println(
				"results are stored to \n" + resultDir.toString());
		}
		//byte[] bufferInp = new byte[100];
		//int cInp = 0;

		/*FileInputStream inputStream = new FileInputStream(commandLine);
		while ((cInp = inputStream.read(bufferInp)) != -1) {
			System.out.println("\n\n cInp:" + cInp + "\n------------");
			if (cInp > 0) {
				String outInp = null;
				outInp = new String(bufferInp, 0, cInp);
				System.out.println(outInp);
			}
		}*/
		//read Kalypso output from results file Kalypso
		File outFile = new File(commandLine);
		BlockTimeSeries blockSerie = new BlockTimeSeries();
		Vector allowedKeys = new Vector();
		allowedKeys.addElement(String.valueOf(rootNode));
		blockSerie.importBlockFile(outFile, allowedKeys);
		Vector resultData = new Vector();
		resultData =
			blockSerie.getDischarge(
				String.valueOf(rootNode),
				startDate_pegel,
				endDate_pegel);
		return resultData;
		/*StringTokenizer stringTok;
		Vector data = new Vector();
		Vector resultData = new Vector();
		//only for this example...
		int searchNode = 103;
		BufferedReader lineReader =
			new BufferedReader(new FileReader(commandLine));
		String line;
		while ((line = lineReader.readLine()) != null) {
			stringTok = new StringTokenizer(line);
			while (stringTok.hasMoreTokens()) {
				data.addElement(stringTok.nextToken());
				//System.out.println(stringTok.nextToken());
			}
		}
		//all information stored in Vector data
		//filter data, in resultData only discharge of node 103 and year 2001
		int dataLength = data.size();
		String actualString = null;
		//only for this example...
		int searchPoint = 5; // Jahr 2001
		for (int n = 0; n < dataLength; n++) {
			actualString = (String) data.elementAt(n);
			if (actualString.equals(Integer.toString(searchNode))) {
				System.out.println("Knoten: " + actualString);
				String numData = (String) data.elementAt(n + 2);
				String numYear = (String) data.elementAt(n + 1);
				int int_numData = Integer.parseInt(numData);
				int int_numYear = Integer.parseInt(numYear);
				System.out.println("Anzahl Daten: " + int_numData);
				System.out.println("Jahr: " + int_numYear);
				System.out.println("Wert von n: " + n);
				if (int_numYear == searchPoint) {
					int index_startData = n + 3;
					resultData.addElement((String) data.elementAt(n + 2));
					for (int i = index_startData;
						i < index_startData + int_numData;
						i++) {
						System.out.println((String) data.elementAt(i));
						resultData.addElement(data.elementAt(i));
					}
				}
			}
		}
		return resultData;*/

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

	public static final String InputFilesPrefix = "tis_eik";

	public void importObject(GisTransferObject gto) {
		try {
			if ("simCase".equals(gto.getTableName())) {
				System.out.println("processing simulationcase-gto");
				/*Integer modelVersion =
					new Integer(gto.getSimpleProperty("m_modelVersion"));
				Integer controlVersion =
					new Integer(gto.getSimpleProperty("m_controlVersion"));*/

				/*File modelFile = new File(xmlTempDir, "model.xml");
				this.myModelXmlFile = modelFile;
				myVersionAccess.xmlExport("Modell", modelVersion, modelFile);*/
				XmlImport modelImport = new XmlImport(modelFile, this);
				System.out.println("start control-import");

				/*File controlFile = new File(xmlTempDir, "control.xml");
				myVersionAccess.xmlExport("Control",controlVersion,controlFile);*/
				XmlImport controlImport = new XmlImport(controlFile, this);

				System.out.println(I18n.get("LV_SD_simCase1"));
				controlImport.start(); // model exists as file

				// model-xml-file exists...now generate hydrotofile with xsl-trafo

				System.out.println(I18n.get("LV_SD_simCase2"));
				modelImport.start(); // controldata is known

			}
			if ("controlData".equals(gto.getTableName())) {
				this.myStartDate = gto.getSimplePropertyAsDate("m_startDate");
				this.myEndDate = gto.getSimplePropertyAsDate("m_endDate");

				if (Float.parseFloat(gto.getSimpleProperty("m_timeStep"))
					> 23.9) {
					this.simulationType = LONGTERM;
					System.out.println(
						"es wird eine LANGzeitsimulation durchgefuert");
				} else
					System.out.println(
						"es wird eine KURZzeitsimulation durchgefuert");
				File startDir = new File(targetDir, "start");
				File naModelDir = new File(targetDir, "na-modell");
				this.myNaModelDir = naModelDir;
				File klimadatDir = new File(targetDir, "klima.dat");
				if (!startDir.exists())
					startDir.mkdirs();
				if (!naModelDir.exists())
					naModelDir.mkdirs();
				if (!klimadatDir.exists())
					klimadatDir.mkdirs();
				KonfigWrite write = new KonfigWrite();

				//			if(Main.props.getProperty("lzpath")!=null)
				//			    gto.addSimpleProperty("lzpath",Main.props.getProperty("lzpath"));

				write.writeKonfig(new File(startDir, "nam.konfig"), gto);
				write.writeFalstart(
					new File(naModelDir, "falstart.lst"),
					targetDir,
					gto);
				System.out.println("processing control-gto");

				// generate ASCII-kalypso-Files
				I_FilterImpl filter = new I_FilterImpl();
				(new File(targetDir, "inp.dat")).mkdirs();
				File inpFiles =
					new File(
						targetDir,
						"inp.dat" + File.separator + InputFilesPrefix);

				Integer rootNode =
					new Integer(gto.getSimpleProperty("m_rootNode"));
				System.out.println(I18n.get("LV_SD_controlData1"));
				HashSet rainStationsLongTerm = new HashSet();
				HashSet rainStationsShortTerm = new HashSet();
				rainStations.clear();
				tempStations.clear();
				filter.exportASCIIFiles(
					modelFile.toString(),
					inpFiles.getPath(),
					rootNode,
					tempStations,
					rainStationsShortTerm,
					rainStationsLongTerm);
				if (isLongTermSimulation())
					rainStations = rainStationsLongTerm;
				else
					rainStations = rainStationsShortTerm;

				System.out.println(
					I18n.get("LV_SD_controlData2") + rainStations.toString());
				System.out.println(
					I18n.get("LV_SD_controlData3") + tempStations.toString());
			}
			if ("node".equals(gto.getTableName())) {
				System.out.println("processing gauchingStation");
				File klimadatDir = new File(targetDir, "klima.dat");
				if (!klimadatDir.exists())
					klimadatDir.mkdirs();
				TimeSeriesGenerator gauchingSeries =
					new TimeSeriesGenerator(
						klimadatDir,
						myStartDate,
						myEndDate);
				gauchingSeries.importObject(gto);
				//			rainSeries.generateLeftStations();
			}
			if ("rainStation".equals(gto.getTableName())) {
				System.out.println("processing rainStation");

				File klimadatDir = new File(targetDir, "klima.dat");
				if (!klimadatDir.exists())
					klimadatDir.mkdirs();
				String fileName =
					TimeSeriesGenerator.name2FileName(
						new String(gto.getSimpleProperty("m_stationName")));
				//			if(intersectsString(rainStations,fileName))
				if (fileName != null && rainStations.contains(fileName)) {
					//				System.out.println(fileName+" IS part of network");
					//
					TimeSeriesGenerator rainSeries =
						new TimeSeriesGenerator(
							klimadatDir,
							myStartDate,
							myEndDate);
					if (simulationType == LONGTERM)
						rainSeries.useLongtermData(true);
					rainSeries.importObject(gto);
					//
				} else
					System.out.print(I18n.get("LV_SD_rainStation1") + fileName);
				System.out.println(
					I18n.get("LV_SD_rainStation2") + rainStations.toString());
			}
			if ("tempStation".equals(gto.getTableName())) {
				System.out.println("processing tempStation");
				File klimadatDir = new File(targetDir, "klima.dat");
				if (!klimadatDir.exists())
					klimadatDir.mkdirs();
				String fileName =
					TimeSeriesGenerator.name2FileName(
						new String(gto.getSimpleProperty("m_stationName")));

				//			if(intersectsString(tempStations,fileName))
				if (fileName != null && tempStations.contains(fileName)) {
					//				System.out.println(fileName+" IS part of network");
					//
					TimeSeriesGenerator tempSeries =
						new TimeSeriesGenerator(
							klimadatDir,
							myStartDate,
							myEndDate);
					tempSeries.importObject(gto);
					//
				} else
					System.out.print(I18n.get("LV_SD_tempStation1") + fileName);
				System.out.println(
					I18n.get("LV_SD_tempStation2") + rainStations.toString());

			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}
