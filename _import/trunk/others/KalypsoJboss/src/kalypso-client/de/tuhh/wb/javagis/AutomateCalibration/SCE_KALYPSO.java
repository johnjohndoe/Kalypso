package de.tuhh.wb.javagis.AutomateCalibration;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.StringTokenizer;
import java.util.TimeZone;
import java.util.Vector;
import java.util.TreeMap;
import java.util.Iterator;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.event.InternalFrameEvent;
import javax.swing.event.InternalFrameListener;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import de.tuhh.wb.javagis.FileSystemUtils;
import de.tuhh.wb.javagis.Main;
import de.tuhh.wb.javagis.tools.I18n;
import de.tuhh.wb.javagis.tools.xml.ServiceTools;
import de.tuhh.wb.javagis.view.ViewManager;
import de.tuhh.wb.javagis.simulation.BlockTimeSeries;

public class SCE_KALYPSO
	extends JInternalFrame
	implements InternalFrameListener, ActionListener {

	//Modeldata,Controldata,SimulationCaseData
	//private File xmlDir = new File("C://Kalypso//temp");
	private File modelFile = null; //new File(xmlDir, "model.xml");
	private File controlFile = null; //new File(xmlDir, "control.xml");
	//private File simCaseFile = new File(xmlDir, "simulationCase.xml");
	//results Kalypso
	private File targetDir = null; //new File("C://Kalypso//simulation1");

	private File controlXML = null;
	//new File("C://Kalypso//TestMode//input_Loop.xml");

	//location SCE-Routine
	private String myCommand_SCE = "kalypsoMain.exe";
	private File myWorkingDir_SCE = null; //new File("sce_tool");

	//xml-input File
	private File inputFile = null; //new File(xmlDir, "input_SCE.xml");

	//location xsl-Transformation
	private File xslFile = new File("xsl", "xml_2_scein.xsl");

	private StartKalypso startKalypso;

	private ObjectiveFunction oFunction;

	//Parameter
	//private double[] paramUpperBounds;
	//private double[] paramLowerBounds;
	//private double[] initialParamValues;
	//private double[] synteticParamValues;
	//private String[] xPaths;
	//private double[] startValues;
	//private String[] modes;
	private CalContext[] calContexts;

	private int anzKalypso = 1;
	private int rootNode = 0;
	private double timeStep = 0;
	private Date startDate_calibration;
	private Date endDate_calibration;
	private String gaugeFile = null;
	private boolean syntetic = true;

	private TreeMap observedDischarge;
	private boolean equalWeights = true;
	private boolean[] objectiveFunctions = new boolean[4];
	private double[] trafoConstants = new double[4];
	private double peakFlowLevel = 0;
	private double lowFlowLevel = 0;

	private static SCE_KALYPSO instance = null;

	//View-Elements
	//Buttons
	private JButton btSCEInp = new JButton(I18n.get("SCE_ButtonStarttext"));
	private JButton btModel = new JButton(I18n.get("SCE_ButtonStarttext"));
	private JButton btControl = new JButton(I18n.get("SCE_ButtonStarttext"));
	private JButton btTargetDir = new JButton(I18n.get("SCE_ButtonStarttext"));
	private JButton btStart = new JButton(I18n.get("SCE_ButtonStart"));

	private JButton btControlXML = new JButton(I18n.get("SCE_ButtonStarttext"));
	private JButton btStart_neu = new JButton(I18n.get("SCE_ButtonStart_neu"));

	//Labels
	private JLabel lbSCEInp = new JLabel(I18n.get("SCE_LabelSCEInp"));
	private JLabel lbModel = new JLabel(I18n.get("SCE_LabelModelData"));
	private JLabel lbControl = new JLabel(I18n.get("SCE_LabelControlData"));
	private JLabel lbTargetDir = new JLabel(I18n.get("SCE_LabelTargetDir"));

	private JLabel lbControlXML = new JLabel(I18n.get("SCE_LabelControlXML"));

	//FileChooser
	private JFileChooser sceInpFileChooser = new JFileChooser();
	private JFileChooser modelFileChooser = new JFileChooser();
	private JFileChooser controlFileChooser = new JFileChooser();
	private JFileChooser targetDirFileChooser = new JFileChooser();

	private JFileChooser controlXMLFileChooser = new JFileChooser();

	//Panels
	private JPanel buttonPanel = new JPanel();
	private JPanel startPanel = new JPanel();

	public static void openSCEView() {
		if (instance == null) {
			instance = new SCE_KALYPSO(I18n.get("SCE_WindowTitle"));
		}
		ViewManager.addToDesktop(instance);
		instance.show();
		//	instance.moveToFront();
	}

	private SCE_KALYPSO(String title) {
		super(title, true, true, true, true);

		String autoCal = Main.props.getProperty("automatedCalibration");
		System.out.println("Automated Calibration: " + autoCal);
		if (autoCal.equals("enabled1")) {
			initView();
		}
		if (autoCal.equals("enabled2")) {
			initView2();
		}
		updateStatus();
		pack();
		this.addInternalFrameListener(this);
		this.moveToFront();
	}

	private void initView() {

		btSCEInp.setActionCommand("SCEInp");
		btSCEInp.addActionListener(this);
		btSCEInp.setBackground(Color.white);
		btSCEInp.setBorderPainted(false);
		btSCEInp.setMargin(new Insets(0, 0, 0, 0));

		btModel.setActionCommand("model");
		btModel.addActionListener(this);
		btModel.setBackground(Color.white);
		btModel.setBorderPainted(false);
		btModel.setMargin(new Insets(0, 0, 0, 0));

		btControl.setActionCommand("control");
		btControl.addActionListener(this);
		btControl.setBackground(Color.white);
		btControl.setBorderPainted(false);
		btControl.setMargin(new Insets(0, 0, 0, 0));

		btTargetDir.setActionCommand("targetDir");
		btTargetDir.addActionListener(this);
		btTargetDir.setBackground(Color.white);
		btTargetDir.setBorderPainted(false);
		btTargetDir.setMargin(new Insets(0, 0, 0, 0));

		btStart.setActionCommand("start");
		btStart.addActionListener(this);

		getContentPane().setLayout(new BorderLayout());

		buttonPanel.setLayout(new GridLayout(4, 2));
		buttonPanel.add(lbSCEInp);
		buttonPanel.add(btSCEInp);
		buttonPanel.add(lbModel);
		buttonPanel.add(btModel);
		buttonPanel.add(lbControl);
		buttonPanel.add(btControl);
		buttonPanel.add(lbTargetDir);
		buttonPanel.add(btTargetDir);

		startPanel.setLayout(new BorderLayout());
		startPanel.add(btStart, BorderLayout.CENTER);

		getContentPane().add(buttonPanel, BorderLayout.CENTER);
		getContentPane().add(startPanel, BorderLayout.SOUTH);

	}

	private void initView2() {
		btControlXML.setActionCommand("controlXML");
		btControlXML.addActionListener(this);
		btControlXML.setBackground(Color.white);
		btControlXML.setBorderPainted(false);
		btControlXML.setMargin(new Insets(0, 0, 0, 0));

		buttonPanel.setLayout(new GridLayout(1, 2));
		buttonPanel.add(lbControlXML);
		buttonPanel.add(btControlXML);

		btStart_neu.setActionCommand("start1");
		btStart_neu.addActionListener(this);

		getContentPane().setLayout(new BorderLayout());

		startPanel.setLayout(new BorderLayout());
		startPanel.add(btStart_neu, BorderLayout.CENTER);

		getContentPane().add(buttonPanel, BorderLayout.CENTER);
		getContentPane().add(startPanel, BorderLayout.SOUTH);
	}

	public void updateStatus() {
		if (inputFile != null
			&& modelFile != null
			&& controlFile != null
			&& targetDir != null
			&& inputFile.exists()
			&& modelFile.exists()
			&& controlFile.exists()
			&& targetDir.exists()) {
			btStart.setEnabled(true);
		} else {
			btStart.setEnabled(false);
		}
	}
	/*public static void main(String[] args) {
		System.out.println("SCE_KALYPSO###");
	
		I18n.sce_getInstance("de");
		//System.out.println("\n\n sce_getInstance ausgeführt \n\n");
	
		SCE_KALYPSO sce_kalypso = new SCE_KALYPSO();
	
		sce_kalypso.readXMLinput();
		sce_kalypso.makeinputFiles();
		sce_kalypso.startSCE();
		//sce_kalypso.testInputFile();
	}*/

	private void startSCE() {

		try {
			String commandLine_SCE =
				myWorkingDir_SCE.getPath()
					+ System.getProperty("file.separator")
					+ myCommand_SCE;
			System.out.println("EXECUTE-Dir: " + myWorkingDir_SCE.getPath());
			System.out.println("EXECUTE:  " + commandLine_SCE);
			String dummy[] = { "" };
			byte bufferInp_SCE[] = new byte[10000];
			byte bufferErr_SCE[] = new byte[10000];
			int cInp_SCE = 0;
			int cErr_SCE = 0;
			int anzSCEstrings = -1;
			String[] all_outInp_SCE = new String[10000];
			StringBuffer outInp_SCE_buff = new StringBuffer();
			boolean behalteSCEstring = false;
			//run SCE process
			try {
				Process process_SCE =
					(Runtime.getRuntime()).exec(
						commandLine_SCE,
						dummy,
						myWorkingDir_SCE);

				InputStream streamInp_SCE = null;
				InputStream streamErr_SCE = null;
				OutputStream streamOut_SCE = null;

				streamInp_SCE = process_SCE.getInputStream();
				streamErr_SCE = process_SCE.getErrorStream();
				streamOut_SCE = process_SCE.getOutputStream();

				int nParam = 0;

				while ((cInp_SCE = streamInp_SCE.read(bufferInp_SCE)) != -1
					|| (cErr_SCE = streamErr_SCE.read(bufferErr_SCE)) != -1) {
					System.out.println(
						"\n\n cInp_SCE:"
							+ cInp_SCE
							+ " cErr_SCE: "
							+ cErr_SCE
							+ "\n----------------");

					// ERROR
					if (cErr_SCE > 0) {
						String outErr = new String(bufferErr_SCE, 0, cErr_SCE);
						System.out.println("ErrorStream: " + outErr);
					}
					if (cErr_SCE < 0) {
						streamErr_SCE.close();
						streamErr_SCE = process_SCE.getErrorStream();
						//System.out.println("reopened ErrorStream");
					}

					// Standard
					if (cInp_SCE > 0) {
						String outInp_SCE = null;
						anzSCEstrings = anzSCEstrings + 1;
						String outInp_SCE1 =
							new String(bufferInp_SCE, 0, cInp_SCE);
						//System.out.println(outInp_SCE);
						all_outInp_SCE[anzSCEstrings] = outInp_SCE1;
						/*System.out.println("Anfang SCEstrings: \n");
						 for(int n=0;n<anzSCEstrings+1;n++){
						 System.out.println(n+"\n"+all_outInp_SCE[n]);
						 }
						 System.out.println("\n Ende SCEstrings \n");*/
						System.out.println(all_outInp_SCE[anzSCEstrings]);
						outInp_SCE_buff.append(all_outInp_SCE[anzSCEstrings]);

						//read SCE output
						//keep SCEstring until all information is read
						if (behalteSCEstring = false) {
							outInp_SCE = all_outInp_SCE[anzSCEstrings];
						} else {
							outInp_SCE = outInp_SCE_buff.toString();
						}
						int pos = outInp_SCE.indexOf("&");
						if (pos != -1) {
							String[] strings = outInp_SCE.split("&");
							//System.out.println(strings[1]);
							if (strings[1].startsWith("Parameter:")) {
								//int nParam;
								System.out.println("Beginn Parameter:");
								int posAnf1 = strings[1].indexOf("=");
								int posEnde1 = strings[1].indexOf("!");
								//read number of parameter
								nParam =
									Integer.parseInt(
										strings[1]
											.substring(
												posAnf1 + 1,
												posEnde1 - 1)
											.trim());
								System.out.println(
									"Anzahl Parameter= " + nParam);
								String[] subStrings1 = strings[1].split("!");
								if (subStrings1.length > 1) {
									String[] subsubStrings1 =
										subStrings1[1].split(";");
									int anzSubsubStrings1 =
										subsubStrings1.length;
									if (anzSubsubStrings1 >= nParam) {
										Double[] valueParam =
											new Double[nParam];
										//read values of parameter
										for (int i = 0; i < nParam; i++) {
											valueParam[i] =
												new Double(subsubStrings1[i]);
											//System.out.println(valueParam[i].toString());
										}
										//transform SCE values(0..1) to real values
										Double[] realValues =
											new Double[nParam];
										realValues = sceToReal(valueParam);
										System.out.println("Parameter real: ");
										for (int n = 0; n < nParam; n++) {
											System.out.println(
												realValues[n].toString());
										}
										System.out.println("\n");
										//start Kalypso.exe
										/*startKalypso(
											nParam,
											valueParam,
											streamOut_SCE);*/
										startKalypso2(
											nParam,
											realValues,
											streamOut_SCE,
											false);
										int lengthBuff =
											outInp_SCE_buff.length();
										outInp_SCE_buff.delete(0, lengthBuff);
										anzSCEstrings = -1;
									} else {
										behalteSCEstring = true;
									}
								} else {
									behalteSCEstring = true;
								}
							}
						}
					}
					if (cInp_SCE < 0) {
						streamInp_SCE.close();
						streamInp_SCE = process_SCE.getInputStream();
						System.out.println("reopened InputStream");
					}
				}
				System.out.println("While_SCE beendet" + cInp_SCE + cErr_SCE);
				streamInp_SCE.close();
				streamErr_SCE.close();
				streamOut_SCE.close();

				//System.out.println("inputStream.closed && errorStream.closed");
				process_SCE.waitFor();
				process_SCE.destroy();
				readSCEOutput(nParam);
			} catch (IOException e) {
				System.out.println("IOException_SCE:" + e.getMessage());
			}
		} catch (InterruptedException e) {
			System.out.println("InterruptedException: " + e.getMessage());
			return;
		}
	}

	private void startKalypso2(
		int nParam,
		Double[] valueParam,
		OutputStream streamOut_SCE,
		boolean saveAll) {
		//write new parameter in xml-files (Kalypso)
		makeModelxml(valueParam, saveAll);
		/*Vector resultData = new Vector();
		//run Kalypso.exe, store discharge results in Vector resultData
		resultData =
			startKalypso.runSimulation(
				saveAll,
				rootNode,
				startDate_pegel,
				endDate_pegel);
		//number of data
		int nData = resultData.size();
		System.out.println("Anzahl Abflüsse: " + nData);
		Double[] valueRunoff = new Double[nData];
		//read values of data
		for (int i = 0; i < nData; i++) {
			valueRunoff[i] = new Double((String) (resultData.elementAt(i)));
			//System.out.println(valueRunoff[i].toString());
		}
		
		//write Kalypso output to SCE
		PrintWriter to_SCE = new PrintWriter(streamOut_SCE, false);
		to_SCE.println(nData);
		to_SCE.flush();
		for (int index = 0; index < nData; index++) {
			to_SCE.println(valueRunoff[index]);
			//System.out.println(index + "=" + valueRunoff[index]);
		}
		to_SCE.flush();
		System.out.println("to_SCE flushed");*/

		TreeMap resultData_sim = new TreeMap();
		//run Kalypso.exe, store discharge results in TreeMap resultData
		resultData_sim = startKalypso.runSimulation(saveAll, rootNode);
		double oFunctionValue = 0;
		//calculate objective function value
		oFunctionValue = oFunction.getObjectiveFunctionValue(resultData_sim);
		System.out.println("Objective function Value: " + oFunctionValue + "!");

		//write value of objective function to SCE
		PrintWriter to_SCE = new PrintWriter(streamOut_SCE, false);
		to_SCE.println(oFunctionValue);
		to_SCE.flush();
		System.out.println("to_SCE flushed");

		//count number of "Kalypsos"
		System.out.println("Anzahl Kalypsos: " + anzKalypso);
		anzKalypso = anzKalypso + 1;
	}

	//method transforms SCE values(0..1) to real values
	private Double[] sceToReal(Double[] sce) {
		int numsce = sce.length;
		double[] realValue = new double[numsce];
		Double[] real_Double = new Double[numsce];
		for (int i = 0; i < sce.length; i++) {
			/*realValue[i] =
				paramLowerBounds[i]
					+ sce[i].doubleValue()
						* (paramUpperBounds[i] - paramLowerBounds[i]);*/
			realValue[i] =
				calContexts[i].getLowerBound()
					+ sce[i].doubleValue()
						* (calContexts[i].getUpperBound()
							- calContexts[i].getLowerBound());
			real_Double[i] = new Double(realValue[i]);
		}
		return real_Double;
	}

	//method transforms real values to SCE values
	private double[] realTosce(double[] real) {
		int numreal = real.length;
		double[] sce = new double[numreal];
		for (int i = 0; i < real.length; i++) {
			/*sce[i] =
				(real[i] - paramLowerBounds[i])
					/ (paramUpperBounds[i] - paramLowerBounds[i]);*/
			sce[i] =
				(real[i] - calContexts[i].getLowerBound())
					/ (calContexts[i].getUpperBound()
						- calContexts[i].getLowerBound());
		}
		return sce;
	}

	//method prepares the file und scein.dat and 
	//sets the TreeMap for observed Data
	private void makeinputFiles() {
		//copy sce-tool to targetDir
		try {
			if (!targetDir.exists())
				targetDir.mkdirs();
			File sceTemplate = new File("sce_tool");
			System.out.println("copy template from " + sceTemplate.toString());
			File sceTargetDir = new File(targetDir, "sce_Tool");
			FileSystemUtils.copyRecursiveDir(sceTemplate, sceTargetDir);
			myWorkingDir_SCE = sceTargetDir;
		} catch (Exception e) {
			e.printStackTrace();
		}
		//prepare scein.dat
		try {
			Runtime.getRuntime().gc();
			String result = ServiceTools.xslTransform(inputFile, xslFile);
			File outputFile = new File(myWorkingDir_SCE, "scein.dat");
			FileWriter out = new FileWriter(outputFile);
			out.write(result);
			out.close();
		} catch (Exception err) {
			System.out.println(
				I18n.get("TrafoView.ErrorMessage")
					+ " \""
					+ err.getMessage()
					+ "\"");
			err.printStackTrace();
		}

		//Set TreeMap for observed data
		if (syntetic) {
			//calculate discharge with true parameter values and set as observed Discharge
			Double[] valueParam_syntetic = new Double[calContexts.length];
			for (int i = 0; i < calContexts.length; i++) {
				valueParam_syntetic[i] =
					new Double(calContexts[i].getSynteticValue());
			}
			makeModelxml(valueParam_syntetic, false);
			TreeMap resultData_sim = new TreeMap();
			//run Kalypso.exe, store discharge results in TreeMap resultData
			resultData_sim = startKalypso.runSimulation(false, rootNode);
			observedDischarge = resultData_sim;
			/*Iterator it_all = observedDischarge.keySet().iterator();
			while (it_all.hasNext()) {
				Object dateKey = it_all.next();
				Date actualDate = (Date) dateKey;
				Object value = (String) observedDischarge.get(dateKey);
				DateFormat dateformat =
					new SimpleDateFormat("dd.MM.yyyy HH:mm");
				dateformat.setTimeZone(TimeZone.getTimeZone("GMT+1:00"));
				System.out.println(
					"Actual Date: "
						+ dateformat.format(actualDate)
						+ "   Value: "
						+ value);
			}*/
		} else {
			GageTimeSeries gts = new GageTimeSeries();
			File gFile = new File(gaugeFile);
			gts.importPegelDat(gFile);
			observedDischarge = gts.getObservedData();
		}

		//Objective Function
		//boolean[] functions = { true, true, false, false };
		//double[] trafoConstants = { 0, 0, 0, 0 };
		if (equalWeights) {
			Double[] valueParam_initial = new Double[calContexts.length];
			for (int i = 0; i < calContexts.length; i++) {
				valueParam_initial[i] =
					new Double(calContexts[i].getInitialValue());
			}
			makeModelxml(valueParam_initial, false);
			TreeMap qSim_Initial = new TreeMap();
			//run Kalypso.exe, store discharge results in TreeMap resultData
			qSim_Initial = startKalypso.runSimulation(false, rootNode);
			oFunction =
				new ObjectiveFunction(
					observedDischarge,
					objectiveFunctions,
					peakFlowLevel,
					lowFlowLevel,
					startDate_calibration,
					endDate_calibration,
					timeStep,
					qSim_Initial);
		} else {
			oFunction =
				new ObjectiveFunction(
					observedDischarge,
					objectiveFunctions,
					trafoConstants,
					peakFlowLevel,
					lowFlowLevel,
					startDate_calibration,
					endDate_calibration,
					timeStep);
		}

		//prepare kalypsoInp.dat
		/*if (syntetic) {
			try {
				File outputFile = new File(myWorkingDir_SCE, "Kalypsoinp.dat");
				writeSynteticDataToFile(outputFile);
			} catch (Exception e) {
				System.out.println("Could not write KalypsoInp-File");
			}
		
		} else {
			GageTimeSeries gts = new GageTimeSeries();
			File gFile = new File(gaugeFile);
			gts.importPegelDat(gFile);
			Vector result = gts.getData(startDate_pegel, endDate_pegel);
			/*for (int i = 0; i < result.size(); i++) {
				System.out.println(i + "= " + result.get(i));
			}
			try {
				File outputFile = new File(myWorkingDir_SCE, "Kalypsoinp.dat");
				gts.writeDataToFile(outputFile, result);
			} catch (Exception e) {
				System.out.println("Could not write KalypsoInp-File");
			}
		}*/
	}

	public void readSCEOutput(int numParam) {
		if (numParam > 0) {
			Double[] valueParam = new Double[numParam];
			Double[] realVal = new Double[numParam];
			Vector data = new Vector();
			PrintStream ps;
			try {
				File psFile = new File(targetDir, "result.dat");
				ps = new PrintStream(new FileOutputStream(psFile));
				String sceOutCommand = "sceout_java.dat";
				String commandLine =
					myWorkingDir_SCE.getPath()
						+ System.getProperty("file.separator")
						+ sceOutCommand;
				StringTokenizer stringTok;
				//Vector resultData = new Vector();
				BufferedReader lineReader =
					new BufferedReader(new FileReader(commandLine));
				String line;
				while ((line = lineReader.readLine()) != null) {
					stringTok = new StringTokenizer(line);
					while (stringTok.hasMoreTokens()) {
						data.addElement(stringTok.nextToken());
					}
				}
				for (int n = 0; n < data.size(); n++) {
					System.out.println((String) data.elementAt(n));
				}

			} catch (Exception e) {
				System.out.println("InterruptedException: " + e.getMessage());
				return;
			}

			//print results to File

			int numHeader0 = 0;
			// If data is syntetic
			/*String test = (String) data.firstElement();
			if (test.equals("TRUE")) {
				numHeader0 = 3 + numParam;
				ps.println("True parameter values: ");
				for (int i = 0; i < numParam; i++) {
					valueParam[i] =
						Double.valueOf((String) data.elementAt(3 + i));
					System.out.println(i + ": " + valueParam[i].toString());
				}
				realVal = sceToReal(valueParam);
				for (int n = 0; n < numParam; n++) {
					ps.println("Parameter " + n + ": " + realVal[n].toString());
				}
				makeModelxml(realVal, true);
				startKalypso.runSimulation(true, rootNode);
			}*/

			//If data is syntetic
			if (syntetic == true) {
				ps.println("True parameter values: ");
				for (int i = 0; i < numParam; i++) {
					valueParam[i] =
						new Double(calContexts[i].getSynteticValue());
					System.out.println(
						i + ": " + calContexts[i].getSynteticValue());
				}
				for (int n = 0; n < numParam; n++) {
					ps.println(
						"Parameter " + n + ": " + valueParam[n].toString());
				}
				makeModelxml(valueParam, true);
				startKalypso.runSimulation(true, rootNode);
			}

			int numHeader1 = 10;
			int numHeader2 = 9;
			int numLoops =
				(data.size() - 20 - numHeader0 - numParam) / (4 + numParam);
			System.out.println("Anzahl Schleifen: " + numLoops);
			ps.println("Anfangswerte: ");
			ps.println("Kriterium: " + data.elementAt(numHeader1 + numHeader0));
			for (int i = 0; i < numParam; i++) {
				valueParam[i] =
					Double.valueOf(
						(String) data.elementAt(
							numHeader0 + numHeader1 + 1 + i));
				System.out.println(i + ": " + valueParam[i].toString());
			}
			realVal = sceToReal(valueParam);
			for (int n = 0; n < numParam; n++) {
				ps.println("Parameter " + n + ": " + realVal[n].toString());
			}
			makeModelxml(realVal, true);
			startKalypso.runSimulation(true, rootNode);

			int startLoops =
				numHeader0 + numHeader1 + 1 + numParam + numHeader2;
			int elementsinLoop = 4 + numParam;
			for (int j = 0; j < numLoops; j++) {
				ps.println("Ergebnis Loop " + j + ": ");
				ps.println(
					"Anzahl Versuche: "
						+ data.elementAt((j * elementsinLoop) + startLoops + 1));
				ps.println(
					"Bester Funktionswert: "
						+ data.elementAt((j * elementsinLoop) + startLoops + 2));
				for (int k = 0; k < numParam; k++) {
					valueParam[k] =
						Double.valueOf(
							(String) data.elementAt(
								(j * elementsinLoop) + startLoops + 4 + k));
					System.out.println(k + ": " + valueParam[k].toString());
				}
				realVal = sceToReal(valueParam);
				for (int n = 0; n < numParam; n++) {
					ps.println("Parameter " + n + ": " + realVal[n].toString());
				}
				makeModelxml(realVal, true);
				startKalypso.runSimulation(true, rootNode);

			}
			modelFile = startKalypso.modelxml;
			System.out.println(modelFile);
			ps.close();
		}
	}

	private void testInputFile() {

		File inputFile = new File("C://Kalypso//xml_temp//model.xml");
		try {
			Document doc = xmlServiceTools.getXML(inputFile);
			//String query = "/theme/parameter[@key=\"m_retAquif\"]/o[@ID=\"1\"]/rbNumber";
			//String query = "/theme/parameter/@key";
			/*String query =
				"/theme/table[@key=\"rb\"]/o/sp[@m_rbNumber=\"104\" or @m_rbNumber=\"102\"]/@m_retAquif";*/
			/*String query =
				"/theme/table[@key=\"rb\"]/o/sp[@m_rbNumber=\"104\" or @m_rbNumber=\"101\"]/@m_retInterflow";*/
			String query =
				"/theme/table[@key=\"rb\"]/o/sp[@m_rbNumber=\"104\"]/../v/v_row[(position()=2) or (position()=4)]/@v_infiltShortTime";
			String[] querys = new String[1];
			System.out.println(xmlServiceTools.getParameter(query, doc));
			querys[0] = query;
			xmlServiceTools.setParameter(querys, "10", doc);
			System.out.println(xmlServiceTools.getParameter(query, doc));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private void makeModelxml(Double[] valueParam, boolean saveAll) {
		try {
			Document doc = xmlServiceTools.getXML(modelFile);

			for (int i = 0; i < valueParam.length; i++) {
				//check, ob Parameter als Faktor oder normal, dann zwei Möglichkeiten
				String mode = calContexts[i].getMode();
				if (mode.equals("factor")) {
					xmlServiceTools.setParameter_Factor(
						calContexts[i].getxPaths(),
						valueParam[i].doubleValue(),
						doc);
				} else if (mode.equals("offset")) {
					xmlServiceTools.setParameter_Offset(
						calContexts[i].getxPaths(),
						valueParam[i].doubleValue(),
						doc);
				} else {
					xmlServiceTools.setParameter(
						calContexts[i].getxPaths(),
						(valueParam[i]).toString(),
						doc);
				}
			}
			String destName = "newModel";
			File destFile =
				new File(modelFile.getParentFile(), destName + ".xml");
			/*if (saveAll) {
				int n = 0;
				try {
					while (destFile.exists()) {
						n++;
						destFile =
							new File(
								xmlDir,
								destName + "_" + Integer.toString(n) + ".xml");
					}
				} catch (Exception e) {
					System.out.println(e.getMessage());
				}
			}*/
			xmlServiceTools.toFile(destFile, doc);
			startKalypso = new StartKalypso(destFile, controlFile, targetDir);
			//xmlServiceTools.toFile(modelFile, doc);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private void readXMLinput() {
		try {
			Document docControl = xmlServiceTools.getXML(controlFile);

			//RootNode
			String queryRootNode = "/theme/table/o/sp/@m_rootNode";
			Node node =
				xmlServiceTools.getXPath_singleNode(queryRootNode, docControl);
			System.out.println(node.getNodeValue());
			rootNode = Integer.parseInt(node.getNodeValue());

			//TimeStep
			String queryTimeStep = "/theme/table/o/sp/@m_timeStep";
			node =
				xmlServiceTools.getXPath_singleNode(queryTimeStep, docControl);
			System.out.println(node.getNodeValue());
			/*if (node.getNodeValue().equals("0.083")) {
				timeStep = 1.0 / 12.0;
				System.out.println("TimeStep: " + timeStep);
			} else {*/
			timeStep = Double.parseDouble(node.getNodeValue());
			//}

		} catch (Exception e) {
			System.out.println(e.getMessage());
		}

		try {
			Document doc = xmlServiceTools.getXML(inputFile);

			DateFormat dateformat = new SimpleDateFormat("dd.MM.yyyy HH:mm");
			dateformat.setTimeZone(TimeZone.getTimeZone("GMT+1:00"));

			//StartDate
			String querystartDate = "/autoCalibration/pegel/startDate";
			Node sd = xmlServiceTools.getXPath_singleNode(querystartDate, doc);
			Node sdValue = sd.getFirstChild();
			String startValue = sdValue.getNodeValue();
			startDate_calibration = dateformat.parse(startValue);
			/*System.out.println(
				"Tag: "
					+ startDate_pegel.getDate()
					+ "\n"
					+ "Monat: "
					+ (startDate_pegel.getMonth() + 1)
					+ "\n"
					+ "Jahr: "
					+ (startDate_pegel.getYear() + 1900)
					+ "\n"
					+ "Stunde: "
					+ startDate_pegel.getHours()
					+ "\n"
					+ "Minuten: "
					+ startDate_pegel.getMinutes());*/
			System.out.println(startValue);

			//EndDate
			String queryendDate = "/autoCalibration/pegel/endDate";
			Node ed = xmlServiceTools.getXPath_singleNode(queryendDate, doc);
			Node edValue = ed.getFirstChild();
			String endValue = edValue.getNodeValue();
			endDate_calibration = dateformat.parse(endValue);
			System.out.println(endValue);

			//Gauging station
			String queryfile = "/autoCalibration/pegel/file";
			Node file = xmlServiceTools.getXPath_singleNode(queryfile, doc);
			Node fValue = file.getFirstChild();
			String fileValue = fValue.getNodeValue();
			gaugeFile = fileValue;
			if (fileValue.equals("syntetic")) {
				syntetic = true;
			} else {
				syntetic = false;
			}
			System.out.println("Syntetic: " + syntetic);

			//if syntetic; synteticValues
			/*if (syntetic) {
				String querySynValues =
					"/autoCalibration/parameterlist/parameter/synteticValue";
				NodeList nlSynVal =
					xmlServiceTools.getXPath(querySynValues, doc);
				int anzParam = nlSynVal.getLength();
				synteticParamValues = new double[anzParam];
				for (int i = 0; i < anzParam; i++) {
					Node n = (nlSynVal.item(i)).getFirstChild();
					synteticParamValues[i] =
						Double.parseDouble(n.getNodeValue());
				}
				/*for (int n = 0; n < anzParam; n++) {
					System.out.println(synteticParamValues[n]);
				}*/
			//}

			//Objectivefunction values
			String queryTrafoConstants =
				"/autoCalibration/optParameter/objectiveFunction/transformationConstants";
			Node tConstants =
				xmlServiceTools.getXPath_singleNode(queryTrafoConstants, doc);
			Node trafoValue = tConstants.getFirstChild();
			String trafoConstantsValue = trafoValue.getNodeValue();
			if (trafoConstantsValue.equals("equal")) {
				equalWeights = true;
			}
			if (trafoConstantsValue.equals("userdefined")) {
				equalWeights = false;
			}

			//Volume Error
			String queryVolumeError_mode =
				"/autoCalibration/optParameter/objectiveFunction/volumeError/@mode";
			Node mode =
				xmlServiceTools.getXPath_singleNode(queryVolumeError_mode, doc);
			Node mValue = mode.getFirstChild();
			String modeValue = mValue.getNodeValue();
			objectiveFunctions[0] = (new Boolean(modeValue)).booleanValue();
			if (!equalWeights && objectiveFunctions[0]) {
				String queryTrafoConstant =
					"/autoCalibration/optParameter/objectiveFunction/volumeError/transformationConstant";
				Node trafoConstant =
					xmlServiceTools.getXPath_singleNode(
						queryTrafoConstant,
						doc);
				Node trafoConstValue = trafoConstant.getFirstChild();
				String trafoConstantValue = trafoConstValue.getNodeValue();
				trafoConstants[0] = Double.parseDouble(trafoConstantValue);
			} else {
				trafoConstants[0] = 0;
			}

			//Overall RMSE
			String queryRMSE_mode =
				"/autoCalibration/optParameter/objectiveFunction/rootMeanSquareError/@mode";
			mode = xmlServiceTools.getXPath_singleNode(queryRMSE_mode, doc);
			mValue = mode.getFirstChild();
			modeValue = mValue.getNodeValue();
			objectiveFunctions[1] = (new Boolean(modeValue)).booleanValue();
			if (!equalWeights && objectiveFunctions[1]) {
				String queryTrafoConstant =
					"/autoCalibration/optParameter/objectiveFunction/rootMeanSquareError/transformationConstant";
				Node trafoConstant =
					xmlServiceTools.getXPath_singleNode(
						queryTrafoConstant,
						doc);
				Node trafoConstValue = trafoConstant.getFirstChild();
				String trafoConstantValue = trafoConstValue.getNodeValue();
				trafoConstants[1] = Double.parseDouble(trafoConstantValue);
			} else {
				trafoConstants[1] = 0;
			}

			//Average RMSE peakFlows
			String queryRMSEpeak_mode =
				"/autoCalibration/optParameter/objectiveFunction/rootMeanSquareError_PeakFlows/@mode";
			mode = xmlServiceTools.getXPath_singleNode(queryRMSEpeak_mode, doc);
			mValue = mode.getFirstChild();
			modeValue = mValue.getNodeValue();
			objectiveFunctions[2] = (new Boolean(modeValue)).booleanValue();
			if (objectiveFunctions[2]) {
				String queryPeakFlowLevel =
					"/autoCalibration/optParameter/objectiveFunction/rootMeanSquareError_PeakFlows/peakFlowLevel";
				Node flowLevel =
					xmlServiceTools.getXPath_singleNode(
						queryPeakFlowLevel,
						doc);
				Node flValue = flowLevel.getFirstChild();
				String flowLevelValue = flValue.getNodeValue();
				peakFlowLevel = Double.parseDouble(flowLevelValue);
			}
			if (!equalWeights && objectiveFunctions[2]) {
				String queryTrafoConstant =
					"/autoCalibration/optParameter/objectiveFunction/rootMeanSquareError_PeakFlows/transformationConstant";
				Node trafoConstant =
					xmlServiceTools.getXPath_singleNode(
						queryTrafoConstant,
						doc);
				Node trafoConstValue = trafoConstant.getFirstChild();
				String trafoConstantValue = trafoConstValue.getNodeValue();
				trafoConstants[2] = Double.parseDouble(trafoConstantValue);
			} else {
				trafoConstants[2] = 0;
			}

			//Average RMSE lowFlows
			String queryRMSElow_mode =
				"/autoCalibration/optParameter/objectiveFunction/rootMeanSquareError_LowFlows/@mode";
			mode = xmlServiceTools.getXPath_singleNode(queryRMSElow_mode, doc);
			mValue = mode.getFirstChild();
			modeValue = mValue.getNodeValue();
			objectiveFunctions[3] = (new Boolean(modeValue)).booleanValue();
			if (objectiveFunctions[3]) {
				String queryLowFlowLevel =
					"/autoCalibration/optParameter/objectiveFunction/rootMeanSquareError_LowFlows/lowFlowLevel";
				Node flowLevel =
					xmlServiceTools.getXPath_singleNode(queryLowFlowLevel, doc);
				Node flValue = flowLevel.getFirstChild();
				String flowLevelValue = flValue.getNodeValue();
				lowFlowLevel = Double.parseDouble(flowLevelValue);
			}
			if (!equalWeights && objectiveFunctions[3]) {
				String queryTrafoConstant =
					"/autoCalibration/optParameter/objectiveFunction/rootMeanSquareError_LowFlows/transformationConstant";
				Node trafoConstant =
					xmlServiceTools.getXPath_singleNode(
						queryTrafoConstant,
						doc);
				Node trafoConstValue = trafoConstant.getFirstChild();
				String trafoConstantValue = trafoConstValue.getNodeValue();
				trafoConstants[3] = Double.parseDouble(trafoConstantValue);
			} else {
				trafoConstants[3] = 0;
			}

			System.out.println("Objective function values: ");
			System.out.println("EqualWeights=" + equalWeights);
			for (int i = 0; i < objectiveFunctions.length; i++) {
				System.out.print(
					"Funktion " + i + ": " + objectiveFunctions[i] + ", ");
				System.out.println("A=" + trafoConstants[i]);
			}
			System.out.println("PeakFlowLevel=" + peakFlowLevel);
			System.out.println("LowFlowLevel=" + lowFlowLevel);

			//xPath, ParamUpperBound, ParamLowerBound
			String queryID = "/autoCalibration/parameterlist/parameter/@ID";
			/*String queryXPath =
				"/autoCalibration/parameterlist/parameter/xpath";*/
			String queryUpBound =
				"/autoCalibration/parameterlist/parameter/upperBound";
			String queryLoBound =
				"/autoCalibration/parameterlist/parameter/lowerBound";
			String querySynValues =
				"/autoCalibration/parameterlist/parameter/synteticValue";
			String queryInitialValues =
				"/autoCalibration/parameterlist/parameter/initialValue";
			String queryFactor =
				"/autoCalibration/parameterlist/parameter/@mode";

			NodeList nlID = xmlServiceTools.getXPath(queryID, doc);
			//NodeList nlXPath = xmlServiceTools.getXPath(queryXPath, doc);
			NodeList nlUpBound = xmlServiceTools.getXPath(queryUpBound, doc);
			NodeList nlLoBound = xmlServiceTools.getXPath(queryLoBound, doc);
			NodeList nlSynVal = xmlServiceTools.getXPath(querySynValues, doc);
			NodeList nlIniVal =
				xmlServiceTools.getXPath(queryInitialValues, doc);
			NodeList nlMode = xmlServiceTools.getXPath(queryFactor, doc);

			int anzParam = nlID.getLength();
			System.out.println("Anzahl Parameter: " + anzParam);

			//xPaths = new String[anzParam];
			//paramUpperBounds = new double[anzParam];
			//paramLowerBounds = new double[anzParam];
			//modes = new String[anzParam];
			calContexts = new CalContext[anzParam];

			/*for (int n = 0; n < anzParam; n++) {
				String id = (nlID.item(n)).getNodeValue();
				String query = "/autoCalibration/parameterlist/parameter[@ID=\""+id+"\"]/xpath";
				NodeList nl = xmlServiceTools.getXPath(query,doc);
				System.out.println(id);
				System.out.println(query);
				for(int k = 0; k < nl.getLength(); k++){
				System.out.println(((nl.item(k)).getFirstChild()).getNodeValue());
				}
			}*/
			for (int i = 0; i < anzParam; i++) {
				CalContext calContext = new CalContext();
				Node n = (nlUpBound.item(i)).getFirstChild();
				double value = Double.parseDouble(n.getNodeValue());
				calContext.setUpperbound(value);
				n = (nlLoBound.item(i)).getFirstChild();
				value = Double.parseDouble(n.getNodeValue());
				calContext.setLowerbound(value);
				n = (nlIniVal.item(i)).getFirstChild();
				value = Double.parseDouble(n.getNodeValue());
				calContext.setInitialValue(value);
				if (syntetic) {
					n = (nlSynVal.item(i)).getFirstChild();
					value = Double.parseDouble(n.getNodeValue());
					calContext.setSynteticValue(value);
				}
				n = nlMode.item(i);
				String nodeValue = n.getNodeValue();
				calContext.setMode(nodeValue);
				String id = (nlID.item(i)).getNodeValue();
				String xPathquery =
					"/autoCalibration/parameterlist/parameter[@ID=\""
						+ id
						+ "\"]/xpath";
				NodeList nlXPath = xmlServiceTools.getXPath(xPathquery, doc);
				String[] xPaths = new String[nlXPath.getLength()];
				for (int k = 0; k < nlXPath.getLength(); k++) {
					xPaths[k] =
						((nlXPath.item(k)).getFirstChild()).getNodeValue();
				}
				calContext.setXPath(xPaths);
				calContexts[i] = calContext;
			}
			for (int n = 0; n < anzParam; n++) {
				System.out.print(
					n
						+ ": "
						+ "UpperBound="
						+ calContexts[n].getUpperBound()
						+ ", LowerBound="
						+ calContexts[n].getLowerBound()
						+ ", Syntetic Value="
						+ calContexts[n].getSynteticValue()
						+ ", Initial Value="
						+ calContexts[n].getInitialValue()
						+ ", Mode="
						+ calContexts[n].getMode());
				String[] xPaths = calContexts[n].getxPaths();
				for (int k = 0; k < xPaths.length; k++) {
					System.out.println(", xPath=" + xPaths[k]);
				}
			}
			/*for (int i = 0; i < anzParam; i++) {
				Node n = (nlXPath.item(i)).getFirstChild();
				xPaths[i] = n.getNodeValue();
			}
			for (int n = 0; n < anzParam; n++) {
				System.out.println(xPaths[n]);
			}*/
			/*for (int i = 0; i < anzParam; i++) {
				Node n = (nlUpBound.item(i)).getFirstChild();
				paramUpperBounds[i] = Double.parseDouble(n.getNodeValue());
			}
			/*for(int n=0;n<anzParam;n++){
				System.out.println(paramUpperBounds[n]);
			}*/
			/*for (int i = 0; i < anzParam; i++) {
				Node n = (nlLoBound.item(i)).getFirstChild();
				paramLowerBounds[i] = Double.parseDouble(n.getNodeValue());
			}
			/*for(int n=0;n<anzParam;n++){
				System.out.println(paramLowerBounds[n]);
			}*/
			/*for (int i = 0; i < anzParam; i++) {
				Node n = nlMode.item(i);
				String nodeValue = n.getNodeValue();
				//System.out.println(nodeValue);
				modes[i] = nodeValue;
			}
			for (int n = 0; n < anzParam; n++) {
				System.out.println(modes[n]);
			}*/

		} catch (Exception e) {
			System.out.println(e.getMessage());
		}
		//read startValues from modelFile
		/*try {
			Document doc = xmlServiceTools.getXML(modelFile);
			Vector nodes = new Vector();
			//System.out.println("Startwerte: ");
			for (int i = 0; i < xPaths.length; i++) {
				NodeList nlStartValues =
					xmlServiceTools.getXPath(xPaths[i], doc);
				for (int n = 0; n < nlStartValues.getLength(); n++) {
					nodes.add(nlStartValues.item(n));
					//String output = (nlStartValues.item(n)).getNodeValue();
					//System.out.println(output);
					//startValues[i]=Double.parseDouble(output);
				}
			}
			startValues = new double[nodes.size()];
			for (int k = 0; k < nodes.size(); k++) {
				Node actualNode = (Node) nodes.elementAt(k);
				//System.out.println("Node: "+actualNode);
				String output = actualNode.getNodeValue();
				startValues[k]=Double.parseDouble(output);
				//System.out.println(output);
			}
			for (int m = 0; m < startValues.length; m++){
				System.out.println(startValues[m]);
			}
		} catch (Exception e) {
			System.out.println(e.getMessage());
		}*/

	}

	/*private void writeSynteticDataToFile(File kalypsoInp) throws IOException {
	
		DecimalFormat decimalFormat1 = new DecimalFormat("000.000000");
		DecimalFormat decimalFormat2 = new DecimalFormat("00000");
	
		FileWriter writer = new FileWriter(kalypsoInp);
		String line1 = "# DATA, OBJ FUNC, DATA TYPE";
		writeln(writer, line1);
		int numData;
		Date startDate = getStartDate();
		Date endDate = endDate_pegel;
		double ts = timeStep;
		Date tempDate = startDate;
		long startDateAsLong = startDate.getTime();
		long timeStepAsLong = ((long) ((float) ts * 1000f)) * 3600l;
		int index = 1;
		while ((tempDate.compareTo(endDate)) < 0) {
			index = index + 1;
			startDateAsLong = startDateAsLong + timeStepAsLong;
			tempDate.setTime(startDateAsLong);
			DateFormat dateformat = new SimpleDateFormat("dd.MM.yyyy HH:mm");
			dateformat.setTimeZone(TimeZone.getTimeZone("GMT+1:00"));
			//System.out.println("Temp Date: " + dateformat.format(tempDate));
		}
		numData = index;
		//System.out.println(numData);
		int flagObjFunc = 1; //default:sls
		int flagDataType = 1; //optimise with syntetic data
		String line2 =
			decimalFormat2.format(numData)
				+ decimalFormat2.format(flagObjFunc)
				+ decimalFormat2.format(flagDataType);
		writeln(writer, line2);
		String line3 = "TRUE PARAMETER VALUES:";
		writeln(writer, line3);
		double[] realValues = new double[calContexts.length];
		double[] synteticParamValues = new double[calContexts.length];
		for (int n = 0; n < synteticParamValues.length; n++) {
			synteticParamValues[n] = calContexts[n].getSynteticValue();
		}
		realValues = realTosce(synteticParamValues);
		String line = "";
		for (int i = 0; i < realValues.length; i++) {
			String stringValue = decimalFormat1.format(realValues[i]);
			String newStringValue = stringValue.replaceAll(",", ".");
			line = line + newStringValue;
		}
		writeln(writer, line);
		writer.close();
	
	}*/

	private void writeln(FileWriter writer, String line) throws IOException {
		line = line + System.getProperty("line.separator");
		writer.write(line, 0, line.length());
	}

	private Date getStartDate() {
		Date startDate = (Date) startDate_calibration.clone();
		return startDate;
	}

	//  ActionListener
	public void actionPerformed(ActionEvent e) {

		String action = e.getActionCommand();

		if ("SCEInp".equals(action)) {
			int returnVal =
				sceInpFileChooser.showDialog(
					this,
					I18n.get("sceView.selectSCEInputFile"));
			if (returnVal == JFileChooser.APPROVE_OPTION) {
				inputFile = sceInpFileChooser.getSelectedFile();
				btSCEInp.setText(inputFile.toString());
			}
		}

		if ("model".equals(action)) {
			int returnVal =
				modelFileChooser.showDialog(
					this,
					I18n.get("sceView.selectModelFile"));
			if (returnVal == JFileChooser.APPROVE_OPTION) {
				modelFile = modelFileChooser.getSelectedFile();
				btModel.setText(modelFile.toString());
			}
		}

		if ("control".equals(action)) {
			int returnVal =
				controlFileChooser.showDialog(
					this,
					I18n.get("sceView.selectControlFile"));
			if (returnVal == JFileChooser.APPROVE_OPTION) {
				controlFile = controlFileChooser.getSelectedFile();
				btControl.setText(controlFile.toString());
			}
		}
		if ("targetDir".equals(action)) {
			targetDirFileChooser.setFileSelectionMode(
				JFileChooser.DIRECTORIES_ONLY);
			int returnVal =
				targetDirFileChooser.showDialog(
					this,
					I18n.get("sceView.selecttargetDir"));
			if (returnVal == JFileChooser.APPROVE_OPTION) {
				targetDir = targetDirFileChooser.getSelectedFile();
				btTargetDir.setText(targetDir.toString());
			}
		}
		if ("start".equals(action)) {
			System.out.println("SCE_KALYPSO###");
			instance.readXMLinput();
			/*Double[] param =
				{
					new Double(10.0),
					new Double(20.0),
					new Double(30.0),
					new Double(40.0),
					new Double(50.0),
					new Double(60.0),
					new Double(70.0)};
			instance.makeModelxml(param, true);*/
			instance.makeinputFiles();
			instance.startSCE();
			JOptionPane.showMessageDialog(
				null,
				I18n.get("sceView.terminationMassage"));
		}
		updateStatus();

		if ("controlXML".equals(action)) {
			int returnVal =
				controlXMLFileChooser.showDialog(
					this,
					I18n.get("sceView.selectcontrolXMLFile"));
			if (returnVal == JFileChooser.APPROVE_OPTION) {
				controlXML = controlXMLFileChooser.getSelectedFile();
				btControlXML.setText(controlXML.toString());
			}
		}

		if ("start1".equals(action)) {
			if (controlXML != null && (controlXML.exists())) {
				try {
					Document doc = xmlServiceTools.getXML(controlXML);

					String queryModelFile = "/autoCalibration/model/file";
					Node file =
						xmlServiceTools.getXPath_singleNode(
							queryModelFile,
							doc);
					Node fValue = file.getFirstChild();
					String fileValue = fValue.getNodeValue();
					instance.modelFile = new File(fileValue);
					System.out.println("ModelFile: " + instance.modelFile);

					String queryID = "/autoCalibration/stationlist/station/@ID";
					NodeList nlID = xmlServiceTools.getXPath(queryID, doc);
					int anzStations = nlID.getLength();
					System.out.println("Anzahl der Stationen: " + anzStations);
					for (int i = 0; i < anzStations; i++) {
						System.out.println("ModelFile: " + instance.modelFile);
						String id = (nlID.item(i)).getNodeValue();
						String controlquery =
							"/autoCalibration/stationlist/station[@ID=\""
								+ id
								+ "\"]/controlFile";
						Node node =
							xmlServiceTools.getXPath_singleNode(
								controlquery,
								doc);
						Node cFileNode = node.getFirstChild();
						String control = cFileNode.getNodeValue();
						instance.controlFile = new File(control);
						System.out.println(
							"ControlFile: " + instance.controlFile);
						String setupquery =
							"/autoCalibration/stationlist/station[@ID=\""
								+ id
								+ "\"]/setupFile";
						node =
							xmlServiceTools.getXPath_singleNode(
								setupquery,
								doc);
						Node sFileNode = node.getFirstChild();
						String setup = sFileNode.getNodeValue();
						instance.inputFile = new File(setup);
						System.out.println("InputFile: " + instance.inputFile);
						String targetquery =
							"/autoCalibration/stationlist/station[@ID=\""
								+ id
								+ "\"]/targetDir";
						node =
							xmlServiceTools.getXPath_singleNode(
								targetquery,
								doc);
						Node tFileNode = node.getFirstChild();
						String target = tFileNode.getNodeValue();
						instance.targetDir = new File(target);
						System.out.println("targetDir: " + instance.targetDir);
						System.out.println("SCE_KALYPSO###" + i);
						instance.readXMLinput();
						instance.makeinputFiles();
						instance.startSCE();
					}
					JOptionPane.showMessageDialog(
						null,
						I18n.get("sceView.terminationMassage"));
				} catch (Exception ex) {
					System.out.println(ex.getMessage());
				}
			} else {
				JOptionPane.showMessageDialog(
					null,
					I18n.get("sceView.selectControlXMLMessage"));
			}
		}
	}

	//	internalFrameListener:

	//          Invoked when an internal frame is activated.
	public void internalFrameActivated(InternalFrameEvent e) {
	}

	//          Invoked when an internal frame has been closed.
	public void internalFrameClosed(InternalFrameEvent e) {
	}

	//          Invoked when an internal frame is in the process of being closed.
	public void internalFrameClosing(InternalFrameEvent e) {
	}

	//          Invoked when an internal frame is de-activated.
	public void internalFrameDeactivated(InternalFrameEvent e) {
	}

	//          Invoked when an internal frame is de-iconified.
	public void internalFrameDeiconified(InternalFrameEvent e) {
	}

	//          Invoked when an internal frame is iconified.
	public void internalFrameIconified(InternalFrameEvent e) {
	}

	public void internalFrameOpened(InternalFrameEvent e) {
	}

	public static void main(String[] args) {
		BlockTimeSeries blockSerie = new BlockTimeSeries();
		int rootNode = 103;
		File outFile =
			new File("C://Kalypso//temp//Test_Bewertung//discharge.dat");
		Vector allowedKeys = new Vector();
		allowedKeys.addElement(String.valueOf(rootNode));
		blockSerie.importBlockFile(outFile, allowedKeys);
		TreeMap resultData_sim =
			blockSerie.getSimulatedDischarge(String.valueOf(rootNode));
		Iterator it_all = resultData_sim.keySet().iterator();
		/*while (it_all.hasNext()) {
			Object dateKey = it_all.next();
			Date actualDate = (Date) dateKey;
			Object value = (String) resultData_sim.get(dateKey);
			DateFormat dateformat = new SimpleDateFormat("dd.MM.yyyy HH:mm");
			dateformat.setTimeZone(TimeZone.getTimeZone("GMT+1:00"));
			System.out.println(
				"Actual Date: "
					+ dateformat.format(actualDate)
					+ "   Value: "
					+ value);
		}*/
		GageTimeSeries gageSerie = new GageTimeSeries();
		outFile = new File("C://Kalypso//temp//Test_Bewertung//pegel.dat");
		gageSerie.importPegelDat(outFile);
		TreeMap resultData_obs = gageSerie.getObservedData();
		/*Iterator it_all = resultData_obs.keySet().iterator();
		while (it_all.hasNext()) {
			Object dateKey = it_all.next();
			Date actualDate = (Date) dateKey;
			Object value = (String) resultData_obs.get(dateKey);
			DateFormat dateformat = new SimpleDateFormat("dd.MM.yyyy HH:mm");
			dateformat.setTimeZone(TimeZone.getTimeZone("GMT+1:00"));
			System.out.println(
				"Actual Date: "
					+ dateformat.format(actualDate)
					+ "   Value: "
					+ value);
		}*/
		boolean[] functions = { false, false, true, true };
		double[] trafoConstants = { 0.0, 0.0, 0, 2.0};
		DateFormat dateformat = new SimpleDateFormat("dd.MM.yyyy HH:mm");
		dateformat.setTimeZone(TimeZone.getTimeZone("GMT+1:00"));
		Date startDate = new Date();
		Date endDate = new Date();
		try {
			startDate = dateformat.parse("01.11.2001 00:00");
			endDate = dateformat.parse("01.04.2002 00:00");
		} catch (Exception e) {
			System.out.println("Cannot parse start- or endDate.");
		}
		double timeStep = 24;
		double peakFlowLevel = 0.1;
		double lowFlowLevel = 0.02;
		ObjectiveFunction oFunction =
			new ObjectiveFunction(
				resultData_obs,
				functions,
				trafoConstants,
				peakFlowLevel,
				lowFlowLevel,
				startDate,
				endDate,
				timeStep);
		//double erg = oFunction.getObjectiveFunctionValue(resultData_sim);
		//System.out.println("Objective function Value: " + erg + "!");
		//openSCEView();
		//instance.inputFile =
		//	new File("C://Kalypso//Test_Meike//input_Meike.xml");
		//inputFile = new File("C://Kalypso//TestMode//input_neu2.xml");
		/*instance.controlFile = new File("C://Kalypso//TestMode//control.xml");
		instance.modelFile = new File("C://Kalypso//TestMode//model.xml");
		instance.targetDir = new File("C://Kalypso//Test_Meike");
		instance.readXMLinput();
		//instance.makeinputFiles();
		int rootNode = 7765;
		File outFile = new File("C://Kalypso//Test_Meike//node_discharge.dat");
		BlockTimeSeries blockSerie = new BlockTimeSeries();
		Vector allowedKeys = new Vector();
		allowedKeys.addElement(String.valueOf(rootNode));
		blockSerie.importBlockFile(outFile, allowedKeys);
		Vector resultData = new Vector();
		resultData =
			blockSerie.getDischarge(
				String.valueOf(rootNode),
				instance.startDate_pegel,
				instance.endDate_pegel);
		for (int i = 0; i < resultData.size(); i++) {
			System.out.println(i+": "+resultData.get(i));
		}*/
		/*Double[] param =
			{
				new Double(10.0),
				new Double(20.0),
				new Double(30.0),
				new Double(40.0),
				new Double(50.0),
				new Double(60.0)};
		instance.makeModelxml(param, true);*/
		//instance.testInputFile();
	}
}
