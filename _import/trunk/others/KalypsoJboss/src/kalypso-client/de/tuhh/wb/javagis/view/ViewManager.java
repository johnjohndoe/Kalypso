package de.tuhh.wb.javagis.view;

//import java.util.Vector;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDesktopPane;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JToolBar;
import javax.swing.ListCellRenderer;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.event.InternalFrameEvent;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import de.tuhh.wb.javagis.Main;
import de.tuhh.wb.javagis.AutomateCalibration.SCE_KALYPSO;
import de.tuhh.wb.javagis.data.Version;
import de.tuhh.wb.javagis.data.VersionAccess;
import de.tuhh.wb.javagis.data.VersionClass;
import de.tuhh.wb.javagis.tools.I18n;
import de.tuhh.wb.javagis.view.netview.GisNetView;
import de.tuhh.wb.javagis.view.projectview.ProjectView;
import de.tuhh.wb.javagis.view.tableview.GisTableView;

public class ViewManager
	extends JFrame
	implements
		WindowListener,
		ActionListener,
		ListSelectionListener,
		MouseListener,
		MouseMotionListener {
	//  public Version version;
	VersionAccess versionAccess;
	int selectedVersion;
	ClassLoader cl = this.getClass().getClassLoader();

	//ToolBars:

	// Buttons
	private JMenuBar menubar = new JMenuBar();
	private JMenu jMenu_ProjectManager = new JMenu();
	//private JMenu jMenu_Preprocessing = new JMenu();
	//private JMenu jMenu_Postprocessing = new JMenu();
	private JMenuItem jMenuItem_OpenProject = new JMenuItem();
	private JMenuItem jMenuItem_openConfView = new JMenuItem();
	private JMenuItem jMenuItem_Close = new JMenuItem();
	private JMenuItem jMenuItem_Exit = new JMenuItem();
	//private JMenuItem jMenuItem_Print = new JMenuItem();
	private JMenuItem jMenuItem_View = new JMenuItem();
	private JMenuItem jMenuItem_Layout = new JMenuItem();
	private JMenuItem jMenuItem_Control = new JMenuItem();
	private JMenuItem jMenuItem_SimCas = new JMenuItem();
	private JMenuItem jMenuItem_Run = new JMenuItem();
	//private JMenu jMenu_Post = new JMenu();
	private JMenuItem jMenuItem_ViewRes = new JMenuItem();
	private JMenuItem jMenuItem_ViewErr = new JMenuItem();
	private JMenuItem jMenuItem_About = new JMenuItem();
	private JMenuItem jMenuItem_Tutorial = new JMenuItem();
	private JMenu jMenu_Help = new JMenu();
	private JMenu jMenu_ModelD = new JMenu();
	private JMenuItem jMenuItem_Net_Param = new JMenuItem();
	private JMenuItem jMenuItem_Net = new JMenuItem();
	private JPanel jPanel1 = new JPanel();
	private JToolBar toolBar = new JToolBar();
	private BorderLayout borderLayout1 = new BorderLayout();
	private BorderLayout borderLayout2 = new BorderLayout();
	private JButton createVButton = new JButton();
	private JButton copyButton = new JButton();
	private JButton tableButton = new JButton();
	private JButton netButton = new JButton();
	private JButton renameButton = new JButton();
	private JButton removeButton = new JButton();
	private JButton xmlEDataButton = new JButton();
	private JButton xmlIButton = new JButton();
	private JButton calibrationButton = new JButton();

	private static JComboBox comboBox;
	private static boolean processingFlag = true;

	private static ProjectView projectView;

	public static JDesktopPane desktop = new JDesktopPane();
	//    KalypsoInterface kalypsoInterface;

	public static ConfigurationView confView;

	public static void addToDesktop(JInternalFrame frame) {
		desktop.add(frame);
	}

	public ViewManager() {
		JOptionPane.setRootFrame(this);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		getContentPane().setLayout(new BorderLayout());
		desktop.setDragMode(JDesktopPane.OUTLINE_DRAG_MODE);

		desktop.setPreferredSize(new Dimension(600, 500));
		getContentPane().add(desktop, BorderLayout.CENTER);

		//	I18n.setLanguage("deu");
		UIDefaults defaults = UIManager.getDefaults();
		defaults.put("OptionPane.yesButtonText", I18n.get("Dia_Yes"));
		defaults.put("OptionPane.noButtonText", I18n.get("Dia_No"));
		defaults.put("OptionPane.cancelButtonText", I18n.get("Dia_Cancel"));
		defaults.put("OptionPane.okButtonText", I18n.get("Dia_OK"));
		setTitle(I18n.get("windowTitle"));
		setVisible(true);

		try {
			Main.connectBCE();
		} catch (Exception e) {

			System.out.println(
				"could not connect to time series data base, check configuration");
			System.out.println(e.getMessage());
		}
		//	this.kalypsoInterface=new KalypsoInterface(desktop);
		//	JFrame frame=new JFrame("test");
		//	frame.getContentPane().add( kalypsoInterface.getProjectTreePanel() );
		//	frame.setVisible(true);
		//      kalypsoInterface.connectDataBase("localhost","port","user","pass");
		//	kalypsoInterface.createNewVersion("project","Modell","testing","V1","test...");

		//  System.out.println("testing");
		this.addWindowListener(this);
		try {
			jbInit();

			pack();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public ViewManager(JDesktopPane desktopPane) {
		//this.desktop = desktopPane;
		desktop = desktopPane;
	}

	public void showObjectTableView(Version version) {
		setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		Toolkit.getDefaultToolkit().sync();
		repaint();
		
		/*WaitingDialog waitDialog = new WaitingDialog();
		waitDialog.setVisible(true);
		waitDialog.setSize(150, 150);
		desktop.add(waitDialog);
		waitDialog.moveToFront();
		repaint();*/

		//GisTableView gisTableView = new GisTableView("TestVersion",version.getGisTableModels(),null,0,null,GisTableView.IS_GISELEMENTLIST);
		String beginTitle = null;
		if (version.getThemeKey().equals("Modell")) {
			beginTitle = I18n.get("windowTitleTV_Objects");
		} else {
			beginTitle = I18n.get("windowTitleTV");
		}
		String title =
			beginTitle
				+ version.getVersionProject()
				+ "/"
				+ version.getThemeKey()
				+ "/"
				+ version.getVersionState()
				+ "/"
				+ version.getVersionName()
				+ " (#"
				+ version.getVersionId()
				+ ")";
		boolean open = isViewOpen(title);
		if (!open) {
			GisTableView gisTableView =
				new GisTableView(title, version.getGisObjectTableModels());
			//System.out.println("Version: "+version.getVersionId());
			//System.out.println("Table: "+I18n.get("windowTitleTV")+version.getLabel()+" geöffnet");
			gisTableView.setVisible(true);
			gisTableView.setSize(670, 300);
			desktop.add(gisTableView);
			gisTableView.moveToFront();
		}
		setCursor(Cursor.getDefaultCursor());
		repaint();
	}

	public void showRelationTableView(Version version) {
		setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		Toolkit.getDefaultToolkit().sync();
		repaint();

		// 	GisTableView gisTableView = new GisTableView("TestVersion",version.getGisTableModels(),null,0,null,GisTableView.IS_GISELEMENTLIST);
		String title =
			I18n.get("windowTitleTV_Relations")
				+ version.getVersionProject()
				+ "/"
				+ version.getThemeKey()
				+ "/"
				+ version.getVersionState()
				+ "/"
				+ version.getVersionName()
				+ " (#"
				+ version.getVersionId()
				+ ")";
		boolean open = isViewOpen(title);
		if (!open) {
			GisTableView gisTableView =
				new GisTableView(title, version.getGisRelationTableModels());
			gisTableView.setVisible(true);
			gisTableView.setSize(670, 300);
			desktop.add(gisTableView);
			gisTableView.moveToFront();
		}

		setCursor(Cursor.getDefaultCursor());
		repaint();
	}

	public void showNetView(Version version) {
		setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		Toolkit.getDefaultToolkit().sync();
		repaint();

		String title =
			I18n.get("windowTitleNV")
				+ version.getVersionProject()
				+ "/"
				+ version.getThemeKey()
				+ "/"
				+ version.getVersionState()
				+ "/"
				+ version.getVersionName()
				+ " (#"
				+ version.getVersionId()
				+ ")";
		boolean open = isViewOpen(title);
		if (!open) {
			GisNetView gisNetView =
				new GisNetView(title, version.getGisNetModel());
			gisNetView.setVisible(true);
			gisNetView.setSize(400, 300);
			desktop.add(gisNetView);
			gisNetView.moveToFront();
			gisNetView.repaint();

			//	setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
			//	repaint();

			gisNetView.gisMap.zoomToFullExtent();
		}

		setCursor(Cursor.getDefaultCursor());
		repaint();
	}

	public void showProjectView() {
		setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		repaint();
		/*WaitingDialog waitDialog = new WaitingDialog();
		waitDialog.setVisible(true);
		waitDialog.setSize(150,150);
		desktop.add(waitDialog);
		waitDialog.moveToFront();
		repaint();*/

		String title = I18n.get("windowTitlePV");
		boolean open = isViewOpen(title);
		if (!open) {
			projectView = new ProjectView(title);
			projectView.setVisible(true);
			projectView.setSize(400, 300);
			desktop.add(projectView);
			projectView.moveToFront();
		}

		setCursor(Cursor.getDefaultCursor());
		repaint();
		//waitDialog.dispose();
	}

	public void xmlImport(String themeKey, Object vId, File file) {
		versionAccess.xmlImport(themeKey, vId, file);
	}

	public void xmlExport(String themeKey, Object vId, File file) {
		versionAccess.xmlExport(themeKey, vId, file);
	}

	/**private void selectionBar(Vector objectClassNames)
	 {
	 JToolBar selectionBar=new JToolBar("SelectionBar");
	 selectionBar.setLayout(new GridLayout (objectClassNames.size(),1));
	 for(int i=0;i<objectClassNames.size();i++)
	 {
	 selectionBar.add(new JCheckBox(objectClassNames.elementAt(i).toString(),true));
	 }
	 selectionBar.addSeparator();
	 selectionBar.setVisible(true);
	 //	add(selectionBar);
	 }*/

	public void mouseClicked(MouseEvent e) {
		Object source = e.getSource();
	}

	public void mouseEntered(MouseEvent e) {
	}

	public void mouseExited(MouseEvent e) {

	}

	public void mousePressed(MouseEvent e) {

	}

	public void mouseMoved(MouseEvent e) {
	}

	public void mouseDragged(MouseEvent e) {
	}
	public void mouseReleased(MouseEvent e) {
	}

	// ListSelectionListener
	public void valueChanged(ListSelectionEvent e) {
	}

	// AktionListener:
	public void actionPerformed(ActionEvent e) {
		String action = e.getActionCommand();

		if (action.equals("automateCalibration")) {
			System.out.println("start AutomateCalibration");
			SCE_KALYPSO.openSCEView();
		}

		if ("comboBoxChanged".equals(e.getActionCommand())) {
			if (processingFlag) {
				int index = comboBox.getItemCount();
				//System.out.println("Number of items: "+index);
				if (index > 0) {
					JInternalFrame frame =
						(JInternalFrame) comboBox.getSelectedItem();
					//System.out.println("Selcted item: "+frame.getTitle());
					try {
						frame.setIcon(false);
					} catch (Exception exception) {
						System.out.println("Cannot de-iconify frame!");
					}
					frame.moveToFront();
				}
				if ((comboBox.getItemCount()) == 0) {
					System.out.println("Set selected Item null");
					//comboBox.addItem(null);
					//ComboBoxModel boxModel = comboBox.getModel();
					comboBox.setSelectedItem(null);
					//comboBox.setSelectedIndex(-1);
				}
			}
		}
	}

	// WindowListener
	public void windowActivated(WindowEvent e) {
	} //          Invoked when the Window is set to be the active Window.

	public void windowClosed(WindowEvent e) {
		System.out.println("bye");
	} //       Invoked when a window has been closed as the result of calling dispose on the window.

	public void windowClosing(WindowEvent e) {
		// free resources...
		VersionClass.freeResources();
		System.out.println("bye");
	} //  Invoked when the user attempts to close the window from the window's system menu.

	public void windowDeactivated(WindowEvent e) {
	} //          Invoked when a Window is no longer the active Window.

	public void windowDeiconified(WindowEvent e) {
	} //          Invoked when a window is changed from a minimized to a normal state.

	public void windowIconified(WindowEvent e) {
	} //          Invoked when a window is changed from a normal to a minimized state.

	public void windowOpened(WindowEvent e) {
	}
	private void jbInit() throws Exception {
		//jMenu_ProjectManager.setMnemonic('P');
		jMenu_ProjectManager.setText(I18n.get("jMenu_ProjectManager"));
		//jMenu_Preprocessing.setText(I18n.get("jMenu_Preprocessing"));
		//jMenu_Preprocessing.setMnemonic('R');
		//jMenu_Preprocessing.setText("Preprocessing");
		//jMenu_Postprocessing.setMnemonic('e');
		//jMenu_Postprocessing.setText(I18n.get("jMenu_Processing"));
		jMenuItem_OpenProject.setText(I18n.get("jMenuItem_OpenProject"));
		jMenuItem_openConfView.setText(I18n.get("jMenuItem_openConfView"));
		jMenuItem_OpenProject.setActionCommand("getTree");
		jMenuItem_openConfView.setActionCommand("openConf");
		jMenuItem_OpenProject
			.addActionListener(new java.awt.event.ActionListener() {

			public void actionPerformed(ActionEvent e) {
				if ("getTree".equals(e.getActionCommand())) {
					showProjectView();
					jMenuItem_OpenProject.setEnabled(false);
					jMenuItem_Close.setEnabled(true);
					jMenuItem_View.setEnabled(true);
					jMenuItem_Control.setEnabled(true);
					jMenuItem_SimCas.setEnabled(true);
					jMenuItem_ViewErr.setEnabled(true);
					jMenuItem_ViewRes.setEnabled(true);
					jMenuItem_Run.setEnabled(true);
					jMenu_ModelD.setEnabled(true);
				}
			}
		});
		jMenuItem_openConfView
			.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent event) {
				if ("openConf".equals(event.getActionCommand())) {
					confView =
						new ConfigurationView(I18n.get("CV_Title"), true);
					confView.show();
				}
			}
		});
		jMenuItem_Close.setText(I18n.get("jMenuItem_Close"));
		jMenuItem_Close.setEnabled(false);
		jMenuItem_Close.setActionCommand("Close_Project");
		jMenuItem_Close.addActionListener(new java.awt.event.ActionListener() {

			public void actionPerformed(ActionEvent e) {
				if ("Close_Project".equals(e.getActionCommand())) {
					projectView.dispose();
					jMenuItem_OpenProject.setEnabled(true);
					jMenuItem_Close.setEnabled(false);
					jMenuItem_ViewErr.setEnabled(false);
				}
			}
		});
		jMenuItem_Exit.setText(I18n.get("jMenuItem_Exit"));
		//jMenuItem_Exit.setMnemonic('X');

		jMenuItem_Exit.setActionCommand("exit");
		jMenuItem_Exit.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				int n = 0;
				if ("exit".equals(e.getActionCommand())) {
					//       n = JOptionPane.showConfirmDialog(frame, "Do You really want to quit the program?");
					Object[] options =
						{
							I18n.get("Dia_Yes"),
							I18n.get("Dia_No"),
							I18n.get("Dia_Cancel")};
					n =
						JOptionPane.showOptionDialog(
							null,
							I18n.get("ExitDia_Question"),
							I18n.get("ExitDia_Title"),
							JOptionPane.YES_NO_CANCEL_OPTION,
							JOptionPane.QUESTION_MESSAGE,
							null,
							options,
							options[1]);

					switch (n) {
						case JOptionPane.NO_OPTION :
							break;
						case JOptionPane.YES_OPTION :
							VersionClass.freeResources();
							System.exit(0);
						case JOptionPane.CANCEL_OPTION :
							break;
						default :
							break;

					}
				}
			}
		});

		//jMenuItem_Print.setEnabled(false);
		//jMenuItem_Print.setText(I18n.get("jMenuItem_Print"));
		//jMenuItem_View.setMnemonic('V');
		jMenuItem_View.setText(I18n.get("jMenuItem_View"));
		jMenuItem_View.setEnabled(false);
		jMenuItem_Layout.setEnabled(false);
		//jMenuItem_Layout.setMnemonic('L');
		jMenuItem_Layout.setText(I18n.get("jMenuItem_Layout"));
		//jMenuItem_Control.setMnemonic('C');
		jMenuItem_Control.setText(I18n.get("jMenuItem_Control"));
		jMenuItem_Control.setEnabled(false);
		//jMenuItem_SimCas.setMnemonic('D');
		jMenuItem_SimCas.setText(I18n.get("jMenuItem_SimCas"));
		jMenuItem_SimCas.setEnabled(false);
		//jMenuItem_Run.setMnemonic('U');
		jMenuItem_Run.setText(I18n.get("jMenuItem_Run"));
		jMenuItem_Run.setEnabled(false);
		jMenuItem_Run.setActionCommand("simulate");
		jMenuItem_Run.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
			}
		});

		//jMenu_Post.setMnemonic('S');
		//jMenu_Post.setText(I18n.get("jMenu_Postprocessing"));
		//jMenuItem_ViewRes.setMnemonic('I');
		jMenuItem_ViewRes.setText(I18n.get("jMenuItem_ViewRes"));
		jMenuItem_ViewRes.setEnabled(false);
		//jMenuItem_ViewErr.setMnemonic('W');
		jMenuItem_ViewErr.setText(I18n.get("jMenuItem_ViewErr"));
		jMenuItem_ViewErr.setEnabled(false);
		jMenuItem_ViewErr.setActionCommand("viewLog");
		jMenuItem_ViewErr
			.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {

				if ("viewLog".equals(e.getActionCommand())) {
					LogView.getInstance().show();
				}
			}
		});

		//jMenuItem_About.setMnemonic('A');
		jMenuItem_About.setText(I18n.get("jMenuItem_About"));
		jMenuItem_About.setActionCommand("about");
		jMenuItem_About.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if ("about".equals(e.getActionCommand()))
					JOptionPane.showMessageDialog(null, "Kalypso 2003");
			}
		});

		//jMenuItem_Tutorial.setMnemonic('T');
		jMenuItem_Tutorial.setText(I18n.get("jMenuItem_Tutorial"));
		jMenuItem_Tutorial.setActionCommand("viewDocs");
		jMenuItem_Tutorial
			.addActionListener(new java.awt.event.ActionListener() {

			public void actionPerformed(ActionEvent e) {
				if ("viewDocs".equals(e.getActionCommand())) {
					Tutorial doc = new Tutorial();
				}
			}
		});
		//jMenu_Help.setMnemonic('H');
		jMenu_Help.setText(I18n.get("jMenu_Help"));
		//jMenu_ModelD.setMnemonic('E');
		jMenu_ModelD.setText(I18n.get("jMenuItem_Model1D"));
		jMenu_ModelD.setEnabled(false);
		jMenuItem_Net_Param.setText(I18n.get("jMenuItem_NetParam"));
		jMenuItem_Net.setEnabled(false);
		jMenuItem_Net.setText(I18n.get("jMenuItem_Net"));
		jPanel1.setLayout(borderLayout1);
		this.getContentPane().setLayout(borderLayout2);
		jPanel1.setPreferredSize(new Dimension(300, 40));
		/*createVButton.setMaximumSize(new Dimension(35, 309));
		createVButton.setPreferredSize(new Dimension(35, 35));
		createVButton.setToolTipText(I18n.get("ButtonCreateTT"));
		createVButton.setIcon(
			(new ImageIcon(cl.getResource("symbols/CreateV24.gif"))));
		//createVButton.setIcon((new ImageIcon( "symbols/CreateV24.gif")));
		copyButton.setMaximumSize(new Dimension(35, 309));
		copyButton.setPreferredSize(new Dimension(35, 35));
		copyButton.setToolTipText(I18n.get("ButtonCopyTT"));
		copyButton.setIcon(
			(new ImageIcon(cl.getResource("symbols/Copy24.gif"))));
		tableButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				tableButton_actionPerformed(e);
			}
		});
		tableButton.setMaximumSize(new Dimension(35, 110));
		tableButton.setPreferredSize(new Dimension(35, 35));
		tableButton.setToolTipText(I18n.get("ButtonTableTT"));
		tableButton.setText(I18n.get("ButtonTable"));
		netButton.setMaximumSize(new Dimension(35, 110));
		netButton.setPreferredSize(new Dimension(35, 35));
		netButton.setToolTipText(I18n.get("ButtonNetTT"));
		netButton.setText(I18n.get("ButtonNet"));
		renameButton.setMaximumSize(new Dimension(35, 110));
		renameButton.setPreferredSize(new Dimension(35, 35));
		renameButton.setToolTipText(I18n.get("ButtonRenameTT"));
		renameButton.setIcon(
			(new ImageIcon(cl.getResource("symbols/Rename24.gif"))));
		removeButton.setMaximumSize(new Dimension(35, 110));
		removeButton.setPreferredSize(new Dimension(35, 35));
		removeButton.setToolTipText(I18n.get("ButtonRemoveTT"));
		removeButton.setIcon(
			(new ImageIcon(cl.getResource("symbols/Delete24.gif"))));
		removeButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				removeButton_actionPerformed(e);
			}
		});
		xmlEDataButton.setMaximumSize(new Dimension(35, 110));
		xmlEDataButton.setPreferredSize(new Dimension(35, 35));
		xmlEDataButton.setToolTipText(I18n.get("ButtonXMLEDataTT"));
		xmlEDataButton.setIcon(
			(new ImageIcon(cl.getResource("symbols/Export24.gif"))));
		xmlIButton.setMaximumSize(new Dimension(35, 119));
		xmlIButton.setPreferredSize(new Dimension(35, 35));
		xmlIButton.setToolTipText(I18n.get("ButtonXMLIDataTT"));
		xmlIButton.setIcon(
			(new ImageIcon(cl.getResource("symbols/Import24.gif"))));*/
		calibrationButton.setMaximumSize(new Dimension(30, 110));
		calibrationButton.setPreferredSize(new Dimension(30, 30));
		calibrationButton.setToolTipText(I18n.get("ButtonAutomateCalibration"));
		calibrationButton.setIcon(
			(new ImageIcon(cl.getResource("symbols/calibration.gif"))));
		calibrationButton.addActionListener(this);
		calibrationButton.setActionCommand("automateCalibration");

		comboBox = new JComboBox();
		comboBox.setRenderer(new MyCellRenderer());
		comboBox.addActionListener(this);

		menubar.setBorderPainted(true);
		menubar.add(jMenu_ProjectManager);
		//menubar.add(jMenu_Preprocessing);
		//menubar.add(jMenu_Postprocessing);
		//menubar.add(jMenu_Post);
		menubar.add(jMenu_Help);

		jMenu_ProjectManager.add(jMenuItem_OpenProject);
		jMenu_ProjectManager.add(jMenuItem_Close);
		jMenu_ProjectManager.add(jMenuItem_openConfView);
		jMenu_ProjectManager.add(jMenuItem_ViewErr);
		jMenu_ProjectManager.addSeparator();
		//jMenu_ProjectManager.add(jMenuItem_Print);
		jMenu_ProjectManager.add(jMenuItem_Exit);

		//jMenu_Preprocessing.add(jMenuItem_View);
		//jMenu_Preprocessing.add(jMenu_ModelD);
		//jMenu_Preprocessing.add(jMenuItem_Layout);
		//jMenu_Postprocessing.add(jMenuItem_Control);
		//jMenu_Postprocessing.add(jMenuItem_SimCas);
		//jMenu_Postprocessing.addSeparator();
		//jMenu_Postprocessing.add(jMenuItem_Run);
		//jMenu_Post.add(jMenuItem_ViewRes);
		//jMenu_Post.add(jMenuItem_ViewErr);
		jMenu_Help.add(jMenuItem_Tutorial);
		jMenu_Help.add(jMenuItem_About);
		jMenu_ModelD.add(jMenuItem_Net);
		jMenu_ModelD.add(jMenuItem_Net_Param);
		//this.getContentPane().add(jPanel1, BorderLayout.NORTH);
		//this.getContentPane().add(jPanel1, BorderLayout.CENTER);

		/*toolBar.add(createVButton, null);
		toolBar.add(copyButton, null);
		toolBar.add(tableButton, null);
		toolBar.add(netButton, null);
		toolBar.add(renameButton, null);
		toolBar.add(removeButton, null);
		toolBar.add(xmlEDataButton, null);
		toolBar.add(xmlIButton, null);*/
		String autoCal = Main.props.getProperty("automatedCalibration");
		System.out.println("Automated Calibration: " + autoCal);
		//if(autoCal!=null && autoCal.equals("enabled")){
		if (autoCal != null
			&& (autoCal.equals("enabled1") || autoCal.equals("enabled2"))) {
			//jPanel1.add(toolBar, BorderLayout.CENTER);
			//toolBar.add(calibrationButton, null);
			menubar.add(calibrationButton);
		}
		this.getContentPane().add(desktop, BorderLayout.CENTER);

		menubar.add(comboBox);
		this.setJMenuBar(menubar);

	}

	/*
	 //File | Exit action performed
	 public void jMenuFileExit_actionPerformed(ActionEvent e)
	 {
	 VersionClass.freeResources();
	 System.exit(0);
	 }
	 */

	void tableButton_actionPerformed(ActionEvent e) {

	}

	void removeButton_actionPerformed(ActionEvent e) {

	}

	public static void addViewToList(InternalFrameEvent e) {
		processingFlag = false;
		JInternalFrame frame = e.getInternalFrame();
		//System.out.println("Event: " + e + ", Title: " + frame.getTitle());
		comboBox.addItem(frame);
		processingFlag = true;
	}

	public static void removeViewFromList(InternalFrameEvent e) {
		processingFlag = false;
		int index = -1;
		JInternalFrame frame = e.getInternalFrame();
		//String closedFrame = frame.getTitle();
		//System.out.println("Event: " + e + ", Title: " + frame.getTitle());
		//System.out.println("Title: " + closedFrame);
		for (int i = 0; i < comboBox.getItemCount(); i++) {
			Object comboItemObject = comboBox.getItemAt(i);
			/*String comboItemString =
				((JInternalFrame) comboItemObject).getTitle();*/
			//System.out.println("Actual Title: "+comboItemString);
			if (frame.equals(comboItemObject)) {
				index = i;
			}
		}
		//ProjectView.comboBox.removeItem(frame);
		if (index > -1) {
			comboBox.removeItemAt(index);
			//System.out.println("Number of items (remove): "+comboBox.getItemCount());
		}
		processingFlag = true;
	}

	public static boolean isViewOpen(String title) {
		int itemCount = comboBox.getItemCount();
		//System.out.println("itemCount: "+itemCount);
		//System.out.println("search title: "+title);
		boolean flag = false;
		for (int i = 0; i < itemCount; i++) {
			Object comboItemObject = comboBox.getItemAt(i);
			String comboItemString =
				((JInternalFrame) comboItemObject).getTitle();
			//System.out.println("Actual Title: "+comboItemString);
			if (title.equals(comboItemString)) {
				JInternalFrame frame = (JInternalFrame) comboItemObject;
				try {
					frame.setIcon(false);
				} catch (Exception exception) {
					System.out.println("Cannot de-iconify frame!");
				}
				frame.moveToFront();
				flag = true;
			}
		}
		//System.out.println("Flag: "+flag);
		return flag;
	}

}

class MyCellRenderer extends JLabel implements ListCellRenderer {
	public MyCellRenderer() {
		setOpaque(true);
	}
	public Component getListCellRendererComponent(
		JList list,
		Object value,
		int index,
		boolean isSelected,
		boolean cellHasFocus) {
		if (value != null) {
			JInternalFrame frame = (JInternalFrame) value;
			setText(frame.getTitle());
			//System.out.println("Class: "+value.getClass().toString());
			//setText(value.toString());
			setBackground(isSelected ? Color.red : Color.white);
			setForeground(isSelected ? Color.white : Color.black);
		} else {
			setText("");
			setBackground(isSelected ? Color.red : Color.white);
			setForeground(isSelected ? Color.white : Color.black);
		}
		return this;

	}
	
}
