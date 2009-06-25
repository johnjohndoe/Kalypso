package de.tuhh.wb.javagis.view;

import java.util.Vector;



import javax.swing.event.ListSelectionListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.InternalFrameListener;
import javax.swing.event.InternalFrameEvent;

import java.util.Vector;
import java.lang.Math;

import de.tuhh.wb.javagis.data.VersionClass;
import de.tuhh.wb.javagis.Main;
import de.tuhh.wb.javagis.data.VersionAccess;
import de.tuhh.wb.javagis.data.VersionAccessImpl;
import de.tuhh.wb.javagis.view.JobRequest;
import de.tuhh.wb.javagis.data.event.VersionListener;
import de.tuhh.wb.javagis.data.event.KalypsoEventManager;
import ejb.event.EJBEvent;
import javax.swing.*;
import java.awt.*;
import de.tuhh.wb.javagis.data.Version;
import de.tuhh.wb.javagis.view.tableview.GisTableView;
import de.tuhh.wb.javagis.view.netview.GisNetView;
import de.tuhh.wb.javagis.view.projectview.ProjectView;
import de.tuhh.wb.javagis.view.projectview.SimulationDialog;
import de.tuhh.wb.javagis.view.Tutorial;
import com.borland.jbcl.layout.*;
import java.awt.event.*;


public class ViewManager extends JFrame implements WindowListener,ActionListener, ListSelectionListener, MouseListener,MouseMotionListener
{
    //  public Version version;
    VersionAccess versionAccess;
       int selectedVersion;

    //ToolBars:

    // Buttons
  private JMenuBar menubar = new JMenuBar();
  private JMenu jMenu_ProjectManager = new JMenu();
  private JMenu jMenu_Preprocessing = new JMenu();
  private JMenu jMenu_Postprocessing = new JMenu();
  private JMenuItem jMenuItem_OpenProject = new JMenuItem();
  private JMenuItem jMenuItem_Close = new JMenuItem();
  private JMenuItem jMenuItem_Exit = new JMenuItem();
  private JMenuItem jMenuItem_Print = new JMenuItem();
  private JMenuItem jMenuItem_View = new JMenuItem();
  private JMenuItem jMenuItem_Layout = new JMenuItem();
  private JMenuItem jMenuItem_Control = new JMenuItem();
  private JMenuItem jMenuItem_SimCas = new JMenuItem();
  private JMenuItem jMenuItem_Run = new JMenuItem();
  private JMenu jMenu_Post = new JMenu();
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

  public static JDesktopPane desktop = new JDesktopPane();
    //    KalypsoInterface kalypsoInterface;


    public ViewManager()
    {
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        getContentPane().setLayout(new BorderLayout());
        desktop.setDragMode(JDesktopPane.OUTLINE_DRAG_MODE);

	desktop.setPreferredSize(new Dimension(600,500));
        getContentPane().add(desktop,BorderLayout.CENTER);


        setTitle("Kalypso-Rainfall-Runoff-Model");
        setVisible(true);

        ProjectView projectView=new ProjectView();
        projectView.setVisible(true);
        projectView.setSize(400,300);
	//     desktop.add(projectView);
	//    projectView.moveToFront();

        Main.connectBCE();
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
    }
    catch(Exception e) {
      e.printStackTrace();
    }
    }

    public ViewManager(JDesktopPane desktopPane)
    {
        this.desktop=desktopPane;
    }


    public void showObjectTableView(Version version)
    {
      //GisTableView gisTableView = new GisTableView("TestVersion",version.getGisTableModels(),null,0,null,GisTableView.IS_GISELEMENTLIST);
        GisTableView gisTableView = new GisTableView("TableView: "+version.getLabel(),version.getGisObjectTableModels());
        gisTableView.setVisible(true);
        gisTableView.setSize(670,300);
        desktop.add(gisTableView);
        gisTableView.moveToFront();
    }

    public void showRelationTableView(Version version)
    {
        // 	GisTableView gisTableView = new GisTableView("TestVersion",version.getGisTableModels(),null,0,null,GisTableView.IS_GISELEMENTLIST);
         GisTableView gisTableView = new GisTableView("TableView: "+version.getLabel(),version.getGisRelationTableModels());
        gisTableView.setVisible(true);
        gisTableView.setSize(670,300);
        desktop.add(gisTableView);
        gisTableView.moveToFront();
    }

    public void showNetView(Version version)
    {
        GisNetView gisNetView = new GisNetView(version.getGisNetModel());
        gisNetView.setVisible(true);
        gisNetView.setSize(400,300);
        desktop.add(gisNetView);
        gisNetView.moveToFront();
        gisNetView.repaint();

    }

    public void showProjectView()
    {
        ProjectView projectView = new ProjectView();
        projectView.setVisible(true);
        projectView.setSize(400,300);
        desktop.add(projectView);

    }





    private void selectionBar(Vector objectClassNames)
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
    }

    public void mouseClicked(MouseEvent e)
    {
        Object source = e.getSource();
    }

    public void mouseEntered(MouseEvent e)
    {}

    public void mouseExited(MouseEvent e)
    {

    }

    public void mousePressed(MouseEvent e)
    {

    }

    public void mouseMoved(MouseEvent e)
    {
    }

    public void mouseDragged(MouseEvent e)
    {
    }
    public void mouseReleased(MouseEvent e)
    {
    }


    // ListSelectionListener
    public void valueChanged(ListSelectionEvent e)
    {
    }

    // AktionListener:
    public void actionPerformed(ActionEvent e)
    {
    }

    // WindowListener
    public void windowActivated(WindowEvent e)
    {}//          Invoked when the Window is set to be the active Window.

    public void windowClosed(WindowEvent e)
    {
        System.out.println("bye");
    }//       Invoked when a window has been closed as the result of calling dispose on the window.


    public   void windowClosing(WindowEvent e)
    {
        // free resources...
        VersionClass.freeResources();
        System.out.println("bye");       
    }//  Invoked when the user attempts to close the window from the window's system menu.

    public   void windowDeactivated(WindowEvent e)
    {}//          Invoked when a Window is no longer the active Window.

    public   void windowDeiconified(WindowEvent e)
    {}//          Invoked when a window is changed from a minimized to a normal state.

    public   void windowIconified(WindowEvent e)
    {}//          Invoked when a window is changed from a normal to a minimized state.

    public   void windowOpened(WindowEvent e)
    {}
  private void jbInit() throws Exception {
    jMenu_ProjectManager.setMnemonic('P');
    jMenu_ProjectManager.setText("Project Manager");
    jMenu_Preprocessing.setText("Preprocessing");
    jMenu_Preprocessing.setMnemonic('R');
    jMenu_Preprocessing.setText("Preprocessing");
    jMenu_Postprocessing.setMnemonic('e');
    jMenu_Postprocessing.setText("Processing");
    jMenuItem_OpenProject.setText("Open Project/Log In");

  jMenuItem_OpenProject.setActionCommand("getTree");
   jMenuItem_OpenProject.addActionListener(new java.awt.event.ActionListener()
   {

  public void actionPerformed(ActionEvent e) {
    if("getTree".equals(e.getActionCommand()))
              {
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

    jMenuItem_Close.setText("Close Project/Log Out");
    jMenuItem_Close.setEnabled(false);
    jMenuItem_Exit.setText("Exit");
    jMenuItem_Exit.setMnemonic('X');

    jMenuItem_Exit.setActionCommand("exit");
    jMenuItem_Exit.addActionListener(new java.awt.event.ActionListener() {
    public void actionPerformed(ActionEvent e) {
        int n = 0 ;
        if("exit".equals(e.getActionCommand()))
            {
                //       n = JOptionPane.showConfirmDialog(frame, "Do You really want to quit the program?");
                n = JOptionPane.showConfirmDialog(null, "Do You really want to quit the program?");

                switch (n)
                    {
                   case JOptionPane.NO_OPTION:
                    break;
                    case JOptionPane.YES_OPTION:
			VersionClass.freeResources();
                        System.exit(0);
                    default:
                        break ;

                    }
            }
    }} );

    jMenuItem_Print.setEnabled(false);
    jMenuItem_Print.setText("Print");
    jMenuItem_View.setMnemonic('V');
    jMenuItem_View.setText("View");
    jMenuItem_View.setEnabled(false);
    jMenuItem_Layout.setEnabled(false);
    jMenuItem_Layout.setMnemonic('L');
    jMenuItem_Layout.setText("Layout");
    jMenuItem_Control.setMnemonic('C');
    jMenuItem_Control.setText("Control Data Setup");
    jMenuItem_Control.setEnabled(false);
    jMenuItem_SimCas.setMnemonic('D');
    jMenuItem_SimCas.setText("Defining Simualtion Cases");
    jMenuItem_SimCas.setEnabled(false);
    jMenuItem_Run.setMnemonic('U');
    jMenuItem_Run.setText("Run Model");
    jMenuItem_Run.setEnabled(false);
    jMenuItem_Run.setActionCommand("simulate");
    jMenuItem_Run.addActionListener(new java.awt.event.ActionListener() {
    public void actionPerformed(ActionEvent e)
            {
            }
    });

    jMenu_Post.setMnemonic('S');
    jMenu_Post.setText("Postprocessing");
    jMenuItem_ViewRes.setMnemonic('I');
    jMenuItem_ViewRes.setText("View Results");
    jMenuItem_ViewRes.setEnabled(false);
    jMenuItem_ViewErr.setMnemonic('W');
    jMenuItem_ViewErr.setText("View Log File");
    jMenuItem_ViewErr.setEnabled(false);
    jMenuItem_ViewErr.setActionCommand("viewLog");
    jMenuItem_ViewErr.addActionListener(new java.awt.event.ActionListener() {
    public void actionPerformed(ActionEvent e) {

    if("viewLog".equals(e.getActionCommand()))
            {
                LogView.getInstance().show();
	    }
    }});

    jMenuItem_About.setMnemonic('A');
    jMenuItem_About.setText("About ");
    jMenuItem_About.setActionCommand("about");
    jMenuItem_About.addActionListener(new java.awt.event.ActionListener() {
    public void actionPerformed(ActionEvent e) {
        if("about".equals(e.getActionCommand()))

       JOptionPane.showMessageDialog(null, "Kalypso 2003");
    }} );

    jMenuItem_Tutorial.setMnemonic('T');
    jMenuItem_Tutorial.setText("Tutorial");
    jMenuItem_Tutorial.setActionCommand("viewDocs");
    jMenuItem_Tutorial.addActionListener(new java.awt.event.ActionListener()
    {

   public void actionPerformed(ActionEvent e) {
     if("viewDocs".equals(e.getActionCommand()))
               {
            Tutorial doc=new Tutorial();
	       }
   }
    });
    jMenu_Help.setMnemonic('H');
    jMenu_Help.setText("Help");
    jMenu_ModelD.setMnemonic('E');
    jMenu_ModelD.setText("Edit Model Data");
    jMenu_ModelD.setEnabled(false);
    jMenuItem_Net_Param.setText("Edit Parameters");
    jMenuItem_Net.setEnabled(false);
    jMenuItem_Net.setText("Edit Network Data");
    jPanel1.setLayout(borderLayout1);
    this.getContentPane().setLayout(borderLayout2);
    jPanel1.setPreferredSize(new Dimension(300, 40));
    createVButton.setMaximumSize(new Dimension(35, 309));
    createVButton.setPreferredSize(new Dimension(35, 35));
    createVButton.setToolTipText("CreateVersion");
    createVButton.setIcon((new ImageIcon( "symbols/CreateV24.gif")));
    copyButton.setMaximumSize(new Dimension(35, 309));
    copyButton.setPreferredSize(new Dimension(35, 35));
    copyButton.setToolTipText("CopyVersion");
    copyButton.setIcon((new ImageIcon( "symbols/Copy24.gif")));
    tableButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        tableButton_actionPerformed(e);
      }
    });
    tableButton.setMaximumSize(new Dimension(35, 110));
    tableButton.setPreferredSize(new Dimension(35, 35));
    tableButton.setToolTipText("OpenTableView");
    tableButton.setText("Tbl");
    netButton.setMaximumSize(new Dimension(35, 110));
    netButton.setPreferredSize(new Dimension(35, 35));
    netButton.setToolTipText("OpenNetView");
    netButton.setText("Net");
    renameButton.setMaximumSize(new Dimension(35, 110));
    renameButton.setPreferredSize(new Dimension(35, 35));
    renameButton.setToolTipText("RenameVersion");
    renameButton.setIcon((new ImageIcon( "symbols/Rename24.gif")));
    removeButton.setMaximumSize(new Dimension(35, 110));
    removeButton.setPreferredSize(new Dimension(35, 35));
    removeButton.setToolTipText("RemoveVersion");
    removeButton.setIcon((new ImageIcon( "symbols/Delete24.gif")));
    removeButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        removeButton_actionPerformed(e);
      }
    });
    xmlEDataButton.setMaximumSize(new Dimension(35, 110));
    xmlEDataButton.setPreferredSize(new Dimension(35, 35));
    xmlEDataButton.setToolTipText("XML-Export");
    xmlEDataButton.setIcon((new ImageIcon( "symbols/Export24.gif")));
    xmlIButton.setMaximumSize(new Dimension(35, 119));
    xmlIButton.setPreferredSize(new Dimension(35, 35));
    xmlIButton.setToolTipText("XML-Import");
    xmlIButton.setIcon((new ImageIcon( "symbols/Import24.gif")));
    menubar.add(jMenu_ProjectManager);
    menubar.add(jMenu_Preprocessing);
    menubar.add(jMenu_Postprocessing);
    menubar.add(jMenu_Post);
    menubar.add(jMenu_Help);
    jMenu_ProjectManager.add(jMenuItem_OpenProject);
    jMenu_ProjectManager.add(jMenuItem_Close);
    jMenu_ProjectManager.addSeparator();
    jMenu_ProjectManager.add(jMenuItem_Print);
    jMenu_ProjectManager.add(jMenuItem_Exit);
    jMenu_Preprocessing.add(jMenuItem_View);
    jMenu_Preprocessing.add(jMenu_ModelD);
    jMenu_Preprocessing.add(jMenuItem_Layout);
    jMenu_Postprocessing.add(jMenuItem_Control);
    jMenu_Postprocessing.add(jMenuItem_SimCas);
    jMenu_Postprocessing.addSeparator();
    jMenu_Postprocessing.add(jMenuItem_Run);
    jMenu_Post.add(jMenuItem_ViewRes);
    jMenu_Post.add(jMenuItem_ViewErr);
    jMenu_Help.add(jMenuItem_Tutorial);
    jMenu_Help.add(jMenuItem_About);
    jMenu_ModelD.add(jMenuItem_Net);
    jMenu_ModelD.add(jMenuItem_Net_Param);
    this.getContentPane().add(jPanel1, BorderLayout.NORTH);
    jPanel1.add(toolBar, BorderLayout.CENTER);
    toolBar.add(createVButton, null);
    toolBar.add(copyButton, null);
    toolBar.add(tableButton, null);
    toolBar.add(netButton, null);
    toolBar.add(renameButton, null);
    toolBar.add(removeButton, null);
    toolBar.add(xmlEDataButton, null);
    toolBar.add(xmlIButton, null);
    this.getContentPane().add(desktop,  BorderLayout.CENTER);

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
}

/*
;*/





