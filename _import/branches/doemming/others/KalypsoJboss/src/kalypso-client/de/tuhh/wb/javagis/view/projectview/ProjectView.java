package de.tuhh.wb.javagis.view.projectview;

import java.util.Vector;
import java.util.Hashtable;
import javax.swing.tree.*;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import java.io.File;
import java.awt.event.MouseListener;
import java.awt.event.MouseEvent;
import de.tuhh.wb.javagis.view.LogView;
import de.tuhh.wb.javagis.view.Tutorial;
import de.tuhh.wb.javagis.data.VersionAccess;
import de.tuhh.wb.javagis.data.VersionAccessImpl;
import de.tuhh.wb.javagis.view.JobRequest;
import de.tuhh.wb.javagis.data.event.VersionListener;
import de.tuhh.wb.javagis.data.event.KalypsoEventManager;
import ejb.event.EJBEvent;
import javax.swing.*;
import java.awt.*;

public class ProjectView extends JInternalFrame implements ActionListener, MouseListener, VersionListener
{
    VersionAccess versionAccess;
    int selectedVersion;
    Hashtable versionHash;

    JScrollPane jScrollPane = new JScrollPane();

    JTree jTree;
    DefaultTreeModel treeModel;

    JFileChooser fileChooser;

    public ProjectView()
    {
	super("ProjectView",true,false,true,true);
	this.selectedVersion=-1;
	this.versionAccess=null;
	this.versionHash=new Hashtable();
	this.fileChooser=new JFileChooser();
	KalypsoEventManager.getInstance().addVersionListener(this);
	initMask();
    }

    public void initMask()
    {
	getContentPane().removeAll();
	treeModel=new DefaultTreeModel(new DefaultMutableTreeNode("not connected to kalypso-server"));
	jTree= new JTree(treeModel);
   //     this.setFrameIcon((new ImageIcon("symbols/Legend16.gif")));
	JPanel jPanel = new JPanel();
	jScrollPane.getViewport().add(jTree);
	jTree.addMouseListener(this);
	//	jSplitPane.resetToPreferredSizes();
	this.getContentPane().add(jScrollPane);
	createFileMenu();
    }

    public void createFileMenu()
    {
        JMenuBar menubar = new JMenuBar ();
        JMenu edit = new JMenu ("Data Manager");
        JMenu view = new JMenu ("View");
	edit.addActionListener(this);
	view.addActionListener(this);
     // edit.setActionCommand("Zoom In");
        edit.setIcon((new ImageIcon("symbols/Edit16.gif")));
        JMenuItem mi;

        mi = new JMenuItem("Connect to Database");
        mi.setIcon((new ImageIcon ("symbols/FullExtent16.gif")));
        mi.setActionCommand("connectToServer");
        mi.addActionListener(this);
        edit.add(mi);

        mi = new JMenuItem("Create Version");
        mi.addActionListener(this);
        mi.setIcon((new ImageIcon("symbols/New16.gif")));
        mi.setActionCommand("createVersion");
	//        mi.addActionListener(this);
        edit.add(mi);

        mi = new JMenuItem("Reload Project Tree");
        mi.setIcon((new ImageIcon( "symbols/Refresh16.gif")));
        mi.setActionCommand("updateProjectTree");
        mi.addActionListener(this);
        edit.add(mi);
    /*    mi = new JMenuItem("View Log File");
        mi.setIcon((new ImageIcon("symbols/View16.gif")));
        mi.setActionCommand("viewLog");
        mi.addActionListener(this);
        edit.add(mi);*/


      ////Adding Buttons to the view/////////////////

    /*    JButton connect = new JButton();
        connect.setIcon((new ImageIcon ("symbols/FullExtent16.gif")));
        connect.setToolTipText("ConnectToDatabase");
        connect.setActionCommand("connectToServer");
        connect.addActionListener(this);
        JButton createV = new JButton();
        createV.setIcon((new ImageIcon ("symbols/vectorSets16.gif")));
        createV.setToolTipText("CreateVersion");
        createV.setActionCommand("createVersion");
        createV.addActionListener(this);
        createV.setEnabled(false);
        JButton reload = new JButton();
        reload.setIcon((new ImageIcon ("symbols/Refresh16.gif")));
        reload.setEnabled(false);
        reload.setToolTipText("UpdateProjectTree");
        reload.setActionCommand("updateProjectTree");
        reload.addActionListener(this);*/
	// JMenuBar menubar= new JMenuBar();

          menubar.add(edit);
    //    menubar.add(view);
    //    menubar.add(connect);
    //    menubar.add(createV);
    //    menubar.add(reload);

	this.setJMenuBar(menubar);
    }

    private void reloadProjectTree()
    {
	// server access is allowed only for local users
	String host="localhost";
	String port="";
	String user="kalypso-user";
	String pass="";

	DefaultMutableTreeNode rootNode= new DefaultMutableTreeNode(host);

	if(versionAccess==null)
	    versionAccess=(VersionAccess) new VersionAccessImpl(host,port,user,pass);
	versionAccess.updateIndex();

	Vector themeKeys=versionAccess.getThemeKeys();
	//	for(int i=0;i<themeKeys.size();i++)
	//	    System.out.println("ThemeKey form ThemeManager: "+(themeKeys.elementAt(i))+".");
	Hashtable nodes=new Hashtable();
	nodes.put("root",rootNode);
	buildTree(0,nodes,versionAccess);
	treeModel.setRoot(rootNode);
	versionHash.clear();
	for(int row=0;row<versionAccess.getSize();row++)
	    {
		String path=host+getPath(row,3,versionAccess);
		versionHash.put(path,new Integer(row));
	    }
    }

    public void buildTree(int deep,Hashtable parentNodes,VersionAccess versionAccess)
    {
	Hashtable nodes=new Hashtable();
	for(int row=0;row<versionAccess.getSize();row++)
	    {
		String path=getPath(row,deep,versionAccess);
		if(!nodes.containsKey(path))
		    {
			String newText=getText(row,deep,versionAccess);
			DefaultMutableTreeNode newNode;
			//	if(newText==null || !"-".equals(newText))
			//			    {
			newNode=new DefaultMutableTreeNode(newText);
			((DefaultMutableTreeNode)parentNodes.get(getPath(row,deep-1,versionAccess))).add(newNode);
			//			    }
			//			else
			//			    newNode=(DefaultMutableTreeNode)parentNodes.get(getPath(row,deep-1,versionAccess));
			nodes.put(path,newNode);
		    }
	    }
	if(deep<3)
	    buildTree(deep+1,nodes,versionAccess);
    }

    private String getPath(int row,int deep,VersionAccess versionAccess)
    {
	String path="";
	if(deep<0)
	    return "root";
	for(int i=0;i<=deep;i++)
	    path=path+getText(row,i,versionAccess);
	return path;
    }

    private String getText(int row,int col,VersionAccess versionAccess)
    {
	switch(col)
	    {
	    case 0:
		return versionAccess.getProjectName(row);
	    case 1:
		return versionAccess.getThemeName(row);
	    case 2:
		return versionAccess.getState(row);
	    case 3:
		return versionAccess.getName(row);
	    default:
		break;
	    }
	return "ERROR";
    }

    //MouseListener
    public void mouseClicked(MouseEvent e)
    {}
    public  void mouseEntered(MouseEvent e)
    {}
    public  void mouseExited(MouseEvent e)
    {}
    public  void mousePressed(MouseEvent e)
    {
	maybeShowPopup(e);
    }

    public void mouseReleased(MouseEvent e)
    {
	maybeShowPopup(e);
    }

    public void maybeShowPopup(MouseEvent e)
    {
	int selRow = jTree.getRowForLocation(e.getX(), e.getY());
	jTree.setSelectionRow(selRow);
	if(selRow!=-1)
	    {
		TreePath selPath = jTree.getPathForLocation(e.getX(), e.getY());
		Object[] objects=selPath.getPath();

		String selectedVersion="";

		if(selPath.getPathCount()==5 && e.isPopupTrigger())
		    {
			String theme=objects[2].toString();
			for(int i=0;i<objects.length;i++)
			    {
				selectedVersion+=objects[i].toString();
			    }
			selectVersion(selectedVersion);

			JPopupMenu popup = new JPopupMenu();
			JMenuItem menuItem = new JMenuItem("copy Version");
                        menuItem.setIcon((new ImageIcon( "symbols/vectorSets16.gif")));
			menuItem.setActionCommand("copySelectedVersion");
			menuItem.addActionListener(this);
			popup.add(menuItem);

			menuItem = new JMenuItem(" Tbl open TableView (objects)");
			menuItem.setActionCommand("openObjectTableViewFromSelectedVersion");
			menuItem.addActionListener(this);
			popup.add(menuItem);

			
			if("Modell".equals(theme))
			    {
				menuItem = new JMenuItem("open TableView (relations)");
				menuItem.setActionCommand("openRelationTableViewFromSelectedVersion");
				menuItem.addActionListener(this);
				popup.add(menuItem);
			    }

			if("Modell".equals(theme))
			    {
				menuItem = new JMenuItem(" Net Open NetView");
				menuItem.setActionCommand("openNetViewFromSelectedVersion");
				menuItem.addActionListener(this);
				popup.add(menuItem);
			    }

			menuItem = new JMenuItem("Rename Version");
			menuItem.setActionCommand("renameSelectedVersion");
                        menuItem.setIcon((new ImageIcon( "symbols/Rename16.gif")));
			menuItem.addActionListener(this);
			popup.add(menuItem);

			menuItem = new JMenuItem("Remove Version");
			menuItem.setActionCommand("removeSelectedVersion");
                        menuItem.setIcon((new ImageIcon( "symbols/Delete16.gif")));
			menuItem.addActionListener(this);
			popup.add(menuItem);

			if("SimulationCase".equals(theme))
			    {
				menuItem = new JMenuItem("run simulation (from SimulationCase)");
				menuItem.setActionCommand("simulate");
				menuItem.addActionListener(this);
				popup.add(menuItem);
			    }
			menuItem = new JMenuItem("XML-export");
			menuItem.setActionCommand("xml-export");
                        menuItem.setIcon((new ImageIcon( "symbols/Export16.gif")));
			menuItem.addActionListener(this);
			popup.add(menuItem);

			menuItem = new JMenuItem("XML-import");
			menuItem.setActionCommand("xml-import");
                        menuItem.setIcon((new ImageIcon( "symbols/FullExtent16.gif")));
			menuItem.addActionListener(this);
			popup.add(menuItem);
			popup.show(e.getComponent(),
				   e.getX(), e.getY());
		    }
	    }
    }

    private void selectVersion(String version)
    {
	if(versionHash.containsKey(version))
	    {
		selectedVersion=((Integer)versionHash.get(version)).intValue();
	    }
	else
	    selectedVersion=-1;
    }

    // ActionListener
    public void actionPerformed(ActionEvent e)
    {
	if("updateProjectTree".equals(e.getActionCommand()) ||
	   "connectToServer".equals(e.getActionCommand()))
	    {
		reloadProjectTree();

	    }

	if("viewLog".equals(e.getActionCommand()))
	    {
		LogView.getInstance().show();
	    }

//	if("viewDocs".equals(e.getActionCommand()))
//	    {
	//	Tutorial doc=new Tutorial();
		//LogView.getInstance().show();
//	    }

	if("createVersion".equals(e.getActionCommand()))
	    {
		if(versionAccess!=null)
		    {
			new VersionDialog("create Version",VersionDialog.CREATE,versionAccess,null,null);
		    }
		else
		    System.out.println("please connect to kalypso-server");
	    }
	if("renameSelectedVersion".equals(e.getActionCommand()))
	    {
		if(selectedVersion!=-1)
		{
		    String themeKey=versionAccess.getThemeKey(selectedVersion);
		    Object vId=versionAccess.getVersionId(selectedVersion);
		    new VersionDialog("rename Version",VersionDialog.RENAME,versionAccess,themeKey,vId);
		}
	    }
	if("copySelectedVersion".equals(e.getActionCommand()))
	    {
		if(selectedVersion!=-1)
		{
		    String themeKey=versionAccess.getThemeKey(selectedVersion);
		    Object vId=versionAccess.getVersionId(selectedVersion);

		    new VersionDialog("copy Version",VersionDialog.COPY,versionAccess,themeKey,vId);
		}
	    }

	if("removeSelectedVersion".equals(e.getActionCommand()))
	    {
		if(selectedVersion!=-1)
		{
		    versionAccess.removeVersion(selectedVersion);
		}
	    }

	if("openObjectTableViewFromSelectedVersion".equals(e.getActionCommand()))
	    {
		if(selectedVersion!=-1)
		    versionAccess.openObjectTableView(selectedVersion);
	    }
	if("openRelationTableViewFromSelectedVersion".equals(e.getActionCommand()))
	    {
		if(selectedVersion!=-1)
		    versionAccess.openRelationTableView(selectedVersion);
	    }
	if("openNetViewFromSelectedVersion".equals(e.getActionCommand()))
	    {
		if(selectedVersion!=-1)
		    versionAccess.openNetView(selectedVersion);
	    }
	if("xml-import".equals(e.getActionCommand()))
	    {
		if(selectedVersion!=-1)
		    {
			String themeKey=versionAccess.getThemeKey(selectedVersion);
			Object vId=versionAccess.getVersionId(selectedVersion);

			int returnVal = fileChooser.showDialog(this, "import xml");
			if(returnVal == JFileChooser.APPROVE_OPTION)
			    {
				File file=fileChooser.getSelectedFile();
				LogView.print("importing file to database ... ");
				versionAccess.xmlImport(themeKey,vId,file);
				LogView.println("done");
			    }
		    }
	    }
	if("xml-export".equals(e.getActionCommand()))
	    {
		if(selectedVersion!=-1)
		    {
			String themeKey=versionAccess.getThemeKey(selectedVersion);
			Object vId=versionAccess.getVersionId(selectedVersion);

			int returnVal = fileChooser.showDialog(this, "export xml");
			if(returnVal == JFileChooser.APPROVE_OPTION)
			    {
				File file=fileChooser.getSelectedFile();
				LogView.print("exporting data to file ... ");
				versionAccess.xmlExport(themeKey,vId,file);
				LogView.println("done");
			    }
		    }
	    }
	if("simulate".equals(e.getActionCommand()))
	    {
		if(selectedVersion!=-1)
		    {
			String themeKey=versionAccess.getThemeKey(selectedVersion);
			Object vId=versionAccess.getVersionId(selectedVersion);
			new SimulationDialog(versionAccess,themeKey,vId);
		    }
	    }
    }

    //VersionListener:
    public void onVersionChanged(EJBEvent event)
    {
	reloadProjectTree();
    }

}
