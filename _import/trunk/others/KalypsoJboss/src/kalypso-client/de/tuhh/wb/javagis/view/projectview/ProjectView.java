package de.tuhh.wb.javagis.view.projectview;

import java.util.Vector;
import java.util.Hashtable;
import javax.swing.tree.*;
import de.tuhh.wb.javagis.Main;

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
import de.tuhh.wb.javagis.tools.I18n;
import ejb.event.EJBEvent;
import javax.swing.*;
import java.awt.*;
import java.lang.ClassLoader;

import de.tuhh.wb.javagis.view.trafoview.TrafoView;

public class ProjectView extends JInternalFrame implements ActionListener, MouseListener, VersionListener
{
    VersionAccess versionAccess;
    int selectedVersion;
    Hashtable versionHash;
	ClassLoader cl = this.getClass().getClassLoader();
	
    JScrollPane jScrollPane = new JScrollPane();
	
    JTree jTree;
    DefaultTreeModel treeModel;
	
    JFileChooser fileChooser;
	
    public ProjectView()
    {
		super(I18n.get("windowTitlePV"),true,false,true,true);
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
		treeModel=new DefaultTreeModel(new DefaultMutableTreeNode(I18n.get("PV_TN_Default")));
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
		JMenu edit = new JMenu (I18n.get("PVJMenuEdit"));
		JMenu view = new JMenu (I18n.get("PVJMenuView"));
		edit.addActionListener(this);
		view.addActionListener(this);
		// edit.setActionCommand("Zoom In");
		edit.setIcon((new ImageIcon(cl.getResource("symbols/Edit16.gif"))));
		JMenuItem mi;
		
		mi = new JMenuItem(I18n.get("PVJMenuItem_ConnectDatabase"));
		mi.setIcon((new ImageIcon (cl.getResource("symbols/FullExtent16.gif"))));
		mi.setActionCommand("connectToServer");
		mi.addActionListener(this);
		edit.add(mi);
		
		mi = new JMenuItem(I18n.get("PVJMenuItem_CreateVersion"));
		mi.addActionListener(this);
		mi.setIcon((new ImageIcon(cl.getResource("symbols/New16.gif"))));
		mi.setActionCommand("createVersion");
		//        mi.addActionListener(this);
		edit.add(mi);
		
		mi = new JMenuItem(I18n.get("PVJMenuItem_ReloadTree"));
		mi.setIcon((new ImageIcon(cl.getResource( "symbols/Refresh16.gif"))));
		mi.setActionCommand("updateProjectTree");
		mi.addActionListener(this);
		edit.add(mi);
		
		mi = new JMenuItem(I18n.get("PVJMenuItem_OpenTrafoView"));
		//	mi.setIcon((new ImageIcon( "symbols/Refresh16.gif")));
		mi.setActionCommand("openTrafoView");
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
		
		String host=Main.props.getProperty("jboss_host");
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
				JMenuItem menuItem = new JMenuItem(I18n.get("PV_PopMen_CopyVersion"));
				menuItem.setIcon((new ImageIcon(cl.getResource( "symbols/vectorSets16.gif"))));
				menuItem.setActionCommand("copySelectedVersion");
				menuItem.addActionListener(this);
				popup.add(menuItem);
				
				menuItem = new JMenuItem(I18n.get("PV_PopMen_TblObj"));
				menuItem.setActionCommand("openObjectTableViewFromSelectedVersion");
				menuItem.addActionListener(this);
				popup.add(menuItem);
				
				
				if("Modell".equals(theme))
			    {
					menuItem = new JMenuItem(I18n.get("PV_PopMen_TblRel"));
					menuItem.setActionCommand("openRelationTableViewFromSelectedVersion");
					menuItem.addActionListener(this);
					popup.add(menuItem);
			    }
				
				if("Modell".equals(theme))
			    {
					menuItem = new JMenuItem(I18n.get("PV_PopMen_NetView"));
					menuItem.setActionCommand("openNetViewFromSelectedVersion");
					menuItem.addActionListener(this);
					popup.add(menuItem);
			    }
				
				menuItem = new JMenuItem(I18n.get("PV_PopMen_Rename"));
				menuItem.setActionCommand("renameSelectedVersion");
				menuItem.setIcon((new ImageIcon(cl.getResource( "symbols/Rename16.gif"))));
				menuItem.addActionListener(this);
				popup.add(menuItem);
				
				menuItem = new JMenuItem(I18n.get("PV_PopMen_Remove"));
				menuItem.setActionCommand("removeSelectedVersion");
				menuItem.setIcon((new ImageIcon(cl.getResource( "symbols/Delete16.gif"))));
				menuItem.addActionListener(this);
				popup.add(menuItem);
				
				if("SimulationCase".equals(theme))
				    {
					menuItem = new JMenuItem(I18n.get("PV_PopMen_Sim"));
					menuItem.setActionCommand("simulate");
					menuItem.addActionListener(this);
					popup.add(menuItem);
				    }

				if("HWS_Combination".equals(theme))
				    {
					menuItem = new JMenuItem(I18n.get("HWS_Combination_startCalculation"));
					menuItem.setActionCommand("hws_startCalculation");
					menuItem.addActionListener(this);
					popup.add(menuItem);
				    }

				menuItem = new JMenuItem(I18n.get("PV_PopMen_XMLex"));
				menuItem.setActionCommand("xml-export");
				menuItem.setIcon((new ImageIcon(cl.getResource( "symbols/Export16.gif"))));
				menuItem.addActionListener(this);
				popup.add(menuItem);
				
				menuItem = new JMenuItem(I18n.get("PV_PopMen_XMLim"));
				menuItem.setActionCommand("xml-import");
				menuItem.setIcon((new ImageIcon(cl.getResource( "symbols/FullExtent16.gif"))));
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
		
		if("openTrafoView".equals(e.getActionCommand()))
	    {
			TrafoView.openTrafoView();
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
				new VersionDialog(I18n.get("PV_VD_TitleCreate"),VersionDialog.CREATE,versionAccess,null,null);
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
				new VersionDialog(I18n.get("PV_VD_TitleRename"),VersionDialog.RENAME,versionAccess,themeKey,vId);
			}
	    }
		if("copySelectedVersion".equals(e.getActionCommand()))
	    {
			if(selectedVersion!=-1)
			{
				String themeKey=versionAccess.getThemeKey(selectedVersion);
				Object vId=versionAccess.getVersionId(selectedVersion);
				
				new VersionDialog(I18n.get("PV_VD_TitleCopy"),VersionDialog.COPY,versionAccess,themeKey,vId);
			}
	    }
		
		if("removeSelectedVersion".equals(e.getActionCommand()))
	    {
			if(selectedVersion!=-1)
			{
				Object[] options = {I18n.get("Dia_Yes"),I18n.get("Dia_No"),I18n.get("Dia_Cancel")};
				int n = JOptionPane.showOptionDialog(null,I18n.get("RemoveDia_Question"),I18n.get("RemoveDia_Title"),JOptionPane.YES_NO_CANCEL_OPTION,JOptionPane.QUESTION_MESSAGE,null,options,options[1]);
				
				switch (n)
				{
					case JOptionPane.NO_OPTION:
						break;
					case JOptionPane.YES_OPTION:
						versionAccess.removeVersion(selectedVersion);
					case JOptionPane.CANCEL_OPTION:
						break;
					default:
						break ;}
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
					LogView.print(I18n.get("LV_PV_XML_im1"));
					versionAccess.xmlImport(themeKey,vId,file);
					LogView.println(I18n.get("LV_PV_XML_im2"));
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
					LogView.print(I18n.get("LV_PV_XML_ex1"));
					versionAccess.xmlExport(themeKey,vId,file);
					LogView.println(I18n.get("LV_PV_XML_ex2"));
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
		
		if("hws_startCalculation".equals(e.getActionCommand()))
		    {
			if(selectedVersion!=-1)
			    {
				String themeKey=versionAccess.getThemeKey(selectedVersion);
				Object vId=versionAccess.getVersionId(selectedVersion);
				HwsSimulation simulation=new HwsSimulation();
				try
				    {
					simulation.startSimulation(versionAccess,themeKey,vId);
				    }
				catch(Exception err)
				    {
					String message="sorry, calculation failed by: "+err.getMessage();
					JOptionPane.showMessageDialog(null,message,"calculation message",JOptionPane.ERROR_MESSAGE);
					err.printStackTrace();
				    }
				//	new SimulationDialog(versionAccess,themeKey,vId);
			    }
		    }
    }
	
    //VersionListener:
    public void onVersionChanged(EJBEvent event)
    {
		reloadProjectTree();
    }
	
}
