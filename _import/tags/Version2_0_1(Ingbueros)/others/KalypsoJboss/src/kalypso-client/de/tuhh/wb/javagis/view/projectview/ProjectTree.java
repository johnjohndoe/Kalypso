package de.tuhh.wb.javagis.view.projectview;

import javax.swing.JPanel;

public class ProjectTree extends JPanel// implements ActionListener, MouseListener, VersionListener
{
    /*
    VersionAccess versionAccess;
    int selectedVersion;
    Hashtable versionHash;
  
    JTree jTree;
    String host,port,user,pass;
    
    DefaultTreeModel treeModel;
  
    public ProjectTree()
    {
	//super("ProjectView",true,true,true,true);
	this.selectedVersion=-1;
	this.versionAccess=null;
	this.versionHash=new Hashtable();
	initMask();
	this.host=null;
	this.port=null;
	this.user=null;
	this.pass=null;    
    }
   
    public void connectDataBase(String host,String port,String user,String pass)
    {
	this.host=host;
	this.port=port;
	this.user=user;
	this.pass=pass;	
	reloadProjectTree();
    }
    public boolean isConnectedToDataBase()
    {
	if(versionAccess!=null)
	    return true;
	else
	    return false;
    }
  
    
    public void initMask()
    {
	this.removeAll();
	treeModel=new DefaultTreeModel(new DefaultMutableTreeNode("not connected to kalypso-server"));
	jTree= new JTree(treeModel);
	this.add(jTree, null);
	jTree.addMouseListener(this);
    }
  
    private void reloadProjectTree()
    {
	if(!((host==null) || (user==null) || (port==null) || (pass==null)))
	   {
	       DefaultMutableTreeNode rootNode= new DefaultMutableTreeNode(host);
	       //	       if(versionAccess!=null)
	       //		   versionAccess.unRegister(this);
	       //   {
	       versionAccess=(VersionAccess) new VersionAccessImpl(host,port,user,pass);
	       //	       versionAccess.register(this);
	       //   }
	       //	else
	       //  versionAccess.updateIndex();
	       
	       Vector themeKeys=versionAccess.getThemeKeys();
	       for(int i=0;i<themeKeys.size();i++)
		   System.out.println("ThemeKey from ThemeManager: "+(themeKeys.elementAt(i))+".");
	       Hashtable nodes=new Hashtable();
	       nodes.put("root",rootNode);
	       buildTree(0,nodes,versionAccess);
	       treeModel.setRoot(rootNode);
	       versionHash.clear();
	       for(int row=0;row<versionAccess.getSize();row++)
		   {
		       String path=host+getPath(row,3,versionAccess);
		       System.out.println("hash: "+path);
		       versionHash.put(path,new Integer(row));
		   }
	   }
    }
    
    public void buildTree(int deep,Hashtable parentNodes,VersionAccess versionAccess)
    {
	System.out.println("build...tree");
	Hashtable nodes=new Hashtable();
	for(int row=0;row<versionAccess.getSize();row++)
	    {
		String path=getPath(row,deep,versionAccess);
		if(!nodes.containsKey(path))
		    {
			String newText=getText(row,deep,versionAccess);
			DefaultMutableTreeNode newNode;
			if(newText==null || !"-".equals(newText))
			    {
				newNode=new DefaultMutableTreeNode(newText);
				((DefaultMutableTreeNode)parentNodes.get(getPath(row,deep-1,versionAccess))).add(newNode);
			    }
			else
			    newNode=(DefaultMutableTreeNode)parentNodes.get(getPath(row,deep-1,versionAccess));
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
		
	System.out.println("Mouse Pressed");
	int selRow = jTree.getRowForLocation(e.getX(), e.getY());
	jTree.setSelectionRow(selRow);
	if(selRow!=-1)
	    {
		TreePath selPath = jTree.getPathForLocation(e.getX(), e.getY());
		Object[] objects=selPath.getPath();
		String selectedVersion="";
		
		
		if(selPath.getPathCount()==5 && e.isPopupTrigger())
		    {
			for(int i=0;i<objects.length;i++)
			    selectedVersion+=objects[i].toString();
			selectVersion(selectedVersion);			
			
			System.out.println("selectedVersion:"+selectedVersion);
			JPopupMenu popup = new JPopupMenu();
			JMenuItem menuItem = new JMenuItem("open TableView");
			menuItem.setActionCommand("openTableViewFromSelectedVersion");
			menuItem.addActionListener(this);
			popup.add(menuItem);
			
			menuItem = new JMenuItem("open NetView");
			menuItem.setActionCommand("openNetViewFromSelectedVersion");
			menuItem.addActionListener(this);
			popup.add(menuItem);
			
			menuItem = new JMenuItem("remove Version");
			menuItem.setActionCommand("removeSelectedVersion");
			menuItem.addActionListener(this);
			popup.add(menuItem);
			
			menuItem = new JMenuItem("xml-export");
			menuItem.setActionCommand("xml-export");
			menuItem.addActionListener(this);
			popup.add(menuItem);
			
			menuItem = new JMenuItem("xml-import");
			menuItem.setActionCommand("xml-import");
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
		System.out.println("selected Version-Nr: "+selectedVersion);
	    }
	else
	    selectedVersion=-1;
    }    

    // ActionListener
    public void actionPerformed(ActionEvent e)
    {
	if("updateProjectTree".equals(e.getActionCommand()))
	    {
		reloadProjectTree();
	    }
	if("createVersion".equals(e.getActionCommand()))
	    {
		if(versionAccess!=null)
		    {
		    }
		    else
		    System.out.println("please connect to kalypso-server");
	    }
	if("removeSelectedVersion".equals(e.getActionCommand()))
	    {
		removeSelectedVersion();
	    }

	if("openTableViewFromSelectedVersion".equals(e.getActionCommand()))
	    {
		openTableViewFromSelectedVersion();
	    }
	if("openNetViewFromSelectedVersion".equals(e.getActionCommand()))
	    {
		openNetViewFromSelectedVersion();
	    }
	if("xml-import".equals(e.getActionCommand()))
	    {
		if(selectedVersion!=-1)
		    new JobRequest("Import from XML","importXmlFileSelected",this);
	    }
	if("importXmlFileSelected".equals(e.getActionCommand()))
	    {
		if(selectedVersion!=-1)
		    {
			Object source=e.getSource();
			String fileName="/tmp/"+((JTextField)source).getText();
			versionAccess.xmlImport(selectedVersion,fileName);
		    }
	    }
	if("xml-export".equals(e.getActionCommand()))
	    {
		if(selectedVersion!=-1)
		    versionAccess.xmlExport(selectedVersion);
	    }
    }

    public Version getSelectedVersion()
    {
	if(selectedVersion!=-1)
	    return versionAccess.getVersion(selectedVersion);
	else
	    return null;
    }

    public void openTableViewFromSelectedVersion()
    {
	if(selectedVersion!=-1)
	    versionAccess.openTableView(selectedVersion);
    }

    public void openNetViewFromSelectedVersion()
    {
	if(selectedVersion!=-1)
	    versionAccess.openNetView(selectedVersion);
    }
    
    public void removeSelectedVersion()
    {
	if(selectedVersion!=-1)
	    versionAccess.removeVersion(selectedVersion);	
    }

    public void createNewVersion(String project,String theme,String state,String name,String description)
    {
	versionAccess.createVersion(project,
				    theme,
				    state,
				    name,
				    description);
    }


    //VersionListener:
    public void onVersionChanged(EJBEvent event)
    {
	reloadProjectTree();	
    }
*/
}

