package de.tuhh.wb.javagis.view.tableview;



import javax.swing.event.TableModelEvent;

import javax.swing.event.InternalFrameListener;

import javax.swing.event.InternalFrameEvent;

import javax.swing.JInternalFrame;



import javax.swing.table.TableModel;

import javax.swing.table.TableColumnModel;

import javax.swing.table.TableColumn;

import java.awt.BorderLayout;



import javax.swing.JButton;

import javax.swing.JSplitPane;

import javax.swing.JPanel;

import javax.swing.JComboBox;

import javax.swing.JTabbedPane;

import javax.swing.JTable;

import javax.swing.JScrollPane;

import javax.swing.JToolBar;

import javax.swing.JMenu;

import javax.swing.JMenuItem;

import javax.swing.JMenuBar;

import javax.swing.JOptionPane;

import javax.swing.JPopupMenu;



import java.util.Vector;

import java.util.Hashtable;

import java.util.Enumeration;



import java.awt.event.MouseListener;

import java.awt.event.MouseEvent;

import java.awt.event.MouseMotionListener;



import java.awt.event.ActionListener;

import java.awt.event.ActionEvent;

import java.awt.Point;

import de.tuhh.wb.javagis.Main;

import de.tuhh.wb.javagis.view.ViewManager;

import de.tuhh.wb.javagis.view.GisView;

import de.tuhh.wb.javagis.view.singleview.GisSingleObjectView;

import de.tuhh.wb.javagis.view.singleview.GisSingleObjectView1;

import de.tuhh.wb.javagis.data.GisElement;

import de.tuhh.wb.javagis.data.GisElementClass;

import de.tuhh.wb.javagis.model.GisInterfaceTableModel;

import de.tuhh.wb.javagis.tools.I18n;

import de.tuhh.wb.javagis.view.tableview.DateChooser;



import de.tuhh.wb.javagis.view.LogView;

import de.tuhh.wb.javagis.data.DoubleKeyHash;

import java.awt.event.ItemEvent;

import java.awt.event.ItemListener;

import javax.ejb.ObjectNotFoundException;



public class GisTableView extends JInternalFrame implements InternalFrameListener,ActionListener,MouseListener,GisView,ItemListener

{

    private Vector cards;

    private Vector tableModels;

    private JTabbedPane tabbedPane;

    private Point selectedPoint=null;

    private int selectedCol=-1;

    private int selectedRow=-1;

    private Object selectedValue=null;

    public static JComboBox jcombo=null;

    

    // starting with default profile "all"

    //private String myProfileName=I18n.get("TV_GTV_DefaultProfile");

	private String myProfileName="all";



    // storage of TableCols, for easy removes and adds

    private DoubleKeyHash tableColHash=new DoubleKeyHash();

    private Hashtable tableModelHash=new Hashtable();



    public GisTableView(String title,Vector tableModels)

    {

	super(title,true,true,true,true);

	this.cards=new Vector();

	this.tableModels=tableModels;

	this.tabbedPane=new JTabbedPane();

	

	for(int index=0;index<tableModels.size();index++)

	    {

		GisTableModel tableModel=(GisTableModel)tableModels.elementAt(index);

		String tableKey=tableModel.getKey();





		JTable jTable=new JTable((TableModel)tableModel,null);

		DateChooser.setUpDateChooser(jTable);
		BCEChooser.setUpBCEChooser(jTable);

		cards.add(jTable);

		JScrollPane scroller=new JScrollPane(jTable);

		//		scroller.setBounds(10,10,(getSize().width-30),(getSize().height-120));

		

		JPanel jPanel=new JPanel(new BorderLayout());

		jPanel.add(scroller);

		jPanel.add(new GisTableFilter(this,tableModel), BorderLayout.SOUTH);

		//		tabbedPane.addTab(tableModel.getName(),jPanel);



		tabbedPane.addTab(tableModel.getName(),

				  tableModel.getIcon(),

				  jPanel,

				  tableModel.getDescription());

		    

		//		tabbedPane.addTab(tableModel.getName(),scroller);

		//		tabbedPane.setToolTipTextAt(index,"<html>m<sup>2</sup></html>");

		

		tabbedPane.setToolTipTextAt(index,Main.toHtml(tableModel.getDescription(),50));

		tabbedPane.doLayout();

		jTable.addMouseListener(this);

		if(tableModel.getColumnCount()>1)

		    {

			TableColumnModel colModel=jTable.getColumnModel();

			tableModelHash.put(tableKey,colModel);

			for(int col=1;col<tableModel.getColumnCount();col++)

			    {

				TableColumn tableCol=colModel.getColumn(col);

				String colKey=tableModel.getColKey(col);

				tableColHash.put(tableKey,colKey,tableCol);

					/*if(tableModel.isBCEButton(col))

				    {

					TableCellRendererBce bceCells= new TableCellRendererBce(jTable,tableModel);

					tableCol.setCellRenderer(bceCells);

					tableCol.setCellEditor(bceCells);

					 }*/

				/*if(tableModel.isDateButton(col))

				  {

				  DateChooser dateChooser = new DateChooser(jTable);

				  }*/

			    }

		    }

		tableModel.setGisView(this);

	    }

	getContentPane().add(tabbedPane,BorderLayout.CENTER);

	createFileMenu();

	this.addInternalFrameListener(this);

	this.moveToFront();

    }

    

    // ActionListener:

    public void actionPerformed(ActionEvent event)

    {

	String action = event.getActionCommand();

	int selectedTab =tabbedPane.getSelectedIndex();

	GisTableModel tableModel = (GisTableModel)tableModels.elementAt(selectedTab);

	JTable jTable = (JTable)cards.elementAt(selectedTab);

	int rows = jTable.getRowCount();

	int[] selectedRows = jTable.getSelectedRows();



	if(action.equals("remove"))

	    {

		if (selectedRows.length<=0)

		    {

			JOptionPane jop = new JOptionPane();

			jop.showMessageDialog(this,I18n.get("GTV_DiaRemove_Message"),I18n.get("GTV_DiaRemove_Title"),JOptionPane.INFORMATION_MESSAGE);

		    }

		else

		    {

			int number = selectedRows.length;

			tableModel.removeObject(selectedRows[0]);

		    }

	    }



	if(action.equals("new"))

	    {

		tableModel.createNewObject();

	    }

	if(action.equals("getAllElements"))

	    {

		tableModel.showAllElements();

	    }

	if(action.equals("columnOrder"))

	    {

		if(selectedCol>=0)

		    tableModel.orderColumnBy(selectedCol,false);

	    }

	if(action.equals("setAllSimplePropertiesTo"))

	    {

		if(selectedCol>=0)

		    tableModel.setAllSimplePropertiesTo(selectedCol,selectedValue);

	    }

	if(action.equals("columnOrderReverse"))

	    {

		if(selectedCol>=0)

		    tableModel.orderColumnBy(selectedCol,true);

	    }

	if(action.equals("hidecolumn"))

	    {

		if(selectedCol>=1)

		    {

			//			TableColumnModel colModel=jTable.getColumnModel();

			GisElementClass elementClass=tableModel.getGisElementClass();



			String elementKey=elementClass.getKey();

			String propKey=elementClass.getSimplePropertyKey(selectedCol-1);



			//			TableColumn tableColToHide=(TableColumn)tableColHash.get(elementKey,propKey);

			//			colModel.removeColumn(tableColToHide);



			/*

			  int viewCol=jTable.convertColumnIndexToView(selectedCol);

			  System.out.println("hide:"+viewCol);

			  

			  colModel.removeColumn(colModel.getColumn(viewCol));

			*/

			

			TableProfile.getInstance().hide(myProfileName,elementKey,propKey);

			updateColProfile();

		    }

	    }

	

	if(action.equals("showSingeObjectView"))

	    {

		int row=selectedRows[0];

		GisElement gisElement=((GisTableModel)tableModel).getGisElement(row);

		Object eId=((GisTableModel)tableModel).getElementId(row);

		String title=getTitle()+"/"+tableModel.getName()+"#"+eId.toString();

		GisSingleObjectView1.load(title,gisElement);

		/*

		  gisSingleObjectView.setVisible(true);

		  ViewManager.desktop.add(gisSingleObjectView);

		  gisSingleObjectView.moveToFront();

		*/

	    }

	

	

	if(action.equals("showVectorSetView") && tableModel.hasVectorSets())

	    {

		//		try

		//		    {

			int row=selectedRows[0];

			Object eId=tableModel.getElementId(row);

			GisElementClass tabGisElementClass=tableModel.getGisElementClass();

			Vector vectorSetTableModels = tableModel.getVectorSetTableModels(row);

			String frameName=getTitle()+"/"+tableModel.getName()+"#"+eId.toString()+"/VectorSet";

			GisVectorSetTableView gisVectorSetTableView = new GisVectorSetTableView(tableModel,frameName,vectorSetTableModels,eId,tabGisElementClass);

			gisVectorSetTableView.setVisible(true);

			gisVectorSetTableView.setSize(670,300);

			ViewManager.desktop.add(gisVectorSetTableView);

			gisVectorSetTableView.moveToFront();

			//		    }

			//	    	catch(ObjectNotFoundException e)

			//		    {

			//		    }

	    }

	if(action.equals("saveColumnProfile"))

	    {

		TableProfile.getInstance().save();

	    }

	

	if(action.equals("newColumnProfile"))

	    {

		String newProfileName=TableProfile.getInstance().newProfile();

		if(newProfileName!=null)

		    {

			jcombo.addItem(newProfileName);

			jcombo.setSelectedItem(newProfileName);

		    }

		//		createFileMenu();

	    }

	if(action.equals("removeColumnProfile"))

	    {

		TableProfile.getInstance().removeProfile(myProfileName);

		//if(myProfileName!=null && !"all".equals(myProfileName))

		//jcombo.removeItem(myProfileName);

		//jcombo.setSelectedItem("all");

		//		createFileMenu();

	    }

	/*

	if(action.equals("loadColumnProfile"))

	    {

		TableProfile.getInstance().load();

				

		for(int tab=0;tab<tableModels.size();tab++)

		    {

			GisTableModel iModel = (GisTableModel)tableModels.elementAt(tab);

			Vector colsToRemove=new Vector();

			GisElementClass elementClass=iModel.getGisElementClass();

			String elementKey=elementClass.getKey();

			JTable iTable = (JTable)cards.elementAt(tab);

			TableColumnModel colModel=iTable.getColumnModel();

			for(int i=0;i<elementClass.getSimplePropertySize();i++)

			    {

				String propKey=elementClass.getSimplePropertyKey(i);

				if(TableProfile.getInstance().shouldBeHidden(myProfileName,elementKey,propKey))

				    {

					int viewCol=jTable.convertColumnIndexToView(i+1);

					if(viewCol>=0)

					    colModel.removeColumn(colModel.getColumn(viewCol));

				    }

			    }

		    }

	    }

	*/

    }

    

    public void updateColProfile()

    {

	Enumeration tableKeys=tableColHash.getFirstKeys();

	while(tableKeys.hasMoreElements())

	    {

		String tableKey=(String)tableKeys.nextElement();

		TableColumnModel colModel=(TableColumnModel)tableModelHash.get(tableKey);

		Enumeration colKeys=tableColHash.getSubKeys(tableKey);

		while(colKeys.hasMoreElements())

		    {

			String colKey=(String)colKeys.nextElement();

			TableColumn tableCol=(TableColumn)tableColHash.get(tableKey,colKey);

			colModel.removeColumn(tableCol);

			

			if(!TableProfile.getInstance().shouldBeHidden(myProfileName,tableKey,colKey))

			    {

				colModel.addColumn(tableCol);

			    }

			



		    }

	    }

    }

    

    public void refreshView()

    {

	int tab=tabbedPane.getSelectedIndex();

	JTable table=(JTable)cards.elementAt(tab);

	table.tableChanged(new TableModelEvent(table.getModel()));

    }

    

	

    public void mouseClicked(MouseEvent e)

    {}

    

    public void mousePressed(MouseEvent e)

    {

	maybeShowPopup(e);

    }

    

    public void mouseReleased(MouseEvent e)

    {

	maybeShowPopup(e);

    }

    

    public void mouseEntered(MouseEvent e)

    {}

    

    

    public void mouseExited(MouseEvent e)

    {}





    public void createFileMenu()

    {

	JMenu edit = new JMenu(I18n.get("TV_GTV_jMenu_edit"));



	JMenuItem mi;

	mi = new JMenuItem(I18n.get("TV_GTV_jMenuItem_new"));

	mi.setActionCommand("new");

	mi.addActionListener(this);

	edit.add(mi);



  	mi = new JMenuItem(I18n.get("TV_GTV_jMenuItem_remove"));

	mi.setActionCommand("remove");

	mi.addActionListener(this);

	edit.add(mi);



	//create.addSeparator();

	edit.addSeparator();



	JMenu open = new JMenu (I18n.get("TV_GTV_jMenu_view"));



	mi = new JMenuItem(I18n.get("TV_GTV_jMenuItem_refresh"));

	mi.setActionCommand("getAllElements");

	mi.addActionListener(this);

	open.add(mi);



	mi = new JMenuItem(I18n.get("TV_GTV_jMenuItem_detail"));

	mi.setActionCommand("showSingeObjectView");

	mi.addActionListener(this);

	open.add(mi);



	mi = new JMenuItem(I18n.get("TV_GTV_jMenuItem_vectorSets"));

	mi.setActionCommand("showVectorSetView");

	mi.addActionListener(this);

	open.add(mi);

	

	JMenu colProfile = new JMenu (I18n.get("TV_GTV_jMenu_profile"));



	mi = new JMenuItem(I18n.get("TV_GTV_jMenuItem_pNew"));

	mi.setActionCommand("newColumnProfile");

	mi.addActionListener(this);

	colProfile.add(mi);



	mi = new JMenuItem(I18n.get("TV_GTV_jMenuItem_pRemove"));

	mi.setActionCommand("removeColumnProfile");

	mi.addActionListener(this);

	colProfile.add(mi);

	/*

	  mi = new JMenuItem("load profile");

	  mi.setActionCommand("loadColumnProfile");

	  mi.addActionListener(this);

	  colProfile.add(mi);

	*/

	

	mi = new JMenuItem(I18n.get("TV_GTV_jMenuItem_pSave"));

	mi.setActionCommand("saveColumnProfile");

	mi.addActionListener(this);

	colProfile.add(mi);



	JMenuBar menubar= new JMenuBar();

	//menubar.add(create);

	menubar.add(edit);

	menubar.add(open);

	jcombo=new JComboBox(TableProfile.getInstance().getProfiles());

	jcombo.addItemListener(this);

	jcombo.setSize(100,20);

	menubar.add(jcombo);

	menubar.add(colProfile);



	//	menubar.add(new JComboBox(tableProfiles.getProfiles()));



	this.setJMenuBar(menubar);

    }

    

    private void maybeShowPopup(MouseEvent e)

    {

	int selectedTab = tabbedPane.getSelectedIndex();

	GisTableModel tableModel = (GisTableModel)tableModels.elementAt(selectedTab);

	JTable table = (JTable)cards.elementAt(selectedTab);

	selectedPoint=e.getPoint();

	selectedCol=table.convertColumnIndexToModel(table.columnAtPoint(selectedPoint));

	//selectedCol=table.columnAtPoint(selectedPoint);

	selectedRow=table.rowAtPoint(selectedPoint);

	if(selectedRow>=0 && selectedCol>=0)

	    {selectedValue=tableModel.getValueAt(selectedRow,selectedCol);

	

		}

	else

	    selectedValue=null;



        if (e.isPopupTrigger() && selectedRow>=0 && selectedCol>=0)

	    {

	



		JMenuItem mi;

		JPopupMenu popup = new JPopupMenu();



		mi = new JMenuItem(I18n.get("TV_GTV_PopMen_New"));

		mi.setActionCommand("new");

		mi.addActionListener(this);

		popup.add(mi);



		mi = new JMenuItem(I18n.get("TV_GTV_PopMen_Remove"));

		mi.setActionCommand("remove");

		mi.addActionListener(this);

		popup.add(mi);



		mi = new JMenuItem(I18n.get("TV_GTV_PopMen_Detail"));

		mi.setActionCommand("showSingeObjectView");

		mi.addActionListener(this);

		popup.add(mi);

		

		if(tableModel.hasVectorSets())

		    {

			mi = new JMenuItem(I18n.get("TV_GTV_PopMen_VectorSets"));

			mi.setActionCommand("showVectorSetView");

			mi.addActionListener(this);

			popup.add(mi);

		    }



		mi = new JMenuItem(I18n.get("TV_GTV_PopMen_orderC"));

		mi.setActionCommand("columnOrder");

		mi.addActionListener(this);

		popup.add(mi);



		mi = new JMenuItem(I18n.get("TV_GTV_PopMen_orderCr"));

		mi.setActionCommand("columnOrderReverse");

		mi.addActionListener(this);

		popup.add(mi);



		if(selectedCol>0 && selectedValue!=null)

		    {

			String string1 = I18n.get("TV_GTV_PopMen_setAll1");

			String string2 = I18n.get("TV_GTV_PopMen_setAll2");

			mi = new JMenuItem("<html>"+string1+tableModel.getColumnNameNoHtml(selectedCol)+string2+selectedValue+"</html>");

			mi.setActionCommand("setAllSimplePropertiesTo");

			mi.addActionListener(this);

			popup.add(mi);

		    }

		

		if(selectedCol>0) // do not hide col "ID"

		    {

				mi = new JMenuItem(I18n.get("TV_GTV_PopMen_hide"));

				mi.setActionCommand("hidecolumn");

				mi.addActionListener(this);

		     	popup.add(mi);

			}

		String description=tableModel.getDescription(selectedCol);

		if(description!=null && !"".equals(description))

		    {



			mi = new JMenuItem(Main.toHtml("<i>"+I18n.get("TV_GTV_PopMen_nothing")+"</i>"+" \""

						       +tableModel.getColumnNameNoHtml(selectedCol)

						       +"\": "+description,40));

			mi.setActionCommand("nothing");

			mi.addActionListener(this);

			popup.addSeparator();

			popup.add(mi);

		    }



		popup.show(e.getComponent(),

			   e.getX(), e.getY());



	    }

    }



    // internalFrameListener:



    //          Invoked when an internal frame is activated.

    public void internalFrameActivated(InternalFrameEvent e)

    {}



    //          Invoked when an internal frame has been closed.

    public void internalFrameClosed(InternalFrameEvent e)

    {

	for(int index=0;index<tableModels.size();index++)

	    {

		GisTableModel tableModel=(GisTableModel)tableModels.elementAt(index);

		tableModel.close();

	    }

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



    // ItemListener

    public void itemStateChanged(ItemEvent e)

    {

	String newProfileName=(String)jcombo.getSelectedItem();

	if(!myProfileName.equals(newProfileName))

	{

	    myProfileName=newProfileName;

	    updateColProfile();

	}

    }

}

