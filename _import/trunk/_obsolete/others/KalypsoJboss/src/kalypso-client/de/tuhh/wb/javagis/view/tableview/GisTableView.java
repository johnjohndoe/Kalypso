package de.tuhh.wb.javagis.view.tableview;

import java.awt.BorderLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

import javax.swing.JComboBox;
import javax.swing.JInternalFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.event.InternalFrameEvent;
import javax.swing.event.InternalFrameListener;
import javax.swing.event.TableModelEvent;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;
import javax.swing.table.JTableHeader;

import de.tuhh.wb.javagis.Main;
import de.tuhh.wb.javagis.data.DoubleKeyHash;
import de.tuhh.wb.javagis.data.GisElement;
import de.tuhh.wb.javagis.data.GisElementClass;
import de.tuhh.wb.javagis.data.GisObjectClass;
import de.tuhh.wb.javagis.data.Version;
import de.tuhh.wb.javagis.tools.I18n;
import de.tuhh.wb.javagis.view.GisView;
import de.tuhh.wb.javagis.view.ViewManager;
import de.tuhh.wb.javagis.view.singleview.GisSingleObjectView;
import de.tuhh.wb.tools.event.ActionManager;

public class GisTableView
	extends JInternalFrame
	implements
		InternalFrameListener,
		ActionListener,
		MouseListener,
		GisView,
		ItemListener {

	private Vector cards;

	private Vector tableModels;

	private JTabbedPane tabbedPane;

	private Point selectedPoint = null;

	private int selectedCol = -1;

	private int selectedRow = -1;

	private Object selectedValue = null;

	public static JComboBox jcombo = null;
	//private JCheckBoxMenuItem cbMenuItem;
	private String topicName_newObject = null;
	private String topicName = null;

	// starting with default profile "all"

	//private String myProfileName=I18n.get("TV_GTV_DefaultProfile");

	private String myProfileName = "all";

	// storage of TableCols, for easy removes and adds

	private DoubleKeyHash tableColHash = new DoubleKeyHash();

	private Hashtable tableModelHash = new Hashtable();

	public GisTableView(String title, Vector tableModels) {

		super(title, true, true, true, true);

		this.cards = new Vector();

		this.tableModels = tableModels;

		this.tabbedPane = new JTabbedPane();
		//this.isEditable = true;

		for (int index = 0; index < tableModels.size(); index++) {

			GisTableModel tableModel =
				(GisTableModel) tableModels.elementAt(index);

			String tableKey = tableModel.getKey();
			//tableModel.setEditable(true);

			JTable jTable = new JTable((TableModel) tableModel, null);
			jTable.setTableHeader(new MyJTableHeader(jTable.getColumnModel(),tableModel));

			DateChooser.setUpDateChooser(jTable);
			BCEChooser.setUpBCEChooser(jTable);

			cards.add(jTable);

			JScrollPane scroller = new JScrollPane(jTable);

			//		scroller.setBounds(10,10,(getSize().width-30),(getSize().height-120));

			JPanel jPanel = new JPanel(new BorderLayout());

			jPanel.add(scroller);

			jPanel.add(
				new GisTableFilter(this, tableModel),
				BorderLayout.SOUTH);

			//		tabbedPane.addTab(tableModel.getName(),jPanel);

			tabbedPane.addTab(
				tableModel.getName(),
				tableModel.getIcon(),
				jPanel,
				tableModel.getDescription());

			//		tabbedPane.addTab(tableModel.getName(),scroller);

			//		tabbedPane.setToolTipTextAt(index,"<html>m<sup>2</sup></html>");

			tabbedPane.setToolTipTextAt(
				index,
				Main.toHtml(tableModel.getDescription(), 50));

			tabbedPane.doLayout();

			jTable.addMouseListener(this);

			if (tableModel.getColumnCount() > 1) {

				TableColumnModel colModel = jTable.getColumnModel();

				tableModelHash.put(tableKey, colModel);

				for (int col = 1; col < tableModel.getColumnCount(); col++) {

					TableColumn tableCol = colModel.getColumn(col);

					String colKey = tableModel.getColKey(col);

					tableColHash.put(tableKey, colKey, tableCol);

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

		getContentPane().add(tabbedPane, BorderLayout.CENTER);

		GisElementClass myGisElementClass =
			((GisTableModel) tableModels.elementAt(0)).getGisElementClass();
		topicName_newObject = "NewObject" + title;
		Version myversion = myGisElementClass.getVersion();
		topicName = myversion.getTopicName();
		if (myversion.isEditable()) {
			ActionManager.getInstance().register(
				topicName_newObject,
				"new",
				this);
			ActionManager.getInstance().register(
				topicName,
				"setUnEditable",
				this);
		} else {
			ActionManager.getInstance().register(
				topicName,
				"setEditable",
				this);
		}

		createFileMenu();

		this.addInternalFrameListener(this);

		this.moveToFront();
		validate();

	}

	// ActionListener:

	public void actionPerformed(ActionEvent event) {

		String action = event.getActionCommand();

		int selectedTab = tabbedPane.getSelectedIndex();

		GisTableModel tableModel =
			(GisTableModel) tableModels.elementAt(selectedTab);

		JTable jTable = (JTable) cards.elementAt(selectedTab);

		int rows = jTable.getRowCount();

		int[] selectedRows = jTable.getSelectedRows();

		if (action.equals("remove")) {

			if (selectedRows.length <= 0) {

				JOptionPane jop = new JOptionPane();

				jop.showMessageDialog(
					this,
					I18n.get("GTV_DiaRemove_Message"),
					I18n.get("GTV_DiaRemove_Title"),
					JOptionPane.INFORMATION_MESSAGE);

			} else {

				int number = selectedRows.length;

				tableModel.removeObject(selectedRows[0]);

			}

		}

		if (action.equals("new")) {

			tableModel.createNewObject();

		}
		if (action.equals(topicName_newObject + ".new")) {

			Version version = (tableModel.getGisElementClass()).getVersion();
			if (version.isEditable()) {
				tableModel.createNewObject();
			}

		}

		if (action.equals("getAllElements")) {

			tableModel.showAllElements();

		}

		if (action.equals("columnOrder")) {

			if (selectedCol >= 0)
				tableModel.orderColumnBy(selectedCol, false);

		}

		if (action.equals("setAllSimplePropertiesTo")) {

			if (selectedCol >= 0)
				tableModel.setAllSimplePropertiesTo(selectedCol, selectedValue);

		}

		if (action.equals("columnOrderReverse")) {

			if (selectedCol >= 0)
				tableModel.orderColumnBy(selectedCol, true);

		}

		if (action.equals("hidecolumn")) {

			if (selectedCol >= 1) {

				//			TableColumnModel colModel=jTable.getColumnModel();

				GisElementClass elementClass = tableModel.getGisElementClass();

				String elementKey = elementClass.getKey();

				String propKey =
					elementClass.getSimplePropertyKey(selectedCol - 1);

				//			TableColumn tableColToHide=(TableColumn)tableColHash.get(elementKey,propKey);

				//			colModel.removeColumn(tableColToHide);

				/*
				
				  int viewCol=jTable.convertColumnIndexToView(selectedCol);
				
				  System.out.println("hide:"+viewCol);
				
				  
				
				  colModel.removeColumn(colModel.getColumn(viewCol));
				
				*/

				TableProfile.getInstance().hide(
					myProfileName,
					elementKey,
					propKey);

				updateColProfile();

			}

		}

		if (action.equals("showSingeObjectView") && selectedRows.length > 0) {

			int row = selectedRows[0];

			GisElement gisElement =
				((GisTableModel) tableModel).getGisElement(row);

			Object eId = ((GisTableModel) tableModel).getElementId(row);

			String title =
				getTitle() + "/" + tableModel.getName() + "#" + eId.toString();

			GisSingleObjectView.load(title, gisElement);

			/*
			
			  gisSingleObjectView.setVisible(true);
			
			  ViewManager.desktop.add(gisSingleObjectView);
			
			  gisSingleObjectView.moveToFront();
			
			*/

		}

		if (action.equals("showVectorSetView")
			&& tableModel.hasVectorSets()
			&& selectedRows.length > 0) {

			//		try

			//		    {

			int row = selectedRows[0];

			Object eId = tableModel.getElementId(row);

			GisElementClass tabGisElementClass =
				tableModel.getGisElementClass();

			Vector vectorSetTableModels =
				tableModel.getVectorSetTableModels(row);

			String frameName =
				getTitle()
					+ "/"
					+ tableModel.getName()
					+ "#"
					+ eId.toString()
					+ "/VectorSet";

			boolean open = ViewManager.isViewOpen(frameName);
			if (!open) {
				GisVectorSetTableView gisVectorSetTableView =
					new GisVectorSetTableView(
						tableModel,
						frameName,
						vectorSetTableModels,
						eId,
						tabGisElementClass);

				gisVectorSetTableView.setVisible(true);

				gisVectorSetTableView.setSize(670, 300);

				ViewManager.desktop.add(gisVectorSetTableView);

				gisVectorSetTableView.moveToFront();
			}

			//		    }

			//	    	catch(ObjectNotFoundException e)

			//		    {

			//		    }

		}

		if (action.equals("saveColumnProfile")) {

			TableProfile.getInstance().save();

		}

		if (action.equals("newColumnProfile")) {

			String newProfileName = TableProfile.getInstance().newProfile();

			if (newProfileName != null) {

				jcombo.addItem(newProfileName);

				jcombo.setSelectedItem(newProfileName);

			}

			//		createFileMenu();

		}

		if (action.equals("removeColumnProfile")) {

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
		if (action.equals(topicName + ".setUnEditable")) {
			ActionManager.getInstance().unregister(
				topicName_newObject,
				"new",
				this);
			ActionManager.getInstance().register(
				topicName,
				"setEditable",
				this);
			ActionManager.getInstance().unregister(
				topicName,
				"setUnEditable",
				this);
		}
		if (action.equals(topicName + ".setEditable")) {
			ActionManager.getInstance().register(
				topicName_newObject,
				"new",
				this);
			ActionManager.getInstance().register(
				topicName,
				"setUnEditable",
				this);
			ActionManager.getInstance().unregister(
				topicName,
				"setEditable",
				this);
		}

	}

	public void updateColProfile() {

		Enumeration tableKeys = tableColHash.getFirstKeys();

		while (tableKeys.hasMoreElements()) {

			String tableKey = (String) tableKeys.nextElement();

			TableColumnModel colModel =
				(TableColumnModel) tableModelHash.get(tableKey);

			Enumeration colKeys = tableColHash.getSubKeys(tableKey);

			while (colKeys.hasMoreElements()) {

				String colKey = (String) colKeys.nextElement();

				TableColumn tableCol =
					(TableColumn) tableColHash.get(tableKey, colKey);

				colModel.removeColumn(tableCol);

				if (!TableProfile
					.getInstance()
					.shouldBeHidden(myProfileName, tableKey, colKey)) {

					colModel.addColumn(tableCol);

				}

			}

		}

	}

	public void refreshView() {

		int tab = tabbedPane.getSelectedIndex();

		JTable table = (JTable) cards.elementAt(tab);

		table.tableChanged(new TableModelEvent(table.getModel()));

	}

	public void mouseClicked(MouseEvent e) {

	}

	public void mousePressed(MouseEvent e) {
		//System.out.println("Mouse Pressed");
		maybeShowPopup(e);

	}

	public void mouseReleased(MouseEvent e) {
		//System.out.println("Mouse released");
		maybeShowPopup(e);

	}

	public void mouseEntered(MouseEvent e) {

	}

	public void mouseExited(MouseEvent e) {

	}

	public void createFileMenu() {

		JMenuItem mi;
		GisElementClass myGisElementClass =
			((GisTableModel) tableModels.elementAt(0)).getGisElementClass();
		JMenuBar menubar = new JMenuBar();
		//System.out.println("GisElementClass: " + myGisElementClass);
		if (myGisElementClass instanceof GisObjectClass) {
			JMenu edit = new JMenu(I18n.get("TV_GTV_jMenu_edit"));
			/*JMenuItem mi_new1 = new JMenuItem(I18n.get("TV_GTV_jMenuItem_new"));
			mi_new1.setActionCommand("new");
			mi_new1.addActionListener(this);*/
			JMenuItem mi_new1 =
				ActionManager.getInstance().getJMenuItem(
					topicName_newObject,
					"new");
			mi_new1.setText(I18n.get("TV_GTV_jMenuItem_new"));
			edit.add(mi_new1);

			/*mi_remove1 = new JMenuItem(I18n.get("TV_GTV_jMenuItem_remove"));
			mi_remove1.setActionCommand("remove");
			mi_remove1.addActionListener(this);
			mi_remove1.setEnabled(true);
			edit.add(mi_remove1);*/
			edit.addSeparator();
			menubar.add(edit);
		}

		JMenu open = new JMenu(I18n.get("TV_GTV_jMenu_view"));

		mi = new JMenuItem(I18n.get("TV_GTV_jMenuItem_refresh"));
		mi.setActionCommand("getAllElements");
		mi.addActionListener(this);
		open.add(mi);

		/*cbMenuItem =
			new JCheckBoxMenuItem(I18n.get("TV_GTV_jMenuItem_editable"));
		cbMenuItem.setSelected(true);
		cbMenuItem.addItemListener(this);
		open.add(cbMenuItem);*/

		Version myVersion = myGisElementClass.getVersion();
		String topicName = myVersion.getTopicName();
		JMenuItem mi_editable =
			ActionManager.getInstance().getJMenuItem(topicName, "setEditable");
		mi_editable.setText(I18n.get("TV_GTV_jMenuItem_editable"));
		open.add(mi_editable);
		JMenuItem mi_uneditable =
			ActionManager.getInstance().getJMenuItem(
				topicName,
				"setUnEditable");
		mi_uneditable.setText(I18n.get("TV_GTV_jMenuItem_uneditable"));
		open.add(mi_uneditable);
		/*
		mi = new JMenuItem(I18n.get("TV_GTV_jMenuItem_detail"));
		
		mi.setActionCommand("showSingeObjectView");
		
		mi.addActionListener(this);
		
		open.add(mi);
		*/

		/*
		mi = new JMenuItem(I18n.get("TV_GTV_jMenuItem_vectorSets"));
		
		mi.setActionCommand("showVectorSetView");
		
		mi.addActionListener(this);
		
		open.add(mi);
		*/

		JMenu colProfile = new JMenu(I18n.get("TV_GTV_jMenu_profile"));

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

		menubar.add(open);

		jcombo = new JComboBox(TableProfile.getInstance().getProfiles());
		jcombo.addItemListener(this);
		jcombo.setSize(100, 20);
		menubar.add(jcombo);

		menubar.add(colProfile);

		//	menubar.add(new JComboBox(tableProfiles.getProfiles()));
		this.setJMenuBar(menubar);
	}

	private void maybeShowPopup(MouseEvent e) {

		int selectedTab = tabbedPane.getSelectedIndex();

		GisTableModel tableModel =
			(GisTableModel) tableModels.elementAt(selectedTab);

		JTable table = (JTable) cards.elementAt(selectedTab);

		selectedPoint = e.getPoint();

		selectedCol =
			table.convertColumnIndexToModel(table.columnAtPoint(selectedPoint));

		//selectedCol=table.columnAtPoint(selectedPoint);

		selectedRow = table.rowAtPoint(selectedPoint);
		if (table.getSelectedRow() < 0 && selectedRow >= 0) // no Row Selected
			table.setRowSelectionInterval(selectedRow, selectedRow);

		if (selectedRow >= 0 && selectedCol >= 0)
			selectedValue = tableModel.getValueAt(selectedRow, selectedCol);
		else
			selectedValue = null;

		JPopupMenu popup = new JPopupMenu();
		/*if (e.isPopupTrigger()) {
			GisElementClass myGisElementClass = tableModel.getGisElementClass();
			if (myGisElementClass instanceof GisObjectClass) {
				mi_new2 = new JMenuItem(I18n.get("TV_GTV_PopMen_New"));
				mi_new2.setActionCommand("new");
				mi_new2.addActionListener(this);
				popup.add(mi_new2);
			}
		}*/

		if (e.isPopupTrigger() && selectedRow >= 0 && selectedCol >= 0) {

			JMenuItem mi;

			//JPopupMenu popup = new JPopupMenu();
			GisElementClass myGisElementClass = tableModel.getGisElementClass();

			if (table.getSelectedRow() >= 0) {
				//System.out.println("GisElementClass: " + myGisElementClass);
				if (myGisElementClass instanceof GisObjectClass) {
					JMenuItem mi_new2 =
						new JMenuItem(I18n.get("TV_GTV_PopMen_New"));
					mi_new2.setActionCommand("new");
					mi_new2.addActionListener(this);
					popup.add(mi_new2);

					JMenuItem mi_remove2 =
						new JMenuItem(I18n.get("TV_GTV_PopMen_Remove"));
					mi_remove2.setActionCommand("remove");
					mi_remove2.addActionListener(this);
					popup.add(mi_remove2);

					if ((myGisElementClass.getVersion()).isEditable()) {
						mi_new2.setEnabled(true);
						mi_remove2.setEnabled(true);
					} else {
						mi_new2.setEnabled(false);
						mi_remove2.setEnabled(false);
					}
				}

				mi = new JMenuItem(I18n.get("TV_GTV_PopMen_Detail"));
				mi.setActionCommand("showSingeObjectView");
				mi.addActionListener(this);
				popup.add(mi);

				if (tableModel.hasVectorSets()) {
					mi = new JMenuItem(I18n.get("TV_GTV_PopMen_VectorSets"));
					mi.setActionCommand("showVectorSetView");
					mi.addActionListener(this);
					popup.add(mi);
				}
			}

			mi = new JMenuItem(I18n.get("TV_GTV_PopMen_orderC"));

			mi.setActionCommand("columnOrder");

			mi.addActionListener(this);

			popup.add(mi);

			mi = new JMenuItem(I18n.get("TV_GTV_PopMen_orderCr"));

			mi.setActionCommand("columnOrderReverse");

			mi.addActionListener(this);

			popup.add(mi);

			if (selectedCol > 0 && selectedValue != null) {

				String string1 = I18n.get("TV_GTV_PopMen_setAll1");

				String string2 = I18n.get("TV_GTV_PopMen_setAll2");

				JMenuItem mi_setAll =
					new JMenuItem(
						"<html>"
							+ string1
							+ tableModel.getColumnNameNoHtml(selectedCol)
							+ string2
							+ selectedValue
							+ "</html>");
				/*JMenuItem mi_setAll =
					new JMenuItem(
						string1
							+ tableModel.getColumnNameNoHtml(selectedCol)
							+ string2
							+ selectedValue);*/

				mi_setAll.setActionCommand("setAllSimplePropertiesTo");

				mi_setAll.addActionListener(this);

				if ((myGisElementClass.getVersion()).isEditable()) {
					popup.add(mi_setAll);
				}

			}

			if (selectedCol > 0) // do not hide col "ID"

				{

				mi = new JMenuItem(I18n.get("TV_GTV_PopMen_hide"));

				mi.setActionCommand("hidecolumn");

				mi.addActionListener(this);

				popup.add(mi);

			}

			String description = tableModel.getDescription(selectedCol);

			/*if (description != null && !"".equals(description)) {

				mi =
					new JMenuItem(
						Main.toHtml(
							"<i>"
								+ I18n.get("TV_GTV_PopMen_nothing")
								+ "</i>"
								+ " \""
								+ tableModel.getColumnNameNoHtml(selectedCol)
								+ "\": "
								+ description,
							40));

				mi.setActionCommand("nothing");

				mi.addActionListener(this);

				popup.addSeparator();

				popup.add(mi);

			}*/

			popup.show(e.getComponent(), e.getX(), e.getY());

		}

	}

	// internalFrameListener:

	//          Invoked when an internal frame is activated.

	public void internalFrameActivated(InternalFrameEvent e) {

	}

	//          Invoked when an internal frame has been closed.

	public void internalFrameClosed(InternalFrameEvent e) {

		for (int index = 0; index < tableModels.size(); index++) {

			GisTableModel tableModel =
				(GisTableModel) tableModels.elementAt(index);

			tableModel.close();

		}

		ActionManager.getInstance().unregister(topicName, "setEditable", this);
		ActionManager.getInstance().unregister(
			topicName,
			"setUnEditable",
			this);
		ActionManager.getInstance().unregister(
			topicName_newObject,
			"new",
			this);
		ViewManager.removeViewFromList(e);
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
		ViewManager.addViewToList(e);
	}

	// ItemListener

	public void itemStateChanged(ItemEvent e) {

		Object source = e.getItemSelectable();

		if (source == jcombo) {
			String newProfileName = (String) jcombo.getSelectedItem();
			if (!myProfileName.equals(newProfileName)) {
				myProfileName = newProfileName;
				updateColProfile();
			}
		}

		/*if (source == cbMenuItem) {
			if (e.getStateChange() == ItemEvent.DESELECTED) {
				for (int i = 0; i < tableModels.size(); i++) {
					GisTableModel tableModel =
						(GisTableModel) tableModels.elementAt(i);
					//tableModel.setEditable(false);
					GisElementClass myGisElementClass =
						tableModel.getGisElementClass();
					if (myGisElementClass instanceof GisObjectClass) {
						mi_new1.setEnabled(false);
						//mi_remove1.setEnabled(false);
						isEditable = false;
					}
				}
			}
			if (e.getStateChange() == ItemEvent.SELECTED) {
				for (int i = 0; i < tableModels.size(); i++) {
					GisTableModel tableModel =
						(GisTableModel) tableModels.elementAt(i);
					//tableModel.setEditable(true);
					GisElementClass myGisElementClass =
						tableModel.getGisElementClass();
					if (myGisElementClass instanceof GisObjectClass) {
						mi_new1.setEnabled(true);
						//mi_remove1.setEnabled(true);
						isEditable = true;
					}
				}
			}
		}*/

	}

	class MyJTableHeader extends JTableHeader {
		GisTableModel tableModel;

		MyJTableHeader(
			TableColumnModel columnModel,
			GisTableModel tableModel) {
			super(columnModel);
			this.tableModel = tableModel;
		}
		public String getToolTipText(MouseEvent e) {
			String tip = null;
			java.awt.Point p = e.getPoint();
			int index = columnModel.getColumnIndexAtX(p.x);
			int realIndex = columnModel.getColumn(index).getModelIndex();
			//System.out.println("RealIndex= " + realIndex);
			//System.out.println("TableModel: " + tableModel);
			return tableModel.getDescription(realIndex);
		}

	}

}
