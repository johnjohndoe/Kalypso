package de.tuhh.wb.javagis.view.tableview;
import java.awt.BorderLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Vector;

import javax.swing.JInternalFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.event.InternalFrameEvent;
import javax.swing.event.InternalFrameListener;
import javax.swing.event.TableModelEvent;

import de.tuhh.wb.javagis.Main;
import de.tuhh.wb.javagis.data.GisElementClass;
import de.tuhh.wb.javagis.data.Version;
import de.tuhh.wb.javagis.model.GisInterfaceTableModel;
import de.tuhh.wb.javagis.tools.I18n;
import de.tuhh.wb.javagis.view.ViewManager;
import de.tuhh.wb.tools.event.ActionManager;
public class GisVectorSetTableView
	extends JInternalFrame
	implements InternalFrameListener, ActionListener, MouseListener {
	private GisElementClass myGisElementClass;
	private Object myId;
	private Vector cards;
	private Vector tableModels;
	private Vector dummyTableModels;
	private JTabbedPane tabbedPane;
	private GisTableModel myGisTableModel;
	private Point selectedPoint = null;
	private int selectedCol = -1;
	private int selectedRow = -1;
	private Object selectedValue = null;
	//private JCheckBoxMenuItem cbMenuItem;
	private DummyTableModel dummyTableModel;
	private String topicName = null;
	private String topicName_newObject = null;
	public GisVectorSetTableView(
		GisTableModel gisTableModel,
		String frameName,
		Vector tableModels,
		Object gisElementId,
		GisElementClass gisElementClass) {
		super(frameName, true, true, true, true);
		this.myGisTableModel = gisTableModel;
		this.myId = gisElementId;
		this.myGisElementClass = gisElementClass;
		this.cards = new Vector();
		this.tableModels = tableModels;
		this.dummyTableModels = new Vector();
		this.tabbedPane = new JTabbedPane();
		for (int index = 0; index < tableModels.size(); index++) {
			dummyTableModel =
				new DummyTableModel(
					(GisInterfaceTableModel) tableModels.elementAt(index),
					myGisElementClass);
			dummyTableModels.addElement(dummyTableModel);
			JTable jTable = new JTable(dummyTableModel, null);
			/*
			GisInterfaceTableModel tableModel=(GisInterfaceTableModel)tableModels.elementAt(index);
			JTable jTable=new JTable(new DummyTableModel((GisInterfaceTableModel)tableModels.elementAt(index)),null);
			 */
			cards.add(jTable);
			JScrollPane scroller = new JScrollPane(jTable);
			scroller.setBounds(
				10,
				10,
				(getSize().width - 30),
				(getSize().height - 120));
			tabbedPane.addTab(dummyTableModel.getName(), scroller);
			tabbedPane.setToolTipTextAt(
				index,
				Main.toHtml(dummyTableModel.getDescription(), 50));
			jTable.addMouseListener(this);
		}
		getContentPane().add(tabbedPane, BorderLayout.CENTER);
		topicName_newObject = "NewObject" + frameName;
		Version version = myGisElementClass.getVersion();
		topicName = version.getTopicName();
		if (version.isEditable()) {
			ActionManager.getInstance().register(
				topicName,
				"setUnEditable",
				this);
			ActionManager.getInstance().register(
				topicName_newObject,
				"new",
				this);
		} else {
			ActionManager.getInstance().register(
				topicName,
				"setEditable",
				this);
		}
		createFileMenu();
		this.addInternalFrameListener(this);
		//	myGisElementClass.register(this);
	}
	// ActionListener:
	public void actionPerformed(ActionEvent event) {
		String action = event.getActionCommand();
		System.out.println(action);
		int selectedTab = tabbedPane.getSelectedIndex();
		GisInterfaceTableModel tableModel =
			(GisInterfaceTableModel) tableModels.elementAt(selectedTab);
		System.out.println(tableModel.getName());
		JTable table = (JTable) cards.elementAt(selectedTab);
		int rows = table.getRowCount();
		int[] selectedRows = table.getSelectedRows();
		if (action.equals("remove")) {
			//System.out.println("Ein Objekt ist zu löschen");
			if (selectedRows.length <= 0) {
				JOptionPane jop = new JOptionPane();
				jop.showMessageDialog(
					this,
					I18n.get("GTV_DiaRemove_Message"),
					I18n.get("GTV_DiaRemove_Title"),
					JOptionPane.INFORMATION_MESSAGE);
			} else {
				tableModel.removeObject(selectedRows[0]);
				refreshTable();
			}
		}
		if (action.equals("new")) {
			tableModel.createNewObject();
			refreshTable();
		}
		if (action.equals(topicName_newObject + ".new")) {
			if ((myGisElementClass.getVersion()).isEditable()) {
			tableModel.createNewObject();
			}
			refreshTable();
		}
		if (action.equals("insert")) {
			if (selectedRows.length <= 0) {
				JOptionPane jop = new JOptionPane();
				jop.showMessageDialog(
					this,
					I18n.get("GTV_DiaRemove_Message"),
					I18n.get("GTV_DiaRemove_Title"),
					JOptionPane.INFORMATION_MESSAGE);
			} else {
				tableModel.insertNewObjectAt(selectedRows[0]);
				refreshTable();
			}
		}
		if (action.equals("setPropertyInSelectedObject")) {
			myGisTableModel.setPropertyInVectorSet(
				selectedTab,
				selectedRow,
				selectedCol,
				selectedValue);
			refreshTable();
		}
		if (action.equals("showVectorSetView") && tableModel.hasVectorSets()) {
			int row = selectedRows[0];
			Vector vectorSetTableModels =
				tableModel.getVectorSetTableModels(row);
			String frameName =
				getTitle()
					+ "/"
					+ tableModel.getName()
					+ "#"
					+ myId.toString()
					+ "/VectorSet";
			boolean open = ViewManager.isViewOpen(frameName);
			if (!open) {
				GisInnerVectorSetTableView gisInnerVectorSetTableView =
					new GisInnerVectorSetTableView(
						frameName,
						vectorSetTableModels,
						myId,
						myGisElementClass,
						tableModel,
						selectedTab);
				gisInnerVectorSetTableView.setVisible(true);
				gisInnerVectorSetTableView.setSize(670, 300);
				ViewManager.desktop.add(gisInnerVectorSetTableView);
				gisInnerVectorSetTableView.moveToFront();
			}
		}
		//    public GisInnerVectorSetTableView(String frameName,Vector tableModels,Object gisElementId,GisElementClass gisElementClass,Object outerVectorSet,int vectorSetPos)
		if (action.equals("save")) {
			store();
		}
		if (action.equals(topicName + ".setEditable")) {
			ActionManager.getInstance().unregister(
				topicName,
				"setEditable",
				this);
			ActionManager.getInstance().register(
				topicName,
				"setUnEditable",
				this);
			ActionManager.getInstance().register(
				topicName_newObject,
				"new",
				this);
		}
		if (action.equals(topicName + ".setUnEditable")) {
			ActionManager.getInstance().register(
				topicName,
				"setEditable",
				this);
			ActionManager.getInstance().unregister(
				topicName,
				"setUnEditable",
				this);
			ActionManager.getInstance().unregister(
				topicName_newObject,
				"new",
				this);
		}
	}
	public void store() {
		myGisElementClass.setVectorSets(myId, tableModels);
	}
	public void refreshTable() {
		int tab = tabbedPane.getSelectedIndex();
		JTable table = (JTable) cards.elementAt(tab);
		table.tableChanged(new TableModelEvent(table.getModel()));
	}
	public void mouseClicked(MouseEvent e) {
	}
	public void mousePressed(MouseEvent e) {
		maybeShowPopup(e);
	}
	public void mouseReleased(MouseEvent e) {
		maybeShowPopup(e);
	}
	public void mouseEntered(MouseEvent e) {
	}
	public void mouseExited(MouseEvent e) {
	}
	public void createFileMenu() {
		JMenu edit = new JMenu(I18n.get("TV_GVectorTV_jMenu_edit"));
		JMenuItem mi;
		/*JMenuItem mi_new1 =
			new JMenuItem(I18n.get("TV_GVectorTV_jMenuItem_new"));
		mi_new1.setActionCommand("new");
		mi_new1.addActionListener(this);*/
		JMenuItem mi_new1 =
			ActionManager.getInstance().getJMenuItem(
				topicName_newObject,
				"new");
		mi_new1.setText(I18n.get("TV_GTV_jMenuItem_new"));
		edit.add(mi_new1);
		/* mi = new JMenuItem(I18n.get("TV_GVectorTV_jMenuItem_insert"));
		mi.setActionCommand("insert");
		mi.addActionListener(this);
		edit.add(mi);
		mi = new JMenuItem(I18n.get("TV_GVectorTV_jMenuItem_remove"));
		mi.setActionCommand("remove");
		mi.addActionListener(this);
		edit.add(mi);*/
		mi = new JMenuItem(I18n.get("TV_GVectorTV_jMenuItem_save"));
		mi.setActionCommand("save");
		mi.addActionListener(this);
		edit.add(mi);
		/*cbMenuItem =
			new JCheckBoxMenuItem(I18n.get("TV_GTV_jMenuItem_editable"));
		cbMenuItem.setSelected(true);
		cbMenuItem.addItemListener(this);
		edit.add(cbMenuItem);*/
		JMenuItem mi_editable =
			ActionManager.getInstance().getJMenuItem(topicName, "setEditable");
		mi_editable.setText(I18n.get("TV_GTV_jMenuItem_editable"));
		edit.add(mi_editable);
		JMenuItem mi_uneditable =
			ActionManager.getInstance().getJMenuItem(
				topicName,
				"setUnEditable");
		mi_uneditable.setText(I18n.get("TV_GTV_jMenuItem_uneditable"));
		edit.add(mi_uneditable);
		//create.addSeparator();
		edit.addSeparator();
		/*
		  JMenu open = new JMenu ("View");
		  mi = new JMenuItem("Show VectorSets");
		  mi.setActionCommand("showVectorSetView");
		  mi.addActionListener(this);
		  open.add(mi);
		*/
		JMenuBar menubar = new JMenuBar();
		//menubar.add(create);
		menubar.add(edit);
		//	menubar.add(open);
		this.setJMenuBar(menubar);
	}
	private void maybeShowPopup(MouseEvent e) {
		int selectedTab = tabbedPane.getSelectedIndex();
		/*GisInterfaceTableModel tableModel =
			(GisInterfaceTableModel) tableModels.elementAt(selectedTab);*/
		DummyTableModel tableModel =
			(DummyTableModel) dummyTableModels.elementAt(selectedTab);
		JTable table = (JTable) cards.elementAt(selectedTab);
		if (e.isPopupTrigger()) {
			selectedPoint = e.getPoint();
			selectedCol =
				table.convertColumnIndexToModel(
					table.columnAtPoint(selectedPoint));
			selectedRow = table.rowAtPoint(selectedPoint);
			selectedValue = tableModel.getValueAt(selectedRow, selectedCol);
			JMenuItem mi;
			JPopupMenu popup = new JPopupMenu();
			JMenuItem mi_new2 =
				new JMenuItem(I18n.get("TV_GVectorTV_PopMen_new"));
			mi_new2.setActionCommand("new");
			mi_new2.addActionListener(this);
			popup.add(mi_new2);
			JMenuItem mi_remove1 =
				new JMenuItem(I18n.get("TV_GVectorTV_PopMen_remove"));
			mi_remove1.setActionCommand("remove");
			mi_remove1.addActionListener(this);
			popup.add(mi_remove1);
			JMenuItem mi_insert1 =
				new JMenuItem(I18n.get("TV_GVectorTV_PopMen_insert"));
			mi_insert1.setActionCommand("insert");
			mi_insert1.addActionListener(this);
			popup.add(mi_insert1);
			if ((myGisElementClass.getVersion()).isEditable()) {
				mi_new2.setEnabled(true);
				mi_remove1.setEnabled(true);
				mi_insert1.setEnabled(true);
			} else {
				mi_new2.setEnabled(false);
				mi_remove1.setEnabled(false);
				mi_insert1.setEnabled(false);
			}
			/*mi = new JMenuItem(I18n.get("TV_GVectorTV_PopMen_save"));
			mi.setActionCommand("save");
			mi.addActionListener(this);
			popup.add(mi);*/
			if (selectedValue != null) {
				mi =
					new JMenuItem(
						"<html>"
							+ I18n.get("TV_GVectorTV_PopMen_set1")
							+ myGisTableModel.getName()
							+ " ("
							+ myGisTableModel.getRowCount()
							+ ")<br>"
							+ I18n.get("TV_GVectorTV_PopMen_set2")
							+ tableModel.getName()
							+ " "
							+ I18n.get("TV_GVectorTV_PopMen_set3")
							+ selectedRow
							+ "<br>"
							+ I18n.get("TV_GVectorTV_PopMen_set4")
							+ tableModel.getColumnName(selectedCol)
							+ I18n.get("TV_GVectorTV_PopMen_set5")
							+ selectedValue
							+ "<html>");
				/*mi =
					new JMenuItem(
						I18n.get("TV_GVectorTV_PopMen_set1")
							+ myGisTableModel.getName()
							+ " ("
							+ myGisTableModel.getRowCount()
							+ ") "
							+ I18n.get("TV_GVectorTV_PopMen_set2")
							+ tableModel.getName()
							+ " "
							+ I18n.get("TV_GVectorTV_PopMen_set3")
							+ selectedRow
							+ " "
							+ I18n.get("TV_GVectorTV_PopMen_set4")
							+ tableModel.getColumnName(selectedCol)
							+ I18n.get("TV_GVectorTV_PopMen_set5")
							+ selectedValue);*/
				mi.setActionCommand("setPropertyInSelectedObject");
				mi.addActionListener(this);
				if ((myGisElementClass.getVersion()).isEditable()) {
					popup.add(mi);
				}
			}
			if (tableModel.hasVectorSets()) {
				mi = new JMenuItem(I18n.get("TV_GVectorTV_PopMen_vector"));
				mi.setActionCommand("showVectorSetView");
				mi.addActionListener(this);
				popup.add(mi);
			}
			String description = tableModel.getDescription(selectedCol);
			if (description != null && !"".equals(description)) {
				mi =
					new JMenuItem(
						Main.toHtml(
							I18n.get("TV_GVectorTV_PopMen_nothing")
								+ " \""
								+ tableModel.getColumnName(selectedCol)
								+ "\": "
								+ description,
							40));
				mi.setActionCommand("nothing");
				mi.addActionListener(this);
				popup.addSeparator();
				popup.add(mi);
			}
			popup.show(e.getComponent(), e.getX(), e.getY());
		}
	}
	// internalFrameListener:
	//          Invoked when an internal frame is activated.
	public void internalFrameActivated(InternalFrameEvent e) {
	}
	//          Invoked when an internal frame has been closed.
	public void internalFrameClosed(InternalFrameEvent e) {

		ActionManager.getInstance().unregister(
			topicName,
			"setUnEditable",
			this);
		ActionManager.getInstance().unregister(topicName, "setEditable", this);
		ActionManager.getInstance().unregister(
			topicName_newObject,
			"new",
			this);

		ViewManager.removeViewFromList(e);
		//	myGisElementClass.unRegister(this);
	}
	//          Invoked when an internal frame is in the process of being closed.
	public void internalFrameClosing(InternalFrameEvent e) {
		store();
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
	/*public void itemStateChanged(ItemEvent e) {
		Object source = e.getItemSelectable();
		if (source == cbMenuItem) {
			if (e.getStateChange() == ItemEvent.DESELECTED) {
					tableModel.setEditable(false);
					isEditable = false;
					mi_new1.setEnabled(false);
			}
			if (e.getStateChange() == ItemEvent.SELECTED) {
					tableModel.setEditable(true);
					isEditable = true;
					mi_new1.setEnabled(true);
			}
		}
	}*/
	/*
	//GisElementListener
	public void onGisElementChanged(GisElementEvent event)
	{
	if(event.isAffected(myId))
	    {
		switch(event.getEventType())
		    {
		    case GisElementEvent.Removed:
			dispose();
			break;
		    default:
			break;
		    }
	    }
	}
	*/
}
