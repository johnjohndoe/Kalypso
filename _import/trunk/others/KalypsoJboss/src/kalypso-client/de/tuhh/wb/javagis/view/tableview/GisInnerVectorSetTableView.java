package de.tuhh.wb.javagis.view.tableview;

import java.awt.BorderLayout;
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

import de.tuhh.wb.javagis.data.GisElementClass;
import de.tuhh.wb.javagis.model.GisInterfaceTableModel;
import de.tuhh.wb.javagis.tools.I18n;
import de.tuhh.wb.javagis.view.ViewManager;
public class GisInnerVectorSetTableView extends JInternalFrame implements InternalFrameListener,ActionListener,MouseListener
{
    private GisElementClass myGisElementClass;
    private Object myId;
    private int myVectorSetPos;
    private Object myOuterVectorSet;
    
    private Vector cards;
    private Vector tableModels;

    private JTabbedPane tabbedPane;
    
    public GisInnerVectorSetTableView(String frameName,Vector tableModels,Object gisElementId,GisElementClass gisElementClass,Object outerVectorSet,int vectorSetPos)
    {
	super(frameName,true,true,true,true);
	this.myGisElementClass=gisElementClass;
	this.myId=gisElementId;
	this.myVectorSetPos=vectorSetPos;
	this.myOuterVectorSet=outerVectorSet;
	
	this.cards=new Vector();
	this.tableModels=tableModels;
	this.tabbedPane=new JTabbedPane();
	
	for (int index=0;index<tableModels.size();index++)
	    {
		GisInterfaceTableModel tableModel=(GisInterfaceTableModel)tableModels.elementAt(index);
		JTable jTable=new JTable(new DummyTableModel((GisInterfaceTableModel)tableModels.elementAt(index),myGisElementClass),null);
		cards.add(jTable);
		JScrollPane scroller=new JScrollPane(jTable);
		scroller.setBounds(10,10,(getSize().width-30),(getSize().height-120));
		tabbedPane.addTab(tableModel.getName(),scroller);
		tabbedPane.setToolTipTextAt(index,tableModel.getDescription());
		jTable.addMouseListener(this);
	    }
	getContentPane().add(tabbedPane,BorderLayout.CENTER);
	createFileMenu();
	this.addInternalFrameListener(this);
	//	myGisElementClass.register(this);
    }
    


    // ActionListener:
    public void actionPerformed(ActionEvent event)
    {
	String action = event.getActionCommand();
	System.out.println(action);
		
	int tab =tabbedPane.getSelectedIndex();
	GisInterfaceTableModel tableModel = (GisInterfaceTableModel)tableModels.elementAt(tab);
	System.out.println(tableModel.getName());
	JTable table = (JTable)cards.elementAt(tab);
	int rows = table.getRowCount();
	int[] selectedRows = table.getSelectedRows();
		
	if (action.equals("remove"))
	    {
		System.out.println("Ein Objekt ist zu loeschen");
		
		if(selectedRows.length<=0)
		    {
			JOptionPane jop = new JOptionPane();
			jop.showMessageDialog(this,I18n.get("GTV_DiaRemove_Message"),I18n.get("GTV_DiaRemove_Title"),JOptionPane.INFORMATION_MESSAGE);
		    }
		else
		    {
			tableModel.removeObject(selectedRows[0]);
			refreshTable();
		    }
	    }

	if(action.equals("new"))
	    {
		tableModel.createNewObject();
		refreshTable();
	    }
	if(action.equals("insert"))
	    {
		tableModel.insertNewObjectAt(selectedRows[0]);
		refreshTable();
	    }
	if(action.equals("save"))
	    {
		store();
	    }
    }
    
    private void store()
    {
	myGisElementClass.setVectorSet(myId,myVectorSetPos,myOuterVectorSet);
    }

    public void refreshTable()
    {
	int tab=tabbedPane.getSelectedIndex();
	JTable table=(JTable)cards.elementAt(tab);
	table.tableChanged(new TableModelEvent(table.getModel()));
    }
    
	
    public void mouseClicked(MouseEvent e)
    {
	
    }
    
    public void mousePressed(MouseEvent e)
    {
	maybeShowPopup(e);
    }
    
    public void mouseReleased(MouseEvent e)
    {
	maybeShowPopup(e);
    }
    
    public void mouseEntered(MouseEvent e)
    {
    }
    
    public void mouseExited(MouseEvent e)
    {
    }
    
    public void createFileMenu()
    {
	JMenu edit = new JMenu (I18n.get("TV_GVectorTV_jMenu_edit"));

	JMenuItem mi;
	mi = new JMenuItem(I18n.get("TV_GVectorTV_jMenuItem_new"));
	mi.setActionCommand("new");
	mi.addActionListener(this);
	edit.add(mi);

	mi = new JMenuItem(I18n.get("TV_GVectorTV_jMenuItem_insert"));
	mi.setActionCommand("insert");
	mi.addActionListener(this);
	edit.add(mi);

 	mi = new JMenuItem();
	mi.setActionCommand(I18n.get("TV_GVectorTV_jMenuItem_remove"));
	mi.addActionListener(this);
	edit.add(mi);

 	mi = new JMenuItem();
	mi.setActionCommand(I18n.get("TV_GVectorTV_jMenuItem_save"));
	mi.addActionListener(this);
	edit.add(mi);

	//create.addSeparator();
	edit.addSeparator();

	JMenuBar menubar= new JMenuBar();
	//menubar.add(create);
	menubar.add(edit);
	this.setJMenuBar(menubar);
    }

    
    private void maybeShowPopup(MouseEvent e)
    {
	int selectedTab = tabbedPane.getSelectedIndex();
	GisInterfaceTableModel tableModel = (GisInterfaceTableModel)tableModels.elementAt(selectedTab);
	
        if (e.isPopupTrigger())
	    {
		JMenuItem mi;
		JPopupMenu popup = new JPopupMenu();

		mi = new JMenuItem(I18n.get("TV_GVectorTV_PopMen_new"));
		mi.setActionCommand("new");
		mi.addActionListener(this);
		popup.add(mi);

		mi = new JMenuItem(I18n.get("TV_GVectorTV_PopMen_remove"));
		mi.setActionCommand("remove");
		mi.addActionListener(this);
		popup.add(mi);

		mi = new JMenuItem(I18n.get("TV_GVectorTV_PopMen_insert"));
		mi.setActionCommand("insert");
		mi.addActionListener(this);
		popup.add(mi);
		
		mi = new JMenuItem(I18n.get("TV_GVectorTV_PopMen_save"));
		mi.setActionCommand("save");
		mi.addActionListener(this);
		popup.add(mi);
		
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
		ViewManager.removeViewFromList(e);
	//	myGisElementClass.unRegister(this);
    }

    //          Invoked when an internal frame is in the process of being closed.
    public void internalFrameClosing(InternalFrameEvent e)
    {
	store();
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
    {
		ViewManager.addViewToList(e);
    }

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

