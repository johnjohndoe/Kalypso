package de.tuhh.wb.javagis.view.projectview;

import java.util.Vector;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import javax.swing.JInternalFrame;
import javax.swing.*;
import javax.swing.JLabel;
import javax.swing.tree.*;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import java.awt.event.MouseListener;
import java.awt.event.MouseEvent;

import javax.swing.event.InternalFrameListener;
import javax.swing.event.InternalFrameEvent;

import de.tuhh.wb.javagis.data.VersionAccess;
import de.tuhh.wb.javagis.data.VersionAccessImpl;
import de.tuhh.wb.javagis.view.ViewManager;
import de.tuhh.wb.javagis.data.event.VersionListener;
import de.tuhh.wb.javagis.data.event.KalypsoEventManager;
import de.tuhh.wb.javagis.tools.I18n;
import ejb.event.EJBEvent;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import de.tuhh.wb.javagis.view.LogView;

public class VersionDialog extends JInternalFrame implements ActionListener, VersionListener,InternalFrameListener
{
    public final static int NOTHING=0;
    public final static int CREATE=1;
    public final static int RENAME=2;
    public final static int COPY=3;

    private int myStatus=NOTHING;
    private VersionAccess myVersionAccess;
    private String myThemeKey;
    private Object myVersionId;
    private JPanel panel=new JPanel(new GridBagLayout());
    private String myTitle;

    JTextField jProject     = new JTextField();
    JComboBox  jTheme       = new JComboBox();
    JTextField jState       = new JTextField();
    JTextField jName        = new JTextField();
    JTextField jDescription = new JTextField();
    JButton jCancel=new JButton(I18n.get("PV_VD_Bcancel"));
    JButton jOK;

    public VersionDialog(String title,int status,VersionAccess versionAccess,String themeKey,Object vId)
    {
	super(title,true,true,true,true);
	this.myTitle=title;
	this.myVersionAccess=versionAccess;
	this.myThemeKey=themeKey;
	this.myVersionId=vId;
	this.myStatus=status;
	jOK=new JButton(myTitle);
	initMask();
	ViewManager.desktop.add(this);
	setVisible(true);
	setSize(300,300);
	moveToFront();
    }
    
    private void initMask()
    {
	switch(myStatus)
	    {
	    case CREATE:
		jProject.setText(I18n.get("PV_VD_TFjProject_CREATE"));
		Vector themeKeys=myVersionAccess.getThemeKeys();
		for(int i=0;i<themeKeys.size();i++)
		    jTheme.addItem((String)themeKeys.elementAt(i));
		jState.setText(I18n.get("PV_VD_TFjState_CREATE"));
		jName.setText(I18n.get("PV_VD_TFjName_CREATE"));
		jDescription.setText(I18n.get("PV_VD_TFjDescription_CREATE"));
		break;
	    case RENAME:
	    case COPY:
		jProject.setText(myVersionAccess.getProjectName(myThemeKey,myVersionId));
		jTheme.addItem(myThemeKey);
		jState.setText(myVersionAccess.getState(myThemeKey,myVersionId));
		jName.setText(myVersionAccess.getName(myThemeKey,myVersionId));
		jDescription.setText(myVersionAccess.getDescription(myThemeKey,myVersionId));
		break;
	    default:
		break;
 	    }
	add2View(new JLabel(I18n.get("PV_VD_LabeljProject")));
	add2ViewLastInRow(jProject);
	add2View(new JLabel(I18n.get("PV_VD_LabeljTheme")));
	add2ViewLastInRow(jTheme);
	add2View(new JLabel(I18n.get("PV_VD_LabeljState")));
	add2ViewLastInRow(jState);
	add2View(new JLabel(I18n.get("PV_VD_LabeljName")));
	add2ViewLastInRow(jName);
	add2View(new JLabel(I18n.get("PV_VD_LabeljDescription")));
	add2ViewLastInRow(jDescription);
	add2View(jOK);
	add2ViewLastInRow(jCancel);
	jCancel.setActionCommand("cancel");
	jCancel.addActionListener(this);
	jOK.setActionCommand("ok");
	jOK.addActionListener(this);
	getContentPane().add(panel);
    }
    
    // ActionListener
    public void actionPerformed(ActionEvent e)
    {
	String command=e.getActionCommand();
	    
	if("cancel".equals(command))
	    {
		dispose();
	    }
	if("ok".equals(command))
	    {
		String project=jProject.getText();
		String themeKey=(String)jTheme.getSelectedItem();
		String state=jState.getText();
		String name=jName.getText();
		String description=jDescription.getText();
		switch(myStatus)
		    {
		    case CREATE:
			myVersionAccess.createVersion(project,themeKey,state,name,description);
			dispose();
			break;
		    case RENAME:
			myVersionAccess.renameVersion(myThemeKey,myVersionId,project,state,name,description);
			dispose();
			break;
		    case COPY:
			LogView.println(I18n.get("LV_VD_copy1"));
			myVersionAccess.copyVersion(myThemeKey,myVersionId,project,state,name,description);
			LogView.println(I18n.get("LV_VD_copy2"));
			dispose();
			break;
		    }
	    }
    }
    
    //VersionListener:
    public void onVersionChanged(EJBEvent event)
    {
	//	reloadProjectTree();
    }

    //          Invoked when an internal frame is activated.
    public void internalFrameActivated(InternalFrameEvent e)
    {}

    //          Invoked when an internal frame has been closed.
    public void internalFrameClosed(InternalFrameEvent e)
    {
	System.out.println("VersionDialogClosed");
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

    public void add2View(JComponent  component)
    {
	GridBagLayout layout=(GridBagLayout)panel.getLayout();
	GridBagConstraints layoutConstraints = new GridBagConstraints();
	layoutConstraints.fill = GridBagConstraints.HORIZONTAL;
	layoutConstraints.gridwidth = 1;
	layoutConstraints.gridheight =1;
	layoutConstraints.weightx = 0.5;
 	layoutConstraints.weighty = 1;
	layout.setConstraints(component, layoutConstraints);
	panel.add(component);
    }
    public void add2ViewLastInRow(JComponent  component)
    {
	GridBagLayout layout=(GridBagLayout)panel.getLayout();
	GridBagConstraints layoutConstraints = new GridBagConstraints();
	layoutConstraints.fill = GridBagConstraints.HORIZONTAL;
	layoutConstraints.gridwidth = GridBagConstraints.REMAINDER;
	layoutConstraints.gridheight =1;
	layoutConstraints.weightx = 0.5;
 	layoutConstraints.weighty = 1;
	layout.setConstraints(component, layoutConstraints);
	panel.add(component);
    }

}
