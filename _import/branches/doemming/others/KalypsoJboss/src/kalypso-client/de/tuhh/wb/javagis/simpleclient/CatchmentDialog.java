package de.tuhh.wb.javagis.simpleclient;


import de.tuhh.wb.javagis.xml.GisTransferObject;
import de.tuhh.wb.javagis.xml.VectorSet;
import org.xml.sax.helpers.AttributesImpl;


import java.awt.GridLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.util.Date;
import java.util.Vector;

import javax.swing.JTable;
import javax.swing.table.TableModel;
import javax.swing.event.TableModelEvent;
import javax.swing.JFileChooser;
import javax.swing.JScrollPane;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JButton;
import javax.swing.JTextField;

public class CatchmentDialog extends JScrollPane implements ActionListener//,MouseListener
{
    JPanel panel;
    JTable jTable;
    JButton addRow;
    CatchmentDialogModel myTableModel;

    public CatchmentDialog()
    {
	super();
	this.panel=new JPanel();
	this.myTableModel=new CatchmentDialogModel();

	addRow=new JButton("add area");
	addRow.setToolTipText("<html>note: you can visualize at maximum 10 graphs at one time with the graphic-tool<br><i>you may produce more than 10 results and load them manually with the graphic-tool</i></html>");

	panel.add(addRow);
	jTable=new JTable(myTableModel);

	JScrollPane scroller=new JScrollPane(jTable);
	scroller.setBounds(10,10,(getSize().width-30),(getSize().height-120));
	scroller.setPreferredSize(new Dimension(80,150));
	panel.add(scroller);
	panel.setPreferredSize(new Dimension(90,100));
	addRow.setActionCommand("addRow");
	addRow.addActionListener(this);
	setViewportView(panel);
	panel.setVisible(true);
	setPreferredSize(new Dimension(100,200));
	doLayout();
	repaint();
	setVisible(true);
	scroller.setBounds(10,10,(getSize().width-30),(getSize().height-120));
    }
    
    public void addRow()
    {
	myTableModel.addRow();
	jTable.tableChanged(new TableModelEvent(myTableModel));
    }
    

    public synchronized void actionPerformed(ActionEvent e)
    {
	String command=e.getActionCommand();
	System.out.println(command);
	if("addRow".equals(command))
	    {
		myTableModel.addRow();
	    }
    }
    
    public void loadFromGto(GisTransferObject gto)
    {
	String text;
	VectorSet vs=gto.getVectorSet("m_teilgebiete");
	if(vs!=null)
	    for(int row=0;row<vs.size();row++)
		{
		    text=vs.getSimpleProperty("v_tgn",row);
		    if(text!=null)
			myTableModel.addRow(new Integer(text));			
		}
	jTable.tableChanged(new TableModelEvent(myTableModel));
    }
    
    public void storeToGto(GisTransferObject gto)
    {
	VectorSet vs=new VectorSet("m_teilgebiete");
	for(int row=0;row<myTableModel.getRowCount();row++)
	{
	    AttributesImpl atts=new AttributesImpl();
	    String value=myTableModel.getValueAt(row,0).toString();
	    atts.addAttribute("","v_tgn","","xsi:string",value);
	    vs.addRow(atts);
	}
	gto.addVectorSet(vs);
    }
}    
