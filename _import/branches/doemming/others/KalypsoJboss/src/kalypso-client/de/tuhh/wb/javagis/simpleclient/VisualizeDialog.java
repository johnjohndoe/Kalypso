package de.tuhh.wb.javagis.simpleclient;
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

import de.tuhh.wb.javagis.xml.GisTransferObject;

public class VisualizeDialog extends JPanel
{
    final static String[] views={"Q","h","x"};
    final static String[] keys={"m_tmp",
				"m_pre",
				"m_sch",
				"m_bsp",
				"m_gws",
				"m_qgs",
				"m_qgg",
				"m_qna",
				"m_qif",
				"m_qvs",
				"m_qbs",				
				"m_qgw",
				"m_spi",
				"m_sub"};
    JCheckBox[] showResults;
    
    public VisualizeDialog()
    {
	super();
	showResults=new JCheckBox[views.length];
	setLayout(new GridLayout(2,views.length));
	for(int i=0;i<views.length;i++)
	    add(new JLabel(views[i]));
	for(int i=0;i<views.length;i++)
	    {
		JCheckBox box=new JCheckBox();
		showResults[i]=box;
		add(box);
	    }	
    }
    
    public void storeToGto(GisTransferObject gto)
    {
	for(int i=0;i<keys.length;i++)
	    {
		gto.addSimpleProperty("view_"+keys[i],"true");
	    }
	for(int i=0;i<views.length;i++)
	    {
		gto.addSimpleProperty("visualize_"+views[i],showResults[i].isSelected());
	    }
    }     
    public void loadFromGto(GisTransferObject gto)
    {
	for(int i=0;i<views.length;i++)
	    {
		boolean status=gto.getSimplePropertyAsBoolean("visualize_"+views[i]);
		showResults[i].setSelected(status);
	    }
    }   
}    
