package de.tuhh.wb.javagis.simpleclient;
import java.awt.GridLayout;

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import de.tuhh.wb.javagis.xml.GisTransferObject;

public class VisualizeAreasDialog extends JPanel
{
    final static String[] views={"precipitation","temperature"}; // do not change - string are used as keys
    final static String[] unit={"mm/h","<sup>o</sup>C"};

    final static String[] keys={};
    /*
      "m_tmp","m_pre",
      "m_sch","m_bof","m_bsp","m_gws","m_qgs","m_qgg","m_qna","m_qif","m_qvs","m_qbs",
      "m_qt1","m_qtg","m_qgw","m_kap","m_vet","m_hyd","m_bil","m_nmq","m_spi","m_sub"};
    */
    JCheckBox[] showResults;
    
    public VisualizeAreasDialog()
    {
	super();
	showResults=new JCheckBox[views.length];
	setLayout(new GridLayout(2,views.length));
	//	for(int i=0;i<views.length;i++)
	    //	    add(new JLabel(views[i]));
	for(int i=0;i<views.length;i++)
	    {
		JCheckBox box=new JCheckBox();
		showResults[i]=box;
 		add(new JLabel("<html>"+views[i]+" ["+unit[i]+"]: </html>"));
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
