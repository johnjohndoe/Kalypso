package de.tuhh.wb.javagis.view.tableview;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;

import de.tuhh.wb.javagis.tools.I18n;
import de.tuhh.wb.javagis.view.SingleEditorField;

public class GisTableFilter extends JPanel implements ItemListener
{
    private GisTableModel myGisTableModel;
    private SingleEditorField editField;
    //    private JComboBox todoBox;
    private JComboBox propertyBox;
    private JComboBox equationBox;

    public GisTableFilter(GisTableView myGisTableView,GisTableModel gisTableModel)
    {
	super(new GridBagLayout());
	GridBagLayout layout=(GridBagLayout)getLayout();


	//	String[] todo={"filter from all:","filter from current selection:"};
	String[] equation={"<","<=","=",">=",">"};
	this.myGisTableModel=gisTableModel;

	//	this.todoBox=new JComboBox(todo);
	this.propertyBox=new JComboBox();
	this.equationBox=new JComboBox(equation);
	int max=myGisTableModel.getColumnCount();
	for(int i=0;i<max;i++)
	    propertyBox.addItem(myGisTableModel.getColumnNameNoHtml(i));
	//	JButton selectAll=new JButton("selectAll");
	//	JButton unselectAll=new JButton("unselectAll");
	JButton showAll=new JButton(I18n.get("TV_GTF_jBut_showAll"));
	int col=0;
	



	//	selectAll.setActionCommand("selectAll");
	//	selectAll.addActionListener(myGisTableView);
	//	unselectAll.setActionCommand("unselectAll");
	//	unselectAll.addActionListener(myGisTableView);
	showAll.setActionCommand("getAllElements");
	showAll.addActionListener(myGisTableView);

	editField= new SingleEditorField(this,myGisTableModel.getColumnNameNoHtml(0),myGisTableModel.getColumnClass(0));

	//	add2View(selectAll);
	//	add2View(unselectAll);
	add2View(showAll);
	add2View(new JLabel("<html><center>"+I18n.get("TV_GTF_jLab_Filter")+"</center></html>"));

	//	add2ViewLastInRow(new JSeparator(SwingConstants.HORIZONTAL));
	//	add2View(todoBox);
	add2View(propertyBox);
	add2View(equationBox);
	add2ViewLastInRow(editField);

	doLayout();
	propertyBox.addItemListener(this);
	equationBox.addItemListener(this);
    }

    public void start(Object value)
    {
	System.out.println("filter...");
	boolean selectFromAll=true;
	/*
	  switch(todoBox.getSelectedIndex())
	  {
	  case 0:
	  selectFromAll=true;
	  break;
	  case 1:
	*/
	selectFromAll=false;
	/*
	  break;
	  }
	*/
	int col=propertyBox.getSelectedIndex();
	myGisTableModel.filter(col,value,selectFromAll,equationBox.getSelectedIndex());
    }

    public void itemStateChanged(ItemEvent e)
    {
	int col=propertyBox.getSelectedIndex();
	editField.reload(this,myGisTableModel.getColumnName(col),myGisTableModel.getColumnClass(col));
	
    }

    public void add2View(JComponent  component)
    {
	GridBagLayout layout=(GridBagLayout)getLayout();
	GridBagConstraints layoutConstraints = new GridBagConstraints();
	layoutConstraints.fill = GridBagConstraints.BOTH;
	layoutConstraints.gridwidth = 1;
	layoutConstraints.gridheight =1;
	layoutConstraints.weightx = 0.5;
 	layoutConstraints.weighty = 1;
	layout.setConstraints(component, layoutConstraints);
	add(component);
    }

    public void add2ViewLastInRow(JComponent  component)
    {
	GridBagLayout layout=(GridBagLayout)getLayout();
	GridBagConstraints layoutConstraints = new GridBagConstraints();
	layoutConstraints.fill = GridBagConstraints.BOTH;
	layoutConstraints.gridwidth = GridBagConstraints.REMAINDER;
	layoutConstraints.gridheight =1;
	layoutConstraints.weightx = 0.5;
 	layoutConstraints.weighty = 1;
	layout.setConstraints(component, layoutConstraints);
	add(component);
    }
}
