
package de.tuhh.wb.javagis.view.tableview;

import javax.swing.DefaultCellEditor;
import javax.swing.JTable;
import java.awt.Component;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import java.awt.*;
import java.awt.event.*;
import de.tuhh.wb.javagis.data.TSLink;

public class TableCellEditorBCE extends DefaultCellEditor {

		TSLink selectedLink = null;
		
		public TableCellEditorBCE(JButton b) {

            super(new JCheckBox());

            editorComponent = b;

            setClickCountToStart(1); //This is usually 1 or 2.

		    //setUpDateEditor(table);

            //Must do this so that editing stops when appropriate.

            b.addActionListener(new ActionListener() {

                public void actionPerformed(ActionEvent e) {

                    //fireEditingStopped();

                }

            });

        }


		
		public Component getTableCellEditorComponent(JTable table,

						 Object value,

						 boolean isSelected,

						 int row,

						 int col)

    	{

		if(value!=null)
			{

				((JButton)editorComponent).setText(value.toString());

				selectedLink=(TSLink)value;
			//	System.out.println("getTableCellEditorComponent: "+selectedNode.toString());
			}

			else
			{
				((JButton)editorComponent).setText("");
                selectedLink=null;
			}

            return editorComponent;


	

    	}
		
		public Object getCellEditorValue()

    	{

		//System.out.println("getCellEditorValue: "+selectedLink.toString());

		return selectedLink;
		

    	}
		
		protected void fireEditingStopped() {

            super.fireEditingStopped();

        }
	}

