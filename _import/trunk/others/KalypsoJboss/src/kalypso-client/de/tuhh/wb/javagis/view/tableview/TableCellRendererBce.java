package de.tuhh.wb.javagis.view.tableview;



import javax.swing.table.AbstractTableModel;



//import de.tuhh.wb.javagis.model.ElementSession;

import java.util.Hashtable;

import java.util.Vector;

import java.util.Enumeration;

import de.tuhh.wb.javagis.view.GisView;

import javax.swing.event.TableModelEvent;

import java.awt.Component;

import de.tuhh.wb.javagis.Main;

import de.tuhh.wb.javagis.data.*;

import de.tuhh.wb.javagis.data.event.ElementClassListener;

import de.tuhh.wb.javagis.tools.I18n;

import javax.swing.JButton;

import javax.swing.JTable;

import javax.swing.JLabel;

import de.tuhh.wb.javagis.model.GisInterfaceTableModel;

import java.text.ParseException;

import java.text.SimpleDateFormat;

import java.text.DateFormat;

import java.util.Date;

import java.util.Comparator;

import javax.swing.table.TableCellRenderer;

import javax.swing.table.TableCellEditor;

import java.util.EventObject;

import javax.swing.event.CellEditorListener;

import java.awt.event.ActionEvent;

import java.awt.event.ActionListener;



import timeserieSelection.CTSStruct;


import timeserieSelection.CSelectTSFrame;




public class TableCellRendererBce extends JLabel implements TableCellRenderer

{


    public TableCellRendererBce()

    {
		super();
    }

    

    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int col)

    {
	if(value!=null)

			 setText(value.toString());

			else

			 setText("");

	return this;

    }

}

