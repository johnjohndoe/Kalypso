
package de.tuhh.wb.javagis.view.singleview;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.table.*;
import javax.swing.event.*;
import java.util.EventObject;

public class MultiEditor implements TableCellEditor {
  private final static int      COMBO = 0;
  private final static int    BOOLEAN = 1;
  private final static int     STRING = 2;
  private final static int NUM_EDITOR = 3;
  DefaultCellEditor[] cellEditors;
  JComboBox comboBox;
  int flg;
	
class ComboString {
  String str;
  ComboString(String str) {
    this.str = str;
  }
  public String toString() {
    return str;
  }
}
  public MultiEditor() {
    cellEditors = new DefaultCellEditor[NUM_EDITOR];
    comboBox = new JComboBox();
    comboBox.addItem("true");
    comboBox.addItem("false");
    cellEditors[COMBO]   = new DefaultCellEditor(comboBox);
    JCheckBox checkBox   = new JCheckBox();
    //checkBox.setOpaque( true );
    checkBox.setHorizontalAlignment(JLabel.CENTER);
    cellEditors[BOOLEAN] = new DefaultCellEditor(checkBox);
    JTextField textField = new JTextField();
    cellEditors[STRING]  = new DefaultCellEditor(textField);
    flg = NUM_EDITOR;      // nobody
  }

  public Component getTableCellEditorComponent(JTable table, Object value,
              boolean isSelected, int row, int column) {
    if (value instanceof ComboString) {                       // ComboString
      flg = COMBO;
      String str = (value == null) ? "" : value.toString();
      return cellEditors[COMBO].getTableCellEditorComponent(
                       table, str,   isSelected, row, column);
    } else if (value instanceof Boolean) {                    // Boolean
      flg = BOOLEAN;
      return cellEditors[BOOLEAN].getTableCellEditorComponent(
                       table, value, isSelected, row, column);
    } else if (value instanceof String) {                     // String
      flg = STRING;
      return cellEditors[STRING].getTableCellEditorComponent(
                       table, value, isSelected, row, column);
    }
    return null;
  }

  public Object getCellEditorValue() {
    switch (flg) {
      case   COMBO:
        String str = (String)comboBox.getSelectedItem();
        return new ComboString(str);
      case BOOLEAN:
      case  STRING:
        return cellEditors[flg].getCellEditorValue();
      default:         return null;
    }
  }

  public Component getComponent() {
    return cellEditors[flg].getComponent();
  }
  public boolean stopCellEditing() {
    return cellEditors[flg].stopCellEditing();
  }
  public void cancelCellEditing() {
    cellEditors[flg].cancelCellEditing();
  }
  public boolean isCellEditable(EventObject anEvent) {
    //return cellEditors[flg].isCellEditable(anEvent);
	  return true;
  }
  public boolean shouldSelectCell(EventObject anEvent) {
    return cellEditors[flg].shouldSelectCell(anEvent);
  }
  public void addCellEditorListener(CellEditorListener l) {
    cellEditors[flg].addCellEditorListener(l);
  }
  public void removeCellEditorListener(CellEditorListener l) {
    cellEditors[flg].removeCellEditorListener(l);
  }
  public void setClickCountToStart(int n) {
    cellEditors[flg].setClickCountToStart(n);
  }
  public int getClickCountToStart() {
    return cellEditors[flg].getClickCountToStart();
  }
}

