package com.bce.datacenter.dcbrowser;

import com.bce.datacenter.db.common.DataObject;
import com.bce.datacenter.db.common.Level;

import java.util.List;

import javax.swing.table.DefaultTableModel;

/**
 * Table Model for displaying Level's contents
 * 
 * @author schlienger
 */
public class DCTableModel extends DefaultTableModel
{
  private final static String[] COLUMN_NAMES = { "Name", "Beschreibung" };

  private final static Class[] COLUMN_CLASSES = { String.class, String.class };

  private Level m_currentLevel = null;

  /**
   * Constructor
   * 
   * @param level
   *          one level of the hierarchy
   */
  public DCTableModel( Level level )
  {
    super( 1, 2 );

    m_currentLevel = level;
  }

  /**
   * @see javax.swing.table.TableModel#isCellEditable(int, int)
   */
  public boolean isCellEditable( int row, int column )
  {
    return false;
  }

  /**
   * @see javax.swing.table.TableModel#getColumnClass(int)
   */
  public Class getColumnClass( int column )
  {
    return COLUMN_CLASSES[column];
  }

  /**
   * @see javax.swing.table.TableModel#getColumnCount()
   */
  public int getColumnCount( )
  {
    return COLUMN_NAMES.length;
  }

  /**
   * @see javax.swing.table.TableModel#getColumnName(int)
   */
  public String getColumnName( int column )
  {
    return COLUMN_NAMES[column];
  }

  /**
   * updates table contents
   * 
   * @param level
   * 
   * @throws IllegalArgumentException
   *           if level is null
   */
  public void setCurrentLevel( Level level )
  {
    if( level == null )
      throw new IllegalArgumentException( "Level is null" );

    m_currentLevel = level;

    fireTableDataChanged();
  }

  public Level getCurrentLevel( )
  {
    return m_currentLevel;
  }

  /**
   * @see javax.swing.table.TableModel#getRowCount()
   */
  public int getRowCount( )
  {
    if( m_currentLevel == null )
      return 0;
    else

      return m_currentLevel.getObjects().size();
  }

  /**
   * @see javax.swing.table.TableModel#getValueAt(int, int)
   */
  public Object getValueAt( int row, int column )
  {
    List objects = m_currentLevel.getObjects();

    if( (objects.size() == 0) || (row >= objects.size()) )
      return null;

    DataObject d = (DataObject) objects.get( row );

    if( column == 0 )
      return d.getName();
    else

      return d.getDescription();
  }
}