package org.kalypso.services.calcjob.swing;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ActionMap;
import javax.swing.JTable;
import javax.swing.Timer;
import javax.swing.table.TableCellRenderer;

import org.kalypso.services.calcjob.CalcJobService;

/**
 * @author Belger
 */
public class JobQueueView extends JTable
{
  private final Timer m_timer;
  
  private final TableCellRenderer m_stateRenderer = new JobStatusCellRenderer();
  private final TableCellRenderer m_progressRenderer = new JobProgressCellRenderer();

  public JobQueueView( final CalcJobService service )
  {
    super( new JobQueueTableModel( service ) );
    
    // verboten, da es sonst zu exceptions kommt (beim refresh und gleichzeitigem drag)
    getTableHeader().setReorderingAllowed( false );
    
    final ActionMap am = new ActionMap();
    setActionMap( am );
    
    am.put( "createJob", new CreateJobAction( service ) );
    am.put( "killjob", new KillAction( this, service ) );
    am.put( "getresults", new ResultAction( this, service ) );

    m_timer = new Timer( 1000, new ActionListener() {
      public void actionPerformed( final ActionEvent e )
      {
        refresh();
      }} );
    m_timer.start();
    refresh();
  }
  
  public void refresh()
  {
    final int[] selection = getSelectedRows();
    
    ((JobQueueTableModel)getModel()).refresh();
    
    clearSelection();
    for( int i = 0; i < selection.length; i++ )
      getSelectionModel().addSelectionInterval( selection[i], selection[i] );
  }

  public TableCellRenderer getCellRenderer( final int row, final int column )
  {
    switch( column )
    {
      case 3:
      return m_progressRenderer;
      
      default:
      return m_stateRenderer;  
    }
  }
  
  public String getSeletectedJob()
  {
    final int selectedValue = getSelectedRow();
    if( selectedValue == -1 )
      return null;

    return getValueAt( selectedValue, 0 ).toString();
  }
  
  public String[] getSeletectedJobs()
  {
    final int[] selectedValues = getSelectedRows();
    if( selectedValues == null )
      return new String[0];
      
    final String[] result = new String[selectedValues.length];
    for( int i = 0; i < selectedValues.length; i++ )
      result[i] =  getValueAt( selectedValues[i], 0 ).toString();

    return result;
  }
}