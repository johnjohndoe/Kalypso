package org.kalypso.services.calcjob.swing;

import java.rmi.RemoteException;

import javax.swing.table.AbstractTableModel;

import org.kalypso.services.calcjob.CalcJobDescription;
import org.kalypso.services.calcjob.CalcJobService;
import org.kalypso.services.calcjob.CalcJobStatus;

class JobQueueTableModel extends AbstractTableModel
{ 
  private final CalcJobService m_service;
  
  private CalcJobDescription[] m_jobs = new CalcJobDescription[0];
  
  
  public JobQueueTableModel( final CalcJobService service )
  {
    m_service = service;
  }
  
  public void refresh()
  {
    try
    {
      final String[] jobIDs = m_service.getJobs();
      m_jobs = new CalcJobDescription[jobIDs.length];
      for( int i = 0; i < jobIDs.length; i++ )
        m_jobs[i] = m_service.getJobDescription( jobIDs[i] );
    }
    catch( final RemoteException e )
    {
      e.printStackTrace();
    }

    fireTableDataChanged();
  }

  public int getRowCount()
  {
    return m_jobs.length;
  }

  public int getColumnCount()
  {
    return 4;
  }

  public Object getValueAt( final int rowIndex, final int columnIndex )
  {
    final CalcJobDescription job = m_jobs[rowIndex];
    
    switch( columnIndex )
    {
      case 0:
      return new Integer( job.getId() );
      
      case 1:
      return job.getDescription();
      
      case 2:
      return job.getType();

      case 3:
      return new Integer( job.getProgress() );

      default:
      throw new IndexOutOfBoundsException( "ColumnIndex out of bounds: " + columnIndex );
    }
  }



  public Class getColumnClass( final int columnIndex )
  {
    switch( columnIndex )
    {
      case 0:
      return Integer.class;
      
      case 1:
      return String.class;
      
      case 2:
      return String.class;

      case 3:
      return Integer.class;

      default:
      throw new IndexOutOfBoundsException( "ColumnIndex out of bounds: " + columnIndex );
    }
  }

  public String getColumnName( final int columnIndex )
  {
    switch( columnIndex )
    {
      case 0:
      return "ID";
      
      case 1:
      return "Description";
      
      case 2:
      return "Type";

      case 3:
      return "Progress";
      
      default:
      throw new IndexOutOfBoundsException( "ColumnIndex out of bounds: " + columnIndex );
    }
  }

  public CalcJobStatus getState( final int row )
  {
    return CalcJobStatus.getJobState( m_jobs[row].getState() );
  }

}