package org.kalypso.services.calcjob.swing;

import java.awt.event.ActionEvent;
import java.rmi.RemoteException;

import javax.swing.AbstractAction;
import javax.swing.JOptionPane;

import org.kalypso.services.calcjob.CalcJobService;

/**
 * @author Belger
 */
public class KillAction extends AbstractAction
{
  private final CalcJobService m_service;

  private final JobQueueView m_jqp;

  public KillAction( final JobQueueView jqp, final CalcJobService service )
  {
    super( "Kill Jobs" );

    m_jqp = jqp;
    m_service = service;
  }

  public void actionPerformed( ActionEvent e )
  {
    final String[] jobIDs = m_jqp.getSeletectedJobs();
    if( jobIDs.length == 0 )
    {
      JOptionPane.showMessageDialog( m_jqp, "No entry selected", "Kill job",
          JOptionPane.INFORMATION_MESSAGE );
      return;
    }

    if( JOptionPane.showConfirmDialog( m_jqp, "Kill selected Jobs?", "Kill CalcJobDescription",
        JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE ) == JOptionPane.YES_OPTION )
    {
      try
      {
        for( int i = 0; i < jobIDs.length; i++ )
        {
          m_service.cancelJob( jobIDs[i] );
          m_service.removeJob( jobIDs[i] );
        }

        m_jqp.clearSelection();
      }
      catch( final RemoteException e1 )
      {
        e1.printStackTrace();
      }
    }
  }
}