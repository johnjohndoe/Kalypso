package org.kalypso.services.calcjob.swing;

import java.awt.event.ActionEvent;
import java.net.URL;
import java.rmi.RemoteException;

import javax.swing.AbstractAction;
import javax.swing.Box;
import javax.swing.JLabel;
import javax.swing.JOptionPane;

import org.kalypso.services.calcjob.CalcJobDescription;
import org.kalypso.services.calcjob.CalcJobService;

/**
 * @author Donald Duck
 */
public class ResultAction extends AbstractAction
{
  private final JobQueueView m_jqv;

  private final CalcJobService m_jqs;

  public ResultAction( final JobQueueView jqv, final CalcJobService jqs )
  {
    super( "Retrieve Results" );

    m_jqv = jqv;
    m_jqs = jqs;
  }

  public void actionPerformed( final ActionEvent e )
  {
    final String jobID = m_jqv.getSeletectedJob();
    if( jobID == null )
    {
      JOptionPane.showMessageDialog( m_jqv, "No entry selected", "Kill job",
          JOptionPane.INFORMATION_MESSAGE );
      return;
    }

    if( JOptionPane.showConfirmDialog( m_jqv, "Retrieve Results", "Retrieve CalcJobDescription",
        JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE ) == JOptionPane.YES_OPTION )
    {
      try
      {
        final int seletecIndex = m_jqv.getSelectedRow();
        m_jqv.getSelectionModel().removeIndexInterval( seletecIndex, seletecIndex );
        final CalcJobDescription job = m_jqs.getJobDescription( jobID );
        final URL[] results = m_jqs.retrieveResults( jobID );

        final Box box = Box.createVerticalBox();
        if( results == null )
          box.add( new JLabel( "no results available" ) );
        else
          for( int i = 0; i < results.length; i++ )
            box.add( new JLabel( results[i].toExternalForm() ) );

        JOptionPane.showMessageDialog( m_jqv, box, "Results of CalcJobDescription: "
            + job.getDescription(), JOptionPane.PLAIN_MESSAGE );
      }
      catch( final RemoteException e1 )
      {
        e1.printStackTrace();
      }
    }
  }
}