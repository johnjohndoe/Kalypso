package org.kalypso.services.calcjob.swing;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.rmi.RemoteException;

import javax.swing.AbstractAction;
import javax.swing.JComboBox;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.kalypso.services.calcjob.CalcJobService;

/**
 * @author Belger
 */
public class CreateJobAction extends AbstractAction
{
  private final CalcJobService m_service;

  public CreateJobAction( final CalcJobService service )
  {
    super( "CreateJob" );

    m_service = service;
  }

  public void actionPerformed( final ActionEvent e )
  {
    try
    {
      final String[] types = m_service.getJobTypes();
      
      final JComboBox cb = new JComboBox( types );
      if( types.length != 0 )
        cb.setSelectedIndex( 0 );
        
      final JTextField tf = new JTextField( "<unknown>" );
      
      final JPanel panel = new JPanel( new BorderLayout() );
      panel.add( cb, BorderLayout.NORTH );
      panel.add( tf, BorderLayout.CENTER );
      
      tf.grabFocus();
        
      if( JOptionPane.showConfirmDialog( null, panel, "Choose type", JOptionPane.OK_CANCEL_OPTION, JOptionPane.PLAIN_MESSAGE ) == JOptionPane.OK_OPTION )
        m_service.createJob( cb.getSelectedItem().toString(), tf.getText(), null );
    }
    catch( RemoteException e1 )
    {
      e1.printStackTrace();
    }
  }
}
