package org.kalypso.services.calcjob.swing.test;

import java.awt.BorderLayout;

import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;
//import org.kalypso.services.factory.WebServiceFactory;

/**
 * @author Belger
 */
public class CalcJobGuiTest
{
	//private static final String SERVICE_NAME = "WebService";
  //	private static final String SERVICE_ARG = "http://localhost:8080/";
	
  public static JComponent createJobView()
  {
		//final WebServiceFactory jsf = new WebServiceFactory( "http://localhost:8080" );
	  	
		//final CalcJobService service = (CalcJobService)jsf.createService( CalcJobService.class );
		
    //return new JobQueueView( service );
    return null;
  }
  
  /** For testing purposes */
  public static void main( final String[] parms )
  {
    try
    {
      final JFrame queueFrame = new JFrame( "CalcJobService" );
      final JComponent jobView = createJobView();
      
      final JPanel queuePanel = new JPanel( new BorderLayout() );
      queueFrame.getRootPane().setContentPane( queuePanel );
      
      final JToolBar tb = new JToolBar();
      final Object[] actionKeys = jobView.getActionMap().allKeys();
      if( actionKeys != null )
      {
        for( int i = 0; i < actionKeys.length; i++ )
          tb.add( jobView.getActionMap().get( actionKeys[i] ) );
      }

      queuePanel.add( tb, BorderLayout.NORTH );
      queuePanel.add( new JScrollPane( jobView ), BorderLayout.CENTER );
      
      queueFrame.setBounds( 100, 100, 200, 200 );
      queueFrame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
      queueFrame.show();
    }
    catch( Exception ex )
    {
      ex.printStackTrace(  );
    }
  }
}
