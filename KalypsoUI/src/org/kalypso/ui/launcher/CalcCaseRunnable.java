package org.kalypso.ui.launcher;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.service.calculation.ICalculationService;
import org.kalypso.service.calculation.IJobStatus;
import org.omg.CORBA.portable.InvokeHandler;

/**
 * @author belger
 */
public class CalcCaseRunnable implements IRunnableWithProgress
{
  private ILaunchConfiguration m_configuration;

  public CalcCaseRunnable( final ILaunchConfiguration configuration )
  {
    m_configuration = configuration;
  }

  /**
   * @throws InvocationTargetException
   * @see org.eclipse.jface.operation.IRunnableWithProgress#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  public void run( final IProgressMonitor monitor ) throws InvocationTargetException
  {
    try
    {
      monitor.beginTask( "Berechnung durchführen", 200 );
      
      final IProgressMonitor subMonitor = new SubProgressMonitor( monitor, 100 );
      subMonitor.beginTask( "Berechnung starten", 100 );
      
      // Configuration auslesen

      final Object adapter = m_configuration.getAdapter( IProject.class );
      System.out.println( "Projekt - Adapter: " + adapter );

      // die ProjektNature bekommen

      // von der Nature den Berechnungstyp beziehen

      final String calcType = m_configuration.getAttribute( IKalypsoLaunchConfigurationConstants.CALC_TYPE, "" );
      final String serviceName = m_configuration.getAttribute( IKalypsoLaunchConfigurationConstants.SERVICE, "" );


      final ICalculationService calcService = KalypsoGisPlugin.getDefault().getCalcService( serviceName );
      final String jobID = calcService.startCalcJob( calcType, null );
      
      subMonitor.worked( 100 );
      
      // warten, bis der service fertig ist
      
      final SubProgressMonitor subMonitor2 = new SubProgressMonitor( monitor, 100 );
      subMonitor2.beginTask( "Berechnung wird durchgeführt", 100);
      while( true )
      {
        final IJobStatus jobStatus = calcService.getJobStatus( jobID );
        
        if( jobStatus.isFinished() )
          break;
        
        subMonitor2.worked( (int)( jobStatus.progress() * 100 ) );
        
        try
        {
          Thread.sleep( 100 );
        }
        catch( final InterruptedException e1 )
        {
          e1.printStackTrace();
        }
        
        if( subMonitor2.isCanceled() )
          break;
      }
      
      
      // alles ok?
      
      // ergebnisse abrufen und ablegen
//
//      // monitor auf berechnung starten?
//
//      final int count = 100000;
//
//      monitor.beginTask( "Modellrechnung", count );
//
//      for( int i = 0; i < count; i++ )
//      {
//        final double blubb = i * i;
//        new Double( blubb );
//
//        monitor.worked( i );
//      }

    }
    catch( CoreException e )
    {
      throw new InvocationTargetException( e );
    }

  }
}