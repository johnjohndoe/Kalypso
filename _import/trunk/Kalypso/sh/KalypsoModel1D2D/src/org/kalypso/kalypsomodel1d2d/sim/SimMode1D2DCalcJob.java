package org.kalypso.kalypsomodel1d2d.sim;

import java.io.File;
import java.net.URL;
import java.util.logging.Logger;

import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;

/**
 * Implements the {@link ISimulation} interface to provide
 * the simulation job for the 1d2d model
 * 
 * @author Patrice Congo
 */
public class SimMode1D2DCalcJob implements ISimulation
{
	/**
	 * Relative path to the job specification
	 */
	final static private String CALC_JOB_SPEC_1D2D="resources/nacalcjob_spec.xml";
	
	/**
	 * class logger
	 */
	final static private Logger logger= 
						Logger.getLogger(SimMode1D2DCalcJob.class.getName()); 
	
 
  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider, org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  public void run( 
		  	final File tmpdir, 
		  	final ISimulationDataProvider dataProvider, 
		  	final ISimulationResultEater resultEater, 
		  	final ISimulationMonitor monitor ) 
  			throws SimulationException
  {
    try
    {
    	logger.warning( "run not implemented" );
    }
    catch( Exception e )
    {
      throw new SimulationException( 
    		  	"could not instantiate NAOptimizingJob", e );
    }
  }

  

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  public URL getSpezifikation( )
  {
    return getClass().getResource( CALC_JOB_SPEC_1D2D);
  }

  //why not in the isimulation interface
/**
 * @return true if the job was succesfully executed otherwise false
 */
  public boolean isSucceeded( )
  {
      return false;
  }
}