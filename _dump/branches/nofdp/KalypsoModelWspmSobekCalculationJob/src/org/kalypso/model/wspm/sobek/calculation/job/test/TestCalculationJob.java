package org.kalypso.model.wspm.sobek.calculation.job.test;

import java.io.File;
import java.io.IOException;

import junit.framework.JUnit4TestAdapter;
import junit.framework.TestCase;

import org.apache.commons.io.FileUtils;
import org.junit.Test;
import org.kalypso.model.wspm.sobek.calculation.job.WspmSobekCalcJob;
import org.kalypso.simulation.core.SimulationException;

public class TestCalculationJob extends TestCase
{

  public static junit.framework.Test suite( )
  {
    return new JUnit4TestAdapter( TestCalculationJob.class );
  }

  @Test
  public void test( ) throws IOException, SimulationException
  {
    final File tmpDir = new File( System.getProperty( "java.io.tmpdir" ) );
    File sobekTmpDir = new File( tmpDir, "testSobekCalculationJob" );

    if( sobekTmpDir.exists() )
      FileUtils.deleteDirectory( sobekTmpDir );

    FileUtils.forceMkdir( sobekTmpDir );

    WspmSobekCalcJob job = new WspmSobekCalcJob();
    job.run( sobekTmpDir, new TestCaseSimulationDataProvider(), new TestCaseResultEater(), new TestCaseSimulationMonitor() );
  }
}
