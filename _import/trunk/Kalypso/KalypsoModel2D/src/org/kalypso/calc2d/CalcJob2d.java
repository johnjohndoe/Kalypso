/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of ekalypso:
 Internet based elearning for complex simulation applications
 ("Internet basiertes E-Learning an komplexen Simulationsprogrammen [de]")

 The implementation is realised by: 
 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 The project is sponsored and supported by:  
 local authority of education and research, 
 E-Learning Consortium Hamburg (ELCH) and
 Multimedia Kontor Hamburg.
 
 As this implementation depends on third party open source 
 java code it is consequently also licenced as open source
 in the hope that it will be useful specially (but not exclusively)
 to other e-learning projects that may extend or use this project.
 
 Copyright (C) 2004, 2005 by:
 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 katharina.lupp@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
/**
 * Created on 07.03.2005
 */

package org.kalypso.calc2d;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.CopyUtils;
import org.apache.commons.io.IOUtils;
import org.kalypso.convert.model2d.ConvertBC2Ascii;
import org.kalypso.convert.model2d.ConvertGML2Asci;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.services.calculation.job.impl.AbstractCalcJob;
import org.kalypso.services.calculation.service.CalcJobDataBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 *  
 */
public class CalcJob2d extends AbstractCalcJob
{

  private final String EXE_FILE         = "/start/###StartSimulation###.bat";
//  private final String TEMPLATE_CONF_FILE = "config/SimConf.txt";
  private boolean succeeded             = false;
  private final static String[] subDirs = {"model", "boundaryConditions"};
  private final String resourceBase = "calc2d/";
  private final String TEMPLATE_CONF_FILE = "misc/resourceFile.conf";

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.service.CalcJobDataBean[])
   */
  public void run( File basedir, CalcJobDataBean[] input ) throws CalcJobServiceException
  {
//    final File calcDir = new File(basedir,Constants2D.CALC_DIR_NAME);
    
    final File inputDataDir = new File( basedir, Constants2D.INPUT_DIR_NAME );
    System.out.println("inputDataDir: " + inputDataDir);
    inputDataDir.mkdir();
    
//    System.out.println("calcDir: " + calcDir);
//    final File outDir = new File( basedir, Constants2D.OUTPUT_DIR_NAME );
//    System.out.println("outDir: " + outDir);
//    outDir.mkdirs();
    

    if( !basedir.exists() ) basedir.mkdirs();

    final File exeDir = new File( basedir, "simulation" );
    exeDir.mkdir();
    System.out.println("exeDir: " + exeDir);
    
    try
    {
      setMessage( "creating file system for simulation..." );
      if( isCanceled() ) return;
      prepareBaseDir( exeDir );

      if( isCanceled() ) return;

      setMessage( "generating ascii files for 2D simulation..." );
      if( isCanceled() ) return;

      copyTemplates(basedir);
      
      ConvertGML2Asci gml2asci = new ConvertGML2Asci();
      gml2asci.convertGML2Asci( inputDataDir + "/myResultMesh.gml", inputDataDir+"/2dgml.xsd" );
      setMessage("generating mesh ascii file");

      ConvertBC2Ascii bc2asci = new ConvertBC2Ascii();
      bc2asci.convertBC2Ascii(inputDataDir+"/bc.gml", inputDataDir+"/bc_gml2.xsd");
      setMessage("generating boundary conditions ascii file");

      setMessage( "starting 2D simulation..." );

      if( isCanceled() ) return;
      startCalculation( exeDir );
      checkSucceeded( exeDir );

      if( isSucceeded() )
      {
        setMessage( "loading results..." );
        //TODO load results
//        loadResults( exeDir, modellWorkspace, logBuffer, outDir );
          System.out.println( "Finished 2D Simulation successfully." );
      }else System.out.println( "Finished 2D Simulation not successfully." );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "simulation couldn't be finished", e );
    }
  }

  /**
   * 
   * @see org.kalypso.calc2d 
   */
  private void copyTemplates( File basedir ) throws IOException
  {
    String[] templateResources = getTemplateResources();
    for( int i = 0; i < templateResources.length; i++ )
    {
      final File destFile = new File( basedir, templateResources[i] );
      final String resource = resourceBase + templateResources[i];
      System.out.print( "resource: " + resource );
      if( !destFile.exists() )
      {
        try
        {
          final InputStream inputStream = getClass().getResourceAsStream( resource );
          FileUtilities.makeFileFromStream( false, destFile, inputStream );
          System.out.println( " ...copied" );
        }
        catch( Exception e )
        {
          e.printStackTrace();

          System.out.println( "ERR: " + resource + " max not exist" );
        }
      }
      else
        System.out.println( " exists" );
    }
  }

  /**
   * 
   * @see org.kalypso.calc2d 
   * @return String[]
   */
  private String[] getTemplateResources() throws IOException
  {
    List result = new ArrayList();
    LineNumberReader reader = new LineNumberReader( new InputStreamReader( getClass()
        .getResourceAsStream( TEMPLATE_CONF_FILE ) ) );
    String line = null;
    try
    {
      while( ( line = reader.readLine() ) != null )
        if( !line.startsWith( "#" ) )
          result.add( line );
    }
    catch( IOException e )
    {
      throw e;
    }
    finally
    {
      reader.close();
    }
    return (String[])result.toArray( new String[result.size()] );
  }
  
  /**
   * prepares directory for results of simulation
   */
  private void prepareBaseDir( File baseDir )
  {
    for( int i = 0; i < subDirs.length; i++ )
      ( new File( baseDir, subDirs[i] ) ).mkdirs();
  }

  /**
   * @return boolean succeeded
   */
  public boolean isSucceeded()
  {
    return succeeded;
  }
  
  /**
   * starts 2D simulation
   */
  private void startCalculation( final File basedir ) throws CalcJobServiceException
  {
    InputStreamReader inStream = null;
    InputStreamReader errStream = null;
    PrintWriter outWriter = null;
    PrintWriter errWriter = null;

    try
    {
      final File exeFile = new File( basedir, EXE_FILE );
      final File exeDir = exeFile.getParentFile();
      final String commandString = exeFile.getAbsolutePath();
      System.out.println("commandString: " +commandString);

      final Process process = Runtime.getRuntime().exec( commandString, null, exeDir );

      outWriter = new PrintWriter( new FileWriter( new File( basedir, "exe.log" ) ) );
      errWriter = new PrintWriter( new FileWriter( new File( basedir, "exe.err" ) ) );

      inStream = new InputStreamReader( process.getInputStream() );
      errStream = new InputStreamReader( process.getErrorStream() );
      while( true )
      {
        CopyUtils.copy( inStream, outWriter );
        CopyUtils.copy( errStream, errWriter );

        try
        {
          process.exitValue();
          return;
        }
        catch( IllegalThreadStateException e )
        {
          e.printStackTrace();
        }

        if( isCanceled() )
        {
          process.destroy();
          return;
        }
        Thread.sleep( 100 );
      }
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "error occurred...", e );
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "error occurred...", e );
    }
    finally
    {
      try
      {
        if( outWriter != null )
          outWriter.close();

        if( errWriter != null )
          errWriter.close();

        if( inStream != null )
          inStream.close();

        if( errStream != null )
          errStream.close();
      }
      catch( final IOException e1 )
      {
        e1.printStackTrace();
      }
    }
  }

  /**
   * checks if calculation of simulations is succeeded
   */
  public void checkSucceeded( final File inputDir )
    {
      Reader logFileReader = null;
      LineNumberReader reader = null;
      try
      {
        final File logDir = new File( inputDir, "start2" );
        final File logFile = new File( logDir, "out.2d" );
        logFileReader = new FileReader( logFile );
        reader = new LineNumberReader( logFileReader );
        String line;
        while( ( line = reader.readLine() ) != null )
        {
          if( line.indexOf( "calculation finished without errors" ) >= 0 )
            succeeded = true;
        }
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
      finally
      {
        IOUtils.closeQuietly( reader );
        IOUtils.closeQuietly( logFileReader );
      }
    }

  private void loadResults(final File outputDir){
      
  }
  
    
  //  public boolean isSucceeded()
  //  {
  //    return succeeded;
  //  }
    

}