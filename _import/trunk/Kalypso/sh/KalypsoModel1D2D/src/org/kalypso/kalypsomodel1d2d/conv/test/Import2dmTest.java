package org.kalypso.kalypsomodel1d2d.conv.test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.kalypsomodel1d2d.conv.IFEModelElementHandler;
import org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv;
import org.kalypso.kalypsomodel1d2d.conv.SHPModelImporter;
import org.kalypso.kalypsomodel1d2d.conv.SMS2GmlConv;
import org.kalypso.kalypsomodel1d2d.conv.Sms2dmFEModelElementHandler;
import org.kalypsodeegree.KalypsoDeegreePlugin;

/**
 * @author Thomas Jung
 */
public class Import2dmTest extends TestCase
{

  public void testLoadResults( ) throws Exception
  {
    final String crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    final List<String> inputFiles = new ArrayList<String>();

    inputFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\erlauf_teil1.2dm" );
    inputFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\erlauf_teil2.2dm" );
    inputFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\fluss_Kraubath.2dm" );
    inputFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\fluss_Leising.2dm" );
    inputFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\fluss_seizer.2dm" );
    inputFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\fluss_vbb_teil1.2dm" );
    inputFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\fluss_vbb_teil2.2dm" );
    inputFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\fluss_vbb_teil3-mit_bw.2dm" );
    inputFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\fluss_vbb_teil4.2dm" );
    inputFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\Kraubath.2dm" );
    inputFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\Kraubath_Mur.2dm" );
    inputFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\Leising.2dm" );
    inputFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\Leising_Mur.2dm" );
    inputFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\mitterbach_2406.2dm" );
    inputFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\seizer-ALT.2dm" );
    inputFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\seizer_2406.2dm" );
    inputFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\vbb_teil1.2dm" );
    inputFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\vbb_teil2.2dm" );
    inputFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\vbb_teil3-mit_bw.2dm" );
    inputFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\vbb_teil4.2dm" );
    final String[] input = inputFiles.toArray( new String[inputFiles.size()] );

    final List<String> outputGmlFiles = new ArrayList<String>();

    outputGmlFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\erlauf_teil1.gml" );
    outputGmlFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\erlauf_teil2.gml" );
    outputGmlFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\fluss_Kraubath.gml" );
    outputGmlFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\fluss_Leising.gml" );
    outputGmlFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\fluss_seizer.gml" );
    outputGmlFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\fluss_vbb_teil1.gml" );
    outputGmlFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\fluss_vbb_teil2.gml" );
    outputGmlFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\fluss_vbb_teil3-mit_bw.gml" );
    outputGmlFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\fluss_vbb_teil4.gml" );
    outputGmlFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\Kraubath.gml" );
    outputGmlFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\Kraubath_Mur.gml" );
    outputGmlFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\Leising.gml" );
    outputGmlFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\Leising_Mur.gml" );
    outputGmlFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\mitterbach_2406.gml" );
    outputGmlFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\seizer-ALT.gml" );
    outputGmlFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\seizer_2406.gml" );
    outputGmlFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\vbb_teil1.gml" );
    outputGmlFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\vbb_teil2.gml" );
    outputGmlFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\vbb_teil3-mit_bw.gml" );
    outputGmlFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\vbb_teil4.gml" );

    final String[] outgml = outputGmlFiles.toArray( new String[outputGmlFiles.size()] );
    final List<String> outputShpFiles = new ArrayList<String>();

    outputShpFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\erlauf_teil1" );
    outputShpFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\erlauf_teil2" );
    outputShpFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\fluss_Kraubath" );
    outputShpFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\fluss_Leising" );
    outputShpFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\fluss_seizer" );
    outputShpFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\fluss_vbb_teil1" );
    outputShpFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\fluss_vbb_teil2" );
    outputShpFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\fluss_vbb_teil3-mit_bw" );
    outputShpFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\fluss_vbb_teil4" );
    outputShpFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\Kraubath" );
    outputShpFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\Kraubath_Mur" );
    outputShpFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\Leising" );
    outputShpFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\Leising_Mur" );
    outputShpFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\mitterbach_2406" );
    outputShpFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\seizer-ALT" );
    outputShpFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\seizer_2406" );
    outputShpFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\vbb_teil1" );
    outputShpFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\vbb_teil2" );
    outputShpFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\vbb_teil3-mit_bw" );
    outputShpFiles.add( "P:\\hwa0824123\\modell\\hya\\netze_2206\\vbb_teil4" );

    final String[] outshp = outputShpFiles.toArray( new String[outputShpFiles.size()] );
    for( int j = 0; j < input.length; j++ )
    {

      RMA10S2GmlConv.VERBOSE_MODE = false;
      final String inputFile = input[j];
      final File outputGmlFile = new File( outgml[j] );
      final File outputShpFile = new File( outshp[j] );

      final NullProgressMonitor monitor = new NullProgressMonitor();
      final SMS2GmlConv converter = new SMS2GmlConv( monitor, getNumberOfLines( inputFile ) );
      final IFEModelElementHandler handler2dm = new Sms2dmFEModelElementHandler();

// handler2dm.addImporter( new DiscModelImporter( outputGmlFile ) );

      handler2dm.addImporter( new SHPModelImporter( outputShpFile ) );

      converter.setModelElementHandler( handler2dm );

      converter.parse( new URL( "file:" + inputFile ).openStream() );
    }
  }

  public static int getNumberOfLines( final String fileName )
  {
    final File file = new File( fileName );
    if( file == null || !file.exists() )
    {
      return -1;
    }
    int linesCnt = 0;
    try
    {
      final FileReader file_reader = new FileReader( file );
      final BufferedReader buf_reader = new BufferedReader( file_reader );
      do
      {
        final String line = buf_reader.readLine();
        if( line == null )
          break;
        linesCnt++;
      }
      while( true );
      buf_reader.close();
    }
    catch( final IOException e )
    {
      return -1;
    }
    return linesCnt;
  }

}