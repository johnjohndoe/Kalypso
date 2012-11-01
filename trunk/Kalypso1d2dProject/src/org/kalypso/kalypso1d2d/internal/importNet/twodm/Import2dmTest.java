package org.kalypso.kalypso1d2d.internal.importNet.twodm;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FilenameUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.junit.Ignore;
import org.junit.Test;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.bce.gis.io.zweidm.ISmsModel;
import com.bce.gis.io.zweidm.SmsParser;

/**
 * @author Thomas Jung
 */
public class Import2dmTest
{
  @Test
  // Test is brokne, external trst data....
  @Ignore
  public void testLoadResults( ) throws Exception
  {
    /* Coordinate system of the 2DM file */
    final String sourceSRS = "EPSG:31467"; //$NON-NLS-1$
    final int sourceSrid = JTSAdapter.toSrid( sourceSRS );

    final List<File> inputFiles = new ArrayList<>();

    //    inputFiles.add( new File( "G:\\Projekte\\2DM\\dgmkorr.2dm" ) ); //$NON-NLS-1$
    inputFiles.add( new File( "G:\\Projekte\\2DM\\testhw99.2dm" ) ); //$NON-NLS-1$

    final File[] input = inputFiles.toArray( new File[inputFiles.size()] );

    final String[] outgml = new String[input.length];
    final String[] outshp = new String[input.length];

    for( int i = 0; i < input.length; i++ )
    {
      final File inputPath = input[i];
      final String baseName = FilenameUtils.removeExtension( inputPath.getAbsolutePath() );

      outshp[i] = baseName;
      outgml[i] = baseName + ".gml"; //$NON-NLS-1$
    }

    for( int j = 0; j < input.length; j++ )
    {
      final File inputFile = input[j];

      final SmsParser parser = new SmsParser( sourceSrid );

      final URL url =  inputFile.toURI().toURL();
      final IStatus parseStatus = parser.parse( url, new NullProgressMonitor() );

      System.out.println( parseStatus );

      final ISmsModel model = parser.getModel();
      final SmsConverter converter = new SmsConverter( model );

      final File outputGmlFile = new File( outgml[j] );
      converter.addTarget( new SmsDicretisationModelTarget( outputGmlFile ) );

      // final File outputShpFile = new File( outshp[j] );
      // converter.addTarget( new SHPModelImporter( outputShpFile ) );

      converter.execute();
    }
  }
}