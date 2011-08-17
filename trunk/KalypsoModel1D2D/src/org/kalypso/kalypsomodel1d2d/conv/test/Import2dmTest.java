package org.kalypso.kalypsomodel1d2d.conv.test;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FilenameUtils;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.junit.Test;
import org.kalypso.kalypsomodel1d2d.conv.DiscModelImporter;
import org.kalypso.kalypsomodel1d2d.conv.IFEModelElementHandler;
import org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv;
import org.kalypso.kalypsomodel1d2d.conv.SMS2GmlConv;
import org.kalypso.kalypsomodel1d2d.conv.Sms2dmFEModelElementHandler;

/**
 * @author Thomas Jung
 */
public class Import2dmTest
{
  @Test
  public void testLoadResults( ) throws Exception
  {
    /* Coordinate system of the 2DM file */
    final String sourceSRS = "EPSG:31467"; //$NON-NLS-1$

    final List<File> inputFiles = new ArrayList<File>();

    inputFiles.add( new File( "P:\\hwr0920023\\work\\ruggiero\\Modell\\Hydro_AS\\Vorland_netz_2.2dm" ) ); //$NON-NLS-1$

    final File[] input = inputFiles.toArray( new File[inputFiles.size()] );

    final String[] outgml = new String[input.length];
    final String[] outshp = new String[input.length];

    for( int i = 0; i < input.length; i++ )
    {
      File inputPath = input[i];
      String baseName = FilenameUtils.removeExtension( inputPath.getAbsolutePath() );

      outshp[i] = baseName;
      outgml[i] = baseName + ".gml"; //$NON-NLS-1$
    }

    for( int j = 0; j < input.length; j++ )
    {
      RMA10S2GmlConv.VERBOSE_MODE = false;
      final File inputFile = input[j];

      final NullProgressMonitor monitor = new NullProgressMonitor();

      final SMS2GmlConv converter = new SMS2GmlConv();
      final IFEModelElementHandler handler2dm = new Sms2dmFEModelElementHandler(sourceSRS);

      final File outputGmlFile = new File( outgml[j] );
      handler2dm.addImporter( new DiscModelImporter( outputGmlFile ) );

      //final File outputShpFile = new File( outshp[j] );
      // handler2dm.addImporter( new SHPModelImporter( outputShpFile ) );

      URL url =  inputFile.toURI().toURL();
      converter.parse( url, monitor );
    }
  }
}