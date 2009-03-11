package org.kalypso.ogc.gml.convert;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URL;
import java.util.HashMap;

import javax.xml.bind.JAXBException;

import junit.framework.TestCase;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.core.i18n.Messages;

/**
 * @author belger
 */
public class GmlConvertFactoryTest extends TestCase
{
  private final UrlUtilities m_urlUtilities = new UrlUtilities();

  public void testConvertXml( ) throws IOException, JAXBException, GmlConvertException
  {
    final File tmpdir = FileUtilities.createNewTempDir( getClass().getName() );

    FileInputStream istFis = null;
    FileInputStream sollFis = null;

    try
    {
      ZipUtilities.unzip( getClass().getResourceAsStream( "resources/testcasedata.zip" ), tmpdir ); //$NON-NLS-1$

      doGmc( "1_egmpar_hwp_import.gmc", tmpdir ); //$NON-NLS-1$
      checkFile( "saalemodell1.gml", tmpdir ); //$NON-NLS-1$

      doGmc( "2_wlmpar_hwp_import.gmc", tmpdir ); //$NON-NLS-1$
      checkFile( "saalemodell2.gml", tmpdir ); //$NON-NLS-1$

      doGmc( "3_pegel_std_import.gmc", tmpdir ); //$NON-NLS-1$
      checkFile( "saalemodell3.gml", tmpdir ); //$NON-NLS-1$

      doGmc( "EXPORT_egmpar_hwp.gmc", tmpdir ); //$NON-NLS-1$
      final File resultfile = checkFile( "export_egmpar.csv", tmpdir ); //$NON-NLS-1$

      // diff files
      final File exportSoll = new File( tmpdir, "export_egmpar.csv.soll" ); //$NON-NLS-1$
      istFis = new FileInputStream( resultfile );
      sollFis = new FileInputStream( exportSoll );
      IOUtils.contentEquals( istFis, sollFis );
    }
    finally
    {
      IOUtils.closeQuietly( sollFis );
      IOUtils.closeQuietly( istFis );
      FileUtilities.deleteRecursive( tmpdir );
    }
  }

  private File checkFile( final String filename, final File dir )
  {
    final File file = new File( dir, filename );
    assertTrue( file.exists() );
    return file;
  }

  private void doGmc( final String filename, final File dir ) throws IOException, JAXBException, GmlConvertException
  {
    final File file = new File( dir, filename );
    final URL url = file.toURL();
    assertTrue( Messages.getString("org.kalypso.ogc.gml.convert.GmlConvertFactoryTest.10"), GmlConvertFactory.convertXml( url, m_urlUtilities, new HashMap() ).isOK() ); //$NON-NLS-1$
  }

}
