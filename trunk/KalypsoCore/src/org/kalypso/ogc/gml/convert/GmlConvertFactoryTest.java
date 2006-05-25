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
import org.kalypso.contribs.java.net.IUrlCatalog;
import org.kalypso.contribs.java.net.MultiUrlCatalog;
import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypsodeegree.model.TypeHandlerUtilities;
import org.kalypsodeegree_impl.gml.schema.schemata.DeegreeUrlCatalog;
import org.kalypsodeegree_impl.gml.schema.schemata.UrlCatalogOGC;

/**
 * @author belger
 */
public class GmlConvertFactoryTest extends TestCase
{
  private final UrlUtilities m_urlUtilities = new UrlUtilities();

  /**
   * @see junit.framework.TestCase#setUp()
   */
  @Override
  protected void setUp( ) throws Exception
  {
    super.setUp();

    final ITypeRegistry<IMarshallingTypeHandler> registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    TypeHandlerUtilities.registerXSDSimpleTypeHandler( registry );
    TypeHandlerUtilities.registerTypeHandlers( registry );

    final MultiUrlCatalog catalog = new MultiUrlCatalog( new IUrlCatalog[] { new UrlCatalogOGC(), new DeegreeUrlCatalog() } );
    final File cacheDirectory = FileUtilities.createNewTempDir( "kalypsoschemacache" );
    if( !cacheDirectory.exists() )
      cacheDirectory.mkdirs();
    cacheDirectory.deleteOnExit();
    GMLSchemaCatalog.init( catalog, cacheDirectory );

  }

  public void testConvertXml( ) throws IOException, JAXBException, GmlConvertException
  {
    final File tmpdir = FileUtilities.createNewTempDir( getClass().getName() );

    FileInputStream istFis = null;
    FileInputStream sollFis = null;

    try
    {
      ZipUtilities.unzip( getClass().getResourceAsStream( "resources/testcasedata.zip" ), tmpdir );

      doGmc( "1_egmpar_hwp_import.gmc", tmpdir );
      checkFile( "saalemodell1.gml", tmpdir );

      doGmc( "2_wlmpar_hwp_import.gmc", tmpdir );
      checkFile( "saalemodell2.gml", tmpdir );

      doGmc( "3_pegel_std_import.gmc", tmpdir );
      checkFile( "saalemodell3.gml", tmpdir );

      doGmc( "EXPORT_egmpar_hwp.gmc", tmpdir );
      final File resultfile = checkFile( "export_egmpar.csv", tmpdir );

      // diff files
      final File exportSoll = new File( tmpdir, "export_egmpar.csv.soll" );
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
    assertTrue( "Return status must be ok", GmlConvertFactory.convertXml( url, m_urlUtilities, new HashMap() ).isOK() );
  }

}
