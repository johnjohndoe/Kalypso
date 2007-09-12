/**
 *
 */
package org.kalypso.google.earth.export.wizard;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Marshaller;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.junit.Assert;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.google.earth.export.GoogleEarthThemeVisitor;
import org.kalypso.google.earth.export.constants.IGoogleEarthExportSettings;
import org.kalypso.google.earth.export.utils.GoogleEarthExportUtils;
import org.kalypso.google.earth.export.utils.GoogleEarthUtils;
import org.kalypso.google.earth.export.utils.StyleTypeFactory;
import org.kalypso.google.earth.export.utils.ThemeGoogleEarthExportable;
import org.kalypso.google.earth.export.utils.ZipUtils;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor;
import org.kalypso.ui.views.map.MapView;

import com.google.earth.kml._2.DocumentType;
import com.google.earth.kml._2.FolderType;
import com.google.earth.kml._2.ObjectFactory;

/**
 * @author kuch
 */
public class GoogleEarthExporter implements ICoreRunnableWithProgress
{

  private final IGoogleEarthExportSettings m_settings;

  private final MapView m_view;

  /**
   * @param view
   * @param m_page
   */
  public GoogleEarthExporter( final MapView view, final IGoogleEarthExportSettings settings )
  {
    m_view = view;
    m_settings = settings;
  }

  /**
   * @return
   * @throws IOException
   */
  private File createTmpDir( ) throws IOException
  {
    final URL urlTmpDir = new File( System.getProperty( "java.io.tmpdir" ) ).toURL();
    Assert.assertNotNull( urlTmpDir );

    /* delete old test dir */
    final URL urlBaseDir = new URL( urlTmpDir + "kalypsoGoogleEarthExport/" );

    final File fBaseDir = new File( urlBaseDir.getFile() );
    if( !fBaseDir.exists() )
      FileUtils.forceMkdir( fBaseDir );

    FileUtils.cleanDirectory( fBaseDir );

    return fBaseDir;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException, InterruptedException
  {

    try
    {
      /* basic data stuff for processing */
      final File tmpDir = createTmpDir();
      final MapPanel mapPanel = m_view.getMapPanel();
      final IMapModell mapModell = mapPanel.getMapModell();

      /* kml document root */
      final ObjectFactory factory = new ObjectFactory();
      final DocumentType documentType = factory.createDocumentType();

      /* basic kml settings */
      GoogleEarthUtils.setMapBoundary( mapPanel.getBoundingBox(), mapModell.getCoordinatesSystem(), factory, documentType );
      GoogleEarthUtils.setLookAt( mapPanel.getBoundingBox(), mapModell.getCoordinatesSystem(), factory, documentType );

      documentType.setName( m_settings.getExportName() );
      documentType.setDescription( m_settings.getExportDescription() );

      final JAXBElement<DocumentType> kmlDocument = factory.createDocument( documentType );

      final FolderType folderType = factory.createFolderType();
      folderType.setName( "Kalypso Google Earth Export" );

      /* process map */
      final KalypsoThemeVisitor visitor = new GoogleEarthThemeVisitor( mapPanel, folderType, new ThemeGoogleEarthExportable() );

      final IKalypsoTheme[] themes = mapModell.getAllThemes();
      for( final IKalypsoTheme theme : themes )
        visitor.visit( theme );

      final StyleTypeFactory styleFactory = StyleTypeFactory.getStyleFactory( factory );
      styleFactory.addStylesToDocument( documentType );

      GoogleEarthExportUtils.removeEmtpyFolders( folderType );

      documentType.getFeature().add( factory.createFolder( folderType ) );

      /* marshalling */
      final File file = new File( tmpDir, "doc.kml" );
      final JAXBContext jc = JAXBContext.newInstance( ObjectFactory.class );
      final Marshaller m = jc.createMarshaller();

      final FileOutputStream os = new FileOutputStream( file );
      m.marshal( kmlDocument, os );

      os.flush();
      os.close();

      /* pack kmz file */
      ZipUtils.pack( m_settings.getExportFile(), tmpDir );
    }
    catch( final Exception e )
    {
      return new Status( IStatus.ERROR, "GoogleEarthExporter", e.getMessage() );
    }

    return Status.OK_STATUS;
  }
}
