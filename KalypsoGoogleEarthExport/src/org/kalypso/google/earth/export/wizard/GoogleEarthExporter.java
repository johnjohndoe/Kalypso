/**
 *
 */
package org.kalypso.google.earth.export.wizard;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Marshaller;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.junit.Assert;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.google.earth.export.GoogleEarthThemeVisitor;
import org.kalypso.google.earth.export.GoogleExportDelegate;
import org.kalypso.google.earth.export.constants.IGoogleEarthExportSettings;
import org.kalypso.google.earth.export.interfaces.IGoogleEarthAdapter;
import org.kalypso.google.earth.export.interfaces.IPlacemark;
import org.kalypso.google.earth.export.utils.GoogleEarthExportUtils;
import org.kalypso.google.earth.export.utils.GoogleEarthUtils;
import org.kalypso.google.earth.export.utils.StyleTypeFactory;
import org.kalypso.google.earth.export.utils.ThemeGoogleEarthExportable;
import org.kalypso.google.earth.export.utils.ZipUtils;
import org.kalypso.ogc.gml.AbstractCascadingLayerTheme;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor;
import org.kalypso.ui.views.map.MapView;

import com.google.earth.kml.DocumentType;
import com.google.earth.kml.FeatureType;
import com.google.earth.kml.FolderType;
import com.google.earth.kml.ObjectFactory;

/**
 * @author kuch
 */
public class GoogleEarthExporter implements ICoreRunnableWithProgress
{

  private final IGoogleEarthExportSettings m_settings;

  private final MapView m_view;

  private MapPanel m_mapPanel;

  private final IGoogleEarthAdapter[] m_provider;

  /**
   * @param view
   * @param m_page
   */
  public GoogleEarthExporter( final MapView view, final IGoogleEarthExportSettings settings )
  {
    m_view = view;
    m_settings = settings;

    /* get extension points for rendering geometries and adding additional geometries */
    final IExtensionRegistry registry = Platform.getExtensionRegistry();
    final IConfigurationElement[] elements = registry.getConfigurationElementsFor( IGoogleEarthAdapter.ID ); //$NON-NLS-1$

    // TODO handling of several providers, which provider wins / rules, returns a special geometry, aso
    final List<IGoogleEarthAdapter> provider = new ArrayList<IGoogleEarthAdapter>( elements.length );
    for( final IConfigurationElement element : elements )
    {
      try
      {
        provider.add( (IGoogleEarthAdapter) element.createExecutableExtension( "class" ) ); //$NON-NLS-1$
      }
      catch( final CoreException e )
      {
        e.printStackTrace();
      }
    }

    m_provider = provider.toArray( new IGoogleEarthAdapter[] {} );
  }

  /**
   * @return
   * @throws IOException
   */
  private File createTmpDir( ) throws IOException
  {
    final URL urlTmpDir = new File( System.getProperty( "java.io.tmpdir" ) ).toURL(); //$NON-NLS-1$
    Assert.assertNotNull( urlTmpDir );

    /* delete old test dir */
    final URL urlBaseDir = new URL( urlTmpDir + "kalypsoGoogleEarthExport/" ); //$NON-NLS-1$

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
      m_mapPanel = m_view.getMapPanel();
      final IMapModell mapModell = m_mapPanel.getMapModell();

      /* kml document root */
      final ObjectFactory googleEarthFactory = new ObjectFactory();
      final DocumentType documentType = googleEarthFactory.createDocumentType();

      /* basic kml settings */
      GoogleEarthUtils.setMapBoundary( m_mapPanel.getBoundingBox(), mapModell.getCoordinatesSystem(), googleEarthFactory, documentType );
      GoogleEarthUtils.setLookAt( m_mapPanel.getBoundingBox(), mapModell.getCoordinatesSystem(), googleEarthFactory, documentType );

      documentType.setName( m_settings.getExportName() );
      documentType.setDescription( m_settings.getExportDescription() );

      final JAXBElement<DocumentType> kmlDocument = googleEarthFactory.createDocument( documentType );

      final FolderType folderType = googleEarthFactory.createFolderType();
      folderType.setName( "Kalypso Google Earth (TM) Export" ); //$NON-NLS-1$

      /* process map */
      final KalypsoThemeVisitor visitor = new GoogleEarthThemeVisitor( new ThemeGoogleEarthExportable() );
      for( final IKalypsoTheme theme : mapModell.getAllThemes() )
        visitor.visit( theme );

      final IKalypsoTheme[] themes = visitor.getFoundThemes();
      for( final IKalypsoTheme theme : themes )
      {
        processTheme( folderType, theme );
      }

      final StyleTypeFactory styleFactory = StyleTypeFactory.getStyleFactory( googleEarthFactory );
      styleFactory.addStylesToDocument( documentType );

      GoogleEarthExportUtils.removeEmtpyFolders( folderType );
      documentType.getFeature().add( googleEarthFactory.createFolder( folderType ) );

      /* add additional place marks and clean up providers */
      for( final IGoogleEarthAdapter adapter : m_provider )
      {
        final IPlacemark[] placemarkers = adapter.getAdditionalPlacemarkers();

        adapter.cleanUp();
      }

      // TODO dispose styleFactoryStyles (singelton)
      styleFactory.dispose();

      /* marshalling */
      final File file = new File( tmpDir, "doc.kml" ); //$NON-NLS-1$
      final JAXBContext jc = JAXBContext.newInstance( ObjectFactory.class );
      final Marshaller m = jc.createMarshaller();
      m.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );

      final FileOutputStream os = new FileOutputStream( file );
      m.marshal( kmlDocument, os );

      os.flush();
      os.close();

      /* pack kmz file */
      ZipUtils.pack( m_settings.getExportFile(), tmpDir );
    }
    catch( final Exception e )
    {
      return new Status( IStatus.ERROR, "GoogleEarthExporter", e.getMessage() ); //$NON-NLS-1$
    }

    return Status.OK_STATUS;
  }

  private void processTheme( final FolderType parentFolderType, final IKalypsoTheme theme )
  {
    final ObjectFactory factory = new ObjectFactory();
    final List<JAXBElement< ? extends FeatureType>> myList = parentFolderType.getFeature();

    /* get inner themes */
    if( theme instanceof AbstractCascadingLayerTheme )
    {
      final FolderType folderType = factory.createFolderType();

      folderType.setName( theme.getLabel() );

      final AbstractCascadingLayerTheme cascading = (AbstractCascadingLayerTheme) theme;
      final GisTemplateMapModell inner = cascading.getInnerMapModel();

      final IKalypsoTheme[] themes = inner.getAllThemes();
      for( final IKalypsoTheme t : themes )
      {
        processTheme( folderType, t );
      }

      myList.add( factory.createFolder( folderType ) );
    }
    /* "paint" inner themes */
    else if( theme instanceof IKalypsoFeatureTheme )
      try
      {
        final FolderType folderType = factory.createFolderType();
        folderType.setName( theme.getLabel() );
        final IKalypsoFeatureTheme ft = (IKalypsoFeatureTheme) theme;
        final GoogleExportDelegate delegate = new GoogleExportDelegate( m_provider, m_mapPanel, factory, folderType );
        ft.paintInternal( delegate );

        myList.add( factory.createFolder( folderType ) );
      }
      catch( final CoreException e )
      {
        e.printStackTrace();
      }

  }
}
