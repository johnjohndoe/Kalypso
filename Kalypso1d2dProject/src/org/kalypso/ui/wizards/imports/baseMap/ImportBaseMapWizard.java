/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ui.wizards.imports.baseMap;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;

import org.apache.commons.io.IOUtils;
import org.deegree.ogcwebservices.wms.capabilities.Layer;
import org.deegree.ogcwebservices.wms.capabilities.Style;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.grid.WorldFileReader;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ogc.gml.wms.provider.images.IKalypsoImageProvider;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoServiceConstants;
import org.kalypso.ui.action.AddThemeCommand;
import org.kalypso.ui.views.map.MapView;
import org.kalypso.ui.wizard.wms.IKalypsoImportWMSWizard;
import org.kalypso.ui.wizard.wms.pages.ImportWmsWizardPage;
import org.kalypso.ui.wizards.i18n.Messages;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

enum SelectedPage
{
  PageNONE,
  PageImportIMG,
  PageImportSHP,
  PageImportWMS
}

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 * @author Madanagopal
 */
public class ImportBaseMapWizard extends Wizard implements INewWizard, IKalypsoImportWMSWizard
{
  private IStructuredSelection initialSelection;

  IFolder m_scenarioFolder;

  private ImportBaseMapWizardMainPage m_PageMain;

  protected ImportBaseMapImportImgPage m_pageImportImg;

  protected ImportBaseMapImportShpPage m_PageImportShp;

  protected ImportWmsWizardPage m_PageImportWMS;

  private final ArrayList<String> m_catalog = new ArrayList<String>();

  /**
   * Construct a new instance and initialize the dialog settings for this instance.
   */
  public ImportBaseMapWizard( )
  {
    super();

    /* Get the dialog settings. */
    final IDialogSettings dialogSettings = getDialogSettings();

    /* If not available, add a section inside the settings of the plugin. */
    if( dialogSettings == null )
    {
      final IDialogSettings settings = Kalypso1d2dProjectPlugin.getDefault().getDialogSettings();

      /* Cannot do anything, if even the plugin has no settings. */
      if( settings == null )
        return;

      /* If available, check, if there is a section from this wizard. */
      IDialogSettings section = settings.getSection( "IMPORT_WMS_WIZARD" ); //$NON-NLS-1$
      if( section == null )
      {
        /* There is none available, add a new one. */
        section = settings.addNewSection( "IMPORT_WMS_WIZARD" ); //$NON-NLS-1$
      }

      /* Finally set it. */
      setDialogSettings( section );
    }
  }

  /**
   * @param workbench
   *          the current workbench
   * @param selection
   *          the current object selection
   */
  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    m_scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );

    initialSelection = selection;
    setNeedsProgressMonitor( true );
    setWindowTitle( Messages.getString( "org.kalypso.ui.wizards.imports.baseMap.BaseMapWizard.0" ) ); //$NON-NLS-1$

    // read service catalog file
    final InputStream is = getClass().getResourceAsStream( "wms.catalog" ); //$NON-NLS-1$
    try
    {
      readCatalog( is );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      m_catalog.clear();
    }
    finally
    {
      IOUtils.closeQuietly( is );
    }
  }

  @Override
  public void addPages( )
  {
    m_PageMain = new ImportBaseMapWizardMainPage();
    m_pageImportImg = new ImportBaseMapImportImgPage();
    m_PageImportShp = new ImportBaseMapImportShpPage();
    m_PageImportWMS = new ImportWmsWizardPage( "WmsImportPage", Messages.getString( "org.kalypso.ui.wizards.imports.baseMap.ImportBaseMapWizard.5" ), ImageProvider.IMAGE_UTIL_UPLOAD_WIZ ); //$NON-NLS-1$ //$NON-NLS-2$
    m_pageImportImg.init( initialSelection );
    m_PageImportShp.init( initialSelection );
    addPage( m_PageMain );
    addPage( m_pageImportImg );
    addPage( m_PageImportShp );
    addPage( m_PageImportWMS );
  }

  private SelectedPage getSelectedPage( )
  {
    final IWizardPage currentPage = getContainer().getCurrentPage();
    if( currentPage == m_pageImportImg )
      return SelectedPage.PageImportIMG;
    if( currentPage == m_PageImportShp )
      return SelectedPage.PageImportSHP;
    if( currentPage == m_PageImportWMS )
      return SelectedPage.PageImportWMS;
    return SelectedPage.PageNONE;
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#canFinish()
   */
  @Override
  public boolean canFinish( )
  {
    if( getSelectedPage() == SelectedPage.PageNONE )
      return false;
    else
      return getContainer().getCurrentPage().isPageComplete();
  }

  @Override
  public ArrayList<String> getCatalog( )
  {
    return m_catalog;
  }

  public void readCatalog( final InputStream is ) throws IOException, NullPointerException
  {
    m_catalog.clear();

    // use properties to parse catalog: dont do everything yourself
    // fixes bug with '=' inside of URLs
    final Properties properties = new Properties();
    properties.load( is );

    final Set<Entry<Object, Object>> name = properties.entrySet();
    for( final Entry<Object, Object> entry : name )
    {
      if( entry.getKey().toString().startsWith( KalypsoServiceConstants.WMS_LINK_TYPE ) )
        m_catalog.add( entry.getValue().toString() );
    }
  }

  /**
   * This method is called by the wizard framework when the user presses the Finish button.
   */
  @Override
  public boolean performFinish( )
  {
    final MapView mapView = (MapView) PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().findView( MapView.ID );
    if( mapView == null )
    {
      StatusUtilities.createWarningStatus( Messages.getString( "org.kalypso.ui.wizards.imports.baseMap.ImportBaseMapWizard.6" ) ); //$NON-NLS-1$
      return false;
    }
    try
    {
      final IKalypsoLayerModell mapModell = (GisTemplateMapModell) mapView.getMapPanel().getMapModell();
      switch( getSelectedPage() )
      {
        case PageImportIMG:
          return performFinishIMG( mapView, mapModell );

        case PageImportSHP:
          return performFinishSHP( mapView, mapModell );

        case PageImportWMS:
          return performFinishWMS( mapView, mapModell );

        default:
          return false;
      }
    }
    catch( final CoreException e )
    {
      final StatusDialog statusDialog = new StatusDialog( getShell(), e.getStatus(), getWindowTitle() );
      statusDialog.open();
      return false;
    }
  }

  private boolean performFinishIMG( final MapView mapView, final IKalypsoLayerModell mapModell ) throws CoreException
  {
    final IFolder importsFolder = m_scenarioFolder.getProject().getFolder( "imports" ); //$NON-NLS-1$
    final IFolder dstFileFolder = importsFolder.getFolder( "basemap" ); //$NON-NLS-1$ 

    final IPath sourceLocation = m_pageImportImg.getSourceLocation();
    final File srcFileImage = new File( sourceLocation.toString() );
    final IFile dstFileImage = dstFileFolder.getFile( srcFileImage.getName() );

    final File srcFileGeoreference = WorldFileReader.findWorldFile( srcFileImage );
    if( srcFileGeoreference == null )
      throw new UnsupportedOperationException( Messages.getString( "org.kalypso.ui.wizards.imports.baseMap.BaseMapWizard.0" ) + ": " + sourceLocation.getFileExtension() ); //$NON-NLS-1$ //$NON-NLS-2$

    final IFile dstFileGeoreference = dstFileFolder.getFile( srcFileGeoreference.getName() );

    final String coordinateSystem = m_pageImportImg.getCoordinateSystem();

    final ICoreRunnableWithProgress operation = new ICoreRunnableWithProgress()
    {
      @Override
      public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
      {
        try
        {
          if( !dstFileFolder.exists() )
            dstFileFolder.create( true, true, monitor );

          copy( srcFileImage, dstFileImage, monitor );
          copy( srcFileGeoreference, dstFileGeoreference, monitor );
          return Status.OK_STATUS;
        }
        catch( final IOException e )
        {
          throw new InvocationTargetException( e );
        }
      }
    };

    final IStatus copyResult = RunnableContextHelper.execute( getContainer(), true, false, operation );
    if( !copyResult.isOK() )
      throw new CoreException( copyResult );

    final String type = sourceLocation.getFileExtension();
    final String layerName = sourceLocation.removeFileExtension().lastSegment();

    final URL context = mapModell.getContext();
    final IFile mapFile = ResourceUtilities.findFileFromURL( context );
    final IPath relativeDstPath = ResourceUtilities.makeRelativ( mapFile, dstFileImage );

    //    final String imgHref = "project:" + File.separator + "imports" + File.separator + "basemap" + File.separator + sourceLocation.lastSegment() + "#" + coordinateSystem; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    final String imgHref = String.format( "%s#%s", relativeDstPath.toString(), coordinateSystem );
    final AddThemeCommand command = new AddThemeCommand( mapModell, layerName, type, null, imgHref );
    mapView.postCommand( command, null );

    return true;
  }

  private boolean performFinishSHP( final MapView mapView, final IKalypsoLayerModell mapModell )
  {
    final IFolder dstFilePath = m_scenarioFolder.getProject().getFolder( "imports" + File.separator + "basemap" ); //$NON-NLS-1$ //$NON-NLS-2$

    if( !dstFilePath.exists() )
      try
      {
        dstFilePath.create( true, true, null );
      }
      catch( final CoreException e1 )
      {
        e1.printStackTrace();
      }
    final File srcFileShape = new File( m_PageImportShp.getSourceLocation().toOSString() );
    final IFile dstFileShape = dstFilePath.getFile( m_PageImportShp.getSourceLocation().lastSegment() );
    File srcFileIndex = null;
    IFile dstFileIndex = null;
    File srcFileDBase = null;
    IFile dstFileDBase = null;
    final String extension = m_PageImportShp.getSourceLocation().getFileExtension();
    if( extension.equalsIgnoreCase( "shp" ) ) //$NON-NLS-1$
    {
      srcFileIndex = new File( m_PageImportShp.getSourceLocation().removeFileExtension().addFileExtension( "shx" ).toOSString() ); //$NON-NLS-1$
      dstFileIndex = dstFilePath.getFile( m_PageImportShp.getSourceLocation().removeFileExtension().addFileExtension( "shx" ).lastSegment() ); //$NON-NLS-1$
      srcFileDBase = new File( m_PageImportShp.getSourceLocation().removeFileExtension().addFileExtension( "dbf" ).toOSString() ); //$NON-NLS-1$
      dstFileDBase = dstFilePath.getFile( m_PageImportShp.getSourceLocation().removeFileExtension().addFileExtension( "dbf" ).lastSegment() ); //$NON-NLS-1$
    }
    else
    {
      throw new UnsupportedOperationException( Messages.getString( "org.kalypso.ui.wizards.imports.baseMap.ImportBaseMapWizard.26" ) + extension ); //$NON-NLS-1$
    }
    try
    {
      final File finalSrcIndex = srcFileIndex;
      final IFile finalDstIndex = dstFileIndex;
      final File finalSrcDBase = srcFileDBase;
      final IFile finalDstDBase = dstFileDBase;
      final String coordinateSystem = m_PageImportShp.getCoordinateSystem();

      final ICoreRunnableWithProgress operation = new ICoreRunnableWithProgress()
      {
        @Override
        public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
        {
          try
          {
            if( !dstFilePath.exists() )
            {
              dstFilePath.create( true, true, monitor );
            }
            copy( srcFileShape, dstFileShape, monitor );
            copy( finalSrcIndex, finalDstIndex, monitor );
            copy( finalSrcDBase, finalDstDBase, monitor );

            return Status.OK_STATUS;
          }
          catch( final IOException e )
          {
            throw new InvocationTargetException( e );
          }
        }
      };

      final IStatus status = RunnableContextHelper.execute( getContainer(), true, true, operation );
      ErrorDialog.openError( getShell(), "", "", status ); //$NON-NLS-1$ //$NON-NLS-2$

      if( !status.isOK() )
        return false;

      final String layerName = m_PageImportShp.getSourceLocation().removeFileExtension().lastSegment();
      final String shpHref = "project:" + File.separator + "imports" + File.separator + "basemap" + File.separator + m_PageImportShp.getSourceLocation().removeFileExtension().lastSegment() + "#" + coordinateSystem; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
      final AddThemeCommand command = new AddThemeCommand( mapModell, layerName, "shape", "featureMember", shpHref ); //$NON-NLS-1$ //$NON-NLS-2$
      mapView.postCommand( command, null );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    return true;
  }

  private boolean performFinishWMS( final MapView mapView, final IKalypsoLayerModell mapModell )
  {
    /* Finishes the work on this page (dialog settings). */
    m_PageImportWMS.finish();

    try
    {
      if( m_PageImportWMS.isMultiLayer() )
      {
        final StringBuffer source = new StringBuffer( IKalypsoImageProvider.KEY_URL + "=" + m_PageImportWMS.getBaseURL().toString() ); //$NON-NLS-1$
        final StringBuffer layers = new StringBuffer( IKalypsoImageProvider.KEY_LAYERS + "=" ); //$NON-NLS-1$
        final StringBuffer styles = new StringBuffer( IKalypsoImageProvider.KEY_STYLES + "=" ); //$NON-NLS-1$
        final StringBuffer provider = new StringBuffer( IKalypsoImageProvider.KEY_PROVIDER + "=" ); //$NON-NLS-1$

        final Layer[] layerArray = m_PageImportWMS.getLayersList();
        for( int i = 0; i < layerArray.length; i++ )
        {
          final Layer layer = layerArray[i];
          final String layerName = layer.getName();
          final String styleName;
          final Style[] styles2 = layer.getStyles();
          if( styles2.length > 0 )
            styleName = styles2[0].getName();
          else
            styleName = "default"; //$NON-NLS-1$
          layers.append( layerName );
          styles.append( styleName );
          if( i < layerArray.length - 1 )
          {
            layers.append( "," ); //$NON-NLS-1$
            styles.append( "," ); //$NON-NLS-1$
          }
        }

        final String providerID = m_PageImportWMS.getProviderID();
        if( providerID != null )
          provider.append( providerID );

        final String layerName = "Multi" + source; //$NON-NLS-1$
        source.append( "#" ).append( layers.toString() ); //$NON-NLS-1$
        source.append( "#" ).append( styles.toString() ); //$NON-NLS-1$
        source.append( "#" ).append( provider.toString() ); //$NON-NLS-1$

        final AddThemeCommand command = new AddThemeCommand( mapModell, layerName, "wms", null, source.toString() ); //$NON-NLS-1$
        mapView.postCommand( command, null );
      }
      else
      {
        final Layer[] layerArray = m_PageImportWMS.getLayersList();
        for( final Layer layer : layerArray )
        {
          final StringBuffer source = new StringBuffer( IKalypsoImageProvider.KEY_URL + "=" + m_PageImportWMS.getBaseURL().toString() ); //$NON-NLS-1$

          final String layerName = layer.getName();
          final String styleName;
          final Style[] styles2 = layer.getStyles();
          if( styles2.length > 0 )
            styleName = styles2[0].getName();
          else
            styleName = "default"; //$NON-NLS-1$

          String providerID = m_PageImportWMS.getProviderID();
          if( providerID == null )
            providerID = ""; //$NON-NLS-1$

          final String layerTitle = layer.getTitle();
          source.append( "#" ).append( IKalypsoImageProvider.KEY_LAYERS ).append( "=" ).append( layerName ); //$NON-NLS-1$ //$NON-NLS-2$
          source.append( "#" ).append( IKalypsoImageProvider.KEY_STYLES ).append( "=" ).append( styleName ); //$NON-NLS-1$ //$NON-NLS-2$
          source.append( "#" ).append( IKalypsoImageProvider.KEY_PROVIDER ).append( "=" ).append( providerID ); //$NON-NLS-1$ //$NON-NLS-2$

          final AddThemeCommand command = new AddThemeCommand( mapModell, layerTitle, "wms", null, source.toString() ); //$NON-NLS-1$
          mapView.postCommand( command, null );
        }
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    return true;
  }

  void copy( final File src, final IFile dstFileImage, final IProgressMonitor monitor2 ) throws CoreException, IOException
  {
    InputStream in = null;
    try
    {
      in = new BufferedInputStream( new FileInputStream( src ) );
      if( dstFileImage.exists() )
      {
        dstFileImage.setContents( in, false, true, monitor2 );
      }
      else
      {
        dstFileImage.create( in, false, monitor2 );
      }
    }
    finally
    {
      IOUtils.closeQuietly( in );
    }
  }
}