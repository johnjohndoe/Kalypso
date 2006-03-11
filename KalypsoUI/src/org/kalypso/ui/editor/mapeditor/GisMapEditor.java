/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.editor.mapeditor;

import java.awt.Frame;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URL;

import org.apache.commons.configuration.Configuration;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.GroupMarker;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.swt.events.SWTAWT_ContextMenuMouseAdapter;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.metadoc.IExportableObject;
import org.kalypso.metadoc.IExportableObjectFactory;
import org.kalypso.metadoc.configuration.IPublishingConfiguration;
import org.kalypso.metadoc.ui.ImageExportPage;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapPanelProvider;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ogc.gml.widgets.IWidgetChangeListener;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gistableview.Gistableview.Layer;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.AbstractEditorPart;
import org.kalypso.ui.editor.mapeditor.views.ActionOptionsView;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * <p>
 * Eclipse-Editor zum editieren der GML-Gis-Templates.
 * </p>
 * <p>
 * Zeigt das ganze als Kartendarstellug, die einzelnen Datenquellen k?nnen potentiell editiert werden
 * </p>
 * <p>
 * Implementiert {@link org.kalypso.commons.command.ICommandManager}für die Undo und Redo Action. Gibt alles an den
 * DefaultCommandManager weiter, es wird zusätzlich eine Aktualisierung der View bei jeder Aktion durchgef?hrt
 * </p>
 * 
 * @author belger
 */
public class GisMapEditor extends AbstractEditorPart implements IMapPanelProvider, IExportableObjectFactory
{
  private GisMapOutlinePage m_outlinePage = null;

  private final MapPanel m_mapPanel;

  private GisTemplateMapModell m_mapModell;

  private final IFeatureSelectionManager m_selectionManager = KalypsoCorePlugin.getDefault().getSelectionManager();

  private boolean m_disposed = false;

  public GisMapEditor( )
  {
    final KalypsoGisPlugin plugin = KalypsoGisPlugin.getDefault();
    m_mapPanel = new MapPanel( this, plugin.getCoordinatesSystem(), m_selectionManager );
    m_mapPanel.getWidgetManager().addWidgetChangeListener( new IWidgetChangeListener()
    {
      public void widgetChanged( final IWidget newWidget )
      {
        // the widget changed and there is something to show, so bring this
        // view to top
        try
        {
          if( newWidget instanceof IWidgetWithOptions )
          {
            final IWorkbenchPage page = getSite().getPage();
            page.showView( ActionOptionsView.class.getName(), null, IWorkbenchPage.VIEW_VISIBLE );
          }
        }
        catch( final PartInitException e )
        {
          e.printStackTrace();
        }
      }
    } );
  }

  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  @Override
  public Object getAdapter( final Class adapter )
  {
    if( IContentOutlinePage.class.equals( adapter ) )
    {
      m_outlinePage = new GisMapOutlinePage( getCommandTarget() );

      m_outlinePage.setMapModell( m_mapModell );

      return m_outlinePage;
    }

    if( IExportableObjectFactory.class.equals( adapter ) )
      return this;

    return super.getAdapter( adapter );
  }

  @Override
  protected void doSaveInternal( final IProgressMonitor monitor, final IFileEditorInput input ) throws CoreException
  {
    if( m_mapModell == null )
      return;

    ByteArrayInputStream bis = null;
    try
    {
      monitor.beginTask( "Kartenvorlage speichern", 2000 );
      final GM_Envelope boundingBox = getMapPanel().getBoundingBox();
      final String srsName = KalypsoGisPlugin.getDefault().getCoordinatesSystem().getName();
      final Gismapview modellTemplate = m_mapModell.createGismapTemplate( boundingBox, srsName );

      final ByteArrayOutputStream bos = new ByteArrayOutputStream();

      GisTemplateHelper.saveGisMapView( modellTemplate, bos );

      bis = new ByteArrayInputStream( bos.toByteArray() );
      bos.close();
      monitor.worked( 1000 );

      final IFile file = input.getFile();
      if( file.exists() )
        file.setContents( bis, false, true, monitor );
      else
        file.create( bis, false, monitor );
    }
    catch( final CoreException e )
    {
      throw e;
    }
    catch( final Throwable e )
    {
      System.out.println( e.getLocalizedMessage() );
      e.printStackTrace();

      throw new CoreException( StatusUtilities.statusFromThrowable( e, "XML-Vorlagendatei konnte nicht erstellt werden." ) );
    }
    finally
    {
      monitor.done();

      if( bis != null )
        try
        {
          bis.close();
        }
        catch( IOException e1 )
        {
          // never occurs with a byteinputstream
          e1.printStackTrace();
        }
    }

  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    super.createPartControl( parent );
    final Composite composite = new Composite( parent, SWT.RIGHT | SWT.EMBEDDED );
    // create MapPanel
    final Frame virtualFrame = SWT_AWT.new_Frame( composite );
    virtualFrame.setVisible( true );
    m_mapPanel.setVisible( true );
    virtualFrame.add( m_mapPanel );

    // create Context Menu
    final MenuManager menuManager = new MenuManager();
    menuManager.setRemoveAllWhenShown( true );
    menuManager.addMenuListener( new IMenuListener()
    {
      public void menuAboutToShow( IMenuManager manager )
      {
        manager.add( new GroupMarker( IWorkbenchActionConstants.MB_ADDITIONS ) );
        manager.add( new Separator() );
      }
    } );
    final Menu mapMenu = menuManager.createContextMenu( composite );
    composite.setMenu( mapMenu );
    // register it
    getSite().registerContextMenu( menuManager, m_mapPanel );
    getSite().setSelectionProvider( m_mapPanel );

    m_mapPanel.addMouseListener( new SWTAWT_ContextMenuMouseAdapter( composite, mapMenu ) );

    // add drag and drop support
    // int ops = DND.DROP_COPY | DND.DROP_MOVE | DND.DROP_LINK;
    // Transfer[] transfers = new Transfer[]
    // {
    // LocalSelectionTransfer.getInstance(),
    // PluginTransfer.getInstance() };
    // m_treeViewer.addDragSupport( ops, transfers, new GmlTreeDragListener( this ) );
    // transfers = new Transfer[]
    // { LocalSelectionTransfer.getInstance() };
    // m_dropAdapter = new GmlTreeDropAdapter( this );
    // m_treeViewer.addDropSupport( ops, transfers, m_dropAdapter );
  }

  @Override
  protected final void loadInternal( final IProgressMonitor monitor, final IStorageEditorInput input ) throws Exception, CoreException
  {
    if( m_disposed )
      return;

    // prepare for exception
    setMapModell( null );

    monitor.beginTask( "Kartenvorlage laden", 2000 );

    final Gismapview gisview = GisTemplateHelper.loadGisMapView( input.getStorage() );

    monitor.worked( 1000 );

    final URL context;
    final IProject project;
    if( input instanceof IFileEditorInput )
    {
      final IFile inputFile = ((IFileEditorInput) getEditorInput()).getFile();
      context = ResourceUtilities.createURL( inputFile );
      project = inputFile.getProject();
    }
    else
    {
      context = null;
      project = null;
    }

    if( !m_disposed )
    {
      final GisTemplateMapModell mapModell = new GisTemplateMapModell( gisview, context, KalypsoGisPlugin.getDefault().getCoordinatesSystem(), project, m_selectionManager );
      setMapModell( mapModell );

      GM_Envelope env = GisTemplateHelper.getBoundingBox( gisview );

      getMapPanel().setBoundingBox( env );
    }
    monitor.done();
  }

  private void setMapModell( final GisTemplateMapModell mapModell )
  {
    // dispose old one
    if( m_mapModell != null )
      m_mapModell.dispose();

    m_mapModell = mapModell;

    m_mapPanel.setMapModell( m_mapModell );
    if( m_outlinePage != null )
      m_outlinePage.setMapModell( m_mapModell );
  }

  public void showProperties( final Layer layer )
  {
    // TODO
    MessageDialog.openInformation( getEditorSite().getShell(), "Themeneigenschaften", "Leider noch nicht implementiert" );
    layer.getClass();
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapPanelProvider#getMapPanel()
   */
  public MapPanel getMapPanel( )
  {
    return m_mapPanel;
  }

  public void saveTheme( final IKalypsoFeatureTheme theme, final IProgressMonitor monitor ) throws CoreException
  {
    m_mapModell.saveTheme( theme, monitor );
  }

  @Override
  public void dispose( )
  {
    m_disposed = true;

    setMapModell( null );

    m_mapPanel.dispose();

    if( m_outlinePage != null )
      m_outlinePage.dispose();

    super.dispose();
  }

  public IContentOutlinePage getOutlineView( )
  {
    return m_outlinePage;
  }

  /**
   * @see org.kalypso.metadoc.IExportableObjectFactory#createExportableObjects(org.apache.commons.configuration.Configuration)
   */
  public IExportableObject[] createExportableObjects( final Configuration conf )
  {
    return new IExportableObject[] { new ExportableMap( getMapPanel(), conf.getInt( ImageExportPage.CONF_IMAGE_WIDTH, 640 ), conf.getInt( ImageExportPage.CONF_IMAGE_HEIGHT, 480 ), conf.getString( ImageExportPage.CONF_IMAGE_FORMAT, "png" ) ) };
  }

  /**
   * @see org.kalypso.metadoc.IExportableObjectFactory#createWizardPages(org.kalypso.metadoc.configuration.IPublishingConfiguration,
   *      ImageDescriptor)
   */
  public IWizardPage[] createWizardPages( final IPublishingConfiguration configuration, ImageDescriptor defaultImage )
  {
    final ImageDescriptor imgDesc = AbstractUIPlugin.imageDescriptorFromPlugin( KalypsoGisPlugin.getId(), "icons/util/img_props.gif" );
    final IWizardPage page = new ImageExportPage( configuration, "mapprops", "Export Optionen", imgDesc );

    return new IWizardPage[] { page };
  }
}