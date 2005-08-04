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
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.metadoc.IExportableObject;
import org.kalypso.metadoc.IExportableObjectFactory;
import org.kalypso.metadoc.configuration.IPublishingConfiguration;
import org.kalypso.metadoc.ui.ImageExportPage;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapPanelProvider;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ogc.gml.widgets.IWidgetChangeListener;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gistableview.GistableviewType.LayerType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.AbstractEditorPart;
import org.kalypso.ui.editor.mapeditor.views.ActionOptionsView;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * <p>
 * Eclipse-Editor zum editieren der GML-Gis-Templates.
 * </p>
 * 
 * <p>
 * Zeigt das ganze als Kartendarstellug, die einzelnen Datenquellen k?nnen potentiell editiert werden
 * </p>
 * 
 * <p>
 * Implementiert {@link org.kalypso.commons.command.ICommandManager}f�r die Undo und Redo Action. Gibt alles an den
 * DefaultCommandManager weiter, es wird zus�tzlich eine Aktualisierung der View bei jeder Aktion durchgef?hrt
 * </p>
 * 
 * @author belger
 */
public class GisMapEditor extends AbstractEditorPart implements IMapPanelProvider, ISelectionProvider,
    IExportableObjectFactory
{
  private GisMapOutlinePage m_outlinePage = null;

  private final MapPanel m_MapPanel;

  private GisTemplateMapModell m_mapModell;

  public GisMapEditor()
  {
    final KalypsoGisPlugin plugin = KalypsoGisPlugin.getDefault();
    m_MapPanel = new MapPanel( this, plugin.getCoordinatesSystem() );
    m_MapPanel.getWidgetManager().addWidgetChangeListener( new IWidgetChangeListener()
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

      throw new CoreException( KalypsoGisPlugin
          .createErrorStatus( "XML-Vorlagendatei konnte nicht erstellt werden.", e ) );
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
  public void createPartControl( final Composite parent )
  {
    super.createPartControl( parent );

    // create MapPanel
    final Composite composite = new Composite( parent, SWT.RIGHT | SWT.EMBEDDED );
    final Frame virtualFrame = SWT_AWT.new_Frame( composite );
    virtualFrame.setVisible( true );
    m_MapPanel.setVisible( true );
    virtualFrame.add( m_MapPanel );
  }

  protected final void loadInternal( final IProgressMonitor monitor, final IStorageEditorInput input )
      throws Exception, CoreException
  {
    if( !( input instanceof IFileEditorInput ) )
      throw new IllegalArgumentException( "Kann nur Dateien laden" );

    // prepare for exception
    setMapModell( null );

    monitor.beginTask( "Kartenvorlage laden", 2000 );

    final Gismapview gisview = GisTemplateHelper.loadGisMapView( ( (IFileEditorInput)input ).getFile() );

    monitor.worked( 1000 );

    final IFile inputFile = ( (IFileEditorInput)getEditorInput() ).getFile();
    final URL context = ResourceUtilities.createURL( inputFile );

    final GisTemplateMapModell mapModell = new GisTemplateMapModell( gisview, context, KalypsoGisPlugin.getDefault()
        .getCoordinatesSystem(), inputFile.getProject() );
    setMapModell( mapModell );

    GM_Envelope env = GisTemplateHelper.getBoundingBox( gisview );

    getMapPanel().setBoundingBox( env );
    monitor.done();
  }

  private void setMapModell( final GisTemplateMapModell mapModell )
  {
    m_mapModell = mapModell;

    m_MapPanel.setMapModell( m_mapModell );
    if( m_outlinePage != null )
      m_outlinePage.setMapModell( m_mapModell );
  }

  public void showProperties( final LayerType layer )
  {
    MessageDialog
        .openInformation( getEditorSite().getShell(), "Themeneigenschaften", "Leider noch nicht implementiert" );

    layer.getClass();
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapPanelProvider#getMapPanel()
   */
  public MapPanel getMapPanel()
  {
    return m_MapPanel;
  }

  public void saveTheme( final IKalypsoFeatureTheme theme, final IProgressMonitor monitor ) throws CoreException
  {
    m_mapModell.saveTheme( theme, monitor );
  }

  /*
   *  
   */
  public void dispose()
  {
    if( m_mapModell != null )
      m_mapModell.dispose();

    m_MapPanel.dispose();

    setMapModell( null );

    if( m_outlinePage != null )
      m_outlinePage.dispose();

    super.dispose();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void addSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_MapPanel.addSelectionChangedListener( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
   */
  public ISelection getSelection()
  {
    return m_MapPanel.getSelection();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void removeSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_MapPanel.removeSelectionChangedListener( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( ISelection selection )
  {
    m_MapPanel.setSelection( selection );
  }

  /**
   * @see org.kalypso.metadoc.IExportableObjectFactory#createExportableObjects(org.apache.commons.configuration.Configuration)
   */
  public IExportableObject[] createExportableObjects( Configuration conf )
  {
    return new IExportableObject[]
    { new ExportableMap( getMapPanel(), conf.getInt( ImageExportPage.CONF_IMAGE_WIDTH, 640 ), conf.getInt(
        ImageExportPage.CONF_IMAGE_HEIGHT, 480 ), conf.getString( ImageExportPage.CONF_IMAGE_FORMAT, "png" ) ) };
  }

  /**
   * @see org.kalypso.metadoc.IExportableObjectFactory#createWizardPages(org.kalypso.metadoc.configuration.IPublishingConfiguration)
   */
  public IWizardPage[] createWizardPages( final IPublishingConfiguration configuration )
  {
    final ImageDescriptor imgDesc = AbstractUIPlugin.imageDescriptorFromPlugin( KalypsoGisPlugin.getId(), "icons/util/img_props.gif" );
    final IWizardPage page = new ImageExportPage( configuration, "mapprops", "Export Optionen", imgDesc );

    return new IWizardPage[]
    { page };
  }
}