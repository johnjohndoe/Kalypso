package org.kalypso.ui.editor.mapeditor;

import java.awt.Frame;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import javax.xml.bind.JAXBException;

import org.deegree.model.geometry.GM_Envelope;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapPanelProvider;
import org.kalypso.ogc.gml.mapmodel.MapPanel;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gistableview.GistableviewType.LayerType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.AbstractEditorPart;

/**
 * <p>
 * Eclipse-Editor zum editieren der GML-Gis-Templates.
 * </p>
 * 
 * <p>
 * Zeigt das ganze als Kartendarstellug, die einzelnen Datenquellen k?nnen
 * potentiell editiert werden
 * </p>
 * 
 * <p>
 * Implementiert {@link org.kalypso.util.command.ICommandManager}f?r die Undo
 * und Redo Action. Gibt alles an den DefaultCommandManager weiter, es wird
 * zus?tzlich eine Aktualisierung der View bei jeder Aktion durchgef?hrt
 * </p>
 * 
 * @author belger
 */
public class GisMapEditor extends AbstractEditorPart implements IMapPanelProvider
{
  private GisMapOutlinePage m_outlinePage = null;

  private final MapPanel myMapPanel;

  private IMapModell m_mapModell;

  public GisMapEditor()
  {
    final KalypsoGisPlugin plugin = KalypsoGisPlugin.getDefault();
    myMapPanel = new MapPanel( this, plugin.getCoordinatesSystem(),
        plugin.getDefaultMapSelectionID() );
  }

  public void dispose()
  {
    setMapModell( null );

    if( m_outlinePage != null )
      m_outlinePage.dispose();

    super.dispose();
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

    return super.getAdapter( adapter );
  }

  protected void doSaveInternal( final IProgressMonitor monitor, final IFileEditorInput input )
  {
    if( m_mapModell == null )
      return;

    try
    {
      monitor.beginTask( "Kartenvorlage speichern", 2000 );
      getMapPanel().getBoundingBox();
      Gismapview modellTemplate = ( (GisTemplateMapModell)m_mapModell )
          .createGismapTemplate( getMapPanel().getBoundingBox() );

      final ByteArrayOutputStream bos = new ByteArrayOutputStream();

      GisTemplateHelper.saveGisMapView( modellTemplate, bos );

      final ByteArrayInputStream bis = new ByteArrayInputStream( bos.toByteArray() );
      bos.close();
      monitor.worked( 1000 );

      final IFile file = input.getFile();
      if( file.exists() )
        file.setContents( bis, false, true, monitor );
      else
        file.create( bis, false, monitor );

      bis.close();
      monitor.done();
    }
    catch( JAXBException e )
    {
      e.printStackTrace();
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
    catch( CoreException e )
    {
      e.printStackTrace();
    }

  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( final Composite parent )
  {
    super.createPartControl( parent );

    // create MapPanel
    final Frame virtualFrame = SWT_AWT
        .new_Frame( new Composite( parent, SWT.RIGHT | SWT.EMBEDDED ) );
    virtualFrame.setVisible( true );
    myMapPanel.setVisible( true );
    virtualFrame.add( myMapPanel );
  }

  protected final void loadInternal( final IProgressMonitor monitor, final IFileEditorInput input )
      throws Exception, CoreException
  {
    // prepare for exception
    setMapModell( null );

    monitor.beginTask( "Kartenvorlage laden", 2000 );

    final Gismapview gisview = GisTemplateHelper.loadGisMapView( input.getFile() );

    monitor.worked( 1000 );

    final IProject project = ( (IFileEditorInput)getEditorInput() ).getFile().getProject();

    final IMapModell mapModell = new GisTemplateMapModell( gisview, project, KalypsoGisPlugin
        .getDefault().getCoordinatesSystem() );
    setMapModell( mapModell );

    GM_Envelope env = GisTemplateHelper.getBoundingBox( gisview );

    getMapPanel().setBoundingBox( env );
    monitor.done();
  }

  private void setMapModell( final IMapModell mapModell )
  {
    m_mapModell = mapModell;

    myMapPanel.setMapModell( m_mapModell );
    if( m_outlinePage != null )
      m_outlinePage.setMapModell( m_mapModell );
  }

  public void showProperties( final LayerType layer )
  {
    MessageDialog.openInformation( getEditorSite().getShell(), "Themeneigenschaften",
        "Leider noch nicht implementiert" );

    layer.getClass();
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapPanelProvider#getMapPanel()
   */
  public MapPanel getMapPanel()
  {
    return myMapPanel;
  }
}