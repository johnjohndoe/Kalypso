package org.kalypso.editor.mapeditor;

import java.awt.Frame;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.kalypso.editor.AbstractEditorPart;
import org.kalypso.ogc.IMapModell;
import org.kalypso.ogc.IMapPanelProvider;
import org.kalypso.ogc.MapPanel;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.template.GisTemplateHelper;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gistableview.GistableviewType.LayerType;

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

  private static final int SELECTION_ID = 0x01;

  public GisMapEditor()
  {
    myMapPanel = new MapPanel( this, KalypsoGisPlugin.getDefault().getCoordinatesSystem(), SELECTION_ID );
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
  // TODO: do it!
  //    if( m_gisview == null )
  //      return;
  //
  //    try
  //    {
  //      final ByteArrayOutputStream bos = new ByteArrayOutputStream();
  //      m_marshaller.marshal( m_gisview, bos );
  //      bos.close();
  //
  //      final ByteArrayInputStream bis = new ByteArrayInputStream(
  // bos.toByteArray()
  // );
  //
  //      final IFile file = input.getFile();
  //      if( file.exists() )
  //        file.setContents( bis, false, true, monitor );
  //      else
  //        file.create( bis, false, monitor );
  //
  //      bis.close();
  //
  //      setDirty( false );
  //    }
  //    catch( JAXBException e )
  //    {
  //      e.printStackTrace();
  //    }
  //    catch( IOException e )
  //    {
  //      e.printStackTrace();
  //    }
  //    catch( CoreException e )
  //    {
  //      e.printStackTrace();
  //    }
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
   * @see org.kalypso.ogc.IMapPanelProvider#getMapPanel()
   */
  public MapPanel getMapPanel()
  {
    return myMapPanel;
  }
}