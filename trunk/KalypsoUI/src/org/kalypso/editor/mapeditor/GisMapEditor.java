package org.kalypso.editor.mapeditor;

import java.awt.Frame;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.apache.commons.pool.KeyedObjectPool;
import org.deegree.graphics.sld.UserStyle;
import org.eclipse.core.internal.resources.ResourceException;
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
import org.kalypso.ogc.IMapModellProvider;
import org.kalypso.ogc.IMapPanelProvider;
import org.kalypso.ogc.MapModell;
import org.kalypso.ogc.MapPanel;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.ObjectFactory;
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
public class GisMapEditor extends AbstractEditorPart implements IMapPanelProvider, IMapModellProvider
{
  private final ObjectFactory m_gisviewObjectFactory = new ObjectFactory();

  private final Unmarshaller m_unmarshaller;

  private GisMapOutlinePage m_outlinePage = null;

  private final MapPanel myMapPanel;

  private MapModell m_mapModell;

  public GisMapEditor()
  {
    myMapPanel = new MapPanel( KalypsoGisPlugin.getDefault().getCoordinatesSystem() );

    try
    {
      m_unmarshaller = m_gisviewObjectFactory.createUnmarshaller();
//      m_marshaller = m_gisviewObjectFactory.createMarshaller();
    }
    catch( JAXBException e )
    {
      // sollte nie passieren
      e.printStackTrace();

      throw new RuntimeException( e );
    }
  }


  public void dispose()
  {
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
      m_outlinePage = new GisMapOutlinePage( this );
      
      m_outlinePage.setMapModell(m_mapModell);

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
//      final ByteArrayInputStream bis = new ByteArrayInputStream( bos.toByteArray() );
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
  public void createPartControl( Composite parent )
  {
    super.createPartControl( parent );

    // create MapPanel
    final Frame virtualFrame = SWT_AWT
        .new_Frame( new Composite( parent, SWT.RIGHT | SWT.EMBEDDED ) );
    //     final Frame virtualFrame = new Frame("test");
    virtualFrame.setVisible( true );
    myMapPanel.setVisible( true );
    virtualFrame.add( myMapPanel );
  }

  protected void load()
  {
    final IFileEditorInput input = (IFileEditorInput)getEditorInput();

    Gismapview gisview = null;

    try
    {
      gisview = (Gismapview)m_unmarshaller.unmarshal( input.getStorage().getContents() );
    }
    catch( final ResourceException re )
    {
      // TODO: handle ResourceException: e.g. Resource is out of sync
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }
    catch( final JAXBException e )
    {
      //      KalypsoGisPlugin.getDefault().getOutputLogger().log(
      // Level.SEVERE, "Fehler
      // beim Laden der Datei", e, getSite().getShell() );

      e.printStackTrace();
    }

    final IProject project = ( (IFileEditorInput)getEditorInput() ).getFile().getProject();

    final KeyedObjectPool layerPool = KalypsoGisPlugin.getDefault().getPool( KalypsoFeatureLayer.class );
    final KeyedObjectPool stylePool = KalypsoGisPlugin.getDefault().getPool( UserStyle.class );

    m_mapModell = new MapModell( gisview, KalypsoGisPlugin.getDefault()
            .getCoordinatesSystem(), layerPool, stylePool, project, myMapPanel );
    
    myMapPanel.setMapModell( m_mapModell );
    if( m_outlinePage != null )
      m_outlinePage.setMapModell(m_mapModell);
    
    setDirty( false );

    setContentDescription( input.getFile().getName() );
    setPartName( input.getFile().getName() );
  }

  public void showProperties( final LayerType layer )
  {
    MessageDialog.openInformation( getEditorSite().getShell(), "Themeneigenschaften",
        "Leider noch nicht implementiert" );

    layer.getClass();
  }

//  public List getLayerlist()
//  {
//    return m_gisview == null ? null : m_gisview.getLayers().getLayer();
//  }

//  /**
//   * @see org.kalypso.xml.types.ILayerTypeFactory#createNewLayer()
//   */
//  public LayerType createNewLayer()
//  {
//    try
//    {
//      final GisviewLayerType layer = m_typeObjectFactory.createGisviewLayerType();
//
//      if( new Propsdiag( getEditorSite().getShell(), layer ).open() != Window.OK )
//        return null;
//
//      // hm, wer macht die id?
//      layer.setId( "TODO:" );
//
//      // TODO: should do propsdialog
//      layer.setName( "<neu>" );
//
//      layer.setVisible( false );
//
//      return layer;
//    }
//    catch( JAXBException e )
//    {
//      // sollte nie passieren
//      e.printStackTrace();
//    }
//
//    return null;
//  }
  
  public MapModell getMapModell()
  {
    return m_mapModell;
  }
  public MapPanel getMapPanel()
  {
      return myMapPanel;
  }
}