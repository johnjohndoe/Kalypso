package org.kalypso.editor.mapeditor;

import java.awt.Frame;
import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.deegree.graphics.sld.UserStyle;
import org.eclipse.core.internal.resources.ResourceException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
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
import org.kalypso.ogc.gml.KalypsoTheme;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.GismapviewType;
import org.kalypso.template.gismapview.ObjectFactory;
import org.kalypso.template.gismapview.GismapviewType.LayersType;
import org.kalypso.template.gistableview.GistableviewType.LayerType;
import org.kalypso.template.types.StyledLayerType.StyleType;
import org.kalypso.util.pool.BorrowObjectJob;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;

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
public class GisMapEditor extends AbstractEditorPart implements IMapPanelProvider,
    IMapModellProvider, IPoolListener
{
  private final ResourcePool m_layerPool = KalypsoGisPlugin.getDefault().getPool(
      KalypsoFeatureLayer.class );

  private final ResourcePool m_stylePool = KalypsoGisPlugin.getDefault().getPool( UserStyle.class );

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
  public void createPartControl( Composite parent )
  {
    super.createPartControl( parent );

    // create MapPanel
    final Frame virtualFrame = SWT_AWT
        .new_Frame( new Composite( parent, SWT.RIGHT | SWT.EMBEDDED ) );
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

    m_mapModell = new MapModell( myMapPanel, KalypsoGisPlugin.getDefault().getCoordinatesSystem() );

    loadGisview( gisview );

    myMapPanel.setMapModell( m_mapModell );

    if( m_outlinePage != null )
      m_outlinePage.setMapModell( m_mapModell );

    setDirty( false );

    setContentDescription( input.getFile().getName() );
    setPartName( input.getFile().getName() );
  }

  private void loadGisview( final Gismapview gisview )
  {
    if( gisview == null )
      return;
    
    final IProject project = ( (IFileEditorInput)getEditorInput() ).getFile().getProject();

    final LayersType layerListType = gisview.getLayers();
    final List layerList = layerListType.getLayer();

    for( int i = 0; i < layerList.size(); i++ )
    {
      final GismapviewType.LayersType.Layer layerType = (GismapviewType.LayersType.Layer)layerList
          .get( i );

      final String layerName = layerType.getName();
      final IPoolableObjectType layerKey = new PoolableObjectType( layerType.getLinktype(),
          layerType.getHref(), project );

      final Job layerJob = new BorrowObjectJob( "Thema laden", m_layerPool, this, layerKey,
          new LoadLayerHelper( layerType, layerKey ) );
      layerJob.setRule( new LayerRule( layerName  ) );
      layerJob.schedule();

      final List stylesList = layerType.getStyle();

      for( int is = 0; is < stylesList.size(); is++ )
      {
        final StyleType styleType = ( (StyleType)stylesList.get( is ) );

        final IPoolableObjectType styleKey = new PoolableObjectType( styleType.getLinktype(),
            styleType.getHref(), project );

        final LoadStyleHelper lsh = new LoadStyleHelper( styleType, styleKey, layerName );

        final Job styleJob = new BorrowObjectJob( "Style wird geladen", m_stylePool, this,
            styleKey, lsh );
        styleJob.setRule( new StyleRule( layerName ) );
        styleJob.schedule();
      }
    }
  }

  public void showProperties( final LayerType layer )
  {
    MessageDialog.openInformation( getEditorSite().getShell(), "Themeneigenschaften",
        "Leider noch nicht implementiert" );

    layer.getClass();
  }

  public MapModell getMapModell()
  {
    return m_mapModell;
  }

  /**
   * @see org.kalypso.ogc.IMapPanelProvider#getMapPanel()
   */
  public MapPanel getMapPanel()
  {
    return myMapPanel;
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#onObjectInvalid(java.lang.Object,
   *      boolean)
   */
  public void onObjectInvalid( final Object oldObject, final boolean bCannotReload )
      throws Exception
  {
    if( oldObject instanceof LoadLayerHelper )
    {
      // verzoegertes laden, also neuen layer erzeugen
      final LoadLayerHelper layerHelper = (LoadLayerHelper)oldObject;

      final KalypsoFeatureLayer layer = (KalypsoFeatureLayer)m_layerPool.getObject(
          layerHelper.layerKey, new NullProgressMonitor() );

      final KalypsoTheme theme = new KalypsoTheme( layer, layerHelper.layerType.getName() );

      try
      {
        m_mapModell.addTheme( theme );
      }
      catch( final Exception ex )
      {
        System.out.println( "could not add Theme" );
        ex.printStackTrace();
      }
    }
    else if( oldObject instanceof LoadStyleHelper )
    {
      final LoadStyleHelper styleHelper = (LoadStyleHelper)oldObject;

      final KalypsoUserStyle style = (KalypsoUserStyle)m_stylePool.getObject( styleHelper.styleKey,
          new NullProgressMonitor() );

      // TODO thema finden und style hinzufuegen

      // TODO was passiert, wenn der name doppelt vorkommt
      final KalypsoTheme[] allThemes = m_mapModell.getAllThemes();
      for( int i = 0; i < allThemes.length; i++ )
      {
        if( allThemes[i].getName().equals( styleHelper.themeName ) )
          allThemes[i].addStyle( style );
      }
    }
    else
    {
      // TODO !!!!
      // hier kommen wir an, wenn sich ein objekt aendett, also das objekt suchen und ersetzen
    }
  }

  private static final class LoadLayerHelper
  {
    public final GismapviewType.LayersType.Layer layerType;

    public final IPoolableObjectType layerKey;

    public LoadLayerHelper( final GismapviewType.LayersType.Layer layerTypeArg,
        final IPoolableObjectType layerKeyArg )
    {
      layerType = layerTypeArg;
      layerKey = layerKeyArg;
    }
  }

  /**
   * @author belger
   */
  private class LoadStyleHelper
  {
    public final IPoolableObjectType styleKey;

    public final StyleType styleType;

    public final String themeName;

    public LoadStyleHelper( final StyleType styleTypeArg, final IPoolableObjectType styleKeyArg,
        final String themeNameArg )
    {
      styleType = styleTypeArg;
      styleKey = styleKeyArg;
      themeName = themeNameArg;
    }
  }

  class LayerRule implements ISchedulingRule
  {
    private String m_name;

    public LayerRule( final String name )
    {
      m_name = name;
    }

    public boolean isConflicting( final ISchedulingRule rule )
    {
      return ( rule instanceof StyleRule && ((StyleRule)rule).getName().equals( m_name ) );
    }

    public boolean contains( ISchedulingRule rule )
    {
      return rule == this;
    }

    public String getName()
    {
      return m_name;
    }
  }

  class StyleRule implements ISchedulingRule
  {
    private String m_name;

    public StyleRule( final String name )
    {
      m_name = name;
    }

    public String getName()
    {
      return m_name;
    }

    public boolean isConflicting( final ISchedulingRule rule )
    {
      return ( rule instanceof LayerRule && ((LayerRule)rule).getName().equals( m_name ) );
    }

    public boolean contains( ISchedulingRule rule )
    {
      return rule == this;
    }
  }

  
}