package org.kalypso.ui.editor.gistableeditor;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.net.URL;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IStorageEditorInput;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.ogc.gml.table.celleditors.IFeatureModifierFactory;
import org.kalypso.template.gistableview.Gistableview;
import org.kalypso.template.gistableview.ObjectFactory;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.AbstractEditorPart;
import org.kalypso.ui.editor.gistableeditor.actions.ColumnAction;
import org.kalypso.util.command.ICommandTarget;

/**
 * <p>
 * Eclipse-Editor zum editieren der Gis-Tabellen-Templates.
 * </p>
 * 
 * <p>
 * Zeigt das ganze als Tabelendarstellung, die einzelnen Datenquellen k?nnen
 * potentiell editiert werden
 * </p>
 * 
 * @author belger
 */
public class GisTableEditor extends AbstractEditorPart implements ISelectionProvider,
    ICommandTarget
{
  private final ObjectFactory m_gistableviewFactory = new ObjectFactory();

  private final Marshaller m_marshaller;

  private LayerTableViewer m_layerTable = null;
  
  public GisTableEditor()
  {
    try
    {
      m_marshaller = m_gistableviewFactory.createMarshaller();
      m_marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
    }
    catch( final JAXBException e )
    {
      // sollte nie passieren
      e.printStackTrace();

      throw new RuntimeException( e );
    }
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#dispose()
   */
  public void dispose()
  {
    m_layerTable.dispose();

    super.dispose();
  }

  /** File must exist! */
  protected void doSaveInternal( final IProgressMonitor monitor, final IFileEditorInput input )
  {
    if( m_layerTable == null )
      return;

    try
    {
      final Gistableview tableTemplate = m_layerTable.createTableTemplate();

      // die Vorlagendatei ist klein, deswegen einfach in ein ByteArray
      // serialisieren
      final IFile file = input.getFile();

      final ByteArrayOutputStream bos = new ByteArrayOutputStream();
      final OutputStreamWriter osw = new OutputStreamWriter( bos, file.getCharset() );
      m_marshaller.marshal( tableTemplate, osw );
      
      // TODO close in finally block?
      bos.close();

      final ByteArrayInputStream bis = new ByteArrayInputStream( bos.toByteArray() );
      file.setContents( bis, false, true, monitor );

      // TODO close in finally block?
      bis.close();
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
    catch( final CoreException e )
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

    final KalypsoGisPlugin plugin = KalypsoGisPlugin.getDefault();
    final IFeatureModifierFactory factory = plugin.createFeatureTypeCellEditorFactory();
    m_layerTable = new LayerTableViewer( parent, this, factory, plugin
        .getDefaultMapSelectionID(), false );

    final MenuManager menuMgr = createSpaltenMenu( "spalten" );
    final Control viewerControl = m_layerTable.getControl();
    final Menu menu = menuMgr.createContextMenu( viewerControl );
    viewerControl.setMenu( menu );

    load();
  }

  protected final void loadInternal( final IProgressMonitor monitor, final IStorageEditorInput input)
      throws Exception
  {
    if( !(input instanceof IFileEditorInput) )
      throw new IllegalArgumentException( "Kann nur Dateien laden" );
    
    if( m_layerTable == null )
      return;

    monitor.beginTask( "Vorlage laden", 1000 );

    final Gistableview tableTemplate = GisTemplateHelper.loadGisTableview( ((IFileEditorInput) input).getFile() );

    final IFile inputFile = ( (IFileEditorInput)getEditorInput() ).getFile();
    final URL context = ResourceUtilities.createURL( inputFile );

    final LayerTableViewer viewer = m_layerTable;
    getEditorSite().getShell().getDisplay().asyncExec( new Runnable()
    {
      public void run()
      {
        viewer.applyTableTemplate( tableTemplate, context );
      }
    } );

    monitor.worked( 1000 );
  }

  public LayerTableViewer getLayerTable()
  {
    return m_layerTable;
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void addSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_layerTable.addSelectionChangedListener( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
   */
  public ISelection getSelection()
  {
    return m_layerTable.getSelection();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void removeSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_layerTable.removeSelectionChangedListener( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( final ISelection selection )
  {
    m_layerTable.setSelection( selection );
  }

  public void appendSpaltenActions( final IMenuManager manager )
  {
    final IKalypsoFeatureTheme theme = m_layerTable.getTheme();
    if( theme == null )
      return;
    
    final FeatureTypeProperty[] ftps = theme.getFeatureType().getProperties();
    for( int i = 0; i < ftps.length; i++ )
      manager.add( new ColumnAction( this, m_layerTable, ftps[i].getName() ) );
  }

  public MenuManager createSpaltenMenu( final String id )
  {
    final MenuManager menuMgr = new MenuManager( "Spalten", id );
    menuMgr.setRemoveAllWhenShown( true );
    menuMgr.addMenuListener( new IMenuListener()
    {
      public void menuAboutToShow( final IMenuManager manager )
      {
        appendSpaltenActions( manager );
      }
    } );

    return menuMgr;
  }
}