package org.kalypso.editor.tableeditor;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
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
import org.kalypso.eclipse.jface.viewers.ICellEditorFactory;
import org.kalypso.editor.AbstractEditorPart;
import org.kalypso.editor.tableeditor.actions.ColumnAction;
import org.kalypso.editor.tableeditor.layerTable.LayerTableViewer;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.PoolableKalypsoFeatureTheme;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.template.GisTemplateHelper;
import org.kalypso.template.gistableview.Gistableview;
import org.kalypso.template.gistableview.ObjectFactory;
import org.kalypso.util.command.ICommandTarget;

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
    }
    catch( final JAXBException e )
    {
      // sollte nie passieren
      e.printStackTrace();

      throw new RuntimeException( e );
    }
  }

  /**
   * @see org.kalypso.editor.AbstractEditorPart#dispose()
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

      // die Vorlagendatei ist klein, deswegen einfach in ein ByteArray serialisieren
      final IFile file = input.getFile();

      final ByteArrayOutputStream bos = new ByteArrayOutputStream();
      final OutputStreamWriter osw = new OutputStreamWriter( bos, file.getCharset() );
      m_marshaller.marshal( tableTemplate, osw );
      bos.close();

      final ByteArrayInputStream bis = new ByteArrayInputStream( bos.toByteArray() );
      file.setContents( bis, false, true, monitor );

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
  public void createPartControl( Composite parent )
  {
    super.createPartControl( parent );

    final ICellEditorFactory factory = KalypsoGisPlugin.getDefault()
        .getFeatureTypeCellEditorFactory();
    m_layerTable = new LayerTableViewer( parent, factory, -1 );

    final MenuManager menuMgr = createSpaltenMenu();
    final Control viewerControl = m_layerTable.getControl();
    final Menu menu = menuMgr.createContextMenu( viewerControl );
    viewerControl.setMenu( menu );

    load();
  }

  protected final void loadInternal( final IProgressMonitor monitor, final IFileEditorInput input )
      throws Exception
  {
    if( m_layerTable == null )
      return;

    monitor.beginTask( "Vorlage laden", 1000 );

    
    final Gistableview tableTemplate = GisTemplateHelper.loadGisTableview( input.getFile() );

    final IProject project = ( (IFileEditorInput)getEditorInput() ).getFile().getProject();
    final LayerTableViewer viewer = m_layerTable;
    getEditorSite().getShell().getDisplay().asyncExec( new Runnable()
    {
      public void run()
      {
        viewer.applyTableTemplate( tableTemplate, project );
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
    final PoolableKalypsoFeatureTheme theme = m_layerTable.getTheme();
    if( theme == null )
      return;

    final KalypsoFeatureLayer layer = (KalypsoFeatureLayer)theme.getLayer();
    if( layer == null )
      return;

    final FeatureTypeProperty[] ftps = layer.getFeatureType().getProperties();
    for( int i = 0; i < ftps.length; i++ )
      manager.add( new ColumnAction( this, m_layerTable, ftps[i].getName() ) );
  }

  public MenuManager createSpaltenMenu()
  {
    final MenuManager menuMgr = new MenuManager( "Spalten" );
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