package org.kalypso.editor.tableeditor;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IFileEditorInput;
import org.kalypso.eclipse.jface.viewers.ICellEditorFactory;
import org.kalypso.editor.AbstractEditorPart;
import org.kalypso.editor.tableeditor.layerTable.LayerTableViewer;
import org.kalypso.plugin.KalypsoGisPlugin;
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
 * TODO: resource listener?
 * 
 * @author belger
 */
public class GisTableEditor extends AbstractEditorPart implements ISelectionProvider,
    ICommandTarget
{
  private final ObjectFactory m_gistableviewFactory = new ObjectFactory();

  private final Unmarshaller m_unmarshaller;

  private final Marshaller m_marshaller;

  private LayerTableViewer m_layerTable = null;

  public GisTableEditor()
  {
    try
    {
      m_unmarshaller = m_gistableviewFactory.createUnmarshaller();
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

  protected void doSaveInternal( final IProgressMonitor monitor, final IFileEditorInput input )
  {
    if( m_layerTable == null )
      return;
    /*
     * TODO try { final Gistableview gistableview =
     * m_gistableviewFactory.createGistableview(); final LayerType layer =
     * m_gistableviewFactory.createGistableviewTypeLayerType();
     * 
     * final PoolableObjectType key = m_theme.getLayerKey(); layer.setId( "1" );
     * layer.setHref( key.getSourceAsString() ); layer.setLinktype(
     * key.getType() ); layer.setActuate( "onRequest" ); layer.setType( "simple" );
     * 
     * gistableview.setLayer( layer );
     * 
     * final List columns = layer.getColumn();
     * 
     * final LayerTableModel model = m_layerTable.getModel();
     * 
     * final FeatureTypeProperty[] ftps =
     * model.getFeatureType().getProperties(); for( int i = 0; i < ftps.length;
     * i++ ) { final FeatureTypeProperty ftp = ftps[i]; if( model.isColumn( ftp ) ) {
     * final ColumnType columnType = m_gistableviewFactory
     * .createGistableviewTypeLayerTypeColumnType();
     * 
     * columnType.setName( ftp.getName() ); columnType.setEditable(
     * model.isEditable( ftp ) ); columnType.setWidth( m_layerTable.getWidth(
     * ftp ) );
     * 
     * columns.add( columnType ); } }
     * 
     * final ByteArrayOutputStream bos = new ByteArrayOutputStream();
     * m_marshaller.marshal( gistableview, bos ); bos.close();
     * 
     * final ByteArrayInputStream bis = new ByteArrayInputStream(
     * bos.toByteArray() );
     * 
     * final IFile file = input.getFile(); if( file.exists() ) file.setContents(
     * bis, false, true, monitor ); else file.create( bis, false, monitor );
     * 
     * bis.close(); } catch( JAXBException e ) { e.printStackTrace(); } catch(
     * IOException e ) { e.printStackTrace(); } catch( CoreException e ) {
     * e.printStackTrace(); }
     *  
     */
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( Composite parent )
  {
    super.createPartControl( parent );

    final ICellEditorFactory factory = KalypsoGisPlugin.getDefault()
        .getFeatureTypeCellEditorFactory();
    m_layerTable = new LayerTableViewer( parent, factory );

    load();
  }

  protected final void loadInternal( final IProgressMonitor monitor, final IFileEditorInput input )
      throws Exception, CoreException
  {
    if( m_layerTable == null )
      return;

    monitor.beginTask( "Vorlage laden", 1000 );

    final Gistableview tableTemplate = (Gistableview)m_unmarshaller.unmarshal( input.getStorage()
        .getContents() );

    final IProject project = ( (IFileEditorInput)getEditorInput() ).getFile().getProject();

    getEditorSite().getShell().getDisplay().asyncExec( new Runnable()
    {
      public void run()
      {
        m_layerTable.applyTableTemplate( tableTemplate, project );
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
}