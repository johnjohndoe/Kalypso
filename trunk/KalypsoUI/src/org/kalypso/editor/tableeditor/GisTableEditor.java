package org.kalypso.editor.tableeditor;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.deegree.graphics.Layer;
import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.core.internal.resources.ResourceException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IFileEditorInput;
import org.kalypso.editor.AbstractEditorPart;
import org.kalypso.editor.tableeditor.layerTable.LayerTable;
import org.kalypso.editor.tableeditor.layerTable.LayerTableModel;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.xml.tableview.ObjectFactory;
import org.kalypso.xml.tableview.Tableview;
import org.kalypso.xml.types.TableviewLayerType;
import org.kalypso.xml.types.TableviewLayerType.ColumnType;

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
public class GisTableEditor extends AbstractEditorPart implements ISelectionProvider
{
  private final ObjectFactory m_tableviewObjectFactory = new ObjectFactory();

  private final org.kalypso.xml.types.ObjectFactory m_typeFactory = new org.kalypso.xml.types.ObjectFactory();

  private final Unmarshaller m_unmarshaller;

  private final Marshaller m_marshaller;

  private LayerTable m_layerTable = null;

  private String m_source;

  private String m_type;

  public GisTableEditor()
  {
    try
    {
      m_unmarshaller = m_tableviewObjectFactory.createUnmarshaller();
      m_marshaller = m_tableviewObjectFactory.createMarshaller();
    }
    catch( JAXBException e )
    {
      // sollte nie passieren
      e.printStackTrace();

      throw new RuntimeException( e );
    }
  }
  



  protected void doSaveInternal( final IProgressMonitor monitor, final IFileEditorInput input )
  {
    if( m_layerTable == null )
      return;

    try
    {
      final TableviewLayerType tableviewLayerType = m_typeFactory.createTableviewLayerType();
      tableviewLayerType.setId( "1" );
      tableviewLayerType.setSource( m_source );
      tableviewLayerType.setType( m_type );
      final List columns = tableviewLayerType.getColumn();
      final FeatureTypeProperty[] properties = m_layerTable.getModel().getVisibleProperties();
      for( int i = 0; i < properties.length; i++ )
      {
        final ColumnType columnType = m_typeFactory.createTableviewLayerTypeColumnType();
        columnType.setName( properties[i].getName() );
        columns.add( columnType );
      }

      final Tableview tableview = m_tableviewObjectFactory.createTableview();
      tableview.setLayer( tableviewLayerType );

      final ByteArrayOutputStream bos = new ByteArrayOutputStream();
      m_marshaller.marshal( tableview, bos );
      bos.close();

      final ByteArrayInputStream bis = new ByteArrayInputStream( bos.toByteArray() );

      final IFile file = input.getFile();
      if( file.exists() )
        file.setContents( bis, false, true, monitor );
      else
        file.create( bis, false, monitor );

      bis.close();

      setDirty( false );
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
  public void createPartControl( Composite parent )
  {
    super.createPartControl( parent );
    
    m_layerTable = new LayerTable( parent, this );

    load();
  }

  protected void load()
  {
    if( m_layerTable == null )
      return;
    final IFileEditorInput input = (IFileEditorInput)getEditorInput();

    Tableview tableview = null;

    try
    {
      tableview = (Tableview)m_unmarshaller.unmarshal( input.getStorage().getContents() );
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
      e.printStackTrace();
    }

    final IProject project = ( (IFileEditorInput)getEditorInput() ).getFile().getProject();

    m_layerTable.setModel( null );
    m_source = null;
    m_type = null;
    if( tableview == null )
      return;
    
    final KalypsoGisPlugin plugin = KalypsoGisPlugin.getDefault();

    final TableviewLayerType layerType = tableview.getLayer();

    m_source = layerType.getSource();
    m_type = layerType.getType();

    try
    {
      final KalypsoFeatureLayer layer = (KalypsoFeatureLayer)plugin.getPool( Layer.class )
          .borrowObject(
              new PoolableObjectType( m_type, m_source, project ) );

      final List cols = new ArrayList();
      for( final Iterator iter = layerType.getColumn().iterator(); iter.hasNext(); )
        cols.add( ( (ColumnType)iter.next() ).getName() );

      m_layerTable.setModel( new LayerTableModel( layer, (String[])cols.toArray( new String[cols
          .size()] ) ) );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    setDirty( false );

    setContentDescription( input.getFile().getName() );
    setPartName( input.getFile().getName() );
  }

  public LayerTable getLayerTable()
  {
    return m_layerTable;
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void addSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_layerTable.addSelectionChangedListener(listener);
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
  m_layerTable.removeSelectionChangedListener(listener);
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( ISelection selection )
  {
    m_layerTable.setSelection(selection);
  }
}