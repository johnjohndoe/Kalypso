package org.kalypso.editor.tableeditor;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.deegree.model.feature.FeatureType;
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
import org.kalypso.eclipse.jface.viewers.ICellEditorFactory;
import org.kalypso.editor.AbstractEditorPart;
import org.kalypso.editor.tableeditor.layerTable.LayerTable;
import org.kalypso.editor.tableeditor.layerTable.LayerTableModel;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.event.ModellEventListener;
import org.kalypso.ogc.gml.PoolableKalypsoFeatureTheme;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.template.gistableview.Gistableview;
import org.kalypso.template.gistableview.ObjectFactory;
import org.kalypso.template.gistableview.GistableviewType.LayerType;
import org.kalypso.template.gistableview.GistableviewType.LayerType.ColumnType;
import org.kalypso.util.command.ICommandTarget;
import org.kalypso.util.pool.PoolableObjectType;

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
 * TODO: release keys after use! TODO: resource listener?
 * 
 * @author belger
 */
public class GisTableEditor extends AbstractEditorPart implements ISelectionProvider,
    ModellEventListener, ICommandTarget
{
  private final ObjectFactory m_gistableviewFactory = new ObjectFactory();

  private final Unmarshaller m_unmarshaller;

  private final Marshaller m_marshaller;

  private LayerTable m_layerTable = null;

  private Gistableview m_tableview = null;

  private PoolableKalypsoFeatureTheme m_theme;

  public GisTableEditor()
  {
    try
    {
      m_unmarshaller = m_gistableviewFactory.createUnmarshaller();
      m_marshaller = m_gistableviewFactory.createMarshaller();
    }
    catch( JAXBException e )
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
    if( m_theme != null )
    {
      m_theme.removeModellListener( this );
      m_theme = null;
    }

    super.dispose();
  }

  protected void doSaveInternal( final IProgressMonitor monitor, final IFileEditorInput input )
  {
    if( m_layerTable == null )
      return;

    try
    {
      final Gistableview gistableview = m_gistableviewFactory.createGistableview();
      final LayerType layer = m_gistableviewFactory.createGistableviewTypeLayerType();

      final PoolableObjectType key = m_theme.getLayerKey();
      layer.setId( "1" );
      layer.setHref( key.getSourceAsString() );
      layer.setLinktype( key.getType() );
      layer.setActuate( "onRequest" );
      layer.setType( "simple" );

      gistableview.setLayer( layer );

      final List columns = layer.getColumn();

      final LayerTableModel model = m_layerTable.getModel();

      final FeatureTypeProperty[] ftps = model.getFeatureType().getProperties();
      for( int i = 0; i < ftps.length; i++ )
      {
        final FeatureTypeProperty ftp = ftps[i];
        if( model.isColumn( ftp ) )
        {
          final ColumnType columnType = m_gistableviewFactory
              .createGistableviewTypeLayerTypeColumnType();

          columnType.setName( ftp.getName() );
          columnType.setEditable( model.isEditable( ftp ) );
          columnType.setWidth( m_layerTable.getWidth( ftp ) );

          columns.add( columnType );
        }
      }

      final ByteArrayOutputStream bos = new ByteArrayOutputStream();
      m_marshaller.marshal( gistableview, bos );
      bos.close();

      final ByteArrayInputStream bis = new ByteArrayInputStream( bos.toByteArray() );

      final IFile file = input.getFile();
      if( file.exists() )
        file.setContents( bis, false, true, monitor );
      else
        file.create( bis, false, monitor );

      bis.close();
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

    final ICellEditorFactory factory = KalypsoGisPlugin.getDefault()
        .getFeatureTypeCellEditorFactory();
    m_layerTable = new LayerTable( parent, this, factory );

    load();
  }

  protected final void loadInternal()
  {
    if( m_layerTable == null )
      return;
    final IFileEditorInput input = (IFileEditorInput)getEditorInput();

    try
    {
      m_tableview = (Gistableview)m_unmarshaller.unmarshal( input.getStorage().getContents() );
    }
    catch( final ResourceException re )
    {
      // TODO: handle ResourceException: e.g. Resource is out of sync
      re.printStackTrace();
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
    }

    if( m_theme != null )
    {
      m_theme.removeModellListener( this );
      m_theme.dispose();
    }
    m_theme = null;

    m_layerTable.setModel( null );
    if( m_tableview == null )
      return;

    final IProject project = ( (IFileEditorInput)getEditorInput() ).getFile().getProject();
    
    m_theme = new PoolableKalypsoFeatureTheme( m_tableview.getLayer(), project );
    
    m_theme.addModellListener( this );

    setContentDescription( input.getFile().getName() );
    setPartName( input.getFile().getName() );
  }

  public LayerTable getLayerTable()
  {
    return m_layerTable;
  }

  public PoolableKalypsoFeatureTheme getTheme()
  {
    return m_theme;
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
  public void setSelection( ISelection selection )
  {
    m_layerTable.setSelection( selection );
  }

  /**
   * @see org.kalypso.ogc.event.ModellEventListener#onModellChange(org.kalypso.ogc.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    if( modellEvent != null && modellEvent.getEventSource() == m_theme && modellEvent.getType() == ModellEvent.FULL_CHANGE )
    {
      final FeatureType featureType = m_theme.getLayer().getFeatureType();

      final LayerType layerType = m_tableview.getLayer();
      final List columnList = layerType.getColumn();
      final LayerTableModel.Column[] columns = new LayerTableModel.Column[columnList.size()];
      int count = 0;
      for( final Iterator iter = columnList.iterator(); iter.hasNext(); )
      {
        final ColumnType ct = (ColumnType)iter.next();
        final FeatureTypeProperty ftp = featureType.getProperty( ct.getName() );
        if( ftp != null )
          columns[count++] = new LayerTableModel.Column( ftp, ct.getWidth(), ct.isEditable() );
      }

      getEditorSite().getShell().getDisplay().asyncExec( new Runnable()
      {
        public void run()
        {
          final LayerTable layerTable = getLayerTable();
          if( !layerTable.isDisposed() )
            layerTable.setModel( new LayerTableModel( getTheme(), columns ) );
        }
      } );
    }
  }
}