package org.kalypso.ogc.sensor.editor;

import java.awt.Frame;
import java.util.Date;
import java.util.Hashtable;
import java.util.Map;

import javax.swing.JScrollPane;
import javax.swing.JTable;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IFileEditorInput;
import org.kalypso.editor.AbstractEditorPart;
import org.kalypso.ogc.sensor.IObservationProvider;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.ogc.sensor.template.ColumnPair;
import org.kalypso.ogc.sensor.template.TableViewTemplate;
import org.kalypso.ogc.sensor.view.DateTableCellRenderer;
import org.kalypso.ogc.sensor.view.DoubleTableCellRenderer;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.ResourcePool;

/**
 * The Observation TableEditor.
 * 
 * @author schlienger
 */
public class ObservationTableEditor extends AbstractEditorPart implements IPoolListener
{
  private final ObservationTableModel m_model = new ObservationTableModel();

  private final ResourcePool m_pool = KalypsoGisPlugin.getDefault().getPool(
      IObservationProvider.class );

  private TableViewTemplate m_tableview = null;

  private ColumnPair[] m_cols;

  private Map m_providers = new Hashtable();

  /**
   * Constructor
   */
  public ObservationTableEditor()
  {
    m_pool.addPoolListener( this );
  }

  /**
   * @see org.kalypso.editor.AbstractEditorPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( Composite parent )
  {
    super.createPartControl( parent );

    JTable table = new JTable( m_model );
    table.setDefaultRenderer( Date.class, new DateTableCellRenderer() );
    table.setDefaultRenderer( Double.class, new DoubleTableCellRenderer() );

    // SWT-AWT Brücke für die Darstellung von JFreeChart
    Frame vFrame = SWT_AWT.new_Frame( new Composite( parent, SWT.RIGHT | SWT.EMBEDDED ) );

    vFrame.setVisible( true );
    table.setVisible( true );
    vFrame.add( new JScrollPane( table ) );
  }

  /**
   * @see org.kalypso.editor.AbstractEditorPart#doSaveInternal(org.eclipse.core.runtime.IProgressMonitor,
   *      org.eclipse.ui.IFileEditorInput)
   */
  protected void doSaveInternal( IProgressMonitor monitor, IFileEditorInput input )
  {}

  /**
   * @see org.kalypso.editor.AbstractEditorPart#load()
   */
  protected void load()
  {
    final IFileEditorInput input = (IFileEditorInput)getEditorInput();

    m_tableview = new TableViewTemplate( input.getFile() );

    m_cols = m_tableview.getColumns();

    for( int i = 0; i < m_cols.length; i++ )
      m_cols[i].startBorrowObjectJob( m_pool, this );

    setDirty( false );

    setContentDescription( input.getFile().getName() );
    setPartName( input.getFile().getName() );
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#onObjectInvalid(java.lang.Object,
   *      boolean)
   */
  public void onObjectInvalid( Object oldObject, boolean bCannotReload ) throws Exception
  {
    boolean changed = false;
    
    for( int i = 0; i < m_cols.length; i++ )
    {
      if( m_cols[i].isInvalid( oldObject ) )
      {
        IObservationProvider p = (IObservationProvider)m_pool.getObject( m_cols[i].getKey(), new NullProgressMonitor() );
        
        m_cols[i].setProvider( p );
        
        m_providers.put( m_cols[i], p );
        
        changed = true;
      }
    }
    
    if( changed && m_providers.size() == m_cols.length )
    {
      m_model.setColumns( m_cols, null );
    }
  }
}