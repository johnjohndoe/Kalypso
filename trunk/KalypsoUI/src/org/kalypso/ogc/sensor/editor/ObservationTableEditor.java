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
import org.kalypso.ogc.sensor.renderer.DateTableCellRenderer;
import org.kalypso.ogc.sensor.renderer.MaskedNumberTableCellRenderer;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.ogc.sensor.template.ColumnPair;
import org.kalypso.ogc.sensor.template.TableViewTemplate;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.ResourcePool;

/**
 * The Observation TableEditor.
 * <p>
 * <b>Hinweise zu den internen Verbrauch von BitMask f�r den Tagging von
 * Werte (Themengegliedert)</b>:
 * <pre>
 * G�ltigkeit
 * 0x01 - F�r Berechnung ok
 * 0x02 - F�r Berechnung eventuell nicht geeignet
 * 0x04 - F�r Berechnung nicht geeignet
 * 
 * Benutzer Eingabe
 * 0x08 - ben�tigt
 * 0x0F - optional
 * 0x10 - gesperrt
 * 
 * Typ
 * 0x12 - gemessene
 * 0x14 - vorhergesagte
 * 
 * �nderungen vom Benutzer
 * 0x18 - vom Benutzer nicht ge�ndert
 * 0x1F - vom Benutzer ge�ndert
 * </pre> 
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
    table.setDefaultRenderer( Number.class, new MaskedNumberTableCellRenderer( null ) );

    // SWT-AWT Br�cke f�r die Darstellung von JFreeChart
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
  {
  // TODO: implement it
  }

  /**
   * @see org.kalypso.editor.AbstractEditorPart#load()
   */
  protected void loadInternal( final IProgressMonitor monitor, final IFileEditorInput input )
  {
    m_tableview = new TableViewTemplate( input.getFile() );

    m_cols = m_tableview.getColumns();

    monitor.beginTask( "Spalten initialisieren", m_cols.length );

    for( int i = 0; i < m_cols.length; i++ )
    {
      m_cols[i].startBorrowObjectJob( m_pool, this );
      monitor.worked( 1 );
    }
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#onObjectInvalid(org.kalypso.util.pool.ResourcePool,
   *      org.kalypso.util.pool.IPoolableObjectType, java.lang.Object, boolean)
   */
  public void onObjectInvalid( final ResourcePool pool, final IPoolableObjectType key,
      final Object oldObject, boolean bCannotReload ) throws Exception
  {
    boolean changed = false;

    for( int i = 0; i < m_cols.length; i++ )
    {
      if( m_cols[i].isInvalid( oldObject ) )
      {
        IObservationProvider p = (IObservationProvider)m_pool.getObject( m_cols[i].getKey(),
            new NullProgressMonitor() );

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