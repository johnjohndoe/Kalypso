package org.kalypso.ogc.sensor.editor;

import java.awt.Frame;
import java.net.MalformedURLException;
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
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.ITableViewTemplate;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.ogc.sensor.tableview.swing.renderer.DateTableCellRenderer;
import org.kalypso.ogc.sensor.tableview.swing.renderer.MaskedNumberTableCellRenderer;
import org.kalypso.ogc.sensor.template.ColumnPair;
import org.kalypso.ogc.sensor.template.TableViewTemplateFactory;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypso.util.status.MaskedNumber;

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

  private ITableViewTemplate m_tableview = null;

  private ITableViewColumn[] m_cols;

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
    table.setDefaultRenderer( MaskedNumber.class, new MaskedNumberTableCellRenderer() );

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
    try
    {
      m_tableview = TableViewTemplateFactory.createTemplate( input.getFile() );
    }
    catch( MalformedURLException e )
    {
      // TODO: handling
      throw new RuntimeException( e );
    }

    // the rules will be used for rendering, they are used in the table cell renderer
    m_model.setRules( m_tableview.getRules() );
    
    m_cols = m_tableview.getColumns();

    monitor.beginTask( "Spalten initialisieren", m_cols.length );

    // TODO: bl�der trick hier, ich teste ob es sich um ColumPair handelt, dies
    // ist sicherlich nicht so sch�n.
    for( int i = 0; i < m_cols.length; i++ )
    {
      if( m_cols[i] instanceof ColumnPair )
        ((ColumnPair)m_cols[i]).startBorrowObjectJob( m_pool, this );
      
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
      // TODO: auch nicht sch�n: feste Test ob es sich um ColumnPair handelt
      if( m_cols[i] instanceof ColumnPair && ((ColumnPair)m_cols[i]).isInvalid( oldObject ) )
      {
        IObservationProvider p = (IObservationProvider)m_pool.getObject( ((ColumnPair)m_cols[i]).getKey(),
            new NullProgressMonitor() );

        ((ColumnPair)m_cols[i]).setProvider( p );

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