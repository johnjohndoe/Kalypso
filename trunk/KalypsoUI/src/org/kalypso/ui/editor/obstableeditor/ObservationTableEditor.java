package org.kalypso.ui.editor.obstableeditor;

import java.awt.Frame;

import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IFileEditorInput;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTable;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.ogc.sensor.tableview.template.LinkedTableViewTemplate;
import org.kalypso.ogc.sensor.template.ObservationTemplateHelper;
import org.kalypso.ui.editor.AbstractEditorPart;

/**
 * The Observation TableEditor.
 * 
 * @author schlienger
 */
public class ObservationTableEditor extends AbstractEditorPart
{
  private final ObservationTableModel m_model = new ObservationTableModel();

  private LinkedTableViewTemplate m_template = null;

  private ObservationTable m_table;

  /**
   * @return Returns the model.
   */
  public ObservationTableModel getModel( )
  {
    return m_model;
  }
  
  /**
   * @return Returns the table.
   */
  public ObservationTable getTable( )
  {
    return m_table;
  }
  
  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( final Composite parent )
  {
    super.createPartControl( parent );

    m_table = new ObservationTable( m_model );

    // SWT-AWT Br�cke f�r die Darstellung von JFreeChart
    final Frame vFrame = SWT_AWT.new_Frame( new Composite( parent, SWT.RIGHT
        | SWT.EMBEDDED ) );

    vFrame.setVisible( true );
    m_table.setVisible( true );

    final JScrollPane pane = new JScrollPane( m_table );
    //pane.setBorder( BorderFactory.createEmptyBorder() );
    vFrame.add( pane );
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#dispose()
   */
  public void dispose( )
  {
    if( m_template != null )
    {
      m_template.removeTemplateEventListener( m_table );
      m_template.dispose();
    }
    
    super.dispose();
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#doSaveInternal(org.eclipse.core.runtime.IProgressMonitor,
   *      org.eclipse.ui.IFileEditorInput)
   */
  protected void doSaveInternal( IProgressMonitor monitor,
      IFileEditorInput input )
  {
    final ITableViewColumn[] columns = m_template.getColumns();
    
    for( int i = 0; i < columns.length; i++ )
    {
      if( columns[i].isDirty() )
      {
        final String msg = "Sie haben �nderungen in " + columns[i].getName() + " vorgenommen. Wollen \n" +
        		"Sie die �nderungen �bernehmen und die grundliegende Datei speichern?";
        final boolean b = MessageDialog.openConfirm( getSite().getShell(), "�nderungen speichern", msg );
        
        if( b )
        {
          
        }
      }
    }
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#load()
   */
  protected void loadInternal( final IProgressMonitor monitor,
      final IFileEditorInput input )
  {
    monitor.beginTask( "Laden", IProgressMonitor.UNKNOWN );

    final Runnable runnable = new Runnable()
    {
      public void run( )
      {
        try
        {
          m_template = ObservationTemplateHelper.loadTableViewTemplate( input
              .getFile() );
          m_template.addTemplateEventListener( m_table );
          m_model.setRules( m_template );
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
      }
    };

    try
    {
      SwingUtilities.invokeAndWait( runnable );
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      e.printStackTrace();
    }
    finally
    {
      monitor.done();
    }
  }
}