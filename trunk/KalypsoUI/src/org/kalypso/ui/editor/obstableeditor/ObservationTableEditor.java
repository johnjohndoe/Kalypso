package org.kalypso.ui.editor.obstableeditor;

import java.awt.Frame;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IFileEditorInput;
import org.kalypso.java.lang.CatchRunnable;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.impl.LinkedTableViewTemplate;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTable;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.AbstractEditorPart;

/**
 * The Observation TableEditor.
 * 
 * @author schlienger
 */
public class ObservationTableEditor extends AbstractEditorPart
{
  protected final ObservationTableModel m_model = new ObservationTableModel();

  protected LinkedTableViewTemplate m_template = null;

  protected ObservationTable m_table;

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
      IFileEditorInput input ) throws CoreException
  {
    final List columns = m_template.getColumns();

    for( final Iterator it = columns.iterator(); it.hasNext(); )
    {
      final ITableViewColumn column = (ITableViewColumn) it.next();

      if( column.isDirty() )
      {
        final String msg = "Sie haben �nderungen in "
            + column.getName()
            + " vorgenommen. Wollen \n"
            + "Sie die �nderungen �bernehmen und die grundliegende Datei speichern?";
        final boolean b = MessageDialog.openQuestion( getSite().getShell(),
            "�nderungen speichern", msg );

        if( b )
        {
          final IObservation obs = column.getObservation();

          column.setDirty( false );

          final List cols2save = new ArrayList();
          cols2save.add( column );

          // reset dirty flag for other columns that have the same observation
          for( final Iterator itTmp = columns.iterator(); itTmp.hasNext(); )
          {
            final ITableViewColumn col = (ITableViewColumn) itTmp.next();

            if( col.getObservation().equals( obs ) && col.isDirty() )
            {
              col.setDirty( false );
              cols2save.add( col );
            }
          }

          final ITuppleModel values = m_model.getValues( cols2save );

          try
          {
            obs.setValues( values );

            m_template.saveObservation( obs, monitor );
          }
          catch( Exception e )
          {
            e.printStackTrace();
            throw new CoreException( new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0, "ZML speichern", e ) );
          }
        }
      }
    }
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#loadInternal(org.eclipse.core.runtime.IProgressMonitor,
   *      org.eclipse.ui.IFileEditorInput)
   */
  protected void loadInternal( final IProgressMonitor monitor,
      final IFileEditorInput input )
  {
    monitor.beginTask( "Laden", IProgressMonitor.UNKNOWN );

    final CatchRunnable runnable = new CatchRunnable()
    {
      public void runIntern( ) throws Throwable
      {
        m_template = LinkedTableViewTemplate.loadTableViewTemplate( input
            .getFile() );
        m_template.addTemplateEventListener( m_table );
        m_model.setRules( m_template );
      }
    };

    try
    {
      SwingUtilities.invokeAndWait( runnable );
      if( runnable.getThrown() != null )
        throw runnable.getThrown();
    }
    catch( Throwable e ) // generic throwable caught for simplicity
    {
      e.printStackTrace();
    }
    finally
    {
      monitor.done();
    }
  }
}