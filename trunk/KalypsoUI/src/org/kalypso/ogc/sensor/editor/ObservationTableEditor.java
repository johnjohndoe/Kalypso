package org.kalypso.ogc.sensor.editor;

import java.awt.Frame;
import java.io.IOException;
import java.util.Date;

import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.xml.bind.JAXBException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IFileEditorInput;
import org.kalypso.editor.AbstractEditorPart;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.ogc.sensor.tableview.swing.renderer.DateTableCellRenderer;
import org.kalypso.ogc.sensor.tableview.swing.renderer.MaskedNumberTableCellRenderer;
import org.kalypso.ogc.sensor.template.LinkedTableViewTemplate;
import org.kalypso.template.ObservationTemplateHelper;

/**
 * The Observation TableEditor.
 * 
 * @author schlienger
 */
public class ObservationTableEditor extends AbstractEditorPart
{
  private final ObservationTableModel m_model = new ObservationTableModel();

  private LinkedTableViewTemplate m_template = null;

  /**
   * @see org.kalypso.editor.AbstractEditorPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( final Composite parent )
  {
    super.createPartControl( parent );

    final JTable table = new JTable( m_model );
    table.setDefaultRenderer( Date.class, new DateTableCellRenderer() );
    table.setDefaultRenderer( Number.class, new MaskedNumberTableCellRenderer() );

    // SWT-AWT Brücke für die Darstellung von JFreeChart
    final Frame vFrame = SWT_AWT.new_Frame( new Composite( parent, SWT.RIGHT | SWT.EMBEDDED ) );

    vFrame.setVisible( true );
    table.setVisible( true );

    final JScrollPane pane = new JScrollPane( table );
    //pane.setBorder( BorderFactory.createEmptyBorder() );
    vFrame.add( pane );
  }

  /**
   * @see org.kalypso.editor.AbstractEditorPart#dispose()
   */
  public void dispose()
  {
    super.dispose();
    
    if( m_template != null )
      m_template.removeTemplateEventListener( m_model );
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
      monitor.beginTask( "Laden", 1 );
      
      m_template = ObservationTemplateHelper.loadTableViewTemplate( input.getFile() );
      m_template.addTemplateEventListener( m_model );
      m_model.setRules( m_template );
      
      monitor.worked(1);
    }
    catch( CoreException e )
    {
      e.printStackTrace();
    }
    catch( JAXBException e )
    {
      e.printStackTrace();
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
  }
}