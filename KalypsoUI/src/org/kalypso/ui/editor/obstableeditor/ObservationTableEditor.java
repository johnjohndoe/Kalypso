/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.editor.obstableeditor;

import java.awt.Frame;
import java.io.Writer;
import java.net.URL;

import javax.swing.JScrollPane;

import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IStorageEditorInput;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.eclipse.util.SetContentHelper;
import org.kalypso.ogc.sensor.tableview.TableViewTemplate;
import org.kalypso.ogc.sensor.tableview.TableViewUtils;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTable;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.ogc.sensor.template.TemplateStorage;
import org.kalypso.template.obstableview.ObstableviewType;
import org.kalypso.ui.editor.abstractobseditor.AbstractObservationEditor;

/**
 * The Observation TableEditor.
 * 
 * @author schlienger
 */
public class ObservationTableEditor extends AbstractObservationEditor
{
  protected final ObservationTable m_table;

  /**
   * Constructor: the ObservationTable is already created here because
   * of the listening functionality that needs to be set up before
   * the template gets loaded.
   * <p>
   * Doing this stuff in createPartControl would prove inadequate, because
   * the order in which createPartControl and loadIntern are called is
   * not guaranteed to be always the same.
   */
  public ObservationTableEditor( )
  {
    super( new TableViewTemplate() );
    
    m_table = new ObservationTable( (TableViewTemplate) getTemplate() );
  }
  
  /**
   * @return Returns the observation table model
   */
  public ObservationTableModel getModel( )
  {
    return (ObservationTableModel) m_table.getModel();
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

    // SWT-AWT Brücke für die Darstellung von JFreeChart
    final Frame vFrame = SWT_AWT.new_Frame( new Composite( parent, SWT.RIGHT
        | SWT.EMBEDDED ) );

    final JScrollPane pane = new JScrollPane( m_table );
    vFrame.add( pane );

    vFrame.setVisible( true );
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#dispose()
   */
  public void dispose( )
  {
    m_table.dispose();
    
    super.dispose();
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#doSaveInternal(org.eclipse.core.runtime.IProgressMonitor,
   *      org.eclipse.ui.IFileEditorInput)
   */
  protected void doSaveInternal( IProgressMonitor monitor,
      IFileEditorInput input ) throws CoreException
  {
    final TableViewTemplate template = (TableViewTemplate) getTemplate();
    if( template == null )
      return;

    final SetContentHelper helper = new SetContentHelper()
    {
      protected void write( Writer writer ) throws Throwable
      {
        final ObstableviewType type = TableViewUtils
            .buildTableTemplateXML( template );

        TableViewUtils.saveTableTemplateXML( type, writer );
      }
    };

    helper.setFileContents( input.getFile(), false, true, monitor );
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#loadInternal(org.eclipse.core.runtime.IProgressMonitor,
   *      org.eclipse.ui.IFileEditorInput)
   */
  protected void loadInternal( final IProgressMonitor monitor,
      final IStorageEditorInput input ) throws Exception
  {
    monitor.beginTask( "Tabelle-Vorlage laden", IProgressMonitor.UNKNOWN );

    final TableViewTemplate template = (TableViewTemplate) getTemplate();
    try
    {
      final IStorage storage = input.getStorage();

      if( storage instanceof TemplateStorage )
      {
        final TemplateStorage ts = (TemplateStorage) storage;
        template.addObservation( ts.getName(), ts.getContext(), ts.getHref(),
            "zml", false, null );
      }
      else
      {
        final ObstableviewType baseTemplate = TableViewUtils
            .loadTableTemplateXML( storage.getContents() );

        final String strUrl = ResourceUtilities.createURLSpec( input
            .getStorage().getFullPath() );
        template.setBaseTemplate( baseTemplate, new URL( strUrl ), false );
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      monitor.done();
    }
  }
}