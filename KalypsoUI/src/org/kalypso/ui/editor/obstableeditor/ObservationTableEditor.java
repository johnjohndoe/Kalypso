package org.kalypso.ui.editor.obstableeditor;

import java.awt.Frame;
import java.net.URL;
import java.util.Collection;
import java.util.Iterator;

import javax.swing.JScrollPane;

import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.ITableViewTheme;
import org.kalypso.ogc.sensor.tableview.ObservationTableTemplateFactory;
import org.kalypso.ogc.sensor.tableview.impl.LinkedTableViewTemplate;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTable;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.ogc.sensor.template.ITemplateEventListener;
import org.kalypso.ogc.sensor.template.TemplateEvent;
import org.kalypso.ogc.sensor.template.TemplateStorage;
import org.kalypso.ogc.sensor.zml.ZmlObservation;
import org.kalypso.template.obstableview.ObstableviewType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.AbstractEditorPart;

/**
 * The Observation TableEditor.
 * 
 * @author schlienger
 */
public class ObservationTableEditor extends AbstractEditorPart implements
    ITemplateEventListener
{
  protected final LinkedTableViewTemplate m_template = new LinkedTableViewTemplate();

  protected ObservationTableModel m_model = null;

  protected ObservationTable m_table = null;

  protected ObsTableOutlinePage m_outline = null;

  private boolean m_dirty = false;

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

    m_model = new ObservationTableModel();
    m_model.setRules( m_template );

    m_table = new ObservationTable( m_model );
    m_template.addTemplateEventListener( m_table );

    // SWT-AWT Brücke für die Darstellung von JFreeChart
    final Frame vFrame = SWT_AWT.new_Frame( new Composite( parent, SWT.RIGHT
        | SWT.EMBEDDED ) );

    m_template.addTemplateEventListener( this );

    vFrame.setVisible( true );
    m_table.setVisible( true );

    final JScrollPane pane = new JScrollPane( m_table );
    //pane.setBorder( BorderFactory.createEmptyBorder() );
    vFrame.add( pane );
  }

  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( Class adapter )
  {
    if( adapter == IContentOutlinePage.class )
    {
      // lazy loading
      if( m_outline == null || m_outline.getControl() != null
          && m_outline.getControl().isDisposed() )
      {
        // TODO check if ok to dispose when not null
        if( m_outline != null )
          m_outline.dispose();

        m_outline = new ObsTableOutlinePage();
        m_outline.setTemplate( m_template );
      }

      return m_outline;
    }
    return null;
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#dispose()
   */
  public void dispose( )
  {
    if( m_template != null )
    {
      m_template.removeTemplateEventListener( this );
      m_template.removeTemplateEventListener( m_table );
      m_template.dispose();
    }

    if( m_outline != null )
      m_outline.dispose();

    super.dispose();
  }

  /**
   * Speichert z.Z. nur die Daten.
   * 
   * @see org.kalypso.ui.editor.AbstractEditorPart#doSaveInternal(org.eclipse.core.runtime.IProgressMonitor,
   *      org.eclipse.ui.IFileEditorInput)
   */
  protected void doSaveInternal( IProgressMonitor monitor,
      IFileEditorInput input ) throws CoreException
  {
    final Collection themes = m_template.getThemes();

    for( final Iterator it = themes.iterator(); it.hasNext(); )
    {
      final ITableViewTheme theme = (ITableViewTheme) it.next();

      boolean dirty = false;

      for( Iterator itcol = theme.getColumns().iterator(); itcol.hasNext(); )
      {
        dirty = ((ITableViewColumn) itcol.next()).isDirty();

        // at least one col dirty?
        if( dirty )
          break;
      }

      final IObservation obs = theme.getObservation();

      if( dirty && obs instanceof ZmlObservation )
      {
        final String msg = "Sie haben Änderungen in " + obs.getName()
            + " vorgenommen. Wollen \n" + "Sie die Änderungen übernehmen?";

        final boolean b = MessageDialog.openQuestion( getSite().getShell(),
            "Änderungen speichern", msg );

        if( b )
        {
          for( Iterator itcol = theme.getColumns().iterator(); itcol.hasNext(); )
            ((ITableViewColumn) itcol.next()).setDirty( false );

          final ITuppleModel values = m_model.getValues( theme.getColumns() );

          try
          {
            obs.setValues( values );

            m_template.saveObservation( obs, monitor );
          }
          catch( Exception e )
          {
            e.printStackTrace();
            throw new CoreException( new Status( IStatus.ERROR,
                KalypsoGisPlugin.getId(), 0, "ZML speichern", e ) );
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
      final IStorageEditorInput input ) throws Exception
  {
    monitor.beginTask( "Vorlage Laden", IProgressMonitor.UNKNOWN );

    try
    {
      final IStorage storage = input.getStorage();

      if( storage instanceof TemplateStorage )
      {
        final TemplateStorage ts = (TemplateStorage) storage;
        m_template.addObservation( ts.getName(), ts.getContext(), ts.getHref(),
            "zml", false, null );
      }
      else
      {
        final ObstableviewType baseTemplate = ObservationTableTemplateFactory
            .loadTableTemplateXML( storage.getContents() );

        final String strUrl = ResourceUtilities.createURLSpec( input
            .getStorage().getFullPath() );
        m_template.setBaseTemplate( baseTemplate, new URL( strUrl ) );
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

  /**
   * @see org.kalypso.ogc.sensor.template.ITemplateEventListener#onTemplateChanged(org.kalypso.ogc.sensor.template.TemplateEvent)
   */
  public void onTemplateChanged( TemplateEvent evt )
  {
    if( evt.isType( TemplateEvent.TYPE_ADD | TemplateEvent.TYPE_REMOVE
        | TemplateEvent.TYPE_REMOVE_ALL ) )
    {
      m_dirty = true;

      getSite().getShell().getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          fireDirty();
        }
      } );
    }
  }

  protected void resetDirty( )
  {
    m_dirty = false;
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#isDirty()
   */
  public boolean isDirty( )
  {
    return m_dirty;
  }
}