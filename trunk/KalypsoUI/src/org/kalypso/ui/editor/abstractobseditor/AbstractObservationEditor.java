package org.kalypso.ui.editor.abstractobseditor;

import java.net.URL;

import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.ogc.sensor.diagview.DiagView;
import org.kalypso.ogc.sensor.diagview.DiagViewUtils;
import org.kalypso.ogc.sensor.tableview.TableView;
import org.kalypso.ogc.sensor.tableview.TableViewUtils;
import org.kalypso.ogc.sensor.template.NameUtils;
import org.kalypso.ogc.sensor.template.ObsView;
import org.kalypso.ogc.sensor.template.TemplateStorage;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.template.obstableview.ObstableviewType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.AbstractEditorPart;

/**
 * AbstractObsEditor
 * 
 * @author schlienger
 */
public abstract class AbstractObservationEditor extends AbstractEditorPart
{
  private final ObsView m_view;

  private ObservationEditorOutlinePage m_outline = null;

  public AbstractObservationEditor( ObsView view )
  {
    m_view = view;
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#dispose()
   */
  public void dispose()
  {
    if( m_view != null )
      m_view.dispose();

    if( m_outline != null )
      m_outline.dispose();

    super.dispose();
  }

  /**
   * @return template
   */
  public ObsView getView()
  {
    return m_view;
  }

  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( Class adapter )
  {
    if( adapter == IContentOutlinePage.class )
    {
      // lazy loading
      if( m_outline == null || m_outline.getControl() != null && m_outline.getControl().isDisposed() )
      {
        // dispose when not null (not sure if this is ok)
        if( m_outline != null )
          m_outline.dispose();

        m_outline = new ObservationEditorOutlinePage( this );
        m_outline.setView( m_view );
      }

      return m_outline;
    }
    return null;
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#loadInternal(org.eclipse.core.runtime.IProgressMonitor,
   *      org.eclipse.ui.IFileEditorInput)
   */
  protected void loadInternal( final IProgressMonitor monitor, final IStorageEditorInput input )
  {
    monitor.beginTask( "Vorlage laden", IProgressMonitor.UNKNOWN );

    final ObsView view = getView();

    try
    {
      final IStorage storage = input.getStorage();

      if( storage instanceof TemplateStorage )
      {
        final TemplateStorage ts = (TemplateStorage)storage;
        if( view instanceof DiagView )
          ( (DiagView)view ).setTitle( ts.getName() );

        loadObservation( ts.getContext(), ts.getHref() );
      }
      else
      {
        if( view instanceof DiagView )
        {
          final ObsdiagviewType baseTemplate = DiagViewUtils.loadDiagramTemplateXML( storage.getContents() );

          final String strUrl = ResourceUtilities.createURLSpec( input.getStorage().getFullPath() );
          final MultiStatus status = new MultiStatus( KalypsoGisPlugin.getId(), 0, "Vorlage: " + storage.getName(),
              null );
          DiagViewUtils.applyXMLTemplate( (DiagView)getView(), baseTemplate, new URL( strUrl ), false, status );
        }
        else if( view instanceof TableView )
        {
          final ObstableviewType baseTemplate = TableViewUtils.loadTableTemplateXML( storage.getContents() );

          final String strUrl = ResourceUtilities.createURLSpec( input.getStorage().getFullPath() );
          final MultiStatus status = new MultiStatus( KalypsoGisPlugin.getId(), 0, "Vorlage: " + storage.getName(),
              null );
          TableViewUtils.applyXMLTemplate( (TableView)getView(), baseTemplate, new URL( strUrl ), false, status );
        }
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

  public void loadObservation( final URL context, final String href )
  {
    if( m_view != null )
      m_view.loadObservation( context, href, false, null, NameUtils.DEFAULT_ITEM_NAME,
          new ObsView.ItemData( true, null ) );
  }

}