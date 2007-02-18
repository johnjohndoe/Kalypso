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
package org.kalypso.ui.editor.abstractobseditor;

import java.net.URL;

import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.ogc.sensor.diagview.DiagView;
import org.kalypso.ogc.sensor.diagview.DiagViewUtils;
import org.kalypso.ogc.sensor.tableview.TableView;
import org.kalypso.ogc.sensor.tableview.TableViewUtils;
import org.kalypso.ogc.sensor.template.IObsViewEventListener;
import org.kalypso.ogc.sensor.template.ObsView;
import org.kalypso.ogc.sensor.template.ObsViewEvent;
import org.kalypso.ogc.sensor.template.ObsViewUtils;
import org.kalypso.ogc.sensor.template.TemplateStorage;
import org.kalypso.template.obsdiagview.Obsdiagview;
import org.kalypso.template.obstableview.Obstableview;
import org.kalypso.ui.editor.AbstractEditorPart;

/**
 * AbstractObsEditor
 * 
 * @author schlienger
 */
public abstract class AbstractObservationEditor extends AbstractEditorPart implements IEditorPart, IObsViewEventListener
{
  private final ObsView m_view;

  private ObservationEditorOutlinePage m_outline = null;

  public AbstractObservationEditor( final ObsView view )
  {
    m_view = view;
    m_view.addObsViewEventListener( this );
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_view != null )
    {
      m_view.removeObsViewListener( this );
      m_view.dispose();
    }

    if( m_outline != null )
      m_outline.dispose();

    super.dispose();
  }

  /**
   * @return template
   */
  public ObsView getView( )
  {
    return m_view;
  }

  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  @Override
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
  @Override
  protected void loadInternal( final IProgressMonitor monitor, final IStorageEditorInput input )
  {
    monitor.beginTask( "Vorlage laden", IProgressMonitor.UNKNOWN );

    final ObsView view = getView();

    IStatus status = null;

    try
    {
      final IStorage storage = input.getStorage();

      if( storage instanceof TemplateStorage )
      {
        final TemplateStorage ts = (TemplateStorage) storage;

        loadObservation( ts.getContext(), ts.getHref() );
      }
      else
      {
        final boolean sync = true;
        
        if( view instanceof DiagView )
        {
          final Obsdiagview baseTemplate = DiagViewUtils.loadDiagramTemplateXML( storage.getContents() );

          final String strUrl = ResourceUtilities.createURLSpec( input.getStorage().getFullPath() );
          status = DiagViewUtils.applyXMLTemplate( (DiagView) getView(), baseTemplate, new URL( strUrl ), sync, null );
        }
        else if( view instanceof TableView )
        {
          final Obstableview baseTemplate = TableViewUtils.loadTableTemplateXML( storage.getContents() );

          final String strUrl = ResourceUtilities.createURLSpec( input.getStorage().getFullPath() );
          status = TableViewUtils.applyXMLTemplate( (TableView) getView(), baseTemplate, new URL( strUrl ), sync, null );
        }
        else
          throw new IllegalArgumentException( "Kann Vorlage nicht �ffnen, Typ wird nicht unterst�tzt." );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      status = StatusUtilities.statusFromThrowable( e );
    }
    finally
    {
      monitor.done();
    }

    if( status != null && !status.isOK() )
    {
      final IStatus finalStatus = status;

      getSite().getShell().getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          ErrorDialog.openError( getSite().getShell(), "Vorlage �ffnen", "Siehe Details", finalStatus );
        }
      } );
    }
  }

  public void loadObservation( final URL context, final String href )
  {
    if( m_view != null )
      m_view.loadObservation( context, href, false, ObsViewUtils.DEFAULT_ITEM_NAME, new ObsView.ItemData( true, null, null ) );
  }

  /**
   * @see org.kalypso.ogc.sensor.template.IObsViewEventListener#onObsViewChanged(org.kalypso.ogc.sensor.template.ObsViewEvent)
   */
  public void onObsViewChanged( final ObsViewEvent evt )
  {
    if( evt.getType() != ObsViewEvent.TYPE_ITEM_DATA_CHANGED )
      fireDirty();
  }

  /**
   * @see org.kalypso.ogc.sensor.template.IObsViewEventListener#onPrintObsView(org.kalypso.ogc.sensor.template.ObsViewEvent)
   */
  public void onPrintObsView( final ObsViewEvent evt )
  {
    // empty
  }
}