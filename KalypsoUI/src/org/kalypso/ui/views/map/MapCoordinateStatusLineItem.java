/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ui.views.map;

import java.awt.Point;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.eclipse.ui.menus.WorkbenchWindowControlContribution;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.eclipse.ui.partlistener.AdapterPartListener;
import org.kalypso.contribs.eclipse.ui.partlistener.EditorFirstAdapterFinder;
import org.kalypso.contribs.eclipse.ui.partlistener.IAdapterEater;
import org.kalypso.contribs.eclipse.ui.partlistener.IAdapterFinder;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.listeners.IMapPanelListener;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * This item displays map coordinates in the status line.
 * 
 * @author Dirk Kuch
 */
public class MapCoordinateStatusLineItem extends WorkbenchWindowControlContribution implements IAdapterEater<MapPanel>, IMapPanelListener
{
  private final class UpdateLabelJob extends UIJob
  {
    private GM_Point m_gmPoint;

    public UpdateLabelJob( String name )
    {
      super( name );
    }

    public void setGmPoint( GM_Point gmPoint )
    {
      m_gmPoint = gmPoint;
    }

    @Override
    public IStatus runInUIThread( IProgressMonitor monitor )
    {
      if( m_gmPoint != null && !m_label.isDisposed() ) // Have to check twice, because meanwhile it could have been
      // disposed
      {
        double x = m_gmPoint.getX();
        double y = m_gmPoint.getY();

        m_label.setText( String.format( MapCoordinateStatusLineItem.MAP_POSITION_TEXT, x, y ) );
        m_label.getParent().layout();
      }

      return Status.OK_STATUS;
    }
  }

  protected static String MAP_POSITION_TEXT = "%.2f / %.2f";

  private IAdapterFinder<MapPanel> m_closeFinder = new EditorFirstAdapterFinder<MapPanel>();

  private IAdapterFinder<MapPanel> m_initFinder = m_closeFinder;

  protected AdapterPartListener<MapPanel> m_adapterListener = new AdapterPartListener<MapPanel>( MapPanel.class, this, m_initFinder, m_closeFinder );

  protected Label m_label;

  private Composite m_composite;

  private MapPanel m_panel;

  private UpdateLabelJob m_updateLabelJob;

  /**
   * @see org.eclipse.jface.action.ContributionItem#dispose()
   */
  @Override
  public void dispose( )
  {
    m_adapterListener.dispose();

    super.dispose();
  }

  /**
   * @see org.eclipse.jface.action.ControlContribution#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createControl( Composite parent )
  {
    /* The composite. */
    m_composite = new Composite( parent, SWT.NONE );
    GridLayout gridLayout = new GridLayout( 2, false );
    gridLayout.marginHeight = 0;
    gridLayout.marginWidth = 0;
    m_composite.setLayout( gridLayout );

    /* The target image. */
    ImageHyperlink lnk = new ImageHyperlink( m_composite, SWT.NONE );
    Image image = KalypsoGisPlugin.getImageProvider().getImage( ImageProvider.DESCRIPTORS.STATUS_LINE_SHOW_MAP_COORDS );
    lnk.setImage( image );
    lnk.setEnabled( false );
    lnk.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, true ) );

    /* The label. */
    m_label = new Label( m_composite, SWT.NONE );
    m_label.setToolTipText( "Mouse map position" );
    GridData gridData = new GridData( GridData.FILL, GridData.CENTER, true, true );
    gridData.widthHint = 175;
    m_label.setLayoutData( gridData );

    m_composite.addDisposeListener( new DisposeListener()
    {
      public void widgetDisposed( DisposeEvent e )
      {
        m_adapterListener.dispose();
      }
    } );

    IWorkbenchPage activePage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
    if( activePage != null )
      m_adapterListener.init( activePage );

    m_updateLabelJob = new UpdateLabelJob( "Updating position label ..." );

    return m_composite;
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.partlistener.IAdapterEater#setAdapter(org.eclipse.ui.IWorkbenchPart,
   *      java.lang.Object)
   */
  public void setAdapter( IWorkbenchPart part, MapPanel adapter )
  {
    if( !m_composite.isDisposed() )
      m_composite.setVisible( adapter != null );

    if( m_panel != null )
      m_panel.removeMapPanelListener( this );

    m_panel = adapter;

    if( m_panel != null )
      m_panel.addMapPanelListener( this );
  }

  /**
   * @see org.kalypso.ogc.gml.map.listeners.IMapPanelListener#onExtentChanged(org.kalypso.ogc.gml.map.MapPanel,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  public void onExtentChanged( MapPanel source, GM_Envelope oldExtent, GM_Envelope newExtent )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.map.listeners.IMapPanelListener#onMapModelChanged(org.kalypso.ogc.gml.map.MapPanel,
   *      org.kalypso.ogc.gml.mapmodel.IMapModell, org.kalypso.ogc.gml.mapmodel.IMapModell)
   */
  public void onMapModelChanged( MapPanel source, IMapModell oldModel, IMapModell newModel )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.map.listeners.IMapPanelListener#onMessageChanged(org.kalypso.ogc.gml.map.MapPanel,
   *      java.lang.String)
   */
  public void onMessageChanged( MapPanel source, String message )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.map.listeners.IMapPanelListener#onMouseMoveEvent(org.kalypso.ogc.gml.map.MapPanel,
   *      org.kalypsodeegree.model.geometry.GM_Point, java.awt.Point)
   */
  public void onMouseMoveEvent( MapPanel source, final GM_Point gmPoint, Point mousePosition )
  {
    if( m_updateLabelJob != null )
      m_updateLabelJob.cancel();

    if( m_label != null && !m_label.isDisposed() )
    {
      m_updateLabelJob.setGmPoint( gmPoint );
      m_updateLabelJob.schedule();
    }
  }
}
