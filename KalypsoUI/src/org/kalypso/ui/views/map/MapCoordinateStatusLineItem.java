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
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.eclipse.ui.menus.WorkbenchWindowControlContribution;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.listeners.IMapPanelListener;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author kuch
 */
public class MapCoordinateStatusLineItem extends WorkbenchWindowControlContribution
{
  private static final String MAP_POSITION_TEXT = "Map position: %.2f / %.2f";

  private MapCoordinateWindowListener m_windowListener;

  private IMapPanelListener m_mapPanelListener;

  protected ImageHyperlink m_lnk;

  /**
   * @see org.eclipse.jface.action.ContributionItem#dispose()
   */
  @Override
  public void dispose( )
  {
    PlatformUI.getWorkbench().removeWindowListener( m_windowListener );

    super.dispose();
  }

  /**
   * @see org.eclipse.jface.action.ControlContribution#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createControl( final Composite parent )
  {
    m_windowListener = new MapCoordinateWindowListener( this );
    PlatformUI.getWorkbench().addWindowListener( m_windowListener );

    final Composite composite = new Composite( parent, SWT.NONE );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.marginWidth = gridLayout.horizontalSpacing = gridLayout.verticalSpacing = 0;
    composite.setLayout( gridLayout );

    m_lnk = new ImageHyperlink( composite, SWT.NONE );
    final Image image = KalypsoGisPlugin.getImageProvider().getImage( ImageProvider.DESCRIPTORS.STATUS_LINE_SHOW_MAP_COORDS );
    m_lnk.setImage( image );
    m_lnk.setText( "Map position: " );
    m_lnk.setToolTipText( "Mouse map position" );
    final GridData gridData = new GridData( GridData.FILL, GridData.FILL, false, false );
    gridData.widthHint = 300;
    m_lnk.setLayoutData( gridData );
    m_lnk.setEnabled( false );

    m_lnk.setVisible( false );

    return composite;
  }

  public void setEnabled( final MapPanel panel, final boolean enabled )
  {
    if( !m_lnk.isDisposed() )
      m_lnk.setVisible( enabled );

    if( enabled && (m_mapPanelListener == null) )
    {
      m_mapPanelListener = new IMapPanelListener()
      {
        public void onExtentChanged( final MapPanel source, final GM_Envelope oldExtent, final GM_Envelope newExtent )
        {
        }

        public void onMapModelChanged( final MapPanel source, final IMapModell oldModel, final IMapModell newModel )
        {
        }

        public void onMessageChanged( final MapPanel source, final String message )
        {
        }

        public void onMouseMoveEvent( final MapPanel source, final GM_Point gmPoint, final Point mousePosition )
        {
          if( !m_lnk.isDisposed() )
          {
            final UIJob job = new UIJob( "updating position label..." )
            {
              @Override
              public IStatus runInUIThread( final IProgressMonitor monitor )
              {
                double x = gmPoint.getX();
                double y = gmPoint.getY();

                m_lnk.setText( String.format( MapCoordinateStatusLineItem.MAP_POSITION_TEXT, x, y ) );
                m_lnk.layout();

                return Status.OK_STATUS;
              }
            };

            job.schedule();
          }
        }

      };

      panel.addMapPanelListener( m_mapPanelListener );
    }
    else if( m_mapPanelListener != null )
    {
      panel.removeMapPanelListener( m_mapPanelListener );
      m_mapPanelListener = null;
    }

  }
}
