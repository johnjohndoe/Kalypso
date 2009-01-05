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
package org.kalypso.ogc.gml.map;

import java.awt.Cursor;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.kalypso.ogc.gml.map.listeners.IMapPanelListener;
import org.kalypso.ogc.gml.map.listeners.IMapPanelPaintListener;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.widgets.IWidgetManager;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * @author Gernot Belger
 */
public interface IMapPanel extends ISelectionProvider
{
  void addMapPanelListener( final IMapPanelListener l );

  void addPaintListener( final IMapPanelPaintListener pl );

  void addSelectionChangedListener( final ISelectionChangedListener listener );

  void dispose( );

  GM_Envelope getBoundingBox( );

  double getCurrentScale( );

  IMapModell getMapModell( );

  String getMessage( );

  GeoTransform getProjection( );

  ISelection getSelection( );

  IFeatureSelectionManager getSelectionManager( );

  IWidgetManager getWidgetManager( );

  void removeMapPanelListener( final IMapPanelListener l );

  void removePaintListener( final IMapPanelPaintListener pl );

  void removeSelectionChangedListener( final ISelectionChangedListener listener );

  /**
   * Same as {@link #setBoundingBox(GM_Envelope, true, true)}
   */
  void setBoundingBox( final GM_Envelope wishBBox );

  /**
   * Same as {@link #setBoundingBox(GM_Envelope, boolean, true)}
   */
  void setBoundingBox( final GM_Envelope wishBBox, final boolean useHistory );

  /**
   * Set the bounding box of the panel.
   *
   * @param wishBBox
   *          The new extent, will be adapted so it fits into the current size of the panel. Call
   *          {@link #getBoundingBox()} to get the adjusted extent.
   * @param useHistory
   *          If <code>true</code>, the last extent will be added to the extent history.
   * @param invalidateMap
   *          If <code>true</code>, the contents of the map will be invalidated and all layers will be freshly painted.
   */
  void setBoundingBox( final GM_Envelope wishBBox, final boolean useHistory, final boolean invalidateMap );

  void setMapModell( final IMapModell modell );

  void setMessage( final String message );

  void setSelection( final ISelection selection );

  ExtentHistory getExtentHistory( );

  void setStatus( final IStatus status );

  IStatus getStatus( );

  void fireMouseMouveEvent( int mouseX, int mouseY );

  Rectangle getScreenBounds( );

  void setCursor( Cursor cursor );

  int getWidth( );

  int getHeight( );

  /**
   * Repaints the map. The themes do not get repainted but their buffers will be painted.<br>
   * Only the widget and any {@link IMapPanelPaintListener}s are really repainted.
   */
  void repaintMap( );

  /**
   * Invalidates the map buffer.
   */
  void invalidateMap( );

  /**
   * The current state of this map as an image. No selected items nor widget-stuff.
   */
  BufferedImage getMapImage( );
}