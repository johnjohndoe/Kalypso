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
package org.kalypso.ogc.gml.mapmodel;

import java.awt.Graphics;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.kalypso.commons.i18n.I10nString;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * TODO: getScale etc, hier rausschmeissen! die Umrechnung zwischen Bildschirm und Geokoordinaten ist Aufgabe des
 * MapPanel, die paint Mehtode hier sollte bereits mit Geokoordinaten arbeiten d.h. der Grafik-Kontext wird schon mit
 * umgerechneten Koordinaten übergeben
 * 
 * @author Gernot Belger
 */
public interface IMapModell extends IWorkbenchAdapter
{
  /**
   * Adds a listener to the list of listeners. Has no effect if the same listeners is already registered.
   */
  public void addMapModelListener( final IMapModellListener l );

  /**
   * Removes a listener from the list of listeners. Has no effect if the listeners is not registered.
   */
  public void removeMapModelListener( final IMapModellListener l );

  /** dispose off all themes! */
  public void dispose( );

  public void activateTheme( final IKalypsoTheme theme );

  public IKalypsoTheme getActiveTheme( );

  public void addTheme( final IKalypsoTheme theme );

  public void insertTheme( final IKalypsoTheme theme, final int position );

  /**
   * Gets all themes of this model. Does NOT recurse into cascaded themes.
   */
  public IKalypsoTheme[] getAllThemes( );

  /**
   * This function returns the name of the coordinate system used.
   * 
   * @return The name of the coordinate system.
   */
  public String getCoordinatesSystem( );

  /**
   * renders the map to the passed graphic context
   */
  public void paint( final Graphics g, final GeoTransform p, final GM_Envelope bbox, final double scale, final Boolean selected, final IProgressMonitor monitor ) throws CoreException;

  // TODO: remove position stuff
  public IKalypsoTheme getTheme( final int pos );

  public int getThemeSize( );

  public boolean isThemeActivated( final IKalypsoTheme theme );

  public void moveDown( IKalypsoTheme theme );

  public void moveUp( IKalypsoTheme theme );

  public void removeTheme( final IKalypsoTheme theme );

  public void swapThemes( IKalypsoTheme theme1, IKalypsoTheme theme2 );

  public GM_Envelope getFullExtentBoundingBox( );

  public IProject getProject( );

  // TODO: move to utility class
  public IKalypsoFeatureTheme getScrabLayer( );

  public void accept( final IKalypsoThemeVisitor visitor, int depth );

  /**
   * Iterates through all themes of this modell, starting at the given theme.
   * 
   * @see #accept(KalypsoThemeVisitor, int).
   */
  public void accept( final IKalypsoThemeVisitor visitor, final int depth, final IKalypsoTheme theme );

  public void setName( final I10nString name );

  public I10nString getName( );

  public void invalidate( final GM_Envelope bbox );

  // HACK In order to have nice parents for outline tree even for cascading themes, we something like this...
  public Object getThemeParent( final IKalypsoTheme theme );

  /**
   * Internal method for setting the active theme.
   * <p>
   * This method is not intended to be called from outside of {@link IMapModell} implementations.
   */
  public void internalActivate( IKalypsoTheme theme );

  /**
   * Check if this map modell is still beeing filled with themes.<br>
   * Implementors must ensure, that this flag becomes eventually <code>true</code> (even if there are errors while
   * loading).
   * 
   * @return <code>false</code> if this map modell is under construction (for example if many theme are about to be
   *         added in the near future...)
   */
  public boolean isLoaded( );
}