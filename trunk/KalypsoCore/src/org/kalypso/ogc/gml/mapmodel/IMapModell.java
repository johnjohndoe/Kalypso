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
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.feature.event.ModellEventProvider;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * TODO: getScale etc, hier rausschmeissen! die Umrechnung zwischen Bildschirm und Geokoordinaten ist Aufgabe des
 * MapPanel, die paint Mehtode hier sollte bereits mit Geokoordinaten arbeiten d.h. der Grafik-Kontext wird schon mit
 * umgerechneten Koordinaten übergeben
 * 
 * @author belger
 */
public interface IMapModell extends ModellEventProvider, ModellEventListener
{
  /** dispose off all themes! */
  public void dispose();

  public void activateTheme( final IKalypsoTheme theme );

  public IKalypsoTheme getActiveTheme();

  public void addTheme( final IKalypsoTheme theme );
  
  public void insertTheme( final IKalypsoTheme theme, final int position );  

  public void enableTheme( IKalypsoTheme theme, boolean status );

  public IKalypsoTheme[] getAllThemes();

  public CS_CoordinateSystem getCoordinatesSystem();

  /**
   * renders the map to the passed graphic context
   * 
   * @param g
   */
  public void paint( final Graphics g, final GeoTransform p, final GM_Envelope bbox, final double scale,
      final boolean select );

  public IKalypsoTheme getTheme( int pos );

  public int getThemeSize();

  public boolean isThemeActivated( IKalypsoTheme theme );

  public boolean isThemeEnabled( IKalypsoTheme theme );

  public void moveDown( IKalypsoTheme theme );

  public void moveUp( IKalypsoTheme theme );

  public void removeTheme( final IKalypsoTheme theme );

  public void setCoordinateSystem( CS_CoordinateSystem crs ) throws Exception;

  public void swapThemes( IKalypsoTheme theme1, IKalypsoTheme theme2 );

  public GM_Envelope getFullExtentBoundingBox();

  public IProject getProject();

  public IKalypsoFeatureTheme getScrabLayer();

  public void accept( KalypsoThemeVisitor visitor, int depth_infinite );
  
  public void accept( KalypsoThemeVisitor visitor, int depth_infinite, IKalypsoTheme theme );

  public void setName( String name );
  
  public String getName ();
  

}