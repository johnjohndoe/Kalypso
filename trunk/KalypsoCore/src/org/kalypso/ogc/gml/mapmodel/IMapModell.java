package org.kalypso.ogc.gml.mapmodel;

import java.awt.Graphics;

import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.deegree.model.geometry.GM_Envelope;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * TODO: getScale etc, hier rausschmeissen!
 *  die Umrechnung zwischen Bildschirm und Geokoordinaten ist Aufgabe des MapPanel,
 * die paint Mehtode hier sollte bereits mit Geokoordinaten arbeiten d.h. der Grafik-Kontext wird schon mit
 * umgerechneten Koordinaten übergeben 
 * 
 * @author belger
 */
public interface IMapModell
{
  public void activateTheme( final IKalypsoTheme theme );

  public IKalypsoTheme getActiveTheme();

  public void addTheme( final IKalypsoTheme theme );

  public void clear();

  public void enableTheme( IKalypsoTheme theme, boolean status );

  public IKalypsoTheme[] getAllThemes();

  public CS_CoordinateSystem getCoordinatesSystem();

  /**
   * renders the map to the passed graphic context
   * 
   * @param g
   */
  public void paintSelected( final Graphics g, final GeoTransform p, final GM_Envelope bbox, final double scale, final int selectionId );

  public IKalypsoTheme getTheme( int pos );

  public int getThemeSize();

  public boolean isThemeActivated( IKalypsoTheme theme );

  public boolean isThemeEnabled( IKalypsoTheme theme );

  public void moveDown( IKalypsoTheme theme );

  public void moveUp( IKalypsoTheme theme );

  public void removeTheme( IKalypsoTheme theme );

  public void setCoordinateSystem( CS_CoordinateSystem crs ) throws Exception;

  public void swapThemes( IKalypsoTheme theme1, IKalypsoTheme theme2 );

  public GM_Envelope getFullExtentBoundingBox();

  public void addModellListener( ModellEventListener listener );

  public void removeModellListener( ModellEventListener listener );

  public void fireModellEvent( final ModellEvent event );
}