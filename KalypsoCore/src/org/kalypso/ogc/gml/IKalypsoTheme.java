package org.kalypso.ogc.gml;

import java.awt.Graphics;

import org.deegree.graphics.sld.UserStyle;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.geometry.GM_Envelope;
import org.kalypso.ogc.gml.event.ModellEvent;
import org.kalypso.ogc.gml.event.ModellEventListener;
import org.kalypso.ogc.gml.event.ModellEventProvider;

/**
 * @author Katharina <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 *  
 */
public interface IKalypsoTheme extends ModellEventProvider, ModellEventListener
{
  public final static UserStyle[] NO_STYLE = new UserStyle[0];
  
  public void dispose();
  
  /**
   * returns the name of the layer
   */
  public String getName();

  public void setName( final String name );

//  public void paint( final Graphics g, final GeoTransform p, final double scale, final GM_Envelope bbox );

  /**
   * renders the layer to the submitted graphic context
   */
  public void paintSelected( final Graphics g, final GeoTransform p, final double scale, final GM_Envelope bbox, final int selectionId );

  public UserStyle[] getStyles();

  public void addStyle( final KalypsoUserStyle style );

  public void removeStyle( final KalypsoUserStyle style );

  /**
   * returns the layer that holds the data of the theme
   */
  public IKalypsoLayer getLayer();

  public void addModellListener( final ModellEventListener listener );

  public void fireModellEvent( final ModellEvent event );

  public void removeModellListener( ModellEventListener listener );
}