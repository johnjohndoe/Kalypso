package org.kalypso.ogc.gml;

import java.awt.Graphics;

import org.deegree.graphics.sld.UserStyle;
import org.kalypso.ogc.MapModell;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.event.ModellEventListener;
import org.kalypso.ogc.event.ModellEventProvider;

/**
 * @author Katharina <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 *  
 */
public interface IKalypsoTheme extends ModellEventProvider, ModellEventListener
{
  public void dispose();
  
  /**
   * returns the name of the layer
   */
  public String getName();

  public void setName( final String name );

  /**
   * renders the layer to the submitted graphic context
   */
  public void paint( Graphics g );

  public void paintSelected( Graphics g, int selectionId );

  public UserStyle[] getStyles();

  public void addStyle( final KalypsoUserStyle style );

  public void removeStyle( final KalypsoUserStyle style );

  /**
   * returns the layer that holds the data of the theme
   */
  public KalypsoFeatureLayer getLayer();

  public void setParent( MapModell parent );
  
  public void addModellListener( final ModellEventListener listener );

  public void fireModellEvent( final ModellEvent event );

  public void removeModellListener( ModellEventListener listener );
}